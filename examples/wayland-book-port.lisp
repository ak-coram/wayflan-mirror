;;; examples/wayland-book-port.lisp
;;;
;;; This program is a rewrite of the extended example code found in Drew
;;; Devault's The Wayland Protocol, §7.3 thru §8.2. The program creates
;;; a toplevel surface that shows a moving checkerboard grid.
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; All rights reserved.

(require :wayflan)
(require :posix-shm)

(defpackage #:xyz.shunter.wayflan.client.examples.wayland-book-port
  (:use #:cl
        #:wayflan-client
        #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm)))

(in-package #:xyz.shunter.wayflan.client.examples.wayland-book-port)



(defconstant +most-positive-wl-int+ (1- (ash 1 31)))

(defmacro with-open-display ((display &rest options) &body body)
  "Like WITH-OPEN-FILE, but binding DISPLAY to the result of WL-DISPLAY-CONNECT instead of OPEN.
Executes the body with DISPLAY bound to a freshly connected display."
  `(let ((,display (wl-display-connect ,@options)))
     (unwind-protect
       (progn ,@body)
       (wl-display-disconnect ,display))))

(defmacro with-proxy ((var value) &body body)
  "Bind the proxy variable VAR to VALUE, and destroy it when execution leaves the body."
  `(let ((,var ,value))
     (unwind-protect (progn ,@body)
       (destroy-proxy ,var))))

(defmacro event-case (event &body clauses)
  (a:once-only (event)
    `(case (car ,event)
       ,@(mapcar (lambda (clause)
                   (destructuring-bind (event-name lambda-list &body body) clause
                     `(,event-name
                        (destructuring-bind ,lambda-list (rest ,event)
                          ,@body))))
                 clauses))))

(defmacro elambda (&body clauses)
  (a:with-gensyms (event)
    `(lambda (&rest ,event)
       (event-case ,event ,@clauses))))



(defclass superapp ()
  (;; Globals
   wl-display
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel

   ;; State
   (offset :initform 0.0)
   (last-frame :initform 0))
  (:documentation "An example Wayland application"))

(defun draw-frame (app)
  (with-slots (wl-shm offset) app
    (let* ((width 640)
           (height 480)
           (stride (* width 4))
           (size (* stride height))
           (offset* (floor (mod offset 8)))
           buffer)
      (shm:with-open-shm-and-mmap* (shm pool-data (:direction :io :permissions '(:user-all))
                                        ((cffi:null-pointer) size '(:read :write) 0))
        (with-proxy (pool (wl-shm.create-pool wl-shm (shm:shm-fd shm) size))
          (setf buffer (wl-shm-pool.create-buffer
                         pool 0 width height stride
                         +wl-shm.format-xrgb8888+)))

        ;; Draw checkerboxed background
        (dotimes (y height)
          (dotimes (x width)
            (setf (cffi:mem-aref pool-data :uint32 (+ (* y width) x))
                  (if (< (mod (+ (+ x offset*)
                                 (* (floor (+ y offset*) 8) 8)) 16)
                         8)
                      #xff666666
                      #xffeeeeee))))

        (push (elambda
                (:release ()
                 (destroy-proxy buffer)))
              (wl-proxy-hooks buffer))
        buffer))))

(defun handle-frame-callback (app callback &rest event)
  (event-case event
    (:done (time)
     (with-slots (last-frame offset wl-surface) app
       ;; Destroy this callback
       (destroy-proxy callback)

       ;; Request another frame
       (setf callback (wl-surface.frame wl-surface))
       (push (a:curry 'handle-frame-callback app callback)
             (wl-proxy-hooks callback))

      ;; Update scroll amount at 24px/second
      (unless (zerop last-frame)
        (incf offset (* (/ (- time last-frame) 1000.0) 24)))

      ;; Submit a new frame for this event
      (let ((wl-buffer (draw-frame app)))
        (wl-surface.attach wl-surface wl-buffer 0 0)
        (wl-surface.damage-buffer
          wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
        (wl-surface.commit wl-surface))

      (setf last-frame time)))))

(defun handle-registry (app registry &rest event)
  (with-slots (wl-shm wl-compositor xdg-wm-base) app
    (event-case event
      (:global (name interface version)
       (declare (ignore version))
       (case (a:when-let ((it (find-interface-named interface)))
               (class-name it))
         (wl-shm
           (setf wl-shm (wl-registry.bind
                          registry name 'wl-shm 1)))
         (wl-compositor
           (setf wl-compositor (wl-registry.bind
                                 registry name 'wl-compositor 4)))
         (xdg-wm-base
           (setf xdg-wm-base (wl-registry.bind
                               registry name 'xdg-wm-base 1))
           (push (elambda
                   (:ping (serial)
                    (xdg-wm-base.pong xdg-wm-base serial)))
                 (wl-proxy-hooks xdg-wm-base))))))))

(defun run ()
  (let ((app (make-instance 'superapp)))
    (with-slots (wl-display wl-registry wl-shm wl-compositor
                            xdg-wm-base wl-surface xdg-surface xdg-toplevel) app
      (with-open-display (display)
        (setf wl-display display
              wl-registry (wl-display.get-registry display))
        (push (a:curry 'handle-registry app wl-registry)
              (wl-proxy-hooks wl-registry))
        (wl-display-roundtrip display)

        (setf wl-surface (wl-compositor.create-surface wl-compositor)
              xdg-surface (xdg-wm-base.get-xdg-surface
                            xdg-wm-base wl-surface)
              xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
        (push (elambda
                (:close () (throw :close-app (values))))
              (wl-proxy-hooks xdg-toplevel))
        (push (elambda
                (:configure (serial)
                 (xdg-surface.ack-configure xdg-surface serial)
                 (let ((buffer (draw-frame app)))
                   (wl-surface.attach wl-surface buffer 0 0)
                   (wl-surface.commit wl-surface))))
              (wl-proxy-hooks xdg-surface))
        (xdg-toplevel.set-title xdg-toplevel "Example client")
        (wl-surface.commit wl-surface)

        (let ((cb (wl-surface.frame wl-surface)))
          (push (a:curry 'handle-frame-callback app cb)
                (wl-proxy-hooks cb)))

        (catch :close-app
               (loop (wl-display-dispatch-event display)))))))
