;;; examples/checkerboxed-demo.lisp
;;;
;;; This program is a rewrite of the extended example code found in Drew
;;; Devault's The Wayland Protocol, §7.3 thru §8.2. The program creates
;;; a toplevel surface that shows a moving checkerboard grid.
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.examples.checkerboxed-demo
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.checkerboxed-demo)



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
      (shm:with-open-shm-and-mmap* (shm pool-data (:direction :io) (size))
        (with-proxy (pool (wl-shm.create-pool wl-shm (shm:shm-fd shm) size))
          (setf buffer (wl-shm-pool.create-buffer
                         pool 0 width height stride
                         :xrgb8888)))

        ;; Draw checkerboxed background
        (dotimes (y height)
          (dotimes (x width)
            (setf (cffi:mem-aref pool-data :uint32 (+ (* y width) x))
                  (if (< (mod (+ (+ x offset*)
                                 (* (floor (+ y offset*) 8) 8)) 16)
                         8)
                      #xff666666
                      #xffeeeeee))))

        (push (evelambda
                (:release ()
                 ;; Sent by the compositor when it's no longer using this buffer.
                 (destroy-proxy buffer)))
              (wl-proxy-hooks buffer))
        buffer))))

(defun handle-frame-callback (app callback &rest event)
  (event-ecase event
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
           (format t "found shm~%")
           (setf wl-shm (wl-registry.bind
                          registry name 'wl-shm 1)))
         (wl-compositor
           (format t "found compositor~%")
           (setf wl-compositor (wl-registry.bind
                                 registry name 'wl-compositor 4)))
         (xdg-wm-base
           (format t "found xdg~%")
           (setf xdg-wm-base (wl-registry.bind
                               registry name 'xdg-wm-base 1))
           (push (evelambda
                   (:ping (serial)
                    (xdg-wm-base.pong xdg-wm-base serial)))
                 (wl-proxy-hooks xdg-wm-base))))))))

(defun run ()
  (let ((app (make-instance 'superapp)))
    (with-slots (wl-display wl-registry wl-shm wl-compositor
                            xdg-wm-base wl-surface xdg-surface xdg-toplevel) app
      (with-open-display (display)
        ;; Register all globals
        (setf wl-display display
              wl-registry (wl-display.get-registry display))
        (push (a:curry 'handle-registry app wl-registry)
              (wl-proxy-hooks wl-registry))
        (wl-display-roundtrip display)

        ;; Create the surface & give it the toplevel role
        (setf wl-surface (wl-compositor.create-surface wl-compositor)
              xdg-surface (xdg-wm-base.get-xdg-surface
                            xdg-wm-base wl-surface)
              xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
        (push (evlambda
                (:close ()
                 (return-from run)))
              (wl-proxy-hooks xdg-toplevel))
        (push (evelambda
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

        (loop (wl-display-dispatch-event display))))))
