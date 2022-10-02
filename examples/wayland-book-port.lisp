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

(defclass superapp (wl-event-listener)
  (;; Display Globals
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

(defmethod handle-event ((app superapp) buffer (event wl-buffer.release-event))
  ;; Destroy the buffer when it's no longer being used by the compositor
  (destroy-proxy buffer))

(defun draw-frame (app)
  "Create and return a buffer drawn with a checkerboard pattern."
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

        (push app (wl-proxy-listeners buffer))
        buffer))))

(defmethod handle-event ((app superapp) surface (event xdg-surface.configure-event))
  (xdg-surface.ack-configure surface (wl-event-serial event))
  (let ((buffer (draw-frame app)))
    (with-slots (wl-surface) app
      (wl-surface.attach wl-surface buffer 0 0)
      (wl-surface.commit wl-surface))))

(defmethod handle-event ((app superapp) xdg-wm-base (event xdg-wm-base.ping-event))
  (xdg-wm-base.pong xdg-wm-base (wl-event-serial event)))

(defmethod handle-event ((app superapp) sender (event xdg-toplevel.close-event))
  (throw :close-app (values)))

(defmethod handle-event ((app superapp) registry (event wl-registry.global-event))
  (with-accessors ((name wl-event-name)
                   (interface wl-event-interface)
                   (version wl-event-version)) event
    (with-slots (wl-compositor wl-shm xdg-wm-base) app
      (case (a:when-let ((it (find-interface-named interface)))
              (class-name it))
        (wl-shm
          (setf wl-shm (wl-registry.bind registry name 'wl-shm 1)))
        (wl-compositor
          (setf wl-compositor (wl-registry.bind registry name 'wl-compositor 4)))
        (xdg-wm-base
          (setf xdg-wm-base (wl-registry.bind registry name 'xdg-wm-base 1))
          (push app (wl-proxy-listeners xdg-wm-base)))))))

(defmethod handle-event ((app superapp) display event)
  ;; Ignore all un-handled events
  (declare (ignore app display event)))

(defclass frame-listener (wl-event-listener)
  ((app :initarg :app))
  (:documentation "Request & submit a frame on callback"))

(defmethod handle-event ((listener frame-listener) callback event)
  (let* ((app (slot-value listener 'app))
         (time (wl-event-callback-data event))
         (wl-surface (slot-value app 'wl-surface)))
    (with-slots (last-frame offset) app
      ;; Destroy this callback
      (destroy-proxy callback)

      ;; Request another frame
      (setf callback (wl-surface.frame wl-surface))
      (push listener (wl-proxy-listeners callback))

      ;; Update scroll amount at 24px/second
      (unless (zerop last-frame)
        (incf offset (* (/ (- time last-frame) 1000.0) 24)))

      ;; Submit a new frame for this event
      (let ((wl-buffer (draw-frame app)))
        (wl-surface.attach wl-surface wl-buffer 0 0)
        (wl-surface.damage-buffer
          wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
        (wl-surface.commit wl-surface))

      (setf last-frame time))))

(defun run ()
  (let* ((app (make-instance 'superapp))
         (frame-listener (make-instance 'frame-listener :app app)))
    (with-slots (wl-display wl-registry wl-compositor wl-surface xdg-surface
                            xdg-wm-base xdg-toplevel) app
      (with-open-display (display)
        (setf wl-display display
              wl-registry (wl-display.get-registry display))
        (push app (wl-proxy-listeners wl-registry))
        (wl-display-roundtrip display)

        (setf wl-surface (wl-compositor.create-surface wl-compositor)
              xdg-surface (xdg-wm-base.get-xdg-surface
                            xdg-wm-base wl-surface)
              xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
        (push app (wl-proxy-listeners xdg-surface))
        (push app (wl-proxy-listeners xdg-toplevel))
        (xdg-toplevel.set-title xdg-toplevel "Example client")
        (wl-surface.commit wl-surface)

        (let ((cb (wl-surface.frame wl-surface)))
          (push frame-listener (wl-proxy-listeners cb)))

        (catch :close-app
               (loop (wl-display-dispatch-event display)))))))
