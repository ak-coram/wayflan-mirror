(require :wayflan)
(require :posix-shm)
(defpackage #:xyz.shunter.wayflan.hack
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm)
                    (#:xdg #:wayflan-client.xdg-shell)))

(in-package #:xyz.shunter.wayflan.hack)



(defmacro with-open-display ((display &rest options) &body body)
  "Like WITH-OPEN-FILE, but binding DISPLAY to the result of WL-DISPLAY-CONNECT instead of OPEN.
Executes the body with DISPLAY bound to a freshly connected display."
  `(let ((,display (wl-display-connect ,@options)))
     (unwind-protect
       (progn ,@body)
       (wl-display-disconnect ,display))))

(defclass superapp (wl-event-listener)
  (;; Display Globals
   (wl-display :initarg :display)
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel)
  (:documentation "An example Wayland application"))

(defmethod handle-event ((app superapp) buffer (event wl-buffer-release-event))
  ;; Destroy the buffer when it's no longer being used by the compositor
  (wl-buffer-destroy buffer))

(defun draw-frame (app)
  "Create and return a buffer drawn with a checkerboard pattern."
  (let* ((width 640)
         (height 480)
         (stride (* width 4))
         (size (* stride height))
         pool buffer)
    (with-slots (wl-shm) app
      (posix-shm:with-mmapped-shm* (fd pool-data (:direction :io :permissions '(:user-all))
                                       ((cffi:null-pointer) size '(:read :write) 0))
        (setf pool (wl-shm-create-pool wl-shm fd size)
              buffer (wl-shm-pool-create-buffer pool 0 width height stride
                                                +wl-shm-format-xrgb8888+))

        (wl-shm-pool-destroy pool)

        (dotimes (y height)
          (dotimes (x width)
            (setf (cffi:mem-aref pool-data :uint32 (+ (* y width) x))
                  (if (< (+ x (- y (mod y 8))) 8)
                      #xff666666
                      #xffeeeeee))))

        (push app (wl-proxy-listeners buffer))
        buffer))))

(defmethod handle-event ((app superapp) surface (event xdg:xdg-surface-configure-event))
  (xdg:xdg-surface-ack-configure surface (xdg:wl-event-serial event))
  (let ((buffer (draw-frame app)))
    (with-slots (wl-surface) app
      (wl-surface-attach wl-surface buffer 0 0)
      (wl-surface-commit wl-surface))))

(defmethod handle-event ((app superapp) xdg-wm-base (event xdg:xdg-wm-base-ping-event))
  (xdg:xdg-wm-base-pong xdg-wm-base (xdg:wl-event-serial event)))

(defmethod handle-event :before ((app superapp) sender event)
  ;; Report all events to output for debugging.
  (unless (typep event 'wl-registry-global-event)
    (format t "Handling ~A for ~S~%" (class-of event) sender)))

(defmethod handle-event ((app superapp) registry (event wl-registry-global-event))
  (declare (optimize debug))
  (with-accessors ((name wl-event-name)
                   (interface wl-event-interface)
                   (version wl-event-version)) event
    (with-slots (wl-compositor wl-shm xdg-wm-base) app
      (case (a:when-let ((it (find-interface-named interface)))
              (class-name it))
        (wl-compositor
          (format t "Found wl-compositor~%")
          (setf wl-compositor (wl-registry-bind registry name 'wl-compositor 4)))
        (wl-shm
          (Format t "Found wl-shm~%")
          (setf wl-shm (wl-registry-bind registry name 'wl-shm 1)))
        (xdg:xdg-wm-base
          (format t "Found xdg-wm-base~%")
          (setf xdg-wm-base (wl-registry-bind registry name 'xdg:xdg-wm-base 1))
          (push app (wl-proxy-listeners xdg-wm-base)))))))

(defmethod handle-event ((app superapp) display (event wl-display-delete-id-event))
  ;; Print-debugging
  (format t "Object # ~D deleted~%" (wl-event-id event)))

(defmethod handle-event ((app superapp) display event)
  ;; Ignore all un-handled events
  (declare (ignore app display event)))

(defun run ()
  (declare (optimize debug))
  (with-open-display (display "wayland-2")
    (let ((app (make-instance 'superapp :display display)))
      (with-slots (wl-registry wl-compositor wl-surface xdg-surface
                               xdg-wm-base xdg-toplevel) app
        (setf wl-registry (wl-display-get-registry display))
        (push app (wl-proxy-listeners wl-registry))
        ;; (push app (wl-proxy-listeners display))
        (wl-display-roundtrip display)

        (setf wl-surface (wl-compositor-create-surface wl-compositor)
              xdg-surface (xdg:xdg-wm-base-get-xdg-surface
                            xdg-wm-base wl-surface)
              xdg-toplevel (xdg:xdg-surface-get-toplevel xdg-surface))
        (push app (wl-proxy-listeners xdg-surface))
        (push app (wl-proxy-listeners xdg-toplevel))
        (xdg:xdg-toplevel-set-title xdg-toplevel "Example client")
        (wl-surface-commit wl-surface)

        (loop (print "Dispatching event...")
              (wl-display-dispatch-event display))))))
