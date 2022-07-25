(require :wayflan)
(require :posix-shm)

(defpackage #:hack
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm)
                    (#:xdg #:wayflan-client.xdg-shell)))

(in-package #:hack)



(xyz.shunter.wayflan.autowrap:wl-include
  #.(merge-pathnames #P"protocols/xdg-decoration-unstable-v1.xml"
                     (asdf:system-source-directory '#:wayflan)))

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

(define-condition close-app ()
  ())

(defclass superapp (wl-event-listener)
  (;; Display Globals
   (wl-display :initarg :display)
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base
   zxdg-decoration-manager

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel
   zxdg-decoration

   ;; State
   (offset :initform 0)
   (last-frame :initform 0))
  (:documentation "An example Wayland application"))

(defmethod handle-event ((app superapp) buffer (event wl-buffer-release-event))
  ;; Destroy the buffer when it's no longer being used by the compositor
  (destroy-proxy buffer))

(defun draw-frame (app)
  "Create and return a buffer drawn with a checkerboard pattern."
  (with-slots (wl-shm offset) app
    (let* ((width 640)
           (height 480)
           (stride (* width 4))
           (size (* stride height))
           (offset* (mod offset 8))
           pool
           buffer)
      (posix-shm:with-mmapped-shm* (fd pool-data (:direction :io :permissions '(:user-all))
                                       ((cffi:null-pointer) size '(:read :write) 0))
        (setf pool (wl-shm-create-pool wl-shm fd size)
              buffer (wl-shm-pool-create-buffer pool 0 width height stride
                                                +wl-shm-format-xrgb8888+))

        (destroy-proxy pool)

        (dotimes (y height)
          (dotimes (x width)
            (setf (cffi:mem-aref pool-data :uint32 (+ (* y width) x))
                  (if (< (mod (+ (+ x offset*) (* (floor (+ y offset*) 8) 8)) 16) 8)
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

(defmethod handle-event ((app superapp) sender (event xdg:xdg-toplevel-close-event))
  (signal 'close-app))

(defmethod handle-event :before ((app superapp) sender event)
  ;; Report all events to output for debugging.
  (unless (typep event 'wl-registry-global-event)
    ;;(format t "Handling ~A for ~S~%" (class-of event) sender)
    ))

(defmethod handle-event ((app superapp) registry (event wl-registry-global-event))
  (with-accessors ((name wl-event-name)
                   (interface wl-event-interface)
                   (version wl-event-version)) event
    (with-slots (wl-compositor wl-shm xdg-wm-base zxdg-decoration-manager) app
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
          (push app (wl-proxy-listeners xdg-wm-base)))
        (zxdg-decoration-manager-v1
          (format t "Found zxdg-decoration-manager-v1")
          (setf zxdg-decoration-manager (wl-registry-bind registry name 'zxdg-decoration-manager-v1 1))
          (push app (wl-proxy-listeners zxdg-decoration-manager)))))))

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
      (setf callback (wl-surface-frame wl-surface))
      (push listener (wl-proxy-listeners callback))

      ;; Update scroll amount at 24px/second
      (unless (zerop last-frame)
        (incf offset (* (/ (- time last-frame) 1000.0) 24)))

      ;; Submit a new frame for this event
      (let ((wl-buffer (draw-frame app)))
        (wl-surface-attach wl-surface wl-buffer 0 0)
        (wl-surface-damage-buffer wl-surface 100 100 200 200)
        (wl-surface-commit wl-surface))

      (setf last-frame time))))

(defun run ()
  (declare (optimize debug))
  (with-open-display (display)
    (let ((app (make-instance 'superapp :display display)))
      (with-slots (wl-registry wl-compositor wl-surface xdg-surface
                               xdg-wm-base xdg-toplevel
                               zxdg-decoration-manager
                               zxdg-decoration) app
        (setf wl-registry (wl-display-get-registry display))
        (push app (wl-proxy-listeners wl-registry))
        (wl-display-roundtrip display)

        ;; Create the surface & commit its first frame
        (setf wl-surface (wl-compositor-create-surface wl-compositor)
              xdg-surface (xdg:xdg-wm-base-get-xdg-surface
                            xdg-wm-base wl-surface)
              xdg-toplevel (xdg:xdg-surface-get-toplevel xdg-surface)
              zxdg-decoration (zxdg-decoration-manager-v1-get-toplevel-decoration
                                zxdg-decoration-manager xdg-toplevel))
        (push app (wl-proxy-listeners xdg-surface))
        (push app (wl-proxy-listeners xdg-toplevel))
        (xdg:xdg-toplevel-set-title xdg-toplevel "Example client")
        (zxdg-toplevel-decoration-v1-set-mode
          zxdg-decoration
          +zxdg-toplevel-decoration-v1-mode-server-side+)
        (with-proxy (region (wl-compositor-create-region wl-compositor))
          (wl-region-add region 0 0 10000 10000)
          (wl-surface-set-opaque-region wl-surface region))
        (wl-surface-commit wl-surface)

        (let ((cb (wl-surface-frame wl-surface)))
          (push (make-instance 'frame-listener :app app)
                (wl-proxy-listeners cb)))

        (handler-case (loop (wl-display-dispatch-event display))
          (close-app ()))))))
