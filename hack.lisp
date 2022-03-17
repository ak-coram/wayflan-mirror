(require :posix-shm)
(defpackage #:xyz.shunter.wayhack.hack
  (:use #:cl #:xyz.shunter.wayhack.client)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:xyz.shunter.wayhack.hack)



(defmacro with-open-display ((display &rest options) &body body)
  `(let ((,display (wl-display-connect ,@options)))
     (unwind-protect
       (progn ,@body)
       (wl-display-disconnect ,display))))

(defmacro and@ (&rest forms)
  (reduce (lambda (form body)
            `(a:when-let ((@ ,form))
               ,body))
          forms
          :from-end t))

(defclass superapp (wl-event-listener)
  ((compositor :accessor compositor)
   (shm :accessor shm)))

(defmethod handle-event ((listener superapp) registry (event wl-registry-global-event))
  (with-accessors ((name wl-event-name)
                   (interface wl-event-interface)
                   (version wl-event-version)) event
    (format t "GLOBAL ~D ~S v~D~%" name interface version)
    (case (and@ (find-interface-named interface)
                (class-name @))
      (wl-compositor
        (format t "Compositor with name ~D" name)
        (setf (compositor listener)
              (wl-registry-bind registry name 'wl-compositor)))

      (wl-shm
        (setf (shm listener)
              (wl-registry-bind registry name 'wl-shm))
        (push listener (wl-proxy-listeners (shm listener)))))))

(defmethod handle-event ((listener superapp) shm (event wl-shm-format-event))
  (format t "SHM format: ~A" (wl-event-format event)))

(defun run ()
  (with-open-display (display "wayland-2")
    (let ((superapp (make-instance 'superapp))
          (registry (wl-display-get-registry display)))
      (push superapp (wl-proxy-listeners registry))
      ;; Read in globals and bind to them when needed
      (wl-display-roundtrip display)
      (wl-display-roundtrip display))))

(defun run ()
  (with-open-display (display "/home/anon/hello.sock")
    (let ((registry (wl-display-get-registry display)))
      (wl-registry-bind registry 1 'wl-shm))))
