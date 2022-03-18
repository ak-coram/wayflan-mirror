(require :posix-shm)
(defpackage #:xyz.shunter.wayflan.hack
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:a #:alexandria)))

(in-package #:xyz.shunter.wayflan.hack)



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

(defclass superapp (wl-event-listener) ())

(defmethod handle-event ((listener superapp) registry (event wl-registry-global-event))
  (with-accessors ((name wl-event-name)
                   (interface wl-event-interface)
                   (version wl-event-version)) event
    (format t "GLOBAL #X~8,'0X  ~35S  v~D~%"
            name interface version)
    (case (and@ (find-interface-named interface)
                (class-name @))
      (wl-compositor
        (format t "Found compositor v~D with name #X~8,'0X~%" version name))
      (wl-shm
        (format t "Found shm v~D with name #X~8,'0X~%" version name)))))

(defmethod handle-event ((listener superapp) shm (event wl-shm-format-event))
  (format t "SHM format: ~A" (wl-event-format event)))

(defun run ()
  (with-open-display (display)
    (let ((superapp (make-instance 'superapp))
          (registry (wl-display-get-registry display)))
      (push superapp (wl-proxy-listeners registry))
      ;; Read in globals and bind to them when needed
      (wl-display-roundtrip display)
      (wl-display-roundtrip display))))
