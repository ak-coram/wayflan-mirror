(defpackage #:xyz.shunter.wayhack.hack
  (:use #:cl #:xyz.shunter.wayhack.client))

(in-package #:xyz.shunter.wayhack.hack)

(defclass test-listener (wl-event-listener) ())

(defmethod handle-event ((listener test-listener) proxy (event wl-registry-global-event))
  (format t "#x~8,'0X  ~35A  v~D~%"
          (wl-event-name event)
          (wl-event-interface event)
          (wl-event-version event)))

(defun test ()
  (let ((display (wl-display-connect)))
    (unwind-protect
      (let ((registry (wl-display-get-registry display)))
        (push (make-instance 'test-listener)
              (wl-proxy-listeners registry))
        (wl-display-roundtrip display))
      (wl-display-disconnect display))))

(defmethod handle-event ((listener test-listener) proxy (event wl-display-error-event))
  (format t "Error: object-id=~D, code=~D, message=~S~%"
          (wl-event-object-id event)
          (wl-event-code event)
          (wl-event-message event)))

(define-request (wl-display-invalid-request wl-display 10)
  ((some-int :type :int))
  (:documentation "Make an invalid Wayland request"))

(defun test ()
  (let ((display (wl-display-connect)))
    (unwind-protect
      (progn
        (push (make-instance 'test-listener) (wl-proxy-listeners display))
        (wl-display-invalid-request display 10)
        (wl-display-roundtrip display))
      (wl-display-disconnect display))))
