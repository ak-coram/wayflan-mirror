(in-package #:xyz.shunter.wayhack.client)

(xyz.shunter.wayhack.autowrap:wl-include
  #P"protocols/wayland.xml"
  :export t)



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
