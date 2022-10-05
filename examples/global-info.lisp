;;; examples/global-info.lisp - Grab info of all interfaces and shm formats
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter <dot> xyz>
;;; All rights reserved.

(defpackage #:xyz.shunter.wayflan.examples.global-info
  (:use #:cl #:wayflan-client)
  (:local-nicknames (#:a #:alexandria))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.global-info)



(defun handle-shm (&rest event)
  (event-case event
    (:format (format)
     (format t "~S~%" format))))

(defun run ()
  (with-open-display (display)
    (let (registry shm-name)
      (setf registry (wl-display.get-registry display))
      (push (evlambda
              (:global (name interface version)
               (format t "#x~8,'0X ~32S v~D~%"
                       name interface version)
               (when (eq (a:when-let ((it (find-interface-named interface)))
                           (class-name it))
                         'wl-shm)
                 (setf shm-name name))))
            (wl-proxy-hooks registry))

      (format t "wl-registry globals:~%")
      (wl-display-roundtrip display)

      (push 'handle-shm
            (wl-proxy-hooks (wl-registry.bind registry shm-name 'wl-shm 1)))
      (format t "~%wl-shm formats:~%")
      (wl-display-roundtrip display)))
  (values))
