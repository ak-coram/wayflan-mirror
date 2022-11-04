;;; conditions.lisp -- Conditions raised during Wayland communication
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan)



(define-condition wl-error (error)
  ()
  (:documentation "Error signaled during Wayland communication"))

(define-condition wl-socket-error (wl-error)
  ((%summary :type string :initarg :summary)
   (%errno :type fixnum :initarg :errno
           :initform xyz.shunter.wayflan.ffi:*errno*))
  (:report (lambda (c s)
             (with-slots (%summary %errno) c)
             (format s "~A: ~A" c (xyz.shunter.wayflan.ffi:strerror s))))
  (:documentation
    "Error signaled due to an issue in a Wayland connection's underlying socket"))

;; TODO raise this error if there is no opcode defined for an event
(define-condition wl-message-error (wl-error)
  ((%summary :type (or string null) :initarg :summary :initform nil))
  (:report (lambda (c s)
             (format s "Malformed Wayland message~@[: ~A~]"
                     (slot-value c '%summary))))
  (:documentation
    "Error signaled due to a malformed Wayland message"))

(define-condition wl-server-error (wl-error)
  ((%object :initarg :object)
   (%code :initarg :code)
   (%message :type string :initarg :message))
  (:report (lambda (c s)
             (with-slots (%object %code %message) c
               (format s "Wayland server sent error code ~D on ~S: ~A"
                       %code %object %message)))))
