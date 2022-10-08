;;; sockets/conditions.lisp - Socket condition classes
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel@shunter.xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.sockets)



(define-condition socket-error (error)
  ((errno :initarg :errno :type fixnum
          :reader socket-error-errno))
  (:report (lambda (c s)
             (princ (ffi:strerror (socket-error-errno c)) s)))
  (:documentation
    "Error condition relating to creation or connection of a socket or a preexisting socket stream."))

(define-condition socket-stream-error (socket-error stream-error)
  ()
  (:documentation
    "Error condition relating to a preexisting socket stream."))
