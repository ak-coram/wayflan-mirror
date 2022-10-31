;;; test/test.lisp - Wayflan test suite prelude
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.test
  (:use #:cl #:org.shirakumo.parachute #:wayflan)
  (:local-nicknames (#:wire #:xyz.shunter.wayflan.wire)
                    (#:client #:xyz.shunter.wayflan.client)
                    (#:ffi #:xyz.shunter.wayflan.ffi)))

(in-package #:xyz.shunter.wayflan.test)
(declaim (optimize debug))



(defclass mock-socket (wire::%socket)
  ((messages-in :initform ())
   (messages-out :initform ())
   (fds-in :initform ())
   (fds-out :initform ())))

(defclass active-mock-socket (mock-socket) ())

(defun finish-mock (mocket)
  (with-slots (messages-in messages-out fds-in fds-out) mocket
    (setf messages-in (nreverse messages-in)
          messages-out (nreverse messages-out)
          fds-in (nreverse fds-in)
          fds-out (nreverse fds-out)))
  (change-class mocket 'active-mock-socket))

;; no-op
(defmethod wire:close-socket ((socket active-mock-socket))
  (true (null (slot-value socket 'messages-in)))
  (true (null (slot-value socket 'fds-in))))

(defmethod wire:listen-socket ((socket active-mock-socket))
  (and (slot-value socket 'messages-in) t))

(defmethod wire::%write-fd ((socket active-mock-socket) fd)
  (is = (pop (slot-value socket 'fds-out)) fd))

(defmethod wire::read-fd ((socket active-mock-socket))
  (pop (slot-value socket 'fds-in)))

(defmethod wire::%write-message ((socket active-mock-socket) carray size)
  (assert (>= size (* 2 wire::+wl-word-size+)))
  (let* ((sender-id (cffi:mem-aref carray :uint32 0))
         (length-and-opcode (cffi:mem-aref carray :uint32 1))
         (length (ldb (byte 16 16) length-and-opcode))
         (opcode (ldb (byte 16 0) length-and-opcode))
         (body-length (- (/ length wire::+wl-word-size+) 2))
         (array (make-array body-length :element-type 'wl-uint)))
    (dotimes (i (- (floor size wire::+wl-word-size+) 2))
      (setf (aref array i)
            (cffi:mem-aref carray :uint32 (+ i 2))))

    (destructuring-bind (sender-id* opcode* array*) (pop (slot-value socket 'messages-out))
      (is = sender-id* sender-id)
      (is = opcode* opcode)
      (is = (* (+ 2 (length array*)) wire::+wl-word-size+) length)
      (is equalp array* array))))

(defmethod wire::%call-with-message ((socket active-mock-socket) function)
  (destructuring-bind (sender-id opcode array)
                      (pop (slot-value socket 'messages-in))
    (cffi:with-foreign-object (cptr :uint32 (length array))
      (dotimes (i (length array))
        (setf (cffi:mem-aref cptr :uint32 i) (aref array i)))
      (funcall function sender-id opcode cptr
               (* (length array) wire::+wl-word-size+)))))

(defun contents-words (contents)
  (loop :for content :in contents
        :sum (etypecase content
               ((or wl-uint wl-int) 1)
               (vector (ceiling (length content) 4)))))

(defun contents-to-words (contents)
  (loop :with array := (make-array (contents-words contents)
                                   :element-type 'wl-uint)
        :with i := 0
        :for content :in contents
        :do (etypecase content
              (wl-uint
                (setf (aref array i) content)
                (incf i))
              (wl-int
                (setf (aref array i)
                      (cffi:with-foreign-object (cint :uint32)
                        (setf (cffi:mem-ref cint :int32) content)
                        (cffi:mem-ref cint :uint32)))
                (incf i))
              (vector
                (dotimes (ci (ceiling (length content) 4))
                  (setf (aref array i)
                        (cffi:with-foreign-object (cint :uint32)
                          (setf (cffi:mem-ref cint :uint8 0)
                                (aref content (+ 0 (* 4 ci)))
                                (cffi:mem-ref cint :uint8 1)
                                (aref content (+ 1 (* 4 ci)))
                                (cffi:mem-ref cint :uint8 2)
                                (aref content (+ 2 (* 4 ci)))
                                (cffi:mem-ref cint :uint8 3)
                                (aref content (+ 3 (* 4 ci))))
                          (cffi:mem-ref cint :uint32)))
                  (incf i))))
        :finally (return array)))

(defun expect-message (mocket sender-id opcode contents)
  (push (list sender-id opcode (contents-to-words contents))
        (slot-value mocket 'messages-out)))

(defun expect-fd (mocket fd)
  (push fd (slot-value mocket 'fds-out)))

(defun next-message (mocket sender-id opcode contents)
  (push (list sender-id opcode (contents-to-words contents))
        (slot-value mocket 'messages-in)))

(defun next-fd (mocket fd)
  (push fd (slot-value mocket 'fds-in)))

(defun make-wl-array (octets)
  (make-array (length octets)
              :element-type 'xyz.shunter.wayflan::octet
              :initial-contents octets))
