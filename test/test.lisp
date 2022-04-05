;;; test/test.lisp - Wayflan test suite prelude
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defpackage #:xyz.shunter.wayflan.test
  (:use #:cl)
  (:local-nicknames (#:wire #:xyz.shunter.wayflan.wire)
                    (#:client #:xyz.shunter.wayflan.client)
                    (#:a #:alexandria)
                    (#:io #:fast-io)
                    (#:p #:org.shirakumo.parachute)))

(in-package #:xyz.shunter.wayflan.test)



(defun contents-to-octets (contents)
  "Transform a sequence of unsigned integers (as words) and octet vectors into
   an octet vector."
  (io:with-fast-output (buffer)
    (flet ((write-item (item)
             (etypecase item
               ((unsigned-byte 32)
                #+big-endian
                (io:writeu32-be item buffer)
                #+little-endian
                (io:writeu32-le item buffer))
               (vector
                 (io:fast-write-sequence
                   (a:copy-array item :element-type '(unsigned-byte 8))
                   buffer)))))
      (map nil #'write-item contents))))

(defmacro with-input-from-contents ((buf input) &body body)
  `(io:with-fast-input (,buf (contents-to-octets ,input))
     ,@body))

(defun contents= (byte-array contents)
  "Check whether BYTE-ARRAY is represented by CONTENTS, a sequence of unsinged
integers (as words) and octet vectors."
  (io:with-fast-input (buffer (a:copy-array byte-array
                                            :adjustable nil
                                            :fill-pointer nil))
    (flet ((check-item (item)
             (etypecase item
               ((unsigned-byte 32)
                (= item
                   #+big-endian
                   (io:readu32-be buffer)
                   #+little-endian
                   (io:readu32-le buffer)))
               (vector
                 (let ((seq (make-array (length item)
                                        :element-type '(unsigned-byte 8))))
                   (io:fast-read-sequence seq buffer)
                   (equalp item seq))))))
      (map nil (lambda (item)
                 (unless (check-item item)
                   (return-from contents= nil)))
           contents)
      t)))
