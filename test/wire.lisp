;;; test/wire.lisp - Test suite for Wayflan's wire format implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defpackage #:xyz.shunter.wayflan.test
  (:use #:cl)
  (:local-nicknames (#:wire #:xyz.shunter.wayflan.wire)
                    (#:a #:alexandria)
                    (#:io #:fast-io)
                    (#:p #:org.shirakumo.parachute)))

(in-package #:xyz.shunter.wayflan.test)




(defun octet-vector-to-words (vector)
  (let* ((num-words (/ (length vector) 4))
         (result-vector (make-array num-words :element-type 'wire:wl-uint)))
    (fast-io:with-fast-input (buffer vector)
      (dotimes (i num-words)
        (setf (aref result-vector i)
              #+big-endian
              (io:readu32-be buffer)
              #+little-endian
              (io:readu32-le buffer))))
    result-vector))

(defun word-vector-to-octets (vector)
  (io:with-fast-output (buffer)
    (dotimes (i (length vector))
      #+big-endian
      (io:writeu32-be (aref vector i) buffer)
      #+little-endian
      (io:writeu32-le (aref vector i) buffer))))

(defun mixed-list-to-octets (list)
  (io:with-fast-output (buffer)
    (dolist (item list)
      (etypecase item
        ((unsigned-byte 32)
         #+big-endian
         (io:writeu32-be item buffer)
         #+little-endian
         (io:writeu32-le item buffer))
        (vector
          (io:fast-write-sequence (a:copy-array item :element-type '(unsigned-byte 8))
                                  buffer))))))

(defmacro with-output-to-words ((buf) &body body)
  `(octet-vector-to-words
     (io:with-fast-output (,buf)
       ,@body)))

(defmacro with-input-from-words ((buf input) &body body)
  `(io:with-fast-input (,buf (word-vector-to-octets ,input))
     ,@body))

(defmacro with-input-from-contents ((buf input) &body body)
  `(io:with-fast-input (,buf (mixed-list-to-octets ,input))
     ,@body))

(defun buffer= (byte-array list)
  (io:with-fast-input (buffer byte-array)
    (dolist (item list t)
      (unless (etypecase item
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
                    (equalp item seq))))
        (return nil)))))

(p:define-test read-wl-int
  (p:is equal '(#x0BADBEEF
                #x12345678
                #x70077007
                #x00000000)
        (with-input-from-words (buf #(#x0BADBEEF
                                      #x12345678
                                      #x70077007
                                      #x00000000))
          (list (wire:read-wl-int buf)
                (wire:read-wl-int buf)
                (wire:read-wl-int buf)
                (wire:read-wl-int buf)))
        "read-wl-int can read positive integers")

  (p:is equal '(#x-0BADBEEF
                #x-12345678
                #x-70077007
                #x-80000000
                -1)
        (with-input-from-words (buf #(#xF4524111
                                      #xEDCBA988
                                      #x8FF88FF9
                                      #x80000000
                                      #xFFFFFFFF))
          (list (wire:read-wl-int buf)
                (wire:read-wl-int buf)
                (wire:read-wl-int buf)
                (wire:read-wl-int buf)
                (wire:read-wl-int buf)))
        "read-wl-int can read negative integers"))

(p:define-test write-wl-int
  (p:is equalp #(#x0BADBEEF
                 #x12345678
                 #x70077007
                 #x7FFFFFFF
                 #x00000000)
        (with-output-to-words (buf)
          (wire:write-wl-int #x0BADBEEF buf)
          (wire:write-wl-int #x12345678 buf)
          (wire:write-wl-int #x70077007 buf)
          (wire:write-wl-int #x7FFFFFFF buf)
          (wire:write-wl-int #x00000000 buf))
        "write-wl-int can write positive integers")

  (p:is equalp #(#xF4524111
                 #xEDCBA988
                 #x8FF88FF9
                 #x80000000
                 #xFFFFFFFF)
        (with-output-to-words (buf)
          (wire:write-wl-int #x-0BADBEEF buf)
          (wire:write-wl-int #x-12345678 buf)
          (wire:write-wl-int #x-70077007 buf)
          (wire:write-wl-int #x-80000000 buf)
          (wire:write-wl-int -1 buf))
        "write-wl-int can write negative integers")

  (p:fail (io:with-fast-output (buf)
            (wire:write-wl-int #xDEADBEEF buf))
          type-error
          "write-wl-int can't write out-of-bounds integers")

  (p:fail (io:with-fast-output (buf)
            (wire:write-wl-int #x80000000 buf))
          type-error
          "write-wl-int can't write out-of-bounds integers")

  (p:fail (io:with-fast-output (buf)
            (wire:write-wl-int #x-80000001 buf))
          type-error
          "write-wl-int can't write out-of-bounds integers"))

(p:define-test read-wl-uint
  (p:is equal '(#xDEADBEEF
                #x01234567
                #x89ABCDEF
                #xFFFFFFFF
                #X00000000)
        (with-input-from-words (buf #(#xDEADBEEF
                                      #x01234567
                                      #x89ABCDEF
                                      #xFFFFFFFF
                                      #X00000000))
          (list (wire:read-wl-uint buf)
                (wire:read-wl-uint buf)
                (wire:read-wl-uint buf)
                (wire:read-wl-uint buf)
                (wire:read-wl-uint buf)))
        "read-wl-uint can read positive integers"))

(p:define-test write-wl-uint
  (p:is equalp #(#xDEADBEEF
                 #x01234567
                 #x89ABCDEF
                 #xFFFFFFFF
                 #x00000000)
        (with-output-to-words (buf)
          (wire:write-wl-uint #xDEADBEEF buf)
          (wire:write-wl-uint #x01234567 buf)
          (wire:write-wl-uint #x89ABCDEF buf)
          (wire:write-wl-uint #xFFFFFFFF buf)
          (wire:write-wl-uint #x00000000 buf))
        "write-wl-uint can write positive integers")

  (p:fail (io:with-fast-output (buf)
            (wire:write-wl-uint -1 buf))
          type-error
          "write-wl-uint can't write negative integers")

  (p:fail (io:with-fast-output (buf)
            (wire:write-wl-uint #x100000000 buf))
          type-error
          "write-wl-uint can't write out-of-bounds integers"))

(p:define-test read-wl-fixed
  (p:is equal '(#x12345624/100
                #x77777780/100
                #x000111FF/100
                #x112233
                #x7FFFFFFF/100)
        (with-input-from-words (buf #(#x12345624
                                      #x77777780
                                      #x000111FF
                                      #x11223300
                                      #x7FFFFFFF))
          (list (wire:read-wl-fixed buf)
                (wire:read-wl-fixed buf)
                (wire:read-wl-fixed buf)
                (wire:read-wl-fixed buf)
                (wire:read-wl-fixed buf)))
        "read-wl-fixed can read fixed numbers."))

(p:define-test write-wl-fixed
  ;; TODO figure out what the spec wants for negative numbers. Wayland's spec
  ;; implies that it's sign-and-magnitude, but I think libwayland implies it's
  ;; whatever the host system is.
  (p:is equalp #(#x12345624
                 #x77777780
                 #x000111FF
                 #x11223300
                 #x7FFFFFFF)
        (with-output-to-words (buf)
          (wire:write-wl-fixed #x12345624/100 buf)
          (wire:write-wl-fixed #x77777780/100 buf)
          (wire:write-wl-fixed #x000111FF/100 buf)
          (wire:write-wl-fixed #x112233 buf)
          (wire:write-wl-fixed #x7FFFFFFF/100 buf))
        "write-wl-fixed can write fixed numbers")

  (p:fail (io:with-fast-output (buf)
            (wire:write-wl-fixed #x1000000 buf))
          type-error
          "write-wl-fixed can't write out-of-bounds numbers."))

(p:define-test read-wl-string
  (p:is string= "hello world"
        (with-input-from-contents (buf '(12
                                         #(#x68 #x65 #x6c #x6c
                                           #x6f #x20 #x77 #x6f
                                           #x72 #x6c #x64 #x00)))
          (wire:read-wl-string buf))
        "read-wl-string correctly reads a string with a null terminator")

  (p:is string= "hello"
        (with-input-from-contents (buf '(6
                                         #(#x68 #x65 #x6c #x6c
                                           #x6f #x00 #x00 #x00)))
          (wire:read-wl-string buf))
        "read-wl-string correctly factors in word padding")

  (p:is string= "abcd"
        (with-input-from-contents (buf '(5
                                         #(#x61 #x62 #x63 #x64
                                           #x00 #x00 #x00 #x00)))
          (wire:read-wl-string buf))
        "read-wl-string factors in the null terminator when padding"))

(p:define-test write-wl-string
  (p:is buffer= '(12
                  #(#x68 #x65 #x6c #x6c
                    #x6f #x20 #x77 #x6f
                    #x72 #x6c #x64 #x00))
        (io:with-fast-output (buf)
          (wire:write-wl-string "hello world" buf))
        "write-wl-string correctly writes a string with a null terminator")

  (p:is buffer= '(6
                  #(#x68 #x65 #x6c #x6c
                    #x6f #x00 #x00 #x00))
        (io:with-fast-output (buf)
          (wire:write-wl-string "hello" buf))
        "write-wl-string correctly pads to the word")

  (p:is buffer= '(5
                  #(#x61 #x62 #x63 #x64
                    #x00 #x00 #x00 #x00))
        (io:with-fast-output (buf)
          (wire:write-wl-string "abcd" buf))
        "write-wl-string correctly factors in the null terminator when padding"))

(p:define-test read-wl-array
  (p:is equalp #(2 3 5 8)
        (with-input-from-contents (buf '(4
                                         #(2 3 5 8)))
          (wire:read-wl-array buf))
        "read-wl-array correctly reads an array")

  (p:is equalp #(2 3 5 8 13)
        (with-input-from-contents (buf '(5
                                         #(2 3 5 8
                                           13 0 0 0)))
          (wire:read-wl-array buf))
        "read-wl-array factors in word padding"))

(p:define-test write-wl-array
  (p:is buffer= '(4
                  #(2 3 5 8))
        (io:with-fast-output (buf)
          (wire:write-wl-array
            (make-array 4 :element-type '(unsigned-byte 8)
                        :initial-contents '(2 3 5 8))
            buf))
        "write-wl-array correctly writes an array")

  (p:is buffer= '(5
                  #(2 3 5 8
                    13 0 0 0))
        (io:with-fast-output (buf)
          (wire:write-wl-array
            (make-array 5 :element-type '(unsigned-byte 8)
                        :initial-contents '(2 3 5 8 13))
            buf))
        "write-wl-array correctly pads to the word"))