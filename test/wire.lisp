;;; test/wire.lisp - Wayflan test suite for wire format implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.test)



(p:define-test read-wl-int
  (p:is equal '(#x0BADBEEF
                #x12345678
                #x70077007
                #x00000000)
        (with-input-from-contents (buf #(#x0BADBEEF
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
        (with-input-from-contents (buf #(#xF4524111
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
  (p:is contents= #(#x0BADBEEF
                    #x12345678
                    #x70077007
                    #x7FFFFFFF
                    #x00000000)
        (io:with-fast-output (buf)
          (wire:write-wl-int #x0BADBEEF buf)
          (wire:write-wl-int #x12345678 buf)
          (wire:write-wl-int #x70077007 buf)
          (wire:write-wl-int #x7FFFFFFF buf)
          (wire:write-wl-int #x00000000 buf))
        "write-wl-int can write positive integers")

  (p:is contents= #(#xF4524111
                    #xEDCBA988
                    #x8FF88FF9
                    #x80000000
                    #xFFFFFFFF)
        (io:with-fast-output (buf)
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
        (with-input-from-contents (buf #(#xDEADBEEF
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
  (p:is contents= #(#xDEADBEEF
                    #x01234567
                    #x89ABCDEF
                    #xFFFFFFFF
                    #x00000000)
        (io:with-fast-output (buf)
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
        (with-input-from-contents (buf #(#x12345624
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
  (p:is contents= #(#x12345624
                    #x77777780
                    #x000111FF
                    #x11223300
                    #x7FFFFFFF)
        (io:with-fast-output (buf)
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
  (p:is contents= '(12 #(#x68 #x65 #x6c #x6c
                         #x6f #x20 #x77 #x6f
                         #x72 #x6c #x64 #x00))
        (io:with-fast-output (buf)
          (wire:write-wl-string "hello world" buf))
        "write-wl-string correctly writes a string with a null terminator")

  (p:is contents= '(6 #(#x68 #x65 #x6c #x6c
                        #x6f #x00 #x00 #x00))
        (io:with-fast-output (buf)
          (wire:write-wl-string "hello" buf))
        "write-wl-string correctly pads to the word")

  (p:is contents= '(5 #(#x61 #x62 #x63 #x64
                        #x00 #x00 #x00 #x00))
        (io:with-fast-output (buf)
          (wire:write-wl-string "abcd" buf))
        "write-wl-string correctly factors in the null terminator when padding"))

(p:define-test read-wl-array
  (p:is equalp #(2 3 5 8)
        (with-input-from-contents (buf '(4 #(2 3 5 8)))
          (wire:read-wl-array buf))
        "read-wl-array correctly reads an array")

  (p:is equalp #(2 3 5 8 13)
        (with-input-from-contents (buf '(5 #(2 3 5 8
                                             13 0 0 0)))
          (wire:read-wl-array buf))
        "read-wl-array factors in word padding"))

(p:define-test write-wl-array
  (p:is contents= '(4 #(2 3 5 8))
        (io:with-fast-output (buf)
          (wire:write-wl-array
            (make-array 4 :element-type '(unsigned-byte 8)
                        :initial-contents '(2 3 5 8))
            buf))
        "write-wl-array correctly writes an array")

  (p:is contents= '(5 #(2 3 5 8
                        13 0 0 0))
        (io:with-fast-output (buf)
          (wire:write-wl-array
            (make-array 5 :element-type '(unsigned-byte 8)
                        :initial-contents '(2 3 5 8 13))
            buf))
        "write-wl-array correctly pads to the word"))
