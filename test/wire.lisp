;;; test/wire.lisp -- wire format test suite
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.test)



(define-test send-empty-message
  (let ((mocket (make-instance 'mock-socket)))
    (expect-message mocket 1 0 ())
    (expect-message mocket 2 3 ())
    (expect-message mocket 20 10 ())
    (finish-mock mocket)

    (wire:send-wl-message (mocket 1 0) ())
    (wire:send-wl-message (mocket 2 3) ())
    (wire:send-wl-message (mocket 20 10) ())))

(define-test recv-empty-message
  (let ((mocket (make-instance 'mock-socket)))
    (next-message mocket 1 0 ())
    (next-message mocket 2 3 ())
    (next-message mocket 20 10 ())
    (finish-mock mocket)

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 1 sender-id)
      (is = 0 opcode))

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 2 sender-id)
      (is = 3 opcode))

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 20 sender-id)
      (is = 10 opcode))))

(define-test send-wl-int
  (let ((mocket (make-instance 'mock-socket)))
    (expect-message
      mocket 0 0
      '(0 10 #x7fffffff -10 #x-80000000))
    (finish-mock mocket)

    (wire:send-wl-message
      (mocket 0 0) (:int :int :int :int :int)
      0 10 #x7fffffff -10 #x-80000000)

    (fail (wire:send-wl-message (mocket 0 0) (:int) #x-80000001))
    (fail (wire:send-wl-message (mocket 0 0) (:int) #x80000000))))

(define-test recv-wl-int
  (let ((mocket (make-instance 'mock-socket)))
    (next-message
      mocket 0 0
      '(0 10 #x7fffffff -10 #x-80000000))
    (finish-mock mocket)

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 0 sender-id)
      (is = 0 opcode)

      (is = 0 (wire:read-wl-int buf))
      (is = 10 (wire:read-wl-int buf))
      (is = #x7fffffff (wire:read-wl-int buf))
      (is = -10 (wire:read-wl-int buf))
      (is = #x-80000000 (wire:read-wl-int buf)))))

(define-test send-wl-uint
  (let ((mocket (make-instance 'mock-socket)))
    (expect-message
      mocket 0 0
      '(0 10 #xffffffff))
    (finish-mock mocket)

    (wire:send-wl-message
      (mocket 0 0) (:uint :uint :uint)
      0 10 #xffffffff)

    (fail (wire:send-wl-message (mocket 0 0) (:uint) -1))
    (fail (wire:send-wl-message (mocket 0 0) (:uint) #x100000000))))

(define-test recv-wl-uint
  (let ((mocket (make-instance 'mock-socket)))
    (next-message
      mocket 0 0
      '(0 10 #xffffffff))
    (finish-mock mocket)

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 0 sender-id)
      (is = 0 opcode)

      (is = 0 (wire:read-wl-uint buf))
      (is = 10 (wire:read-wl-uint buf))
      (is = #xffffffff (wire:read-wl-uint buf)))))

(define-test send-wl-fixed
  (let ((mocket (make-instance 'mock-socket)))
    (expect-message
      mocket 0 0
      '(#x00 #xa00 #x7fffff00))
    (expect-message
      mocket 0 0
      '(#xc80 #x7fffffff #x-80000000))
    (finish-mock mocket)

    (wire:send-wl-message
      (mocket 0 0) (:fixed :fixed :fixed)
      0 10 #x7fffff)
    (wire:send-wl-message
      (mocket 0 0) (:fixed :fixed :fixed)
      25/2 #x7fffffff/100 #x-800000)

    (fail (wire:send-wl-message (mocket 0 0) (:fixed) #x800000))
    (fail (wire:send-wl-message (mocket 0 0) (:fixed) #x-80000001/100))))

(define-test recv-wl-fixed
  (let ((mocket (make-instance 'mock-socket)))
    (next-message
      mocket 0 0
      '(#x00 #xa00 #x7fffff00))
    (next-message
      mocket 0 0
      '(#xc80 #x7fffffff #x-80000000))
    (finish-mock mocket)

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 0 sender-id)
      (is = 0 opcode)

      (is = 0 (wire:read-wl-fixed buf))
      (is = 10 (wire:read-wl-fixed buf))
      (is = #x7fffff (wire:read-wl-fixed buf)))

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 0 sender-id)
      (is = 0 opcode)

      (is = 25/2 (wire:read-wl-fixed buf))
      (is = #x7fffffff/100 (wire:read-wl-fixed buf))
      (is = #x-800000 (wire:read-wl-fixed buf)))))

(define-test send-wl-array
  (let ((mocket (make-instance 'mock-socket)))
    (expect-message
      mocket 0 0
      '(12
        #(1 2 3 4
          5 6 7 8
          9 10 11 12)))
    (finish-mock mocket)

    (wire:send-wl-message
      (mocket 0 0) (:array)
      (make-wl-array '(1 2 3 4 5 6 7 8 9 10 11 12)))))

(define-test recv-wl-array
  (let ((mocket (make-instance 'mock-socket)))
    (next-message
      mocket 0 0
      '(12
        #(1 2 3 4
          5 6 7 8
          9 10 11 12)
        10
        #(10 20 30 40
          50 60 70 80
          90 100 123 234)
        1
        #(255 1 2 3)))
    (finish-mock mocket)

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 0 sender-id)
      (is = 0 opcode)

      (is equalp #(1 2 3 4 5 6 7 8 9 10 11 12)
          (wire:read-wl-array buf))
      (is equalp #(10 20 30 40 50 60 70 80 90 100)
          (wire:read-wl-array buf))
      (is equalp #(255)
          (wire:read-wl-array buf)))))

(define-test send-wl-string
  (let ((mocket (make-instance 'mock-socket)))
    (expect-message
      mocket 0 0
      '(12
        #(104 101 108 108
          111 32 119 111
          114 108 100 0)))
    (finish-mock mocket)

    (wire:send-wl-message
      (mocket 0 0) (:string)
      "hello world")))

(define-test recv-wl-string
  (let ((mocket (make-instance 'mock-socket)))
    (next-message
      mocket 0 0
      '(12
        #(104 101 108 108
          111 32 119 111
          114 108 100 0)
        5
        #(104 101 108 111
          0 255 254 253)
        4
        #(104 101 108 0)))
    (finish-mock mocket)

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 0 sender-id)
      (is = 0 opcode)

      (is string= "hello world" (wire:read-wl-string buf))
      (is string= "helo" (wire:read-wl-string buf))
      (is string= "hel" (wire:read-wl-string buf)))))

(define-test send-various
  (let ((mocket (make-instance 'mock-socket)))
    (expect-message
      mocket 1 3
      '(256 -256
        #xa00
        12
        #(104 101 108 108
          111 32 119 111
          114 108 100 0)
        4
        #(10 20 30 40)))
    (expect-fd mocket 4)
    (finish-mock mocket)

    (wire:send-wl-message
      (mocket 1 3) (:uint :int :fixed :fd :string :array)
      256 -256 10 4
      "hello world"
      (make-wl-array '(10 20 30 40)))))

(define-test recv-various
  (let ((mocket (make-instance 'mock-socket)))
    (next-message
      mocket 1 3
      '(256 -256
        #xa00
        6
        #(104 101 108 108
          111 0 255 254)
        5
        #(10 20 30 40
          50 255 254 253)))
    (next-fd mocket 4)
    (finish-mock mocket)

    (wire:with-incoming-message (mocket sender-id opcode buf)
      (is = 1 sender-id)
      (is = 3 opcode)

      (is = 256 (wire:read-wl-uint buf))
      (is = -256 (wire:read-wl-int buf))
      (is = 10 (wire:read-wl-fixed buf))
      (is = 4 (wire:read-fd mocket))
      (is string= "hello" (wire:read-wl-string buf))
      (is equalp #(10 20 30 40 50) (wire:read-wl-array buf)))))
