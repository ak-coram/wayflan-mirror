;;; test/client.lisp -- Wayflan client test suite
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.test)



;; Mock protocol for unit testing

(client:define-interface wf-testable ()
  (:version 1)
  (:interface-name "wf_testable")
  (:documentation "Wayflan testable object"))

(client:define-enum wf-testable.standard-enum ()
  ((:one 1)
   (:two 2)
   (:three 3))
  (:documentation "A standard enum"))

(client:define-enum wf-testable.bitfield-enum ()
  ((:zero 0)
   (:one 1)
   (:two 2)
   (:four 4))
  (:documentation "A bitfield enum")
  (:bitfield t))

(client:define-enum wf-testable.funny-bitfield-enum ()
  ((:zero 0)
   (:one 1)
   (:two 2)
   (:four 4)
   (:five 5))
  (:documentation "A bitfield enum where some members share values")
  (:bitfield t))

(client:define-request (wf-testable.send-empty-request wf-testable 0)
  ()
  (:documentation "Send an empty request"))

(client:define-request (wf-testable.send-values wf-testable 1)
  ((int-value :type :int)
   (uint-value :type :uint)
   (fixed-value :type :fixed)
   (array-value :type :array)
   (string-value :type :string))
  (:documentation "Send various wire-primitive values"))

(client:define-request (wf-testable.send-wf-testable wf-testable 2)
  ((value :type (:object :interface wf-testable)))
  (:documentation "Send a protocoltime-typeknown object"))

(client:define-request (wf-testable.send-object wf-testable 3)
  ((object :type :object))
  (:documentation "Send a runtime-typeknown object"))

(client:define-request (wf-testable.make-testable wf-testable 4)
  ((testable :type (:new-id :interface wf-testable)))
  (:documentation "Create a protocoltime-typeknown object"))

(client:define-request (wf-testable.make-object wf-testable 5)
  ((object :type :new-id))
  (:documentation "Create a runtime-typeknown object"))

(client:define-request (wf-testable.send-enums wf-testable 6)
  ((x1 :type (:uint :enum wf-testable.standard-enum))
   (x2 :type (:uint :enum wf-testable.bitfield-enum))
   (x3 :type (:uint :enum wf-testable.funny-bitfield-enum)))
  (:documentation "Send various enums"))

(client:define-event (:empty wf-testable 0)
  ()
  (:documentation "Signifies a no-value event"))

(client:define-event (:receive-enums wf-testable 1)
  ((x1 :type (:uint :enum wf-testable.standard-enum))
   (x2 :type (:uint :enum wf-testable.bitfield-enum))
   (x3 :type (:uint :enum wf-testable.funny-bitfield-enum))))


(define-test client-api
  (define-test client-api.define-interface
    (of-type client:wl-interface-class
             (find-class 'wf-testable))
    (is = 1
        (client:wl-interface-version 'wf-testable))
    (is = 1
        (client:wl-interface-version (find-class 'wf-testable)))

    (is string= "wf_testable"
        (client:wl-interface-name (find-class 'wf-testable)))
    (is eq (find-class 'wf-testable)
        (client:find-interface-named "wf_testable"))

    (is string= "Wayflan testable object"
        (documentation 'wf-testable 'type))
    (is string= "Wayflan testable object"
        (documentation (find-class 'wf-testable) t)))

  (define-test client-api.define-request
    (of-type function #'wf-testable.send-empty-request)
    (is string= "Send an empty request"
        (documentation 'wf-testable.send-empty-request 'function))
    (is string= "Send an empty request"
        (documentation #'wf-testable.send-empty-request t))))

(define-test client.requests
  (let* ((mocket (make-instance 'mock-socket))
         (display (client:wl-display-connect mocket))
         (testable (wayflan-client::%make-proxy 'wf-testable display))
         new-testable
         new-new-testable)
    (expect-message
      ;; wf-testable@2.send-empty-request
      mocket 2 0
      ())
    (expect-message
      ;; wf-testable@2.send-values
      mocket 2 1
      '(;; int-value :int = #x7eadbeef
        #x7eadbeef
        ;; uint-value :uint = #xdeadbeef
        #xdeadbeef
        ;; fixed-value :fied = #x7deadbeef/100
        #x7eadbeef
        ;; array-value :array = #(10 20 30 40)
        4
        #(10 20 30 40)
        ;; string-value :string = "hello world"
        12
        #(#x68 #x65 #x6C #x6C
          #x6F #x20 #x77 #x6F
          #x72 #x6C #x64 #x00)))
    (expect-message
      ;; wf-testable@2.send-wf-testable
      mocket 2 2
      '(;; value (:object wf-testable) = #<wf-testable :id 2>
        2))
    (expect-message
      ;; wf-testable@2.send-object
      mocket 2 3
      '(;; value :object = #<wl-display :id 1>
        1))
    (expect-message
      ;; wf-testable@2.create-wf-testable
      mocket 2 4
      '(;; value (:new-id wf-testable) = #<wf-testable :id 3>
        3))
    (expect-message
      ;; wf-testable@2.create-object
      mocket 2 5
      '(;; value :new-id = #<wf-testable :id 4>
        ;;   name = "wf_testable"
        12
        #(#x77 #x66 #x5F #x74
          #x65 #x73 #x74 #x61
          #x62 #x6C #x65 #x00)
        ;;   version = 1
        1
        ;;   id = 4
        4))
    (finish-mock mocket)


    (wf-testable.send-empty-request testable)
    (wf-testable.send-values
      testable #x7eadbeef #xdeadbeef #x7eadbeef/100
      (make-wl-array '(10 20 30 40))
      "hello world")

    (wf-testable.send-wf-testable testable testable)
    (wf-testable.send-object testable display)

    (setf new-testable (wf-testable.make-testable testable)
          new-new-testable (wf-testable.make-object testable 'wf-testable 1))

    (is = 2 (client:wl-proxy-id testable))
    (is = 3 (client:wl-proxy-id new-testable))
    (is = 4 (client:wl-proxy-id new-new-testable))

    (of-type wf-testable testable)
    (of-type wf-testable new-testable)
    (of-type wf-testable new-new-testable)

    (client:wl-display-disconnect display)))

(define-test client.events
  (let* ((mocket (make-instance 'mock-socket))
         (display (client:wl-display-connect mocket))
         (testable (client::%make-proxy 'wf-testable display))
         (count 0))
    (next-message
      ;; wf-testable@2.empty
      mocket 2 0
      ())
    (finish-mock mocket)

    (push (client:evelambda
            (:empty ()
             (incf count)))
          (client:wl-proxy-hooks testable))
    (client:wl-display-dispatch-event display)
    (is = 1 count)

    (client:wl-display-disconnect display)))

(define-test client.enums
  (let* ((mocket (make-instance 'mock-socket))
         (display (client:wl-display-connect mocket))
         (testable (client::%make-proxy 'wf-testable display))
         (count 0))
    (expect-message
      ;; wf-testable@2.send-enums
      mocket 2 6
      '(;; x1 (:uint wf-testable.standard-enum) = :one
        1
        ;; x2 (:uint wf-testable.bitfield-enum) = (:two :four)
        6
        ;; x3 (:uint wf-testable.funny-bitfield-enum) = (:five)
        5))
    (next-message
      ;; wf-testable@2.receive-enums
      mocket 2 1
      '(;; x1 (:uint wf-testable.standard-enum) = :one
        1
        ;; x2 (:uint wf-testable.bitfield-enum) = (:two :four)
        6
        ;; x3 (:uint wf-testable.funny-bitfield-enum) = (:five)
        5))
    (finish-mock mocket)

    (wf-testable.send-enums testable :one '(:two :four) '(:five))
    (push (client:evelambda
            (:receive-enums (x1 x2 x3)
             (incf count)
             (is eq :one x1)
             (is equal '(:two :four) x2)
             (is equal '(:one :four :five) x3)))
          (client:wl-proxy-hooks testable))

    (client:wl-display-dispatch-event display)
    (is = 1 count)))



(define-test wayland-protocol.requests
  (let* ((mocket (make-instance 'mock-socket))
         ;; Display is always object id #1 by spec
         ;; Subsequent proxies generated by #'CLIENT::%MAKE-PROXY will have
         ;;   consecutive ID's starting from 2 upwards.
         (display (client:wl-display-connect mocket))
         new-callback
         new-registry)

    (expect-message
      ;; wl-display@1.sync
      mocket 1 0
      '(;; callback (:new-id wl-callback) = #<wl-callback :id 2>
        2))

    (expect-message
      ;; wl-display@1.new-registry
      mocket 1 1
      '(;; registry (:new-id wl-registry) = #<wl-registry :id 3>
        3))

    (finish-mock mocket)

    (setf new-callback (client:wl-display.sync display)
          new-registry (client:wl-display.get-registry display))

    (of-type client:wl-callback new-callback
             "wl-display-sync returns a new wl-callback proxy")
    (is = 2 (client:wl-proxy-id new-callback))

    (of-type client:wl-registry new-registry
             "wl-display-get-registry returns a new wl-registry proxy")
    (is = 3 (client:wl-proxy-id new-registry))))

(define-test wayland-protocol.events
  (let* ((mocket (make-instance 'mock-socket))
         (display (client:wl-display-connect mocket)))
    (next-message
      ;; wl-display@1.error
      mocket 1 0
      '(;; object-id :object = #<wl-display :id 1>
        1
        ;; code :uint = 3
        3
        ;; message :string = "Fatal thing"
        12
        #(#x46 #x61 #x74 #x61
          #x6C #x20 #x74 #x68
          #x69 #x6E #x67 #x00)))
    (finish-mock mocket)

    (fail (client:wl-display-dispatch-event display)
          'client:wl-error
          "wl-display :error event transforms into a CLCS error")))

