;;; test/client.lisp - Wayflan test suite for client implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.test)



;; Mock protocol for unit testing

(client:define-interface wf-testable ()
  (:version 1)
  (:interface-name "wf_testable")
  (:documentation "Wayflan testable object"))

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

(client:define-event (:empty wf-testable 0)
  ()
  (:documentation "Signifies a no-value event"))


(p:define-test client-api
  (p:define-test client-api.define-interface
    (p:of-type client:wl-interface
               (find-class 'wf-testable))
    (p:is = 1
          (client:wl-interface-version 'wf-testable))
    (p:is = 1
          (client:wl-interface-version (find-class 'wf-testable)))

    (p:is string= "wf_testable"
          (client:wl-interface-name (find-class 'wf-testable)))
    (p:is eq (find-class 'wf-testable)
          (client:find-interface-named "wf_testable"))

    (p:is string= "Wayflan testable object"
          (documentation 'wf-testable 'type))
    (p:is string= "Wayflan testable object"
          (documentation (find-class 'wf-testable) t)))

  (p:define-test client-api.define-request
    (p:of-type function #'wf-testable.send-empty-request)
    (p:is string= "Send an empty request"
          (documentation 'wf-testable.send-empty-request 'function))
    (p:is string= "Send an empty request"
          (documentation #'wf-testable.send-empty-request t))))

(p:define-test client
  (p:define-test client.requests
    (let* ((stream (flexi-streams:make-in-memory-output-stream))
           (display (client:wl-display-connect stream))
           (testable (wayflan-client::%make-proxy 'wf-testable display))
           new-testable new-new-testable)
      (wf-testable.send-empty-request testable)

      (wf-testable.send-values
        testable #x7eadbeef #xfeefbeef #x7eadbeef/100
        (make-array 5 :element-type '(unsigned-byte 8)
                    :initial-contents '(10 20 30 40 50))
        "hello world")

      (wf-testable.send-wf-testable testable testable)
      (wf-testable.send-object testable display)

      (setf new-testable (wf-testable.make-testable testable)
            new-new-testable (wf-testable.make-object testable 'wf-testable 1))

      (p:is = 2 (client:wl-proxy-id testable))
      (p:is = 3 (client:wl-proxy-id new-testable))
      (p:is = 4 (client:wl-proxy-id new-new-testable))

      (p:of-type wf-testable testable)
      (p:of-type wf-testable new-testable)
      (p:of-type wf-testable new-new-testable)

      (p:is contents=
            '(;; wf-testable.send-empty-request
              ;; Message header -- object #2, opcode 0, length 8 bytes
              2
              #.(logior (ash 0 0)
                        (ash 8 16))

              ;; wf-testable.send-values
              ;; Message header -- object #2, opcode 1, length 48 bytes
              2
              #.(logior (ash 1 0)
                        (ash 48 16))
              ;; int-value :int = #xdeadbeef
              #x7eadbeef
              ;; uint-value :uint = #xfeefbeef
              #xfeefbeef
              ;; fixed-value :fixed = #xdeadbeefed
              #x7eadbeef
              ;; array-value :array = #(10 20 30 40 50)
              5
              #(10 20 30 40
                50 00 00 00)
              ;; string-value :string = "hello world"
              12
              #(#x68 #x65 #x6C #x6C
                #x6F #x20 #x77 #x6F
                #x72 #x6C #x64 #x00)

              ;; wf-testable.send-wf-testable
              ;; Message header -- object #2, opcode 2, length 12 bytes
              2
              #.(logior (ash 2 0)
                        (ash 12 16))
              ;; value (:object wf-testable) = #<wf-testable :id 2>
              2

              ;; wf-testable.send-object
              ;; Message header -- object #2, opcode 3, length 12 bytes
              2
              #.(logior (ash 3 0)
                        (ash 12 16))
              ;; value :object = #<wl-display :id 1>
              1

              ;; wf-testable.create-wf-testable
              ;; Message header -- object #2, opcode 4, length 12 bytes
              2
              #.(logior (ash 4 0)
                        (ash 12 16))
              ;; return-value (:new-id wf-testable) = #<wf-testable :id 3>
              ;;   id = 3
              3

              ;; wf-testable.create-object
              ;; Message header -- object #2, opcode 5, length 32 bytes
              2
              #.(logior (ash 5 0)
                        (ash 32 16))
              ;; return-value :new-id = #<wf-testable :id 4>
              ;;   name = "wf_testable"
              12
              #(#x77 #x66 #x5F #x74
                #x65 #x73 #x74 #x61
                #x62 #x6C #x65 #x00)
              ;;   version = 1
              1
              ;;   id = 4
              4)
            (flexi-streams:get-output-stream-sequence stream))))

  (p:define-test client.events
    (let* ((stream (flexi-streams:make-in-memory-input-stream
                     (contents-to-octets
                       '(;; wf-testable :empty
                         ;; Message header -- object #2, opcode 0, length 8 bytes
                         2
                         #.(logior (ash 0 0)
                                   (ash 8 16))))))
           (display (client:wl-display-connect stream))
           (testable (client::%make-proxy 'wf-testable display))
           event-detected?)

      (push (client:evelambda
              (:empty ()
               (setf event-detected? t)))
            (client:wl-proxy-hooks testable))
      (client:wl-display-dispatch-event display)
      (p:true event-detected?))))



(p:define-test wayland-protocol
  (p:define-test wayland-protocol.requests
    (let* ((stream (flexi-streams:make-in-memory-output-stream))
           ;; Display is always object id #1 by spec
           ;; Subsequent proxies generated by #'CLIENT::%MAKE-PROXY will have
           ;;   consecutive ID's starting from 2 upwards.
           (display (client:wl-display-connect stream))
           (new-callback (client:wl-display.sync display))
           (new-registry (client:wl-display.get-registry display)))

      (p:of-type client:wl-callback new-callback
               "wl-display-sync returns a new wl-callback proxy")
      (p:is = 2 (client:wl-proxy-id new-callback))

      (p:of-type client:wl-registry new-registry
               "wl-display-get-registry returns a new wl-registry proxy")
      (p:is = 3 (client:wl-proxy-id new-registry))

      (p:is contents=
            '(;; wl-display-sync
              ;; Message header -- object #1, opcode 0, length 12 bytes
              1
              #.(logior (ash 0 0)
                        (ash 12 16))
              ;; callback (:new-id wl-callback) = #<wl-display :id 2>
              ;;   proxy-id = 2
              2

              ;; wl-display-get-registry
              ;; Message header -- object #1, opcode 1, length 12 bytes
              1
              #.(logior (ash 1 0)
                        (ash 12 16))
              ;; return-value = #<wl-registry :id 3>
              ;;   proxy id = 3
              3)
            (flexi-streams:get-output-stream-sequence stream)
            "wl-display requests write the correct message")))

  (p:define-test wayland-protocol.events
    (let* ((stream (flexi-streams:make-in-memory-input-stream
                     (contents-to-octets
                       '(;; wl-display :error
                         ;; Message header -- object #1, opcode 0, length 32 bytes
                         1
                         #.(logior (ash 0 0)
                                   (ash 32 16))
                         ;; object_id :object = #<wl-display :id 1>
                         ;;   id = 1
                         1
                         ;; code :uint = 0
                         0
                         ;; message :string = "Bad thing"
                         10
                         #(#x42 #x61 #x64 #x20
                           #x74 #x68 #x69 #x6E
                           #x67 #x00 #x00 #x00)))))
           (display (client:wl-display-connect stream)))
      (p:fail (client:wl-display-dispatch-event display)
              'client:wl-error
              "wl-display :error event transforms into a CLCS error"))))
