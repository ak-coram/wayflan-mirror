;;; test/client.lisp - Wayflan test suite for client implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.test)



(defvar *expected-event*)
(defclass event-verifier (client:wl-event-listener)
  ()
  (:documentation "Dispatches events on a display and asserts that events it expects to receive are received in order."))

(defmethod client:handle-event ((listener event-verifier) sender event)
  (destructuring-bind (expected-sender expected-event-class callback) *expected-event*
    (p:is eql expected-sender sender
          "Next event's sender is ~A" expected-sender)
    (p:is eql (find-class expected-event-class) (class-of event)
          "Next event's class is ~A" expected-event-class)
    (when event
      (funcall callback event))))

(defun expect (verifier sender event-class callback)
  (push verifier (wayflan-client:wl-proxy-listeners sender))
  (unwind-protect
    (let ((*expected-event* (list sender event-class callback)))
      (client:wl-display-dispatch-event
        (client:wl-proxy-display sender)))
    (setf (client:wl-proxy-listeners sender)
          (remove verifier (client:wl-proxy-listeners sender)))))

(p:define-test wayland-protocol
  (p:define-test wayland-protocol.requests
    (let* ((stream (flexi-streams:make-in-memory-output-stream))
           ;; Display is always object id #1 by spec
           ;; Subsequent proxies generated by #'CLIENT:MAKE-PROXY will have
           ;;   consecutive ID's starting from 2 upwards.
           (display (client:wl-display-connect stream))
           (new-callback (client:wl-display-sync display))
           (new-registry (client:wl-display-get-registry display)))

      (p:of-type client:wl-callback new-callback
               "wl-display-sync returns a new wl-callback proxy")
      (p:is = 2 (client:wl-proxy-id new-callback))

      (p:of-type client:wl-registry new-registry
               "wl-display-get-registry returns a new wl-registry proxy")
      (p:is = 3 (client:wl-proxy-id new-registry))

      (p:is contents=
            '(;; wl-display-sync
              ;; Message header -- object #1, opcode 0, length 8 bytes
              1
              #.(logior (ash 0 0)
                        (ash 12 16))
              ;; callback (:new-id wl-callback) = #<wl-display :id 2>
              ;;   proxy-id = 2
              2

              ;; wl-display-get-registry
              ;; Message header -- object #1, opcode 1, length 8 bytes
              1
              #.(logior (ash 1 0)
                        (ash 12 16))
              ;; registry (:new-id wl-registry) = #<wl-registry :id 3>
              ;;   proxy id = 3
              3)
            (flexi-streams:get-output-stream-sequence stream)
            "wl-display requests write the correct message")))

  (p:define-test wayland-protocol.events
    (let* ((stream (flexi-streams:make-in-memory-input-stream
                     (contents-to-octets
                       '(;; wl-display-error-event
                         ;; Message header -- object #1, opcode 0, length 32 bytes
                         1
                         #.(logior (ash 0 0)
                                   (ash 32 16))
                         ;; object_id :object = #<wl-display :id 1>
                         ;;   proxy id = 1
                         1
                         ;; code :uint = 0
                         0
                         ;; message :string = "Bad thing"
                         10
                         #(#x42 #x61 #x64 #x20
                           #x74 #x68 #x69 #x6E
                           #x67 #x00 #x00 #x00)))))
           (display (client:wl-display-connect stream)))
      (parachute:fail (client:wl-display-dispatch-event display)
                      'client:wl-error
                      "wl-display-error-event transforms into a wl-error"))

    (let* ((stream (flexi-streams:make-in-memory-input-stream
                     (contents-to-octets
                       '(;; wl-registry-global-event
                         ;; Message header -- object #2, opcode 0, length 28 bytes
                         2
                         #.(logior (ash 0 0)
                                   (ash 28 16))
                         ;; name :uint = 10
                         10
                         ;; interface :string = "wl_shm"
                         7
                         #(#x77 #x6c #x5f #x73
                           #x68 #x6d #x00 #x00)
                         ;; version :uint = 1
                         1))))
           (display (client:wl-display-connect stream))
           (verifier (make-instance 'event-verifier))
           (registry (client:make-proxy 'client:wl-registry display)))
      (expect verifier registry 'client:wl-registry-global-event
              (lambda (event)
                (p:is = 10
                      (client:wl-event-name event))
                (p:is string= "wl_shm"
                      (client:wl-event-interface event))
                (p:is = 1
                      (client:wl-event-version event)))))))

(client:define-interface wl-foo ()
  (:event-class wl-foo-event)
  (:interface-name "test_wl_foo"))

(client:define-request (wl-foo-frobnicate-string wl-foo 11)
  ((name :type :string)
   (value :type :uint))
  (:documentation "Request the server to do a bunch of nonsense to the given string."))

(client:define-request (wl-foo-gyre wl-foo 0)
  ()
  (:documentation "Test function that accepts no arguments"))

(client:define-request (wl-foo-gimble wl-foo 1)
  ((tove :type :int)
   (num-wabes :type :uint)
   (borogrove :type :fixed)
   (jubjub-name :type :string)
   (rathe-contents :type :array))
  (:documentation "Test function that accepts all primitive objects"))

(client:define-interface wl-bar ()
  (:event-class wl-bar-event)
  (:interface-name "test_wl_bar"))

(client:define-request (wl-bar-chortle-foo wl-bar 0)
  ((foo :type (:object wl-foo)))
  (:documentation "Test function that accepts a wl-foo proxy"))

(client:define-request (wl-bar-chortle-thing wl-bar 1)
  ((thing :type :object))
  (:documentation "Test function that accepts a proxy"))

(client:define-request (wl-bar-burble-foo wl-bar 10)
  ((foo :type (:new-id wl-foo)))
  (:documentation "Test function that creates a new wl-foo"))

(client:define-request (wl-bar-burble-thing wl-bar 11)
  ((thing :type :new-id))
  (:documentation "Test function that creates and returns a new proxy"))

(p:define-test define-request
  (let* ((stream (flexi-streams:make-in-memory-output-stream))
         (display (client:wl-display-connect stream))
         (foo (client:make-proxy 'wl-foo display)))
    (p:is = 1 (client:wl-proxy-id display))
    (p:is = 2 (client:wl-proxy-id foo))

    (wl-foo-gyre foo)
    (wl-foo-gimble foo 21 13 25/2
                   "Jabberwocky!"
                   (make-array 8
                               :element-type '(unsigned-byte 8)
                               :initial-contents
                               '(0 1 1 2 3 5 8 13)))
    (p:is contents=
          '(;; wl-foo-gimble
            ;; Message header -- object #2, length 8 bytes, opcode 0
            2
            #.(logior (ash 0 0)
                      (ash 8 16))

            ;; wl-foo-gimble
            ;; Message header -- object #2, length 52 bytes, opcode 1
            2
            #.(logior (ash 1 0)
                      (ash 52 16))
            ;; tove :int = 21
            21
            ;; num-wabes :uint = 13
            13
            ;; borogrove :fixed = 25/2 (= #xc80/100)
            #x00000c80
            ;; jubjub-name :string = "Jabberwocky!"
            13
            #(#x4A #x61 #x62 #x62
              #x65 #x72 #x77 #x6F
              #x63 #x6B #x79 #x21
              #x00 #x00 #x00 #x00)
            ;; rathe-contents :array = #(0 1 1 2 3 5 8 13)
            8
            #(0 1 1 2
              3 5 8 13))
          (flexi-streams:get-output-stream-sequence stream)))

  (let* ((stream (flexi-streams:make-in-memory-output-stream))
         (display (client:wl-display-connect stream))
         (foo (client:make-proxy 'wl-foo display))
         (bar (client:make-proxy 'wl-bar display)))
    (p:is = 1 (client:wl-proxy-id display))
    (p:is = 2 (client:wl-proxy-id foo))
    (p:is = 3 (client:wl-proxy-id bar))

    (wl-bar-chortle-foo bar foo)
    (wl-bar-chortle-thing bar foo)

    (p:is contents=
          '(;; wl-bar-chortle-foo
            ;; Message header - object #3, opcode 0, length 12 bytes
            3
            #.(logior (ash 0 0)
                      (ash 12 16))
            ;; foo (:object wl-foo) = #<wl-foo :id 2>
            ;;   object id = 2
            2

            ;; wl-bar-chortle-thing
            ;; Message header - object #3, opcode 1, length 12 bytes
            3
            #.(logior (ash 1 0)
                      (ash 12 16))
            ;; thing :object = #<wl-foo :id 2>
            ;;   object id = 2
            2)
          (flexi-streams:get-output-stream-sequence stream))

    (p:fail (wl-bar-chortle-foo bar bar)))

  (let* ((stream (flexi-streams:make-in-memory-output-stream))
         (display (client:wl-display-connect stream))
         (bar (client:make-proxy 'wl-bar display))
         (foo1 (wl-bar-burble-foo bar))
         (foo2 (wl-bar-burble-thing bar 'wl-foo 1)))
    (p:is = 1 (client:wl-proxy-id display))
    (p:is = 2 (client:wl-proxy-id bar))
    (p:is = 3 (client:wl-proxy-id foo1))
    (p:of-type wl-foo foo1)
    (p:is = 4 (client:wl-proxy-id foo2))
    (p:of-type wl-foo foo2)

    (p:is contents=
          '(;; wl-bar-burble-foo
            ;; Message header - object #2, opcode 10, length 12 bytes
            2
            #.(logior (ash 10 0)
                      (ash 12 16))
            ;; foo (:new-id wl-foo) = #<wl-foo :id 3>
            ;;   object id = 3
            3

            ;; wl-bar-burble-thing
            ;; Message header - object #2, opcode 11, length 32 bytes
            2
            #.(logior (ash 11 0)
                      (ash 32 16))
            ;; thing :new-id = #<wl-foo :id 4 :version 1>
            ;;   interface name = "test_wl_foo"
            12
            #(#x74 #x65 #x73 #x74
              #x5F #x77 #x6C #x5F
              #x66 #x6F #x6F #x00)
            ;;   interface version = 1
            1
            ;;   object id = 4
            4)
          (flexi-streams:get-output-stream-sequence stream))))
