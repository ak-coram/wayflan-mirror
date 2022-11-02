;;; wayflan.asd -- common system definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defsystem #:wayflan
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "From-scratch Wayland client implementation"

  :depends-on (#:wayflan-client)
  :in-order-to ((test-op (test-op :wayflan/test))))

(defsystem #:wayflan/common
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "PRIVATE: Wayflan common source for client and server"

  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:alexandria
               #:babel
               #:cffi
               #:plump)
  :pathname #P"src/"
  :serial nil
  :components ((:file "packages")
               (:file "protocol" :depends-on ("packages"))
               (:file "types" :depends-on ("packages"))
               (:cffi-grovel-file "grovel" :depends-on ("packages"))
               (:file "ffi" :depends-on ("grovel"))
               (:file "conditions" :depends-on ("packages" "ffi"))
               (:file "wire" :depends-on ("ffi" "types" "conditions"))
               (:module #:protocols
                        :pathname "protocols"
                        :components ((:static-file "wayland.xml")
                                     (:static-file "presentation-time.xml")
                                     (:static-file "viewporter.xml")
                                     (:static-file "xdg-shell.xml")))))

(defsystem #:wayflan/test
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "Test suite for Wayflan"

  :depends-on (#:wayflan
               #:parachute)

  :pathname #P"test/"
  :serial t
  :components ((:file "test")
               (:file "wire")
               (:file "client"))

  :perform (test-op (op c)
             (uiop:symbol-call :parachute :test '#:xyz.shunter.wayflan.test)))
