;;; wayflan.asd -- System definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defsystem #:wayflan
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "Wayland protocol implementation for clients"

  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:alexandria
               #:babel
               #:cffi
               #:closer-mop
               #:plump
               #:trivial-features)

  :pathname #P"src/"
  :serial t
  :components ((:module #:protocols
                        :pathname "protocols"
                        :components ((:static-file "wayland.xml")
                                     (:static-file "presentation-time.xml")
                                     (:static-file "viewporter.xml")
                                     (:static-file "xdg-shell.xml")))
               (:file "packages")
               (:cffi-grovel-file "grovel")
               (:file "ffi")
               (:file "wire")
               (:file "client")

               (:file "autowrap")
               (:file "wayland-protocol")
               (:file "stable-protocols"))

  :in-order-to ((test-op (test-op :wayflan/test))))

(defsystem #:wayflan/examples
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "Example suite for Wayflan"

  :depends-on (#:wayflan
               #:cl-colors
               #:cl-cairo2
               #:cl-pango
               #:cl-xkb
               #:input-event-codes
               #:posix-shm)
  :pathname #P"examples/"
  :serial nil
  :components ((:static-file "lisplogo_256.png")
               (:file "hello-world")

               (:file "checkerboxed-demo")
               (:file "cairo-demo"
                      :depends-on ("lisplogo_256.png"))

               ;; Vertical slices showcasing various wl capabilities
               (:file "wl-pointer-demo")
               (:file "wl-touch-demo")
               (:file "wl-keyboard-demo")

               ;; "Practical" application demos
               (:file "Waycalc")
               (:file "Wayruler")))

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
             (uiop:symbol-call :parachute :test :xyz.shunter.wayflan.test)))
