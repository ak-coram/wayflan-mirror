;;; wayflan-client.asd -- client-specific system definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defsystem #:wayflan-client
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "From-scratch Wayland client implementation"

  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:wayflan/common
               #:alexandria
               #:closer-mop
               #:plump)

  :pathname #P"src/client/"
  :serial t
  :components ((:file "packages")
               (:file "client")
               (:file "autowrap")
               (:file "wayland-protocol")
               (:file "stable-protocols"))

  :in-order-to ((test-op (test-op :wayflan-client/test))))

(defsystem #:wayflan-client/examples
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "Example suite for Wayflan"

  :depends-on (#:wayflan-client
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
