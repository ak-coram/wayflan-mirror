;;; wayflan.asd -- System definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defsystem #:wayflan/sockets
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Local socket interface optimized Wayland connections"

  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:alexandria
               #:cffi
               #:trivial-gray-streams)
  :pathname #P"sockets/"
  :serial t
  :components ((:module #:spec
                :pathname "spec"
                :components ((:static-file "socket.h")))
               (:file "package")
               (:cffi-grovel-file "grovel")
               (:file "ffi")
               (:file "conditions")
               (:file "sockets")))

(defsystem #:wayflan
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Wayland protocol implementation for clients"

  :depends-on (#:alexandria
               #:closer-mop
               #:fast-io
               #:plump
               #:trivial-features
               #:wayflan/sockets)

  :serial t
  :components ((:module #:protocols
                        :pathname "protocols"
                        :components ((:static-file "wayland.xml")
                                     (:static-file "presentation-time.xml")
                                     (:static-file "viewporter.xml")
                                     (:static-file "xdg-shell.xml")))
               (:file "packages")
               (:file "wire")
               (:file "client")
               (:file "autowrap")
               (:file "wayland-protocol")
               (:file "stable-protocols"))

  :in-order-to ((test-op (test-op :wayflan/test))))

(defsystem #:wayflan/examples
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Example suite for Wayflan"

  :depends-on (#:wayflan
               #:posix-shm
               #:cl-colors
               #:cl-cairo2
               #:cl-pango)
  :pathname #P"examples/"
  :serial nil
  :components ((:file "global-info")
               (:file "checkerboxed-demo")
               (:static-file "lisplogo_256.png")
               (:file "cairo-demo"
                      :depends-on ("lisplogo_256.png"))))

(defsystem #:wayflan/test
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Test suite for Wayflan"

  :depends-on (#:wayflan
               #:alexandria
               #:flexi-streams
               #:parachute)

  :pathname #P"test/"
  :serial nil
  :components ((:file "test")
               (:file "wire")
               (:file "client"))

  :perform (test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.wayflan.test)))
