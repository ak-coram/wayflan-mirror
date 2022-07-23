;;; wayflan.asd -- System definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defsystem #:wayflan/sockets
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Local socket interface for Wayland connections"

  :depends-on (#:alexandria
               #:cffi
               #:cl-autowrap
               #:trivial-gray-streams)
  :pathname #P"sockets/"
  :serial t
  :components ((:module #:spec
                :pathname "spec"
                :components ((:static-file "socket.h")))
               (:file "package")
               (:file "ffi")
               (:file "conditions")
               (:file "sockets")))

(defsystem #:wayflan
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Wayland implementation for clients (TODO: and servers)"

  :depends-on (#:alexandria
               #:closer-mop
               #:fast-io
               #:plump
               #:trivial-features
               #:wayflan/sockets)

  :serial t
  :components ((:file #:packages)
               (:file #:wire)
               (:file #:client)
               (:file #:autowrap)
               (:file #:wayland-protocol)
               (:file #:protocols))

  :in-order-to ((test-op (test-op :wayflan/test))))

(defsystem #:wayflan/test
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Test sute for wayflan"

  :depends-on (#:wayflan
               #:alexandria
               #:flexi-streams
               #:parachute)

  :pathname #P"test/"
  :serial nil
  :components ((:file #:test)
               (:file #:wire)
               (:file #:client))

  :perform (test-op (op c)
             (uiop:symbol-call :parachute :test :xyz.shunter.wayflan.test)))
