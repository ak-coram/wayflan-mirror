;;; wayflan.asd -- System definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defsystem #:wayflan
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Wayland implementation for clients (TODO: and servers)"

  :depends-on (#:alexandria
               #:fast-io
               #:iolib
               #:plump
               #:trivial-features)

  :serial t
  :components ((:file #:packages)
               (:file #:wire)
               (:file #:client)
               (:file #:autowrap)
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
