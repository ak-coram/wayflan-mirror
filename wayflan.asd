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
               (:file #:protocols)))
