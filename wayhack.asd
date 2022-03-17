;;; wayhack.asd -- System definition
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defsystem #:wayhack
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :depends-on (#:alexandria
               #:fast-io
               #:iolib
               #:plump
               #:posix-shm
               #:trivial-features)

  :components ((:file #:packages)
               (:file #:wire)
               (:file #:client)
               (:file #:autowrap)
               (:file #:protocols))
  :serial t)
