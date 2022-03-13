(defsystem #:wayhack
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :depends-on (#:alexandria
               #:fast-io
               #:iolib
               #:posix-shm
               #:trivial-features)
  :components ((:file #:wire)
               (:file #:client)))
