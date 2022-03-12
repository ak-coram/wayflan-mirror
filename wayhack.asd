(defsystem #:wayhack
  :version "0.0.0"
  :author "Samuel Hunter"
  :license "Proprietary"

  :depends-on (#:fast-io
               #:iolib
               #:posix-shm
               #:trivial-features)
  :components ((:file #:wire)
               (:file #:client)))
