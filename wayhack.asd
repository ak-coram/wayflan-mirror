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
  :components ((:file #:wire)
               (:file #:client)
               (:file #:autowrap)))
