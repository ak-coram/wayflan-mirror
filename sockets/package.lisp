;;; sockets/package.lisp - socket package declarations

(defpackage #:xyz.shunter.wayflan.sockets.ffi
  (:import-from #:cl
                #:t
                #:nil
                #:defpackage
                #:in-package)
  (:export #:*errno*))

(defpackage #:xyz.shunter.wayflan.sockets
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:gray #:trivial-gray-streams)
                    (#:ffi #:xyz.shunter.wayflan.sockets.ffi))
  (:export #:socket-error
           #:socket-stream-error

           #:connect
           #:make-socket
           #:write-fd
           #:read-fd))
