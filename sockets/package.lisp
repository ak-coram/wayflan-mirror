;;; sockets/package.lisp - socket package declarations

(defpackage #:xyz.shunter.wayflan.sockets.ffi
  (:use #:cffi)
  (:import-from #:cl
                #:defpackage
                #:in-package)
  (:export #:sa-faimly
           #:signed-size
           #:size
           #:socklen

           #:+af-local+
           #:+sock-stream+
           #:+sol-socket+
           #:+scm-rights+

           #:iovec
           #:iov-base
           #:iov-len

           #:msghdr
           #:msg-name
           #:msg-namelen
           #:msg-iov
           #:msg-iovlen
           #:msg-control
           #:msg-controllen
           #:msg-flags

           #:cmsghdr
           #:cmsg-len
           #:cmsg-level
           #:cmsg-type

           #:sockaddr-un
           #:sun-family
           #:sun-path

           #:socket
           #:bind
           #:accept
           #:connect
           #:close
           #:sendmsg
           #:recvmsg

           #:bzero
           #:memcpy
           #:memset
           #:memmove
           #:strerror
           #:errno
           #:*errno*))

(defpackage #:xyz.shunter.wayflan.sockets
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:gray #:trivial-gray-streams)
                    (#:ffi #:xyz.shunter.wayflan.sockets.ffi))
  (:export #:local-socket-stream
           #:passive-local-socket-stream
           #:active-local-socket-stream

           #:socket-error
           #:socket-stream-error

           #:connect
           #:make-socket
           #:write-fd
           #:read-fd))
