;;; sockets/grovel.lisp - POSIX.1 socket grovel definition
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel@shunter.xyz>
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.sockets.ffi)
(include "errno.h" "string.h" "sys/socket.h" "sys/un.h" "unistd.h")



(ctype sa-family "sa_family_t")
(ctype signed-size "ssize_t")
(ctype size "size_t")
(ctype socklen "socklen_t")

(constant (+af-local+ "AF_LOCAL" "AF_UNIX"))
(constant (+sock-stream+ "SOCK_STREAM"))
(constant (+sol-socket+ "SOL_SOCKET"))
(constant (+scm-rights+ "SCM_RIGHTS"))

(bitfield msg-flags
  ((:dontwait "MSG_DONTWAIT"))
  ((:peek "MSG_PEEK")))

(constantenum errno
  ((:eacces "EACCES"))
  ((:eaddrinuse "EADDRINUSE"))
  ((:eaddrnotavail "EADDRNOTAVAIL"))
  ((:eafnosupport "EAFNOSUPPORT"))
  ((:ealready "EALREADY"))
  ((:ebadf "EBADF"))
  ((:econnaborted "ECONNABORTED"))
  ((:econnrefused "ECONNREFUSED"))
  ((:econnreset "ECONNRESET"))
  ((:edquot "EDQUOT"))
  ((:efault "EFAULT"))
  ((:einprogress "EINPROGRESS"))
  ((:eintr "EINTR"))
  ((:einval "EINVAL"))
  ((:eio "EIO"))
  ((:eisconn "EISCONN"))
  ((:eloop "ELOOP"))
  ((:emfile "EMFILE"))
  ((:emsgsize "EMSGSIZE"))
  ((:enametoolong "ENAMETOOLONG"))
  ((:enetunreach "ENETUNREACH"))
  ((:enfile "ENFILE"))
  ((:enobufs "ENOBUFS"))
  ((:enoent "ENOENT"))
  ((:enomem "ENOMEM"))
  ((:enospc "ENOSPC"))
  ((:enotconn "ENOTCONN"))
  ((:enotdir "ENOTDIR"))
  ((:enotsock "ENOTSOCK"))
  ((:eopnotsupp "EOPNOTSUPP"))
  ((:eoverflow "EOVERFLOW"))
  ((:eperm "EPERM"))
  ((:epipe "EPIPE"))
  ((:eproto "EPROTO"))
  ((:eprotonosupport "EPROTONOSUPPORT"))
  ((:eprototype "EPROTOTYPE"))
  ((:erofs "EROFS"))
  ((:etimedout "ETIMEDOUT"))
  ((:ewouldblock "EWOULDBLOCK" "EAGAIN")))
