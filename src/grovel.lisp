;;; src/grovel.lisp - POSIX.1 socket grovel definition
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.ffi)
(include "errno.h" "string.h" "sys/socket.h" "sys/un.h" "unistd.h")



(ctype sa-family "sa_family_t")
(ctype signed-size "ssize_t")
(ctype size "size_t")
(ctype socklen "socklen_t")
(ctype errno "int")

(constant (+af-local+ "AF_LOCAL" "AF_UNIX"))
(constant (+sock-stream+ "SOCK_STREAM"))
(constant (+sol-socket+ "SOL_SOCKET"))
(constant (+scm-rights+ "SCM_RIGHTS"))

(constant (+ewouldblock+ "EWOULDBLOCK" "EAGAIN"))
(constant (+epipe+ "EPIPE"))

(bitfield msg-flags
  ((:dontwait "MSG_DONTWAIT"))
  ((:peek "MSG_PEEK")))
