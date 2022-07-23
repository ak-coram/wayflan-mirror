;;; sockets/ffi.lisp - POSIX.1 socket foreign function interface
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel@shunter.xyz>
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.sockets.ffi)



(autowrap:c-include '(#:wayflan/sockets #:spec "socket.h")
                    :spec-path '(#:wayflan/sockets #:spec)
                    :exclude-definitions (".*")
                    :include-definitions ("_t$"
                                          "^__"

                                          ;; Structs
                                          "^iovec$"
                                          "^msghdr$"
                                          "^cmsghdr$"
                                          "^sockaddr$"
                                          "^sockaddr_un$"

                                          ;; Constants
                                          "^O_" ;; open flags
                                          "^S_" ;; permission bits
                                          "^E" ;; errno constants
                                          "^AF_UNIX$"
                                          "^SOCK_STREAM$"
                                          "^MSG_"

                                          ;; Macros
                                          "^CMSG_"

                                          ;; Syscalls
                                          "^socket$"
                                          "^bind$"
                                          "^accept$"
                                          "^connect$"
                                          "^close$"
                                          "^sendmsg$"
                                          "^recvmsg$"

                                          ;; C lib
                                          "^bzero$"
                                          "^memcpy$"
                                          "^memset$"
                                          "^memmove$"
                                          "^strerror$"))

(cffi:defcvar "errno" :int)

(cffi:define-foreign-library libc
  (:default "libc"))

(cffi:use-foreign-library libc)
