;;; src/ffi.lisp - POSIX.1 socket foreign function interface
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.ffi)



(cffi:define-foreign-library libc
  (:default "libc"))

(cffi:use-foreign-library libc)

(defcstruct iovec
  (iov-base :pointer)
  (iov-len size))

(defcstruct msghdr
  (msg-name :pointer)
  (msg-namelen socklen)
  (msg-iov (:pointer (:struct iovec)))
  (msg-iovlen size)
  (msg-control :pointer)
  (msg-controllen size)
  (msg-flags :int))

(defcstruct cmsghdr
  (cmsg-len size)
  (cmsg-level :int)
  (cmsg-type :int)
  ;; Followed by an unsigned char CMSG_DATA(cmsghdr)
  )

(defcstruct sockaddr-un
  (sun-family sa-family)
  (sun-path :char :count 108))

(defcfun "socket" :int
  (domain :int)
  (type :int)
  (protocol :int))

(defcfun "bind" :int
  (sockfd :int)
  (addr :pointer)
  (addrlen socklen))

(defcfun "accept" :int
  (sockfd :int)
  (addr :pointer)
  (addrlen (:pointer socklen)))

(defcfun ("connect" connect-fd) :int
  (sockfd :int)
  (addr :pointer)
  (addrlen socklen))

(defcfun ("close" close-fd) :int
  (fd :int))

(defcfun "sendmsg" signed-size
  (sockfd :int)
  (msg (:pointer (:struct msghdr)))
  (flags msg-flags))

(defcfun "recvmsg" signed-size
  (sockfd :int)
  (msg (:pointer (:struct msghdr)))
  (flags msg-flags))

(defcfun "bzero" :void
  (s :pointer)
  (n size))

(defcfun "memcpy" :pointer
  (dest :pointer)
  (src :pointer)
  (n size))

(defcfun "strerror" :string
  (errnum errno))

(cffi:defcvar ("errno" *errno*) errno)
