;;; src/packages.lisp -- package definitions for client & all stable protocols
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan
  (:nicknames #:wayflan)
  (:use #:cl #:alexandria #:asdf)
  (:export
    ;; Types
    #:wl-int
    #:wl-uint
    #:wl-fixed
    #:wl-array

    #:+most-positive-wl-uint+
    #:+most-positive-wl-int+
    #:+most-positive-wl-fixed+
    #:+most-negative-wl-int+
    #:+most-negative-wl-fixed+

    ;; Conditions
    #:wl-error
    #:wl-socket-error
    #:wl-message-error
    #:wl-server-error

    ;; Protocol
    #:wl-protocol
    #:wl-interface
    #:wl-request
    #:wl-event
    #:wl-enum
    #:wl-entry
    #:wl-arg
    #:wl-description

    #:wl-name
    #:wl-copyright
    #:wl-interfaces
    #:wl-version
    #:wl-requests
    #:wl-events
    #:wl-enums
    #:wl-type
    #:wl-args
    #:wl-since
    #:wl-bitfield
    #:wl-entries
    #:wl-value
    #:wl-summary
    #:wl-text

    #:wl-parse)
  (:documentation "Wayland protocol and type information

Wayland is a protocol for clients to talk to a display server to make themselves visible or get input from the user.
The server can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.

This package defines Wayland protocol CLOS objects, a function that parses a protocol document into CLOS, and a series of Wayland primitives as lisp types."))

(defpackage #:xyz.shunter.wayflan.ffi
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
           #:+ewouldblock+
           #:+epipe+

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
           #:connect-fd
           #:close-fd
           #:sendmsg
           #:recvmsg

           #:bzero
           #:memcpy
           #:strerror
           #:*errno*)
  (:documentation "PRIVATE: This package is private to Wayflan, and its API is susceptible to change. Please do not use this package in your own code."))

(defpackage #:xyz.shunter.wayflan.wire
  (:use #:cl #:alexandria #:xyz.shunter.wayflan
        #:xyz.shunter.wayflan.ffi)
  (:export #:data-socket
           #:make-socket
           #:connect
           #:close-socket
           #:listen-socket
           #:read-fd

           #:send-wl-message

           #:read-wl-int
           #:read-wl-uint
           #:read-wl-fixed
           #:read-wl-string
           #:read-wl-array
           #:with-incoming-message)
  (:documentation "PRIVATE: This package is private to Wayflan, and its API is susceptible to change. Please do not use this package in your own code."))
