;;; src/packages.lisp -- package definitions for client & all stable protocols
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

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
           #:*errno*)
  (:documentation "PRIVATE: This package is private to Wayflan, and its API is susceptible to change. Please do not use this package in your own code."))

(defpackage #:xyz.shunter.wayflan.wire
  (:use #:cl #:alexandria)
  (:local-nicknames (#:ffi #:xyz.shunter.wayflan.ffi))
  (:export #:wl-int
           #:wl-uint
           #:wl-fixed
           #:wl-array

           #:+most-positive-wl-uint+
           #:+most-positive-wl-int+
           #:+most-negative-wl-int+
           #:+most-positive-wl-fixed+
           #:+most-negative-wl-fixed+

           #:data-socket
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
  (:documentation "PRIVATE: Wayland wire format marshalling.

The wayland-wire package defines utilities for communicating primitive data through a local address socket

PRIVATE: This package is private to Wayflan, and its API is susceptible to change. Please do not use this package in your own code."))

(defpackage #:xyz.shunter.wayflan
  (:nicknames #:wayflan)
  (:use #:cl #:alexandria)
  (:export #:wl-protocol
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

           #:parse)
  (:documentation "Wayland protocol information

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package defines Wayland protocol CLOS objects, and a function that parses a protocol document into CLOS."))

(defpackage #:xyz.shunter.wayflan.asdf
  (:use #:cl #:asdf)
  (:export #:wayflan-client-impl))
