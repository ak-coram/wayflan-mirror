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
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:ffi #:xyz.shunter.wayflan.ffi))
  (:export #:wl-int
           #:wl-uint
           #:wl-fixed
           #:wl-array

           #:+most-positive-wl-uint+
           #:+most-positive-wl-int+
           #:+most-positive-wl-fixed+
           #:+most-negative-wl-int+
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

(defpackage #:xyz.shunter.wayflan.client
  (:nicknames #:wayflan-client)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:wire #:xyz.shunter.wayflan.wire))
  (:import-from #:xyz.shunter.wayflan.wire
                #:wl-uint
                #:wl-int
                #:wl-fixed
                #:wl-array

                #:+most-positive-wl-uint+
                #:+most-positive-wl-int+
                #:+most-positive-wl-fixed+
                #:+most-negative-wl-int+
                #:+most-negative-wl-fixed+)
  (:export #:wl-uint
           #:wl-int
           #:wl-fixed
           #:wl-array

           #:wl-interface-class
           #:wl-interface-version
           #:wl-interface-name
           #:find-interface-named

           #:wl-proxy
           #:wl-proxy-id
           #:wl-proxy-display
           #:wl-proxy-version
           #:wl-proxy-hooks
           #:wl-destroyed-proxy

           #:wl-display
           #:wl-display-pathname

           #:find-proxy
           #:destroy-proxy

           #:wl-enum-value
           #:wl-enum-keyword

           #:wl-error
           #:wl-error-object
           #:wl-error-code
           #:wl-error-message

           #:display-pathname
           #:wl-display-connect
           #:wl-display-disconnect
           #:wl-display-listen
           #:wl-display-dispatch-event
           #:wl-display-roundtrip

           #:define-interface
           #:define-enum
           #:define-request
           #:define-event

           #:+most-positive-wl-uint+
           #:+most-positive-wl-int+
           #:+most-positive-wl-fixed+
           #:+most-negative-wl-int+
           #:+most-negative-wl-fixed+

           #:with-open-display
           #:with-proxy
           #:event-case
           #:event-ccase
           #:event-ecase
           #:evlambda
           #:evclambda
           #:evelambda)
  (:documentation "Wayland client and core protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package defines the client's interpretation of various CLOS classes, the event protocol, define- macros to implement wayland protocols, and the core Wayland protocol."))

(defpackage #:xyz.shunter.wayflan.autowrap
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:client #:xyz.shunter.wayflan.client)
                    (#:dom #:org.shirakumo.plump.dom))
  (:export #:wl-client-include)
  (:documentation "Wayland XML protocol definitions auto-wrapper.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package defines WL-INCLUDE, a macro that transforms a Wayland protocol described in an XML file into wayflan definition forms."))

(defpackage #:xyz.shunter.wayflan.client.presentation-time
  (:use #:cl #:xyz.shunter.wayflan.client)
  (:nicknames #:wayflan-client.presentation-time)
  (:documentation "Wayland Presentation time protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package implements the stable Presentation Time protocol."))

(defpackage #:xyz.shunter.wayflan.client.viewporter
  (:use #:cl #:xyz.shunter.wayflan.client)
  (:nicknames #:wayflan-client.viewporter)
  (:documentation "Wayland Viewporter protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package implements the stable Viewporter protocol."))

(defpackage #:xyz.shunter.wayflan.client.xdg-shell
  (:use #:cl #:xyz.shunter.wayflan.client)
  (:nicknames #:wayflan-client.xdg-shell)
  (:documentation "Wayland XDG shell protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package implements the stable XDG shell protocol."))
