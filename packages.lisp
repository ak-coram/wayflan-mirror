;;; packages.lisp -- package definitions for client & all stable protocols
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defpackage #:xyz.shunter.wayflan.wire
  (:use #:cl)
  (:local-nicknames (#:io #:fast-io)
                    (#:a #:alexandria))
  (:export #:wl-int
           #:wl-uint

           #:read-wl-int
           #:read-wl-uint
           #:read-wl-fixed
           #:read-wl-string
           #:read-wl-array
           #:read-wl-message
           #:with-input-from-message

           #:write-wl-int
           #:write-wl-uint
           #:write-wl-fixed
           #:write-wl-string
           #:write-wl-array
           #:write-wl-message
           #:with-output-as-message)
  (:documentation "Wayland wire format marshalling.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

The wayland-wire package defines utilities for communicating primitive data through a fast-io buffer connected to a Wayland UNIX socket stream."))

(defpackage #:xyz.shunter.wayflan.client
  (:nicknames #:wayflan-client)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:io #:fast-io)
                    (#:wire #:xyz.shunter.wayflan.wire)
                    (#:sock #:xyz.shunter.wayflan.sockets))
  (:export #:wl-interface
           #:wl-interface-version
           #:wl-interface-name
           #:find-interface-named

           #:wl-proxy
           #:wl-proxy-id
           #:wl-proxy-display
           #:wl-proxy-version
           #:wl-proxy-hooks

           #:wl-display
           #:wl-destroyed-proxy

           #:find-proxy
           #:destroy-proxy

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
           #:define-event)
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
  (:export #:wl-include)
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
