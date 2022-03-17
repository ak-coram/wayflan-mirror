;;; packages.lisp -- package definitions for client & all stable protocols
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(defpackage #:xyz.shunter.wayhack.wire
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
  (:documentation "Wayland primitive data communication.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

The wayland-wire package defines utilities for communicating primitive data through a fast-io buffer connected to a Wayland UNIX socket stream."))

(defpackage #:xyz.shunter.wayhack.client
  (:nicknames #:wayhack-client)
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:io #:fast-io)
                    (#:wire #:xyz.shunter.wayhack.wire))
  (:export #:wl-proxy
           #:wl-proxy-id
           #:wl-proxy-display
           #:wl-proxy-listeners
           #:wl-deleted-proxy
           #:wl-event
           #:wl-event-sender
           #:wl-event-listener
           #:wl-error
           #:wl-error-object
           #:wl-error-code
           #:wl-error-message
           #:read-event
           #:handle-event

           #:find-interface-named

           #:find-proxy
           #:make-proxy
           #:display-pathname
           #:wl-display
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

(defpackage #:xyz.shunter.wayhack.autowrap
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:dom #:org.shirakumo.plump.dom))
  (:import-from #:xyz.shunter.wayhack.client
                #:define-interface
                #:define-enum
                #:define-request
                #:define-event
                #:wl-display)
  (:export #:wl-include)
  (:documentation "Wayland XML protocol autowrapper

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package defines WL-INCLUDE, a macro that transforms a Wayland protocol described in an XML file into wayhack definition forms."))

(defpackage #:xyz.shunter.wayhack.client.presentation-time
  (:documentation "Wayland Presentation time protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package implements the stable Presentation time protocol."))

(defpackage #:xyz.shunter.wayhack.client.viewporter
  (:documentation "Wayland Viewporter protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package implements the stable Viewporter protocol."))

(defpackage #:xyz.shunter.wayhack.client.xdg-shell
  (:documentation "Wayland XDG shell protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package implements the stable XDG shell protocol."))
