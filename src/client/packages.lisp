;;; src/client-packages.lisp -- client implementation package definitions
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(uiop:define-package #:xyz.shunter.wayflan.client
  (:nicknames #:wayflan-client)
  (:use #:cl #:alexandria #:xyz.shunter.wayflan.wire)
  (:use-reexport #:xyz.shunter.wayflan)
  (:export #:wl-interface-class
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

           #:wl-display-connect
           #:wl-display-disconnect
           #:wl-display-listen
           #:wl-display-dispatch-event
           #:wl-display-roundtrip

           #:with-open-display
           #:with-proxy
           #:event-case
           #:event-ccase
           #:event-ecase
           #:evlambda
           #:evclambda
           #:evelambda

           #:define-interface
           #:define-enum
           #:define-request
           #:define-event)
  (:documentation "Wayland client and core protocol implementation.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

This package defines the client's interpretation of various CLOS classes, the event protocol, define- macros to implement wayland protocols, and the core Wayland protocol."))

(defpackage #:xyz.shunter.wayflan.client.scanner
  (:use #:cl #:alexandria #:xyz.shunter.wayflan.client)
  (:export #:wl-include)
  (:documentation "PRIVATE: Wayland XML protocol definitions auto-wrapper.

This package defines WL-INCLUDE, a macro that transforms a Wayland protocol described in an XML file into wayflan definition forms.

PRIVATE: This package is private to Wayflan, and its API is susceptible to change. Please do not use this package in your own code."))

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
