;;; packages.lisp - package definitions for client & all stable protocols

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
           #:wl-event-listener
           #:read-event
           #:handle-event

           #:find-interface-named
           #:find-proxy
           #:make-proxy
           #:display-pathname
           #:wl-display-connect
           #:wl-display-disconnect
           #:wl-display-listen
           #:wl-display-dispatch-event
           #:wl-display-roundtrip

           #:define-interface-class
           #:define-enum
           #:define-request-function
           #:define-event-handler)
  (:documentation "Wayland client implementation

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers."))

(defpackage #:xyz.shunter.wayhack.client.presentation-time)
(defpackage #:xyz.shunter.wayhack.client.viewporter)
(defpackage #:xyz.shunter.wayhack.client.xdg-shell)
