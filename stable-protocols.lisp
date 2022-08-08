;;; stable-protocols.lisp -- Wayland stable protocols implementations
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

;; The current stable protocol xml's are from wayland-protocols release 1.25.0
;; (Jan 2022)

(in-package #:xyz.shunter.wayflan.client.presentation-time)

(xyz.shunter.wayflan.autowrap:wl-include
  '(#:wayflan #:protocols "presentation-time.xml")
  :export t)

(in-package #:xyz.shunter.wayflan.client.viewporter)

(xyz.shunter.wayflan.autowrap:wl-include
  '(#:wayflan #:protocols "viewporter.xml")
  :export t)

(in-package #:xyz.shunter.wayflan.client.xdg-shell)

(xyz.shunter.wayflan.autowrap:wl-include
  '(#:wayflan #:protocols "xdg-shell.xml")
  :export t)
