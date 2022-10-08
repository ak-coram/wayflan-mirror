;;; stable-protocols.lisp -- Wayland stable protocols implementations
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

;; The current stable protocol xml's are from wayland-protocols release 1.25.0
;; (Jan 2022)

(in-package #:xyz.shunter.wayflan.client.presentation-time)

(xyz.shunter.wayflan.autowrap:wl-client-include
  '(#:wayflan #:protocols "presentation-time.xml")
  :export t)

(in-package #:xyz.shunter.wayflan.client.viewporter)

(xyz.shunter.wayflan.autowrap:wl-client-include
  '(#:wayflan #:protocols "viewporter.xml")
  :export t)

(in-package #:xyz.shunter.wayflan.client.xdg-shell)

(xyz.shunter.wayflan.autowrap:wl-client-include
  '(#:wayflan #:protocols "xdg-shell.xml")
  :export t)
