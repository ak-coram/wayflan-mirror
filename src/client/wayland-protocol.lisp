;;; src/wayland-protocol.lisp -- Wayland core protocol implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

;; The current wayland.xml is from wayland release 1.20.0 (Dec 2021)
;;
;; This file is loaded before all other protocols, as other protocols are
;; dependent on wayland core types (such as wl-output, wl-buffer, etc).
;; Separating them out by file prevents symbol conflicts, since evaluating
;; (export ...) at exec-time generated by WL-INCLUDE happens after interning
;; symbols at macro-expansion time.

(in-package #:xyz.shunter.wayflan.client)

(xyz.shunter.wayflan.client.scanner:wl-include
  '(#:wayflan/common #:protocols "wayland.xml")
  :exclude-defclasses (wl-display)
  :export t)
