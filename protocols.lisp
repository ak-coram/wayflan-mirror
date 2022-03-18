;;; protocols.lisp -- Wayland stable protocols implementations
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

;; The current wayland.xml is from wayland release 1.20.0 (Dec 2021)
;;
;; The current stable protocol xml's are from wayland-protocols release 1.25.0
;; (Jan 2022)

(in-package #:xyz.shunter.wayflan.client)

(xyz.shunter.wayflan.autowrap:wl-include
  #.(merge-pathnames #P"protocols/wayland.xml"
                     (asdf:system-source-directory '#:wayflan))
  :export t)

(in-package #:xyz.shunter.wayflan.client.presentation-time)

(xyz.shunter.wayflan.autowrap:wl-include
  #.(merge-pathnames #P"protocols/presentation-time.xml"
                     (asdf:system-source-directory '#:wayflan))
  :export t)

(in-package #:xyz.shunter.wayflan.client.viewporter)

(xyz.shunter.wayflan.autowrap:wl-include
  #.(merge-pathnames #P"protocols/viewporter.xml"
                     (asdf:system-source-directory '#:wayflan))
  :export t)

(in-package #:xyz.shunter.wayflan.client.xdg-shell)

(xyz.shunter.wayflan.autowrap:wl-include
  #.(merge-pathnames #P"protocols/xdg-shell.xml"
                     (asdf:system-source-directory '#:wayflan))
  :export t)
