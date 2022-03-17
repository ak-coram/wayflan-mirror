;;; protocols.lisp - Stable protocol implementation

;; The current wayland.xml is from wayland release 1.20.0 (Dec 2021)
;;
;; The current stable protocol xml's are from wayland-protocols release 1.25.0
;; (Jan 2022)

(cl:in-package #:xyz.shunter.wayhack.client)

(xyz.shunter.wayhack.autowrap:wl-include
  #.(cl:merge-pathnames #P"protocols/wayland.xml"
                        (asdf:system-source-directory '#:wayhack))
  :export t)

(cl:in-package #:xyz.shunter.wayhack.client.presentation-time)

(xyz.shunter.wayhack.autowrap:wl-include
  #.(cl:merge-pathnames #P"protocols/presentation-time.xml"
                        (asdf:system-source-directory '#:wayhack))
  :export t)

(cl:in-package #:xyz.shunter.wayhack.client.viewporter)

(xyz.shunter.wayhack.autowrap:wl-include
  #.(cl:merge-pathnames #P"protocols/viewporter.xml"
                        (asdf:system-source-directory '#:wayhack))
  :export t)

(cl:in-package #:xyz.shunter.wayhack.client.xdg-shell)

(xyz.shunter.wayhack.autowrap:wl-include
  #.(cl:merge-pathnames #P"protocols/xdg-shell.xml"
                        (asdf:system-source-directory '#:wayhack))
  :export t)
