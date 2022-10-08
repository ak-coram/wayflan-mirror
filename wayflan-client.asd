;;; wayflan-client.asd -- Synonym socket definition
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; All rights reserved.

(defsystem #:wayflan-client
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "Proprietary"

  :description "Wayland client"
  :depends-on (#:wayflan))
