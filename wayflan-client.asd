;;; wayflan-client.asd -- Synonym system definition
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defsystem #:wayflan-client
  :version (:read-file-form "version.lisp")
  :author "Samuel Hunter"
  :license "BSD 3-Clause"

  :description "Wayland client"
  :depends-on (#:wayflan))
