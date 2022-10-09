;;; coverage.lisp - Generate coverage statistics
;;;
;;; Copyright (c) 2021 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(require :sb-cover)

(defpackage #:xyz.shunter.wayflan.coverage
  (:use #:cl)
  (:export #:report))

(in-package #:xyz.shunter.wayflan.coverage)



(defun report (directory)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op :wayflan :force t)
  (asdf:test-system :wayflan)
  (prog1
    (sb-cover:report directory)
    (declaim (optimize (sb-cover:store-coverage-data 0)))))

(report #P"/tmp/wayflan-coverage/")
