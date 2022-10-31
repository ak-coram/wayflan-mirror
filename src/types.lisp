;;; types.lisp -- Wayflan data types
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan)



(deftype octet ()
  '(unsigned-byte 8))

(deftype wl-int ()
  '(signed-byte 32))

(deftype wl-uint ()
  '(unsigned-byte 32))

(deftype wl-fixed ()
  '(real #x-800000 #x7fffffff/100))

(deftype wl-array (&optional (size '*))
  `(simple-array (unsigned-byte 8) (,size)))

(defconstant +most-positive-wl-uint+ (1- (ash 1 32)))
(defconstant +most-positive-wl-int+ (1- (ash 1 31)))
(defconstant +most-positive-wl-fixed+ #x7fffffff/100)
(defconstant +most-negative-wl-int+ (- (ash 1 31)))
(defconstant +most-negative-wl-fixed+ #x-800000)
