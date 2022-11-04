;;; src/client/asdf.lisp -- Wayflan Client ASDF integration
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan)



(defclass wayflan-scan-op (downward-operation)
  ())

(defclass wayflan-client-impl (cl-source-file)
  ((type :initform "xml")
   (in-package :initarg :in-package)
   (export :initarg :export :initform nil))
  (:documentation
    "Generates and loads a lisp file pointing to a protocol XML document"))

(defun wayflan-scan-output-file (c)
  (uiop:merge-pathnames*
    (make-pathname :type "lisp")
    (uiop:compile-file-pathname*
      (component-pathname c))))

;; prepare-op, compile-op, prepare-source-op, load-op
(defmethod component-depends-on ((o prepare-op) (c wayflan-client-impl))
  `((wayflan-scan-op ,c)
    ,@(call-next-method)))

(defmethod input-files ((o compile-op) (c wayflan-client-impl))
  (output-files 'wayflan-scan-op c))

(defmethod component-depends-on ((o prepare-source-op) (c wayflan-client-impl))
  `((wayflan-scan-op ,c)
    ,@(call-next-method)))

(defmethod input-files ((o load-op) (c wayflan-client-impl))
  (output-files 'wayflan-scan-op c))

;; wayflan-scan-op
(defmethod input-files ((o wayflan-scan-op) (c wayflan-client-impl))
  (list (component-pathname c)))

(defmethod output-files ((o wayflan-scan-op) (c wayflan-client-impl))
  (list (wayflan-scan-output-file c)))

(defun wayflan-doc-to-forms (c)
  (let ((package-symbol (make-symbol
                          (string-upcase
                            (slot-value c 'in-package)))))
  `((in-package ,package-symbol)
    (xyz.shunter.wayflan.client.scanner:wl-include
      ,(first (input-files 'wayflan-scan-op c))
      :export ,(slot-value c 'export)))))

(defmethod perform ((o wayflan-scan-op) (c wayflan-client-impl))
  (with-open-file (out (first (output-files o c))
                       :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package "KEYWORD")))
        (dolist (form (wayflan-doc-to-forms c))
          (print form out))))))

(setf (find-class 'asdf::wayflan-client-impl)
      (find-class 'wayflan-client-impl))
