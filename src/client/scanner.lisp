;;; src/autowrap.lisp -- Wayland XML protocol definitions auto-wrapper
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.client.scanner)



(defvar *syms-to-export*)
(defvar *exclude-defclasses*)

;; Transformation Functions

(defun %lispify (object)
  (map 'string
       (lambda (char)
         (if (char= char #\_)
             #\- char))
       (string-upcase
         (if (stringp object)
             object
             (wl-name object)))))

(defun %dot (&rest objects)
  (format nil "~{~A~^.~}" (mapcar '%lispify objects)))

(defun %documentation (description)
  (format nil "~@[~A~%~%~]~A"
          (wl-summary description)
          (wl-text description)))

(defun %transform-arg (arg-interface arg)
  (let ((name (make-symbol (%lispify arg))))
    `(,name
       :type ,(destructuring-bind (name &key interface (allow-null nil anp) enum)
                                  (wl-type arg)
                `(,name
                   ,@(when interface
                       (let ((sym (intern (%lispify interface))))
                         (pushnew sym *syms-to-export*)
                         (list :interface sym)))
                   ,@(when anp
                       (list :allow-null allow-null))
                   ,@(when enum
                       (let ((sym (intern
                                    (if (position #\. enum)
                                        (%lispify enum)
                                        (%dot arg-interface enum)))))
                         (pushnew sym *syms-to-export*)
                         (list :enum sym)))))
       ,@(when-let ((summary (wl-summary arg)))
           (list :documentation summary)))))

(defun %transform-request (interface opcode request)
  (let ((name (intern (%dot interface request)))
        (interface-name (intern (%lispify interface))))
    (pushnew name *syms-to-export*)
    (pushnew interface-name *syms-to-export*)

    `(define-request (,name ,interface-name ,opcode)
       ,(mapcar (curry '%transform-arg interface) (wl-args request))
       (:since ,(wl-since request))
       ,@(when-let ((type (wl-type request)))
           `((:type ,type)))
       ,@(when-let ((description (wl-description request)))
           `((:documentation ,(%documentation description)))))))

(defun %transform-event (interface opcode request)
  (let ((name (make-keyword (%lispify request)))
        (interface-name (intern (%lispify interface))))
    (pushnew interface-name *syms-to-export*)

    `(define-event (,name ,interface-name ,opcode)
       ,(mapcar (curry '%transform-arg interface) (wl-args request))
       (:since ,(wl-since request))
       ,@(when-let ((type (wl-type request)))
           `((:type ,type)))
       ,@(when-let ((description (wl-description request)))
           `((:documentation ,(%documentation description)))))))

(defun %transform-enum (interface enum)
  (let ((name (intern (%dot interface enum))))
    (pushnew name *syms-to-export*)

    `(define-enum ,name ()
       ,(mapcar (lambda (entry)
                  `(,(make-keyword (%lispify entry))
                     ,(wl-value entry)
                     ,@(when-let ((summary (wl-summary entry)))
                         (list :summary summary))))
                (wl-entries enum))
       (:bitfield ,(wl-bitfield enum))
       ,@(when-let ((description (wl-description enum)))
           `((:documentation (%documentation description)))))))

(defun %transform-interface (interface)
  (let ((name (intern (%lispify interface))))
    (pushnew name *syms-to-export*)

    `((define-interface ,name ()
        ,@(when (member name *exclude-defclasses* :test #'string=)
            `((:skip-defclass t)))
        (:version ,(wl-version interface))
        (:interface-name ,(wl-name interface))
        ,@(when-let ((description (wl-description interface)))
            `((:documentation ,(%documentation description)))))
      ,@(mapcar (curry '%transform-enum interface) (wl-enums interface))
      ,@(let ((opcode -1))
          (mapcar (lambda (request)
                    (%transform-request interface (incf opcode) request))
                  (wl-requests interface)))
      ,@(let ((opcode -1))
          (mapcar (lambda (event)
                    (%transform-event interface (incf opcode) event))
                  (wl-events interface))))))

(defun %transform-protocol (protocol)
  (let* ((define-forms (mapcar '%transform-interface
                               (wl-interfaces protocol)))
         ;; Each transformed interface starts with a single DEFINE-INTERFACE
         ;; form, followed by all other DEFINE-X forms.
         (define-interface-forms (mapcar #'first define-forms))
         (other-define-forms (mapcan #'rest define-forms)))
    ;; Transform all DOM interface definitions into define-X lisp forms, and
    ;; then rearrange them such that all DEFINE-INTERFACE forms are on top
    ;; (so that all other DEFINE-* forms has access to all classes).

    ;; If I don't do this, then the compiler will warn about type-checking for
    ;; types that aren't yet defined.
    (append define-interface-forms other-define-forms)))

(defun %pathname-or-input (input)
  (etypecase input
    (cons (asdf:component-pathname
            (or (asdf:find-component (first input) (rest input))
                (error "System ~S path not found: ~S"
                       (first input) (rest input)))))
    ((or pathname string stream) input)))

(defmacro wl-include (input &key export exclude-defclasses)
  "Define the collection of interfaces, enums, requests, and events described by INPUT, for use by a Wayland client.

INPUT - A stream to an XML file, a pathname to an XML file, or an XML string.
EXPORT - If true, export all interned symbols in the current package."
  (let ((*exclude-defclasses* exclude-defclasses)
        *syms-to-export*)
    `(progn
       ,@(%transform-protocol (wl-parse (%pathname-or-input (eval input))))
       ,(when export
          `(export ',*syms-to-export*)))))
