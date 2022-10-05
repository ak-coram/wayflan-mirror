;;; autowrap.lisp -- Wayland XML protocol definitions auto-wrapper
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.autowrap)



;; DOM-walking utils and macros

(defmacro definline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defun child-of-tag (tag-name parent)
  "Return the assumed sole child of PARENT."
  (find-if (lambda (child)
             (and (dom:element-p child)
                  (string= tag-name (dom:tag-name child))))
           (dom:children parent)))

(defun children-of-tag (tag-name parent)
  "Return an array of all child elements with the given tag."
  (remove-if-not
    (lambda (child)
      (and (dom:element-p child)
           (string= tag-name (dom:tag-name child))))
    (dom:children parent)))

(defun child-named (name tag-name parent)
  "Find the sole child tagged TAG-NAME with the given name attribute."
  (find-if (lambda (child)
             (and (dom:element-p child)
                  (string= tag-name (dom:tag-name child))
                  (string= name (dom:attribute child "name"))))
           (children-of-tag tag-name parent)))

(defmacro define-attribute-reader (name &optional attribute-name)
  "Define a reader function for the attribute value of a given element."
  `(definline ,name (elm)
     (dom:attribute elm ,(or attribute-name (string-downcase name)))))

(defmacro define-child-reader (name &optional tag-name)
  "Define a reader function for the sole child of the given tag name of a given parent."
  `(definline ,name (parent)
     (child-of-tag ,(or tag-name (string-downcase name)) parent)))

(defmacro define-children-reader (name &optional tag-name)
  "Define a reader function for all children of the given tag name of a given parent."
  `(definline ,name (parent)
     (children-of-tag ,(or tag-name (string-downcase name)) parent)))


(define-child-reader protocol)

(define-children-reader interfaces "interface")
(define-children-reader requests "request")
(define-children-reader events "event")
(define-children-reader args "arg")
(define-children-reader enums "enum")
(define-children-reader entries "entry")

(define-attribute-reader name)
(define-attribute-reader summary)
(define-attribute-reader bitfield)
(define-attribute-reader version)

(defun description (elm)
  (string-trim
    #(#\Newline #\Space #\Tab)
    (dom:text (child-of-tag "description" elm))))

(defun summary* (elm)
  "Return the summary of an element whose summary is stored in a child description element."
  (a:when-let ((description (child-of-tag "description" elm)))
    (dom:attribute description "summary")))

(defun parse-value (string)
  (if (and (< 1 (length string))
           (char= (char string 0) #\0))
      (if (char= #\x (char string 1))
          (parse-integer string :start 2 :radix 16)
          (parse-integer string :start 1 :radix 8))
      (parse-integer string :radix 10)))

(defun value (entry)
  (parse-value (dom:attribute entry "value")))



(defvar *syms-to-export*)
(defvar *exclude-defclasses*)

;; Transformation Functions

(defun lispify (string)
  "Transform a snake_cased string into a string fit to turn to a symbol."
  (map 'string
       (lambda (char)
         (declare (type character char))
         (if (char= char #\_)
             #\-
             char))
       (string-upcase string)))

(defun hyphenize (&rest strings)
  (format nil "~{~A~^-~}" (mapcar #'lispify strings)))

(defun dot (base &rest rest)
  (format nil "~A.~A" (lispify base) (apply #'hyphenize rest)))

(defun interface-name (dom-interface)
  (intern (lispify (name dom-interface))))

(defun interface-event-name (dom-interface)
  (intern (hyphenize (name dom-interface)
                     '#:event)))

(defun request-name (interface-name dom-request)
  (intern (dot interface-name (name dom-request))))

(defun event-name (dom-event)
  (a:make-keyword (lispify (name dom-event))))

(defun enum-name (interface-name dom-enum)
  (intern (dot interface-name (name dom-enum))))

(defun enum-name* (interface-name enum-name)
  (intern
    (if (position #\. enum-name)
        (lispify enum-name)
        (dot interface-name enum-name))))

(defun enum-entry-name (dom-entry)
  (a:make-keyword (lispify (name dom-entry))))

(defun arg-name (dom-arg)
  (intern (lispify (name dom-arg))))

(defun arg-type (interface-name dom-arg)
  "Return the lispified type of the DOM arg element."
  ;; TODO intern the interface/enum types into symbols
  (a:eswitch ((dom:attribute dom-arg "type") :test 'string=)
    ("int" (a:if-let ((enum (dom:attribute dom-arg "enum")))
             (list :int (enum-name* interface-name enum))
             :int))
    ("uint" (a:if-let ((enum (dom:attribute dom-arg "enum")))
              (list :uint (enum-name* interface-name enum))
              :uint))
    ("fixed" :fixed)
    ("string"
     `(:string
        ,@(a:when-let ((allow-null (dom:attribute dom-arg "allow-null")))
            `(:allow-null (string= allow-null "true")))))
    ("object"
     `(:object
        ,@(a:when-let ((interface (dom:attribute dom-arg "interface")))
            `(:interface ,(intern (lispify interface))))
        ,@(a:when-let ((allow-null (dom:attribute dom-arg "allow-null")))
            `(:allow-null (string= allow-null "true")))))
    ("new_id"
     `(:new-id
        ,@(a:when-let ((interface (dom:attribute dom-arg "interface")))
            `(:interface ,(intern (lispify interface))))))
    ("array" :array)
    ("fd" :fd)))

(defun transform-arg (interface-name dom-arg)
  (let ((name (arg-name dom-arg)))
    `(,name
       :type ,(arg-type interface-name dom-arg)
       ,@(a:when-let ((summary (summary* dom-arg)))
           `(:documentation ,summary)))))

(defun transform-request (dom-request interface opcode)
  (let ((name (request-name interface dom-request)))
    (pushnew name *syms-to-export*)

    `(client:define-request ,(list name interface opcode)
       ,(map 'list (a:curry 'transform-arg interface)
             (args dom-request))
       ,@(a:when-let ((summary (summary* dom-request)))
           `((:documentation ,summary)))
       ,@(a:when-let ((type (dom:attribute dom-request "type")))
           `((:type ,(a:make-keyword (lispify type)))))
       ,@(a:when-let ((since (dom:attribute dom-request "since")))
           `((:since ,(parse-integer since)))))))

(defun transform-event (dom-event interface opcode)
  (let ((name (event-name dom-event)))
    `(client:define-event ,(list name interface opcode)
       ,(map 'list (a:curry 'transform-arg interface)
             (args dom-event))
       ,@(a:when-let ((summary (summary* dom-event)))
           `((:documentation ,summary)))
       ,@(a:when-let ((since (dom:attribute dom-event "since")))
           `((:since ,(parse-integer since)))))))

(defun transform-enum (dom-enum interface)
  (let ((name (enum-name interface dom-enum)))
    `(client:define-enum ,name ()
       ,(map 'list
             (lambda (dom-entry)
               (let ((entry-name (enum-entry-name dom-entry)))
                 (list entry-name
                       (value dom-entry)
                       :documentation (summary dom-entry))))
             (entries dom-enum))
       ,@(a:when-let ((summary (summary* dom-enum)))
           `((:documentation ,summary)))
       ,@(when (string= "true" (bitfield dom-enum))
           `((:bitfield t))))))

(defun transform-interface (dom-interface)
  (let ((name (interface-name dom-interface))
        (version (parse-integer (version dom-interface)))
        (requests (requests dom-interface))
        (events (events dom-interface))
        (enums (enums dom-interface)))
    (pushnew name *syms-to-export*)

    `((client:define-interface ,name ()
        ,@(when (member name *exclude-defclasses*
                        :test #'string=)
            `((:skip-defclass t)))
        (:version ,version)

        (:interface-name ,(name dom-interface))
        ,@(a:when-let ((summary (summary* dom-interface)))
            `((:documentation ,summary))))

       ,@(map 'list
              (lambda (dom-enum)
                (transform-enum dom-enum name))
              enums)

       ,@(let ((opcode -1))
           (map 'list
                (lambda (dom-request)
                  (transform-request dom-request name (incf opcode)))
                requests))

       ,@(let ((opcode -1))
           (map 'list
                (lambda (dom-event)
                  (transform-event dom-event name (incf opcode)))
                events)))))

(defun transform-protocol (dom-protocol)
  (let* ((interface-forms (mapcar #'transform-interface
                                  (coerce (interfaces dom-protocol) 'list)))
         ;; Each transformed interface starts with a single DEFINE-INTERFACE
         ;; form, followed by all other DEFINE-X forms.
         (define-interface-forms (mapcar #'first interface-forms))
         (other-define-forms (mapcan #'rest interface-forms)))
    ;; Transform all DOM interface definitions into define-X lisp forms, and
    ;; then rearrange them such that all DEFINE-INTERFACE forms are on top
    ;; (so that all other DEFINE-* forms has access to all classes).

    ;; If I don't do this, then the compiler will warn about type-checking for
    ;; types that aren't yet defined.
    (append define-interface-forms
            other-define-forms)))

(defun pathname-or-input (input)
  (etypecase input
    (cons (asdf:component-pathname
            (or (asdf:find-component (first input) (rest input))
                (error "System ~S path not found: ~S"
                       (first input) (rest input)))))
    ((or pathname string stream)
     input)))

(defmacro wl-include (input &key export exclude-defclasses)
  "Define the collection of interfaces, enums, requests, and events described by INPUT, for use by a Wayland client.

INPUT - A stream to an XML file, a pathname to an XML file, or an XML string.
EXPORT - If true, export all interned symbols in the current package."
  (let ((plump:*tag-dispatchers* plump:*xml-tags*)
        *syms-to-export*
        (*exclude-defclasses* exclude-defclasses))
    `(progn
       ,@(transform-protocol
           (protocol (plump:parse (pathname-or-input (eval input)))))
       ,@(when export
           `((export ',*syms-to-export*))))))
