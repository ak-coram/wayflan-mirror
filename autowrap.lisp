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

(defun interface-name (dom-interface)
  (intern (hyphenize (name dom-interface))))

(defun interface-event-name (dom-interface)
  (intern (hyphenize (name dom-interface)
                     '#:event)))

(defun request-name (interface-name dom-request)
  (intern (hyphenize interface-name (name dom-request))))

(defun event-name (interface-name dom-event)
  (intern (hyphenize interface-name
                     (name dom-event)
                     '#:event)))

(defun enum-name (interface-name dom-enum)
  (intern (hyphenize interface-name (name dom-enum))))

(defun enum-entry-name (interface-name dom-enum dom-entry)
  (intern (format nil "+~A+"
                  (hyphenize interface-name (name dom-enum) (name dom-entry)))))

(defun event-arg-name (dom-arg)
  (intern (hyphenize '#:wl-event (name dom-arg))))

(defun request-arg-name (dom-arg)
  (intern (lispify (name dom-arg))))

(defun arg-type (dom-arg)
  "Return the lispified type of the DOM arg element."
  ;; TODO intern the interface/enum types into symbols
  (a:eswitch ((dom:attribute dom-arg "type") :test 'string=)
    ("int" :int)
    ("uint" (a:if-let ((enum (dom:attribute dom-arg "enum")))
              (list :uint (intern (lispify enum)))
              :uint))
    ("fixed" :fixed)
    ("string" :string)
    ("object" (a:if-let ((interface (dom:attribute dom-arg "interface")))
                (list :object (intern (lispify interface)))
                :object))
    ("new_id" (a:if-let ((interface (dom:attribute dom-arg "interface")))
                (list :new-id (intern (lispify interface)))
                :new-id))
    ("array" :array)
    ("fd" :fd)))

(defun transform-arg (dom-arg &key event-p)
  (let ((name (if event-p
                  (event-arg-name dom-arg)
                  (request-arg-name dom-arg))))
    (pushnew name *syms-to-export*)
    `(,name
       :type ,(arg-type dom-arg)
       ,@(when event-p
           `(:initarg ,(a:make-keyword (lispify (name dom-arg)))))
       ,@(a:when-let ((summary (summary* dom-arg)))
           `(:documentation ,summary)))))

(defun transform-request (dom-request interface opcode)
  (let ((name (request-name interface dom-request)))
    (pushnew name *syms-to-export*)

    `(client:define-request ,(list name interface opcode)
       ,(map 'list (a:rcurry #'transform-arg :event-p nil)
             (args dom-request))
       ,@(a:when-let ((summary (summary* dom-request)))
           `((:documentation ,summary)))
       ,@(a:when-let ((type (dom:attribute dom-request "type")))
           `((:type ,(a:make-keyword (lispify type))))))))

(defun transform-event (dom-event interface-event interface opcode)
  (let ((name (event-name interface dom-event)))
    (pushnew name *syms-to-export*)
    `(client:define-event ,(list name interface opcode)
       ,(map 'list (a:rcurry #'transform-arg :event-p t)
             (args dom-event))
       (:event-superclasses ,interface-event)
       ,@(a:when-let ((summary (summary* dom-event)))
           `((:documentation ,summary))))))

(defun transform-enum (dom-enum interface)
  (let ((name (enum-name interface dom-enum)))
    `(client:define-enum ,name ()
       ,(map 'list
             (lambda (dom-entry)
               (let ((entry-name (enum-entry-name interface dom-enum dom-entry)))
                 (pushnew entry-name *syms-to-export*)
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
        (event-name (interface-event-name dom-interface))
        (requests (requests dom-interface))
        (events (events dom-interface))
        (enums (enums dom-interface)))
    (pushnew name *syms-to-export*)
    (pushnew event-name *syms-to-export*)

    `((client:define-interface ,name ()
        ,@(when (member name *exclude-defclasses*
                        :test #'string=)
            `((:skip-defclass t)))
        (:version ,version)
        (:event-class ,event-name)

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
                  (transform-event dom-event event-name name (incf opcode)))
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

(defmacro wl-include (input &key export exclude-defclasses)
  "Define the collection of interfaces, enums, requests, and events described by INPUT, for use by a Wayland client.

INPUT - A stream to an XML file, a pathname to an XML file, or an XML string.
EXPORT - If true, export all interned symbols in the current package."
  (let ((plump:*tag-dispatchers* plump:*xml-tags*)
        *syms-to-export*
        (*exclude-defclasses* exclude-defclasses))
    `(progn
       ,@(transform-protocol
           (protocol (plump:parse input)))
       ,@(when export
           `((export ',*syms-to-export*))))))
