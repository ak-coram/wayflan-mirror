;;; autowrap.lisp -- Protocol XML definition auto-wrapper
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(in-package #:xyz.shunter.wayhack.autowrap)

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



;; Transformation Functions

(defun lispify (string)
  "Transform a snake_cased string into a string fit to turn to a symbol."
  (map 'string
       (lambda (char)
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
  (intern (format nil "~A.~A"
                  (lispify interface-name)
                  (lispify (name dom-enum)))))

(defun arg-name (dom-arg)
  (intern (hyphenize '#:wl-event (name dom-arg))))

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

(defun transform-arg (dom-arg syms)
  (let ((name (arg-name dom-arg)))
    (pushnew name (car syms))
    (list* name
           :initarg (a:make-keyword (lispify (name dom-arg)))
           :type (arg-type dom-arg)
           (a:when-let ((summary (summary* dom-arg)))
             (list :documentation summary)))))

(defun transform-request (dom-request interface opcode syms)
  (let ((name (request-name interface dom-request)))
    (pushnew name (car syms))

    `(define-request ,(list name interface opcode)
       ,(map 'list (a:rcurry #'transform-arg syms)
             (args dom-request))
       ,@(a:when-let ((summary (summary* dom-request)))
           `((:documentation ,summary)))
       ,@(a:when-let ((type (dom:attribute dom-request "type")))
           `((:type (a:make-keyword (string-upcase type))))))))

(defun transform-event (dom-event interface-event interface opcode syms)
  (let ((name (event-name interface dom-event)))
    (pushnew name (car syms))
    `(define-event ,(list name interface opcode)
       ,(map 'list (a:rcurry #'transform-arg syms)
             (args dom-event))
       (:event-superclasses ,interface-event)
       ,@(a:when-let ((summary (summary* dom-event)))
           `((:documentation ,summary))))))

(defun transform-enum (dom-enum interface)
  (let ((name (enum-name interface dom-enum)))
    `(define-enum ,name ()
       ,(map 'list
             (lambda (dom-entry)
               (list (a:make-keyword (lispify (name dom-entry)))
                     (value dom-entry)
                     :documentation (summary dom-entry)))
             (entries dom-enum))
       ,@(a:when-let ((summary (summary* dom-enum)))
           `((:documentation ,summary)))
       ,@(when (string= "true" (bitfield dom-enum))
           `((:bitfield t))))))

(defun transform-interface (dom-interface syms)
  (let ((name (interface-name dom-interface))
        (event-name (interface-event-name dom-interface))
        (requests (requests dom-interface))
        (events (events dom-interface))
        (enums (enums dom-interface)))
    (pushnew name (car syms))
    (pushnew event-name (car syms))

    `((define-interface ,name ()
        ;; wl_display is specially defined, so don't stub this out.
        ,@(when (string= (name dom-interface) "wl_display")
            `((:skip-defclass t)))
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
                  (transform-request dom-request name (incf opcode) syms))
                requests))

       ,@(let ((opcode -1))
           (map 'list
                (lambda (dom-event)
                  (transform-event dom-event event-name name (incf opcode) syms))
                events)))))

(defun transform-protocol (dom-protocol syms)
  (mapcan (a:rcurry #'transform-interface syms)
          (coerce (interfaces dom-protocol) 'list)))

(defmacro wl-include (input &key export)
  "Define the collection of interfaces, enums, requests, and events described by INPUT, for use by a Wayland client.

INPUT - A stream to an XML file, a pathname to an XML file, or an XML string.
EXPORT - If true, export all interned symbols in the current package."
  (let ((plump:*tag-dispatchers* plump:*xml-tags*)
        (syms (cons () nil)))
    `(progn
       ,@(transform-protocol
           (protocol (plump:parse input))
           syms)
       ,@(when export
          `((export ',(car syms)))))))
