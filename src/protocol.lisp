;;; src/protocol.lisp -- Protocol definition and scanner
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan)



(defclass %wl-named-object ()
  ((%name :type string :reader wl-name
          :initarg :name)))

(defmethod print-object ((object %wl-named-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (wl-name object) stream)))

(defclass wl-protocol (%wl-named-object)
  ((%copyright :type (or string null) :reader wl-copyright
               :initarg :copyright :initform nil)
   (%description :type (or wl-description null) :reader wl-description
                 :initarg :description :initform nil)
   (%interfaces :reader wl-interfaces
                :initarg :interfaces))
  (:documentation
    "A protocol (as opposed to *the* Wayland protocol) is a collection of interfaces and their requests and events all meant to accomplish some shared aim. Protocols are typically defined in XML documents and are defined by libwayland's wayland-scanner program."))

(defclass wl-interface (%wl-named-object)
  ((%version :type wl-uint :reader wl-version
             :initarg :version :initform 1)
   (%description :type (or wl-description null) :reader wl-description
                 :initarg :description :initform nil)
   (%requests :reader wl-requests
              :initarg :requests)
   (%events :reader wl-events
            :initarg :events)
   (%enums :reader wl-enums
           :initarg :enums))
  (:documentation
    "Interfaces consist of requests that a client can invoke as a method, and requests that a server can emit. All Wayland objects implement one interface.
Interfaces are message-based. Requests are actuated as server-bound messages, while events are client-bound. Both requests and events have opcodes set by the order each was defined, and identify which request or event to act on."))

(deftype %wl-message-type ()
  '(member :desctuctor nil))

(defclass %wl-message (%wl-named-object)
  ((%type :type %wl-message-type :reader wl-type
          :initarg :type :initform nil)
   (%since :type wl-uint :reader wl-since
           :initarg :since :initform 1
           :documentation "The version of the parent interface since the message was introduced")
   (%description :type (or wl-description null) :reader wl-description
                 :initarg :description :initform nil)
   (%args :reader wl-args
          :initarg :args)))

(defclass wl-request (%wl-message) ()
  (:documentation "Represents a message from a client to the compositor"))
(defclass wl-event (%wl-message) ()
  (:documentation "Represents a message from the compositor to a client"))

(defclass wl-enum (%wl-named-object)
  ((%since :type wl-uint :reader wl-since
           :initarg :since :initform 1
           :documentation "The version of the parent interface since the enum was introduced")
   (%bitfield :type boolean :reader wl-bitfield
              :initarg :bitfield :initform nil)
   (%description :type (or wl-description null) :reader wl-description
                 :initarg :description :initform nil)
   (%entries :reader wl-entries
             :initarg :entries :initform nil)))

(defclass wl-entry (%wl-named-object)
  ((%value :type wl-uint :reader wl-value
           :initarg :value)
   (%summary :type (or string null) :reader wl-summary
             :initarg :summary :initform nil)
   (%since :type wl-uint :reader wl-since
           :initarg :since :initform 1
           :documentation "The version of the parent interface since the entry was introduced")
   (%description :type (or wl-description null) :reader wl-description
                 :initarg :description :initform nil)))

(defclass wl-arg (%wl-named-object)
  ;; NOTE while Wayland's protocol/wayland.dtd allows arg elements
  ;; to hold a description elm, I see no stable protocols that do
  ;; this in practice. Was this historical?
  ((%type :type (cons keyword t) :reader wl-type
          :initarg :type)
   (%summary :type (or string null) :reader wl-summary
             :initarg :summary :initform nil)))

(defclass wl-description ()
  ((%summary :type (or string null) :reader wl-summary
             :initarg :summary :initform nil)
   (%text :reader wl-text
          :initarg :text)))



(defun %children (element name)
  (loop :for child :across (plump:children element)
        :when (and (plump:element-p child)
                   (string= name (plump:tag-name child)))
          :collect child))

(defmacro %optionally ((sym node name) &body (keyword form))
  `(when-let ((,sym (first (%children ,node ,name))))
     (list ,keyword ,form)))

(defmacro %optionally* ((sym node name) &body (keyword form))
  `(when-let ((,sym (plump:attribute ,node ,name)))
     (list ,keyword ,form)))

(defun %keyword (string)
  (make-keyword
    (map 'string
         (lambda (c)
           (if (char= c #\_)
               #\- c))
         (string-upcase string))))

(defun %text (dom)
  (flet ((whitespace-p (c)
           (declare (type character c))
           (position c (load-time-value
                         (coerce #(#\Space #\Newline #\Tab #\Return #\Page)
                                 'string)))))
    (with-output-to-string (out)
      (do* ((text (the string (plump:text dom)))
            (prev nil i)
            (i (position-if-not #'whitespace-p text)
               (position-if-not #'whitespace-p text
                                :start (1+ i))))
        ((null i))
        (when (and prev (> i (1+ prev)))
          (write-char #\Space out))
        (write-char (char text i) out)))))

(defun %parse-integer (string)
  (if (and (>= (length string) 2)
           (char= (char string 0) #\0))
      (if (char= #\x (char string 1))
          (parse-integer string :start 2 :radix 16)
          (parse-integer string :start 1 :radix 8))
      (parse-integer string :radix 10)))

(defun %protocol-of (node)
  (apply #'make-instance 'wl-protocol
         :name (plump:attribute node "name")
         (append
           (%optionally (it node "copyright")
             :copyright (%text it))
           (%optionally (it node "description")
             :description (%description-of it))
           (list :interfaces
                 (mapcar '%interface-of (%children node "interface"))))))

(defun %description-of (node)
  (apply #'make-instance 'wl-description
         (append
           (%optionally* (it node "summary")
             :summary it)
           (list :text (%text node)))))

(defun %interface-of (node)
  (apply #'make-instance 'wl-interface
         :name (plump:attribute node "name")
         (append
           (%optionally* (it node "version")
             :version (%parse-integer it))
           (%optionally (it node "description")
             :description (%description-of it))
           (list :requests
                 (mapcar '%request-of (%children node "request")))
           (list :events
                 (mapcar '%event-of (%children node "event")))
           (list :enums
                 (mapcar '%enum-of (%children node "enum"))))))

(defun %message-of (node class)
  (apply #'make-instance class
         :name (plump:attribute node "name")
         (append
           (%optionally* (it node "type")
             :type (%keyword it))
           (%optionally* (it node "since")
             :since (%parse-integer it))
           (%optionally (it node "description")
             :description (%description-of it))
           (list :args
                 (mapcar '%arg-of (%children node "arg"))))))

(defun %request-of (node) (%message-of node 'wl-request))
(defun %event-of (node) (%message-of node 'wl-event))

(defun %enum-of (node)
  (apply #'make-instance 'wl-enum
         :name (plump:attribute node "name")
         (append
           (%optionally* (it node "since")
             :since (%parse-integer it))
           (%optionally* (it node "bitfield")
             :bitfield (not (string= it "false")))
           (%optionally (it node "description")
             :description (%description-of it))
           (list :entries
                 (mapcar '%entry-of (%children node "entry"))))))

(defun %entry-of (node)
  (apply #'make-instance 'wl-entry
         :name (plump:attribute node "name")
         :value (%parse-integer (plump:attribute node "value"))
         (append
           (%optionally* (it node "summary")
             :summary it)
           (%optionally* (it node "since")
             :since (%parse-integer it))
           (%optionally (it node "description")
             :description (%description-of it)))))

(defun %arg-of (node)
  (apply #'make-instance 'wl-arg
         :name (plump:attribute node "name")
         :type (list*
                 (eswitch ((plump:attribute node "type") :test #'string=)
                   ("int" :int)
                   ("uint" :uint)
                   ("fixed" :fixed)
                   ("string" :string)
                   ("object" :object)
                   ("new_id" :new-id)
                   ("array" :array)
                   ("fd" :fd))
                 (append
                   ;; It's tempting to resolve interface and enum names into
                   ;; their CLOS objects, however they may refer to
                   ;; extra-protocol types (e.g. XDG shell's
                   ;; xdg_wm_base.get_xdg_surface accepts Wayland's wl_surface.
                   ;;
                   ;; All type references are left unresolved as a namestring
                   ;; and it's left to the consumer to resolve these types.
                   (%optionally* (it node "interface")
                     :interface it)
                   (%optionally* (it node "allow-null")
                     :allow-null (not (string= it "false")))
                   (%optionally* (it node "enum")
                     :enum it)))
         (%optionally* (it node "summary")
           :summary it)))

(defun wl-parse (input)
  (%protocol-of
    (first (plump:get-elements-by-tag-name
             (let ((plump:*tag-dispatchers* plump:*xml-tags*))
               (plump:parse input))
             "protocol"))))
