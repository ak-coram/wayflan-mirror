;;; src/client.lisp -- Wayland client implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.client)



;; Wayland Interface

(defparameter *interface-table*
  (make-hash-table :test 'equal)
  "Maps all Wayland interface names to their proxy class")

(defun find-interface-named (name)
  "Return the interface linked to the given string NAME."
  (gethash name *interface-table*))

;; Avoiding (setf find-interface-named) to keep it internal
(defun %set-interface-named (interface name)
  (let ((prev-interface (gethash name *interface-table*)))
    (when (and prev-interface (not (eql prev-interface interface)))
      (warn "redefining Wayland interface named ~S" name))
    (setf (gethash name *interface-table*) interface)))

(defgeneric wl-interface-version (interface)
  (:documentation "Return the interface's latest supported version")
  (:method ((interface symbol))
   (wl-interface-version (find-class interface))))

(defgeneric wl-interface-name (interface)
  (:documentation "Return the interface's name discoverable by FIND-INTERFACE-NAMED")
  (:method ((interface symbol))
   (wl-interface-name (find-class interface))))

(defclass wl-interface-class (standard-class)
  ((%version :type wire:wl-uint
             :initarg :version
             :reader wl-interface-version)
   (%interface-name :type string
                    :initarg :interface-name
                    :reader wl-interface-name)))

(defmethod closer-mop:validate-superclass ((class wl-interface-class)
                                           (superclass standard-class))
  t)

(defmethod initialize-instance :after ((interface wl-interface-class)
                                       &key &allow-other-keys)
  (%set-interface-named interface (wl-interface-name interface)))

(defmethod reinitialize-instance :after ((interface wl-interface-class)
                                         &key &allow-other-keys)
  (%set-interface-named interface (wl-interface-name interface)))

;; Wayland Proxy

(defclass wl-proxy ()
  ((%object-id :type wire:wl-uint
               :reader wl-proxy-id
               :accessor %wl-proxy-id)
   (%display :type wl-display
             :initarg :display
             :reader wl-proxy-display
             :accessor %wl-proxy-display)
   (%version :type wire:wl-uint
             :initarg :version
             :reader wl-proxy-version)
   (%deletedp :type boolean
              :initform nil
              :reader deletedp)
   (%hooks :initform ()
           :accessor wl-proxy-hooks))
  (:documentation "A protocol object on the client side"))

(defmacro %check-proxy (proxy interface &optional version)
  (a:once-only (proxy)
    `(assert (and (typep ,proxy ',interface)
                  ,@(when version
                      `((>= (wl-proxy-version ,proxy) ,version)))))))

(defclass wl-destroyed-proxy (wl-proxy)
  ()
  (:documentation "A proxy that has since been deleted by the compositor."))

(defclass wl-display (wl-proxy)
  ((%pathname :type pathname
              :initarg :pathname
              :reader wl-display-pathname)
   (%proxy-table :type hash-table
                 :initform (make-hash-table)
                 :reader %proxy-table)
   (%socket :type wire:data-socket
            :initarg :socket :reader %wl-display-socket))
  (:documentation "A connection to the compositor that acts as a proxy to the wl_display singleton object")
  (:version . 1)
  (:interface-name . "wl_display")
  (:metaclass wl-interface-class))

(define-condition wl-error (error)
  ((%object :initarg :object :reader wl-error-object
            :type (or wl-proxy null)
            :documentation "object where the error occurred")
   (%code :initarg :code :reader wl-error-code
          :type wire:wl-uint
          :documentation "error code")
   (%message :initarg :message :reader wl-error-message
             :type string
             :documentation "error description"))
  (:report (lambda (cond stream)
             (format stream "Error code ~D from ~S: ~A"
                     (wl-error-code cond)
                     (wl-error-object cond)
                     (wl-error-message cond)))))

(defmethod print-object ((object wl-proxy) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~S ~S ~S (~D)"
            :id (wl-proxy-id object)
            :version (wl-proxy-version object)
            :hooks (length (wl-proxy-hooks object)))))

(defmethod print-object ((object wl-display) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~S ~S ~S ~S ~S (~D)"
            :on (wl-display-pathname object)
            :id (wl-proxy-id object)
            :version (wl-proxy-version object)
            :hooks (length (wl-proxy-hooks object)))))

;; enum coding protocol -- implemented by define-enum

(defgeneric wl-enum-value (enum value)
  (:documentation
    "Convert a KEYWORD (or, in the case of a bitfield enum, a list of keywords) into an integer according to ENUM."))

(defgeneric wl-enum-keyword (enum value)
  (:documentation
    "Convert an integer into a KEYWORD (or, in the case of a bitfield enum, a list of keywords) according to ENUM."))

;; Proxy management

(defun find-proxy (display id)
  "Return a proxy with the given ID."
  (gethash id (%proxy-table display)))

(defun %set-proxy (display id new-proxy)
  "Assign a proxy with the given id to the table. Return NEW-PROXY."
  (setf (%wl-proxy-id new-proxy) id
        (gethash id (%proxy-table display)) new-proxy))

(defun %destroy-proxy (proxy)
  (declare (type wl-proxy proxy))
  (change-class proxy 'wl-destroyed-proxy))

(defgeneric destroy-proxy (proxy)
  (:documentation "Delist the proxy from its display and mark it as destroyed.")
  (:method ((proxy wl-proxy))
   (%destroy-proxy proxy)
   nil)
  (:method ((proxy wl-display))
   (error "Clients cannot destroy a ~S. Call ~S instead."
          'wl-display 'wl-display-disconnect)))

(defun %clear-proxies (display)
  "Mark all owned proxies as removed."
  (let ((proxy-table (%proxy-table display)))
    (maphash (lambda (id proxy)
               (declare (ignore id))
               (%destroy-proxy proxy))
             proxy-table)
    (clrhash proxy-table)))

(defun %next-proxy-id (display)
  "Return the next free proxy ID between 1 and #xFEFFFFFF."
  (let ((proxy-table (the hash-table (%proxy-table display))))
    (dotimes (i #xfefffffe)
      (unless (gethash (1+ i) proxy-table)
        (return (1+ i))))))

(defun %make-proxy (class parent &key object-id version)
  "Make a new proxy. If OBJECT-ID is provided, it's assumed it came from the server. Otherwise, it will allocate a new object-id from the client side."

    (let* ((display (wl-proxy-display parent))
           (new-proxy (make-instance
                        class
                        :display display
                        :version (or version
                                     (min (wl-interface-version class)
                                          (wl-proxy-version parent))))))
      (%set-proxy display (or object-id (%next-proxy-id display))
                  new-proxy)))

(defmethod initialize-instance :after ((display wl-display)
                                       &key &allow-other-keys)
  (setf (%wl-proxy-display display) display)
  (%set-proxy display 1 display))

;; Event handling

(defun %dispatch-event (sender event)
  (when (typep sender 'wl-display)
    (case (first event)
      (:delete-id
        (destructuring-bind (id) (rest event)
          (%destroy-proxy (find-proxy sender id))
          (remhash id (%proxy-table sender))))
      (:error
        (destructuring-bind (object-id code message) (rest event)
          (wl-display-disconnect sender)
          (error 'wl-error
                 :object object-id
                 :code code
                 :message message)))))

  (dolist (hook (wl-proxy-hooks sender))
    (apply hook event)))

(defgeneric %read-event (sender opcode buffer)
  (:documentation "Read an event sent from PROXY with the given OPCODE and fast-io BUFFER.
READ-EVENT methods are defined by DEFINE-EVENT-READER."))

;; Display management

(defun display-pathname (&optional display-name)
  "Returns the absolute pathname to a Wayland display given its pathname designator.

DISPLAY-PATHNAME roughly emulates libwayland's algorithm to search a Wayland
socket, and is sensitive to the environment variables $WAYLAND_DISPLAY and
$XDG_RUNTIME_DIR.

If DISPLAY-NAME is nil, $WAYLAND_DISPLAY is used instead, and then if it
describes a relative pathname, it is merged with $XDG_RUNITME_DIR."
  (merge-pathnames
    (or display-name (uiop:getenv "WAYLAND_DISPLAY"))
    (uiop:parse-unix-namestring
      (uiop:getenv "XDG_RUNTIME_DIR")
      :ensure-directory t)))

(defun wl-display-connect (&optional display-name)
  "Return a display connected to the given socket name.

WL-DISPLAY-CONNECT searches and connects to a Wayland display if DISPLAY-NAME
is a pathname, a string, or not provided; if DISPLAY-NAME is a stream, it will
take it in as-is.

Whether or not WL-DISPLAY-CONNECT opened a stream, it will close the display's
underlying stream when WL-DISPLAY-DISCONNECT is called."
  (flet ((connect-socket (pathname)
           "Connect to the Unix socket living in PATHNAME."
           (let ((sock (wire:make-socket)))
             (wire:connect sock (uiop:unix-namestring pathname))
             sock)))
    (make-instance
      'wl-display
      :socket (if (typep display-name 'wire::%socket)
                display-name
                (connect-socket (display-pathname display-name)))
      :pathname (unless (typep display-name 'wire::%socket)
                  (display-pathname display-name))
      :version 1)))

(defgeneric wl-display-disconnect (display)
  (:documentation "Close the display's underlying stream and remove all proxies.")
  (:method ((display wl-display))
   (prog1
     (wire:close-socket (%wl-display-socket display))
     (%clear-proxies display)))
  (:method ((display wl-destroyed-proxy))
   ;; Assume the proxy was a deleted display -- do nothing.
   nil))

(defun wl-display-listen (display)
  "Return whether there is a (partial) message available from the display."
  (wire:listen-socket (%wl-display-socket display)))

(defun wl-display-dispatch-event (display)
  "Read and dispatch the display's next event, or event if more than one is buffered.
Return the number of events processed."
  (do (sender (events 0 (1+ events)))
      ((and (plusp events)
            (not (wl-display-listen display)))
       events)
      (declare (type fixnum events))
      (wire:with-incoming-message ((%wl-display-socket display) sender-id opcode buffer)
        (setf sender (find-proxy display sender-id))
        (%dispatch-event
          sender (%read-event sender opcode buffer)))))



;; Wl protocol macros

(defmacro %specifier-bind (lambda-list specifier &body body)
  "Bind the given LAMBDA-LIST to SPECIFIER, which is transformed to a list of itself if it's not already a list."
  (a:once-only (specifier)
    `(destructuring-bind ,lambda-list (if (listp ,specifier)
                                          ,specifier
                                          (list ,specifier))
       ,@body)))

(defmacro %slambda (lambda-list &body body)
  "Return a single-arity lambda that destructures the argument as a specifier. before evaluating the body."
  (a:with-gensyms (specifier)
    `(lambda (,specifier)
       (%specifier-bind ,lambda-list ,specifier
         ,@body))))

(defmacro %option-bind ((&rest option-names) options &body body)
  (a:once-only (options)
    (let ((binding-forms
            (mapcar (lambda (option-name)
                      `(cdr (assoc ,(a:make-keyword option-name) ,options)))
                    option-names)))
      `(let ,(mapcar #'list option-names binding-forms)
         ,@body))))

(defmacro %wltype-xcase (casename keyform &body cases)
  "Evaluate the clause sleected by the matching Wayland type.

A lisp Wayland type (an s-expr form of libwayland's wl_message) is either a
keyword that specifies the primary type, or a list that contains the keyword at
the start, followed by a symbol, its secondary type.

Parameterized typenames may be consed as the head of a property list, e.g.
a :UINT type may be specialized as (:UINT :ENUM WL-SHM-FORMAT), or an
:OBJECT may be (:OBJECT :INTERFACE WL-SURFACE :ALLOW-NULL T).

Type specializers:

:ENUM - specializes a :UINT to be a member of an enum.
:INTERFACE - specializes an :OBJECT or :NEW-ID to be an instance of an
             interface.
:ALLOW-NULL - specializes an :OBJECT or :STRING to be nullable. Lisp calls
              may pass in NIL.


A WLTYPE-XCASE key is a list containing the matching keyword, followed by a
destructuring lambda-list bound under the case's body."
  (a:once-only (keyform)
    `(,casename (if (listp ,keyform)
                    (first ,keyform)
                    ,keyform)
                ,@(mapcar
                    (lambda (case)
                      (destructuring-bind (typekey &body body) case
                        `(,(if (listp typekey)
                               (first typekey)
                               typekey)
                           ,@(if (listp typekey)
                                 `((destructuring-bind ,(rest typekey)
                                     (when (listp ,keyform)
                                       (rest ,keyform))
                                     ,@body))
                                 body))))
                    cases))))

(defmacro %wltype-case (keyform &body cases)
  `(%wltype-xcase case ,keyform ,@cases))

(defmacro %wltype-ecase (keyform &body cases)
  `(%wltype-xcase ecase ,keyform ,@cases))

(declaim (inline %wltype= %sgetf))
(defun %wltype= (name type)
  (eq name (if (atom type) type (first type))))
(defun %sgetf (indicator specifier)
  ;; Specifier getf -- cut the spec name to getf on the plist tail
  (unless (atom specifier)
    (getf (rest specifier) indicator)))

(defmacro @and (&rest clauses)
  (reduce
    (lambda (clause body)
      `(let ((@ ,clause))
         (when @ ,body)))
    clauses :from-end t))

(defmacro define-interface (name () &body options)
  "Define a wl-proxy CLOS subclass and assign the interface's name to the interface table, accessible via #'FIND-INTERFACE-NAMED.

NAME - The name of the interface class.

OPTIONS:

(:version VERSION) - Latest supported version of the interface.
(:documentation DOCSTRING) - Docstring attached to the defined class.
(:skip-defclass BOOLEAN) - If true, do not define the interface class.
                           Used by cl-autowrap when it reads wl_display.
(:interface-name STRING) - The name of the interface as listed by the wl-registry on a wl-registry-global-event."
  (%option-bind (version documentation skip-defclass interface-name) options
    `(progn
       ;; wl-proxy class
       ,@(unless (first skip-defclass)
           `((defclass ,name (wl-proxy) ()
               ,@(when version
                   `((:version . ,(first version))))
               ,@(when interface-name
                   `((:interface-name . ,(first interface-name))))
               ,@(when documentation
                   `((:documentation ,@documentation)))
               (:metaclass wl-interface-class))))
       ',name)))

(defun %encode-standard-enum (enum table value)
  (cdr (or (find value table :key #'car :test #'eq)
           (error "Unknown enum value ~S ~S" enum value))))

(defun %decode-standard-enum (enum table value)
  (car (or (find value table :key #'cdr :test #'=)
           (error "Unknown enum value ~S ~S" enum value))))

(defun %encode-bitfield-enum (enum table values)
  (let ((result 0))
    (dolist (value values result)
      (setf result (logior result
                           (cdr (or (find value table :key #'car :test #'eq)
                                    (error "Unknown enum value ~S ~S" enum value))))))))

(defun %decode-bitfield-enum (table value zero-value)
  (let (result)
    (dolist (entry table (if result (nreverse result) zero-value))
      (when (and (= (cdr entry) (logand value (cdr entry)))
                 (plusp (cdr entry)))
        (push (car entry) result)))))

(defmacro define-enum (name () &body (entry-specifiers &rest options))
  "Define a parameter that associates each entry keyword with an index in the array.

NAME - The name of the enum class. Used as arguments in Wayland types :INT and :UINT.

OPTIONS:

(:since INTEGER) - The interface version since it appeared.
(:documentation DOCSTRING) - Ignored.
(:bitfield BOOLEAN) - Whether individual bits have specific meanings. If set, enums are decoded as a list of arguments, rather than a single argument."
  (%option-bind (bitfield) options
    `(let ((table ',(mapcar (%slambda (argument value &key &allow-other-keys)
                              (cons argument value))
                            entry-specifiers)))
       (defmethod wl-enum-value ((enum (eql ',name)) value)
         ,(if bitfield
              `(%encode-bitfield-enum enum table value)
              `(%encode-standard-enum enum table value)))

       (defmethod wl-enum-keyword ((enum (eql ',name)) value)
         ,(if bitfield
              `(%decode-bitfield-enum
                 table value
                 ,(a:when-let ((zero-entry (find 0 entry-specifiers
                                                 :key #'second :test #'=)))
                    `'(,(first zero-entry))))
              `(%decode-standard-enum enum table value)))
       ',name)))

(defmacro define-request ((name interface opcode) &body (arg-specifiers &rest options))
  "Define a function implementing the wl request.
DEFINE-REQUEST currently only supports up to one :NEW-ID argument per request.

NAME - The name of the request function.
INTERFACE - The name of the interface (as provided by define-interface) whose proxies send the request.
OPCODE - The integer opcode value of the request.

ARG-SPECIFIERS - Each specifier takes the lambda list (name &key type documentation).
  TYPE - The Wayland type of the arg.
  DOCUMENTATION - The summary of the arg.

OPTIONS:

(:DOCUMENTATION STRING) - Provided to the function as its docstring.
(:TYPE KEYWORD) - So far, only :DESTRUCTOR is a valid type.
(:SINCE VERSION) - Minimum interface version of the proxy object."
  (assert (>= 1 (count :new-id arg-specifiers
                       :test #'%wltype=
                       :key (a:curry #'%sgetf :type))))
  (%option-bind (documentation type since) options
    (flet ((arg-to-types (arg)
             ;; The wire types each wayland type is composed of
             (%specifier-bind (arg-name &key type &allow-other-keys) arg
               (declare (ignore arg-name))
               (%wltype-case type
                 ((:new-id &key interface)
                  (if interface
                      '(:uint)
                      '(:string :uint :uint)))
                 (:object '(:uint))
                 (t `(,(if (listp type)
                           (first type)
                           type))))))
           (arg-to-values (arg)
             (%specifier-bind (arg-name &key type &allow-other-keys) arg
               ;; The name doubles as the place the object exists.
               ;; On :new-id, hard symbols 'interface-class and 'version also
               ;; exist.
               (%wltype-case type
                 ((:new-id &key interface)
                  (if interface
                      `((wl-proxy-id ,arg-name))
                      `((wl-interface-name interface-class)
                        version
                        (wl-proxy-id ,arg-name))))
                 ((:object &key interface allow-null)
                  (list
                    (@and
                      `(wl-proxy-id ,arg-name)
                      (if interface
                          `(progn
                             (check-type ,arg-name ,interface)
                             ,|@|)
                          @)
                      (if allow-null
                          `(if ,arg-name ,|@| 0)
                          @))))
                 (((:uint :int) &optional enum)
                  (if enum
                      `((wl-enum-value ',enum ,arg-name))
                      `(,arg-name)))
                 (t `(,arg-name))))))

      (let* ((destructor? (eq (first type) :destructor))
             (new-id-arg (find :new-id arg-specifiers
                               :test #'%wltype=
                               :key (a:curry #'%sgetf :type)))
             (new-id-interface (%sgetf :interface (%sgetf :type new-id-arg)))
             (lambda-list-tail
               ;; Everything but the sender object param
               (apply #'append
                      (mapcar (%slambda (arg-name &key type &allow-other-keys)
                                (%wltype-case type
                                  ((:new-id &key interface)
                                   (unless interface
                                     '(interface-class version)))
                                  (t `(,arg-name))))
                              arg-specifiers)))
             (defun-body
               (@and
                 ;; The core of the request is the send-wl-message macro..
                 `(wire:send-wl-message
                    ((%wl-display-socket (wl-proxy-display ,interface))
                     (wl-proxy-id ,interface) ,opcode)
                    ,(apply #'append (mapcar #'arg-to-types arg-specifiers))
                    ,@(apply #'append (mapcar #'arg-to-values arg-specifiers)))
                 ;; ...which is wrapped in a let if a new proxy is made...
                 (if new-id-arg
                     `(let ((,(first new-id-arg)
                              (%make-proxy
                                ,(if new-id-interface
                                     `',new-id-interface
                                     'interface-class)
                                ,interface
                                ,@(unless new-id-interface
                                    `(:version version)))))
                        ,|@|
                        ,(first new-id-arg))
                     @)
                 ;; ...and finally, add the docstring and typecheck
                 `(,@documentation
                    (%check-proxy ,interface ,interface ,@since)
                    ,|@|))))
        ;; If the request is a destructor, put the logic in a DESTROY-PROXY
        ;; method and use the function name as a synonym.
        (if destructor?
            (progn
              (assert (null arg-specifiers)
                      (arg-specifiers)
                      "Destructor request ~S cannot have any parameters"
                      name)
              `(progn
                 (defmethod destroy-proxy :before ((,interface ,interface))
                   ,@defun-body)
                 (defun ,name (,interface)
                   (destroy-proxy ,interface))
                 ))
            `(defun ,name (,interface ,@lambda-list-tail)
               ,@defun-body))))))

(defmacro define-event ((name interface opcode) &body (arg-specifiers &rest options))
  "Define an event to be read by WL-DISPLAY-DISPATCH-EVENT.

NAME - The name of the event. It can be anything, but is usually a keyword unique to the interface.
INTERFACE - The name of the interface (as provided by define-interface) whose objects send the event.
OPCODE - The integer opcode value of the event.

ARG-SPECIFIERS - Each specifier takes the lambda list (name &key type documentation).
  TYPE - The Wayland type of the arg.
  DOCUMENTATION - The summary of the arg.

OPTIONS:

(:DOCUMENTATION DOCSTRING) - Provided to the event class as its docstring.
(:TYPE TYPE) - TODO does nothing. So far, only :DESTRUCTOR is a valid type.
(:SINCE VERSION) - TODO in the future, asserts that only proxies of this version of higher should receive this event."
  (%option-bind (documentation) options
    ;; TODO throw an error if the interface's version doesn't support this
    ;; event (error on the server's fault)

    ;; FIXME add support for destructor events.
    (a:with-gensyms (proxy opcode-sym buffer)
      (flet ((arg-to-form (arg)
               (%specifier-bind (name &key type &allow-other-keys) arg
                 (declare (ignore name))
                 (%wltype-ecase type
                   ((:new-id &key interface)
                    (if interface
                        `(%make-proxy ',interface ,proxy
                                      :object-id (wire:read-wl-uint ,buffer))
                        `(%make-proxy
                           (find-interface-named (wire:read-wl-string ,buffer))
                           ,proxy
                           :version (wire:read-wl-uint ,buffer)
                           :object-id (wire:read-wl-uint ,buffer))))
                   ((:object &key interface allow-null)
                    (@and
                      `(find-proxy (wl-proxy-display ,proxy)
                                   id)
                      (if allow-null
                          `(when id ,|@|)
                          @)
                      `(let* ((id (wire:read-wl-uint ,buffer))
                              (proxy ,|@|))
                         ,(when interface
                            `(check-type proxy ,interface))
                         proxy)))
                   (((:int :uint) &optional enum)
                    (@and
                      (if (%wltype= :int type)
                          `(wire:read-wl-int ,buffer)
                          `(wire:read-wl-uint ,buffer))
                      (if enum
                          `(wl-enum-keyword ',enum ,|@|)
                          @)))
                   (:fixed `(wire:read-wl-fixed ,buffer))
                   (:array `(wire:read-wl-array ,buffer))
                   (:string `(wire:read-wl-string ,buffer))
                   (:fd `(wire:read-fd (%wl-display-socket
                                         (wl-proxy-display ,proxy))))))))
        `(progn
           (defmethod %read-event ((,proxy ,interface)
                                   (,opcode-sym (eql ,opcode))
                                   ,buffer)
             ,@documentation
             (list ',name ,@(mapcar #'arg-to-form arg-specifiers)))
           ',name)))))



;; Utilities

(defmacro with-open-display ((display &rest options) &body body)
  "Like WITH-OPEN-FILE, but binding DISPLAY to the result of WL-DISPLAY-CONNECT instead of OPEN.
Executes the body with DISPLAY bound to a freshly connected display."
  `(let ((,display (wl-display-connect ,@options)))
     (unwind-protect (progn ,@body)
       (wl-display-disconnect ,display))))

(defmacro with-proxy ((var value) &body body)
  "Bind the proxy variable VAR to VALUE, and destroy it when execution leaves the body."
  `(let ((,var ,value))
     (unwind-protect (progn ,@body)
       (destroy-proxy ,var))))

(defmacro %event-xcase (xcase event &body clauses)
  (a:once-only (event)
    `(,xcase (car ,event)
       ,@(mapcar (lambda (clause)
                   (destructuring-bind (event-name lambda-list &body body) clause
                     `(,event-name
                        (destructuring-bind ,lambda-list (rest ,event)
                          ,@body))))
                 clauses))))

(defmacro event-case (event &body clauses)
  `(%event-xcase case ,event ,@clauses))
(defmacro event-ccase (event &body clauses)
  `(%event-xcase ccase ,event ,@clauses))
(defmacro event-ecase (event &body clauses)
  `(%event-xcase ecase ,event ,@clauses))

(defmacro evxlambda (event-xcase &body clauses)
  (a:with-gensyms (event)
    `(lambda (&rest ,event)
       (,event-xcase ,event ,@clauses))))

(defmacro evlambda (&body clauses)
  `(evxlambda event-case ,@clauses))
(defmacro evclambda (&body clauses)
  `(evxlambda event-ccase ,@clauses))
(defmacro evelambda (&body clauses)
  `(evxlambda event-ecase ,@clauses))

(defun wl-display-roundtrip (display)
  "Block and dispatch events until all requests sent up to this point have been finalized."
  (with-proxy (callback (wl-display.sync display))
    (push (lambda (&rest event)
            (declare (ignore event))
            (return-from wl-display-roundtrip))
          (wl-proxy-hooks callback))

    (loop (wl-display-dispatch-event display))))
