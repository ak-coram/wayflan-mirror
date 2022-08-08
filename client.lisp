;;; client.lisp -- Wayland client implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.client)



(declaim (optimize debug))

;; Wayland Interface

(defparameter *interface-table*
  (make-hash-table :test 'equal)
  "Maps all Wayland interface names to their proxy class")

(defun find-interface-named (name)
  "Return the interface linked to the given string NAME."
  (gethash name *interface-table*))

(defun %set-interface-named (interface name)
  (let ((prev-interface (gethash name *interface-table*)))
    (when (and prev-interface (not (eql prev-interface interface)))
      (warn "redefining Wayland interface named ~S" name))
    (setf (gethash name *interface-table*) interface)))

(defgeneric wl-interface-version (interface)
  (:documentation "Return the interface's latest supported version"))

(defmethod wl-interface-version ((interface symbol))
  (wl-interface-version (find-class interface)))

(defgeneric wl-interface-name (interface)
  (:documentation "Return the interface's name as conveyed by the wl-registry"))

(defmethod wl-interface-name ((interface symbol))
  (wl-interface-name (find-class interface)))

(defclass wl-interface (standard-class)
  ((version :initarg :version :type (integer 0)
            :documentation "The interface's latest supported version"
            :reader wl-interface-version)
   (interface-name :initarg :interface-name :type string
                   :documentation "The interface's name as conveyed by the wl-registry"
                   :reader wl-interface-name)))

(defmethod closer-mop:validate-superclass ((class wl-interface)
                                           (superclass standard-class))
  t)

(defmethod initialize-instance :after ((interface wl-interface) &key &allow-other-keys)
  (when (slot-boundp interface 'interface-name)
    (%set-interface-named interface (wl-interface-name interface))))

(defmethod reinitialize-instance :after ((interface wl-interface) &key &allow-other-keys)
  (when (slot-boundp interface 'interface-name)
    (%set-interface-named interface (wl-interface-name interface))))

;; Wayland Proxy

(defclass wl-proxy ()
  ((object-id :type wire:wl-uint
              :reader wl-proxy-id
              :accessor %wl-proxy-id)
   (%display :initarg :display
             :type wl-display
             :reader wl-proxy-display
             :accessor %wl-proxy-display)
   (%version :initarg :version
             :reader wl-proxy-version)
   (%listeners :initform ()
               :accessor wl-proxy-listeners)
   (%deletedp :initform nil
              :reader deletedp))
  (:documentation "A protocol object on the client side"))

(defclass wl-display (wl-proxy)
  ((%proxy-table :initform (make-hash-table) :reader %proxy-table)
   (%socket :initarg :socket :reader %wl-display-socket))
  (:documentation "A connection to the compositor that acts as a proxy to the wl_display singleton object")
  (:version . 1)
  (:interface-name . "wl_display")
  (:metaclass wl-interface))

(defclass wl-destroyed-proxy (wl-proxy)
  ()
  (:documentation "A proxy that has since been deleted by the compositor."))

(defclass wl-event ()
  ((%sender :initarg :sender :type wl-proxy
            :reader wl-event-sender
            :documentation "Proxy of the object that sent the event")))

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


;; handle-event protocol -- implemented by subclasses of wl-event-listener

(defclass wl-event-listener ()
  ()
  (:documentation "Classes subclassing Wl-EVENT-LISTENER advertise that they implement HANDLE-EVENT."))

(defgeneric handle-event (listener sender event)
  (:documentation "Notify a listener about an event of interest."))


;; read-event protocol -- implemented by define-event

(defgeneric read-event (sender opcode buffer)
  (:documentation "Read an event sent from PROXY with the given OPCODE and fast-io BUFFER.
READ-EVENT methods are defined by DEFINE-EVENT-READER."))

(defmethod print-object ((object wl-proxy) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~S ~S ~S (~D)"
            :id (wl-proxy-id object)
            :version (wl-proxy-version object)
            :listeners (length (wl-proxy-listeners object)))))

;; Proxy management

(defun find-proxy (display id)
  "Return a proxy with the given ID."
  (gethash id (%proxy-table display)))

(defun set-proxy (display id new-proxy)
  "Assign a proxy with the given id to the table."
  (setf (%wl-proxy-id new-proxy) id
        (gethash id (%proxy-table display)) new-proxy))

(defun %destroy-proxy (proxy)
  (declare (type wl-proxy proxy))
  (change-class proxy 'wl-destroyed-proxy)
  (values))

(defgeneric destroy-proxy (proxy)
  (:documentation "Delist the proxy from its display and mark it as destroyed.")
  (:method ((proxy wl-proxy))
   (%destroy-proxy proxy))
  (:method ((proxy wl-display))
   (error "Clients cannot destroy a ~S. Call ~S instead."
          'wl-display 'wl-display-disconnect)))

(defun clear-proxies (display)
  "Mark all owned proxies as removed and return DISPLAY."
  (let ((proxy-table (%proxy-table display)))
    (with-hash-table-iterator (next-item proxy-table)
      (loop
        (multiple-value-bind (more? id proxy) (next-item)
          (unless more? (return))
          (%destroy-proxy proxy)
          (remhash id proxy-table)))))
  (values))

(defun %next-proxy-id (display)
  "Return the next free proxy ID between 1 and #xFEFFFFFF."
  (let ((proxy-table (%proxy-table display)))
    (dotimes (i #xfeffffff)
      (unless (gethash (1+ i) proxy-table)
        (return (1+ i))))))

(defun make-proxy (class display &key object-id version)
  "Make a new proxy. If OBJECT-ID is provided, it's assumed it came from the server. Otherwise, it will allocate a new object-id from the client side."
  ;; TODO use PARENT over DISPLAY, to change default version to
  ;; (min (wl-interface-version class) (wl-proxy-version parent))
  (let ((new-proxy
          (make-instance class
                         :display display
                         :version (or version (wl-interface-version class)))))
    (set-proxy display (or object-id (%next-proxy-id display)) new-proxy)
    new-proxy))

(defmethod initialize-instance :after ((display wl-display)
                                       &key &allow-other-keys)
  (setf (%wl-proxy-display display) display)
  (set-proxy display 1 display))


;; Event handling

;; Stub out some classes for now -- it'll be redefined by WL-INCLUDE in
;; protocols.lisp
(defclass wl-callback.done-event (wl-event) ())
(defclass wl-display.delete-id-event (wl-event) ())
(defclass wl-display.error-event (wl-event) ())

(defgeneric dispatch-event (sender event)
  (:documentation "Inform a proxy about an event of interest. Usually, emit the event across its listeners."))

(defmethod dispatch-event (sender event)
  "Inform all sender's listeners of the event."
  (dolist (listener (wl-proxy-listeners sender))
    (handle-event listener sender event)))

(defmethod dispatch-event :before (display (event wl-display.delete-id-event))
  "Mark the object as destroyed and remove it from the object table."
  (let* ((id (wl-event-id event))
         (proxy (find-proxy display id)))
    (%destroy-proxy proxy)
    (remhash id (%proxy-table display))))

(defmethod dispatch-event :before (display (event wl-display.error-event))
  ;; Errors are fatal. When an error is received, the display can no longer be
  ;; used.
  (wl-display-disconnect display)
  (error 'wl-error
         :object (wl-event-object-id event)
         :code (wl-event-code event)
         :message (wl-event-message event)))

(defclass roundtrip-listener (wl-event-listener)
  ((%callback :initarg :callback))
  (:documentation "Used by WL-DISPLAY-ROUNDTRIP to funcall an assigned closure."))

(defmethod handle-event ((listener roundtrip-listener) sender (event wl-callback.done-event))
  (funcall (slot-value listener '%callback)))

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
           (sock:connect (sock:make-socket)
                         (uiop:unix-namestring pathname))))
    (make-instance
      'wl-display
      :socket (if (streamp display-name)
                display-name
                (connect-socket (display-pathname display-name)))
      :version 1)))

(defgeneric wl-display-disconnect (display)
  (:documentation "Close the display's underlying stream and remove all proxies."))

(defmethod wl-display-disconnect ((display wl-display))
  (prog1
    (close (%wl-display-socket display))
    (clear-proxies display)))

(defmethod wl-display-disconnect ((display wl-destroyed-proxy))
  ;; Assume the proxy was a deleted display, do nothing.
  nil)

(defun wl-display-listen (display)
  "Return whether there is a (partial) message available from the display."
  (listen (%wl-display-socket display)))

(defun wl-display-dispatch-event (display)
  "Read and dispatch the display's next event."
  (let (sender event)
    (do ((events 0 (1+ events)))
        ((and (plusp events)
              (not (wl-display-listen display)))
         events)
        (wire:with-input-from-message (buffer sender-id opcode
                                              nil (%wl-display-socket display))
          (setf sender (find-proxy display sender-id)
                event (read-event sender opcode buffer))
          (dispatch-event sender event)))))

(defun wl-display-roundtrip (display)
  "Block and dispatch events until all requests sent up to this point have been finalized."
  (let ((callback (wl-display.sync display))
        (sync-complete nil))
    (unwind-protect
      (progn
        (push (make-instance 'roundtrip-listener
                             :callback (lambda () (setf sync-complete t)))
              (wl-proxy-listeners callback))
        (do () (sync-complete)
            (wl-display-dispatch-event display)))
      (destroy-proxy callback))))



;; Wl protocol macros

(defmacro %specifier-bind (lambda-list specifier &body body)
  "Bind the given LAMBDA-LIST to SPECIFIER, which is transformed to a list of itself if it's not already a list."
  (a:once-only (specifier)
    `(destructuring-bind ,lambda-list (if (listp ,specifier)
                                          ,specifier
                                          (list ,specifier))
       ,@body)))

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

(defmacro read-arg (type sender buffer)
  "Read an object from the Wayland buffer depending on the given TYPE."
  (%wltype-ecase type
    (:int `(wire:read-wl-int ,buffer))
    (:uint `(wire:read-wl-uint ,buffer))
    (:fixed `(wire:read-wl-fixed ,buffer))
    (:string `(wire:read-wl-string ,buffer))
    ((:object &key interface allow-null)
      ;; TODO When the interface is given, check that the server has provided
      ;; the correct type.
      (a:with-gensyms (id)
        (let* ((proxy-form
                 `(find-proxy (wl-proxy-display ,sender) ,id))
               (checked-proxy-form
                 (if allow-null
                     `(when ,id ,proxy-form)
                     proxy-form)))
          `(let* ((,id (wire:read-wl-uint ,buffer))
                  (proxy ,checked-proxy-form))
             ,@(when interface
                 `((check-type proxy ,interface)))
             proxy))))
    ((:new-id &key interface)
     (if interface
         `(make-proxy ',interface
                      (wl-proxy-display ,sender)
                      :version (wire:read-wl-uint ,buffer))
         `(error "Don't know how to read an untyped :NEW-ID yet.")))
    (:array `(wire:read-wl-array ,buffer))
    (:fd `(error "Don't know how to read fd's yet."))))

(defmacro write-arg (place type sender buffer)
  "Write an object stored in PLACE to the Wayland buffer depending on the given TYPE."
  (%wltype-ecase type
    (:int `(wire:write-wl-int ,place ,buffer))
    (:uint `(wire:write-wl-uint ,place ,buffer))
    (:fixed `(wire:write-wl-fixed ,place ,buffer))
    (:string `(wire:write-wl-string ,place ,buffer))
    ((:object &key interface allow-null)
     (a:once-only (place)
       (let* ((proxy-id-form
                (if allow-null
                    `(if ,place (wl-proxy-id ,place) 0)
                    `(wl-proxy-id ,place)))
              (checked-id-form
                (if interface
                    `(progn
                       (check-type ,place ,(if allow-null
                                               `(or null ,interface)
                                               interface))
                       ,proxy-id-form)
                    proxy-id-form)))
         `(wire:write-wl-uint
            ,checked-id-form
            ,buffer))))
    ((:new-id &key interface)
     (if interface
         `(wire:write-wl-uint (wl-proxy-id ,place) ,buffer)
         `(progn
            (wire:write-wl-string (wl-interface-name (class-of ,place))
                                  ,buffer)
            (wire:write-wl-uint (wl-proxy-version ,place) ,buffer)
            (wire:write-wl-uint (wl-proxy-id ,place) ,buffer))))
    (:array `(wire:write-wl-array ,place ,buffer))
    (:fd `(sock:write-fd
            (%wl-display-socket (wl-proxy-display ,sender))
            ,place))))

(defmacro define-interface (name () &body options)
  "Define a wl-proxy CLOS subclass an associated wl-event CLOS subclass, and assign the interface's name to the interface table, accessible via #'FIND-INTERFACE-NAMED.

NAME - The name of the interface class.

OPTIONS:

(:version VERSION) - Latest supported version of the interface.
(:documentation DOCSTRING) - Docstring attached to the defined class.
(:event-class CLASS-NAME) - Define a WL-EVENT subclass with the given class name
(:skip-defclass BOOLEAN) - If true, do not define the interface class.
                           Used by cl-autowrap when it reads wl_display.
(:interface-name STRING) - The name of the interface as listed by the wl-registry on a wl-registry-global-event."
  (%option-bind (version documentation event-class skip-defclass interface-name) options
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
               (:metaclass wl-interface))))

       ;; wl-event class
       ,@(when event-class
           `((defclass ,(first event-class) (wl-event) ()
               (:documentation ,(format nil "Event sent from a ~A proxy."
                                        name)))))

       ',name)))

(defmacro define-enum (name () &body (entry-specifiers &rest options))
  "Define a parameter that associates each entry keyword with an index in the array."
  (declare (ignore name options))
  `(progn
     ,@(mapcar (lambda (specifier)
                 (destructuring-bind (variable value &key documentation) specifier
                   `(defparameter ,variable ,value ,documentation)))
               entry-specifiers)))

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

(:DOCUMENTATION DOCSTRING) - Provided to the function as its docstring."
  (%option-bind (documentation type) options
    (a:with-gensyms (buffer)
      (let* ((destructor? (eq (first type) :destructor))
             (new-proxy-interface
               ;; If a new proxy is being created, in this request, the form to
               ;; evaluate to the proxy's interface. If the type is fixed, it's
               ;; that type, quoted. Otherwise, it's the interface's name given in
               ;; the lambda list.
               (dolist (specifier arg-specifiers)
                 (let ((type (getf (rest specifier) :type)))
                   (%wltype-case type
                     ((:new-id &key interface)
                      (return (if interface
                                  `(quote ,interface)
                                  'new-interface)))))))
             (lambda-list-tail
               (mapcan (lambda (specifier)
                         (%specifier-bind (name &key type &allow-other-keys) specifier
                           (%wltype-case type
                             ((:new-id &key interface)
                              (unless interface
                                (list new-proxy-interface 'version)))
                             (t (list name)))))
                       arg-specifiers))
             (output-body
               `((wire:with-output-as-message (,buffer (wl-proxy-id ,interface)
                                                       ,opcode
                                                       (%wl-display-socket (wl-proxy-display ,interface)))
                   ,@(mapcar (lambda (specifier)
                               (%specifier-bind (name &key type &allow-other-keys) specifier
                                 `(write-arg ,(%wltype-case type
                                                (:new-id 'new-proxy)
                                                (t name))
                                             ,type
                                             ,interface
                                             ,buffer)))
                             arg-specifiers))))
             (defun-body
               `(,@documentation
                  ;; Destructor methods won't need the check-type, because
                  ;; it's already specialized in the parameter list.
                  ,@(unless destructor?
                      `((check-type ,interface ,interface)))
                  ,(cond
                     (new-proxy-interface
                       `(let ((new-proxy (make-proxy ,new-proxy-interface
                                                     (wl-proxy-display ,interface)
                                                     ,@(when (member 'version lambda-list-tail)
                                                         '(:version version)))))
                          ,@output-body
                          new-proxy))
                     ((> (length output-body) 1)
                      `(progn ,@output-body))
                     (t
                      (first output-body)))))
             (defun-head `(defun ,name ,(list* interface lambda-list-tail))))
        ;; If the request is a destructor, put the logic in a DESTROY-PROXY
        ;; method and use the function name as a synonym.
        (if destructor?
            (progn
              (assert (null arg-specifiers)
                      (arg-specifiers)
                      "Destructor request cannot have any parameters")
              `(prog1
                 (,@defun-head
                   (destroy-proxy ,interface))
                 (defmethod destroy-proxy :before ((,interface ,interface))
                   ,@defun-body)))
            `(,@defun-head
               ,@defun-body))))))

(defmacro define-event ((name interface opcode) &body (arg-specifiers &rest options))
  "Define a class and a READ-EVENT method to support WL-DISPLAY-DISPATCH-EVENT.

NAME - The name of the event class.
INTERFACE - The name of the interface (as provided by define-interface) whose objects send the event.
OPCODE - The integer opcode value of the event.

ARG-SPECIFIERS - Each specifier takes the lambda list (name &key type documentation).
  TYPE - The Wayland type of the arg.
  DOCUMENTATION - The summary of the arg.

OPTIONS:

(:DOCUMENTATION DOCSTRING) - Provided to the event class as its docstring.
(:EVENT-SUPERCLASSES CLASS-NAMES...) - When provided, the event class subclasses these given classes. Otherwise, it subclasses WL-EVENT."
  (%option-bind (documentation event-superclasses) options
    (a:with-gensyms (proxy opcode-sym buffer)
      (let ((slot-specifiers
              (mapcar (lambda (arg-specifier)
                        (%specifier-bind (arg-name &key initarg documentation &allow-other-keys)
                                        arg-specifier
                          `(,arg-name :reader ,arg-name
                                  :initarg ,initarg
                                  ,@(when documentation
                                      `(:documentation ,@documentation)))))
                      arg-specifiers))
            (initargs
              (mapcan
                (lambda (arg-specifier)
                  (%specifier-bind (arg-name &key initarg type &allow-other-keys)
                                  arg-specifier
                    (declare (ignore arg-name))
                    (list initarg
                          `(read-arg ,type ,proxy ,buffer))))
                arg-specifiers)))
        `(progn
           ;; Event class
           (defclass ,name ,(or event-superclasses '(wl-event))
             ,slot-specifiers
             ,@(when documentation
                 `((:documentation ,@documentation))))
           ;; Event reader
           (defmethod read-event ((,proxy ,interface)
                                  (,opcode-sym (eql ,opcode))
                                  ,buffer)
             ,@documentation
             (make-instance ',name
                            :sender ,proxy
                            ,@initargs)))))))
