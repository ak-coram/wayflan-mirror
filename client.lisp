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
  (:documentation "Return the interface's latest supported version")
  (:method ((interface symbol))
   (wl-interface-version (find-class interface))))

(defgeneric wl-interface-name (interface)
  (:documentation "Return the interface's name discoverable by FIND-INTERFACE-NAMED")
  (:method ((interface symbol))
   (wl-interface-name (find-class interface))))

(defclass wl-interface (standard-class)
  ((%version :initarg :version :type (integer 0)
             :reader wl-interface-version)
   (%interface-name :initarg :interface-name :type string
                    :reader wl-interface-name)))

(defmethod closer-mop:validate-superclass ((class wl-interface)
                                           (superclass standard-class))
  t)

(defmethod initialize-instance :after ((interface wl-interface) &key &allow-other-keys)
  (%set-interface-named interface (wl-interface-name interface)))

(defmethod reinitialize-instance :after ((interface wl-interface) &key &allow-other-keys)
  (%set-interface-named interface (wl-interface-name interface)))

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
   (%hooks :initform ()
           :accessor wl-proxy-hooks)
   (%deletedp :initform nil
              :reader deletedp))
  (:documentation "A protocol object on the client side"))

(defmacro %check-proxy (proxy interface &optional version)
  (a:once-only (proxy)
    `(assert (and (typep ,proxy ',interface)
                  ,@(when version
                      `((>= (wl-proxy-version ,proxy) ,version)))))))

(defclass wl-display (wl-proxy)
  ;; FIXME the table is being polluted with destroyed proxies junk.
  ((%proxy-table :initform (make-hash-table) :reader %proxy-table)
   (%socket :initarg :socket :reader %wl-display-socket))
  (:documentation "A connection to the compositor that acts as a proxy to the wl_display singleton object")
  (:version . 1)
  (:interface-name . "wl_display")
  (:metaclass wl-interface))

(defclass wl-destroyed-proxy (wl-proxy)
  ()
  (:documentation "A proxy that has since been deleted by the compositor."))

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


;; read-event protocol -- implemented by define-event

(defgeneric %read-event (sender opcode buffer)
  (:documentation "Read an event sent from PROXY with the given OPCODE and fast-io BUFFER.
READ-EVENT methods are defined by DEFINE-EVENT-READER."))

(defmethod print-object ((object wl-proxy) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~S ~S ~S (~D)"
            :id (wl-proxy-id object)
            :version (wl-proxy-version object)
            :hooks (length (wl-proxy-hooks object)))))

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

(defun %clear-proxies (display)
  "Mark all owned proxies as removed."
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

(defun %make-proxy (class display &key object-id version)
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
    (%clear-proxies display)))

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
                event (%read-event sender opcode buffer))
          (%dispatch-event sender event)))))

(defun wl-display-roundtrip (display)
  "Block and dispatch events until all requests sent up to this point have been finalized."
  (let ((callback (wl-display.sync display))
        (sync-complete nil))
    (unwind-protect
      (progn
        (push (lambda (&rest event)
                (declare (ignore event))
                (setf sync-complete t))
              (wl-proxy-hooks callback))

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

(defmacro @and (&rest clauses)
  (reduce
    (lambda (clause body)
      `(let ((@ ,clause))
         (when @ ,body)))
    clauses :from-end t))

(defmacro %read-arg (type sender buffer)
  "Read an object from the Wayland buffer depending on the given TYPE."
  (%wltype-ecase type
    (:int `(wire:read-wl-int ,buffer))
    (:uint `(wire:read-wl-uint ,buffer))
    (:fixed `(wire:read-wl-fixed ,buffer))
    (:string `(wire:read-wl-string ,buffer))
    ((:object &key interface allow-null)
      (a:with-gensyms (id)
        (@and
          `(find-proxy (wl-proxy-display ,sender) ,id)
          (if allow-null
              `(when ,id ,|@|)
              @)
          `(let* ((,id (wire:read-wl-uint ,buffer))
                  (proxy ,|@|))
             ,(when interface
                `(check-type proxy ,interface))
             proxy))))
    ((:new-id &key interface)
     (if interface
         `(%make-proxy ',interface
                       (wl-proxy-display ,sender)
                       :version (wire:read-wl-uint ,buffer))
         `(error "Don't know how to read an untyped :NEW-ID yet.")))
    (:array `(wire:read-wl-array ,buffer))
    (:fd `(sock:read-fd (%wl-display-socket (wl-proxy-display ,sender))))))

(defmacro %write-arg (place type socket buffer)
  "Write an object stored in PLACE to the Wayland buffer depending on the given TYPE."
  (%wltype-ecase type
    (:int `(wire:write-wl-int ,place ,buffer))
    (:uint `(wire:write-wl-uint ,place ,buffer))
    (:fixed `(wire:write-wl-fixed ,place ,buffer))
    (:string `(wire:write-wl-string ,place ,buffer))
    ((:object &key interface allow-null)
     (a:once-only (place)
       (@and
         `(wl-proxy-id ,place)
         (if allow-null
             `(if ,place ,|@| 0)
             @)
         (if interface
             `(progn
                (check-type ,place ,(if allow-null
                                        `(or null ,interface)
                                        interface))
                ,|@|)
             @)
         `(wire:write-wl-uint ,|@| ,buffer))))
    ((:new-id &key interface)
     (if interface
         `(wire:write-wl-uint (wl-proxy-id ,place) ,buffer)
         `(progn
            (wire:write-wl-string (wl-interface-name (class-of ,place))
                                  ,buffer)
            (wire:write-wl-uint (wl-proxy-version ,place) ,buffer)
            (wire:write-wl-uint (wl-proxy-id ,place) ,buffer))))
    (:array `(wire:write-wl-array ,place ,buffer))
    (:fd `(sock:write-fd ,socket ,place))))

(defmacro %send-request (sender opcode &body arg-specifiers)
  (a:with-gensyms (buffer socket)
    (a:once-only (sender)
      `(let ((,socket (%wl-display-socket (wl-proxy-display ,sender))))
         (wire:with-output-as-message (,buffer (wl-proxy-id ,sender)
                                               ,opcode ,socket)
           ,@(mapcar
               (%slambda (name &key type &allow-other-keys)
                 `(%write-arg ,name ,type ,socket ,buffer))
               arg-specifiers))))))

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
               (:metaclass wl-interface))))
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

(:DOCUMENTATION STRING) - Provided to the function as its docstring.
(:TYPE KEYWORD) - So far, only :DESTRUCTOR is a valid type.
(:SINCE VERSION) - Minimum interface version of the proxy object."
  (%option-bind (documentation type since) options
    (let* ((destructor? (eq (first type) :destructor))
           ;; The expression that evaluates to the new proxy's interface, if
           ;; such proxy will be created. If the type is protocol-defined,
           ;; then it's the quoted interface name. If the type is known
           ;; at runtime, then it's picked up from the lambda list.
           (new-proxy-interface
             (dolist (specifier arg-specifiers)
               (let ((type (getf (rest specifier) :type)))
                 (%wltype-case type
                   ((:new-id &key interface)
                    (return (if interface
                                `(quote ,interface)
                                'interface-class)))))))
           (lambda-list-tail
             (mapcan (%slambda (name &key type &allow-other-keys)
                       (%wltype-case type
                         ((:new-id &key interface)
                          (unless interface
                            (list new-proxy-interface 'version)))
                         (t (list name))))
                     arg-specifiers))
           (defun-head
             `(defun ,name ,(list* interface lambda-list-tail)))
           (defun-body
             (@and
               ;; The core of the request procedure is the %send-request macro,
               `(%send-request ,interface ,opcode
                  ,@(mapcar
                      (lambda (specifier)
                        (%specifier-bind (&key type &allow-other-keys)
                                         (rest specifier)
                          (%wltype-case type
                            (:new-id `(new-proxy ,@(rest specifier)))
                            (t specifier))))
                      arg-specifiers))
               ;; ...and then if a new proxy is created, wrap that around a let
               (if new-proxy-interface
                   `(let ((new-proxy (%make-proxy
                                       ,new-proxy-interface
                                       (wl-proxy-display ,interface)
                                       ,@(when (member 'version lambda-list-tail)
                                           '(:version version)))))
                      ,|@|
                      new-proxy)
                   @)
               ;; ...and finally, add the docstring and typecheck
               ;; for a full body.
               `(,@documentation
                ;; Destructor methods won't need the check-type, because
                ;; it's already specialized in the parameter list.
                ,(unless destructor?
                   `(%check-proxy ,interface ,interface ,(first since)))
                  ,|@|
                  ))))
      ;; If the request is a destructor, put the logic in a DESTROY-PROXY
      ;; method and use the function name as a synonym.
      (if destructor?
          (progn
            (assert (null arg-specifiers)
                    (arg-specifiers)
                    "Destructor request cannot have any parameters")
            `(prog2
               (declaim (inline ,name))
               (,@defun-head
                 (destroy-proxy ,interface))
               (defmethod destroy-proxy :before ((,interface ,interface))
                 ,@defun-body)))
          `(,@defun-head
             ,@defun-body)))))

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
(:SINCE VERSION) - Does nothing.
(:EVENT-SUPERCLASSES CLASS-NAMES...) - When provided, the event class subclasses these given classes. Otherwise, it subclasses WL-EVENT."
  (%option-bind (documentation) options
    (a:with-gensyms (proxy opcode-sym buffer)
      `(progn
         ;; Event reader
         (defmethod %read-event ((,proxy ,interface)
                                 (,opcode-sym (eql ,opcode))
                                 ,buffer)
           ,@documentation
           (list ',name
                 ,@(mapcar
                     (%slambda (arg-name &key type &allow-other-keys)
                       (declare (ignore arg-name))
                       `(%read-arg ,type ,proxy ,buffer))
                     arg-specifiers)))
         ',name))))



;; Utility Macros

(defmacro with-open-display ((display &rest options) &body body)
  "Like WITH-OPEN-FILE, but binding DISPLAY to the result of WL-DISPLAY-CONNECT instead of OPEN.
Executes the body with DISPLAY bound to a freshly connected display."
  `(let ((,display (wl-display-connect ,@options)))
     (unwind-protect
       (progn ,@body)
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
