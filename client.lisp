;;; client.lisp -- Wayland client implementation
;;;
;;; Copyright (c) 2022 Samuel Hunter.
;;; All rights reserved.

(in-package #:xyz.shunter.wayhack.client)



(defparameter *interface-table*
  (make-hash-table :test 'equal)
  "Maps all Wayland interface names to their proxy class")

(defclass wl-proxy ()
  ((object-id :reader wl-proxy-id)
   (display :initarg :display
            :reader wl-proxy-display)
   (listeners :initform ()
              :accessor wl-proxy-listeners))
  (:documentation "A protocol object on the client side"))

(defclass wl-deleted-proxy (wl-proxy)
  ()
  (:documentation "A proxy that has since been deleted by the compositor."))

(defclass wl-display (wl-proxy)
  ((proxy-table :initform (make-hash-table))
   (socket :initarg :socket :reader display-socket))
  (:documentation "A connection to the compositor that acts as a proxy to the wl_display singleton object"))

(defclass wl-event ()
  ((sender :initarg :sender :reader wl-event-sender)))

(defclass wl-event-listener ()
  ()
  (:documentation "Classes subclassing Wl-EVENT-LISTENER advertise that they implement HANDLE-EVENT."))

(defgeneric handle-event (listener sender event)
  (:documentation "Notify a listener about an event of interest."))

(defgeneric read-event (sender opcode buffer)
  (:documentation "Read an event sent from PROXY with the given OPCODE and fast-io BUFFER.
READ-EVENT methods are defined by DEFINE-EVENT-READER."))

(defgeneric dispatch-event (sender event)
  (:documentation "Inform a proxy about an event of interest. Usually, emit the event across its listeners."))

(defmethod print-object ((object wl-proxy) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~S (~D)"
            :id (wl-proxy-id object)
            :listeners (length (wl-proxy-listeners object)))))

(defun find-interface-named (name)
  "Return the interface linked to the given string NAME."
  (gethash name *interface-table*))

;; Proxy management

(defun find-proxy (display id)
  "Return a proxy with the given ID."
  (gethash id (slot-value display 'proxy-table)))

(defun set-proxy (display id new-proxy)
  "Assign a proxy with the given id to the table."
  (declare (optimize debug))
  (setf (slot-value new-proxy 'object-id) id
        (gethash id (slot-value display 'proxy-table)) new-proxy))

(defun remove-proxy (display id)
  "Mark the proxy with the given ID as removed."
  (change-class (find-proxy display id) 'wl-deleted-proxy)
  (remhash id (slot-value display 'proxy-table))
  (values))

(defun clear-proxies (display)
  "Mark all owned proxies as removed and return DISPLAY."
  (let ((proxy-table (slot-value display 'proxy-table)))
    (with-hash-table-iterator (next-item proxy-table)
      (loop
        (multiple-value-bind (more? id proxy) (next-item)
          (unless more? (return))
          (change-class proxy 'wl-deleted-proxy)
          (remhash id proxy-table)))))
  (values))

(defun next-proxy-id (display)
  "Return the next free proxy ID between 1 and #xFEFFFFFF."
  (let ((proxy-table (slot-value display 'proxy-table)))
    (dotimes (i #xfeffffff)
      (unless (gethash (1+ i) proxy-table)
        (return (1+ i))))))

(defun make-proxy (class display &optional object-id)
  "Make a new proxy. If OBJECT-ID is provided, it's assumed it came from the server. Otherwise, it will allocate a new object-id from the client side."
  (let ((new-proxy (make-instance class :display display)))
    (set-proxy display (or object-id (next-proxy-id display)) new-proxy)))

(defmethod initialize-instance :after ((instance wl-display)
                                       &key &allow-other-keys)
  (setf (slot-value instance 'display) instance)
  (set-proxy instance 1 instance))

;; Display management

(defun display-pathname (&optional display-name)
  (merge-pathnames
    (or display-name (uiop:getenv "WAYLAND_DISPLAY"))
    (uiop:parse-unix-namestring
      (uiop:getenv "XDG_RUNTIME_DIR")
      :ensure-directory t)))

(defun wl-display-connect (&optional display-name)
  "Return a display connected to the given socket name.

DISPLAY-NAME is an optional pathname designator pointing to the display socket. If it is a relative path, it will be merged with the XDG_RUNTIME_DIR environment variable."
  (make-instance 'wl-display
                 :socket (sockets:connect
                           (sockets:make-socket :address-family :local
                                                :type :stream)
                           (sockets:make-address
                             (uiop:unix-namestring
                               (display-pathname display-name))))))

(defun wl-display-disconnect (display)
  "Close the connection and remove all proxies."
  (prog1
    (close (display-socket display))
    (clear-proxies display)))

(defun wl-display-listen (display)
  "Return whether there is a (partial) message available from the display."
  (listen (display-socket display)))

(defun wl-display-dispatch-event (display)
  "Read and dispatch the display's next event."
  (let (sender event)
    (wire:with-input-from-message (buffer sender-id opcode
                                          nil (display-socket display))
      (setf sender (find-proxy display sender-id)
            event (read-event sender opcode buffer))
      (dispatch-event sender event))))

(defclass roundtrip-listener (wl-event-listener)
  ((callback :initarg :callback)))

(defun wl-display-roundtrip (display)
  "Request to synchronize the display and dispatch all events until synchronization is complete."
  (let ((callback (wl-display-sync display))
        (sync-complete nil))
    (push (make-instance 'roundtrip-listener
                         :callback (lambda () (setf sync-complete t)))
          (wl-proxy-listeners callback))
    (loop :until sync-complete
          :do (wl-display-dispatch-event display))))

;; Event handling

;; Stub out some classes for now -- it'll be redefined by WL-INCLUDE in
;; protocols.lisp
(defclass wl-callback-done-event (wl-event) ())
(defclass wl-display-delete-id-event (wl-event) ())
(defclass wl-display-error-event (wl-event) ())

(defmethod handle-event ((listener roundtrip-listener) sender (event wl-callback-done-event))
  (funcall (slot-value listener 'callback)))

(defmethod dispatch-event (sender event)
  "Inform all proxy's listeners of the event."
  (dolist (listener (wl-proxy-listeners sender))
    (handle-event listener sender event)))

(defmethod dispatch-event :before (display (event wl-display-delete-id-event))
  "Mark the object as deleted and remove it from the object table."
  (remove-proxy display (wl-event-id event)))

(defmethod dispatch-event :before (display (event wl-display-error-event))
  (error "Wl-error event dispatched"))



;; Wl protocol macros

(defmacro specifier-bind (lambda-list specifier &body body)
  "Bind the given LAMBDA-LIST to SPECIFIER, which is transformed to a list of itself if it's not already a list."
  (a:once-only (specifier)
    `(destructuring-bind ,lambda-list (if (listp ,specifier)
                                          ,specifier
                                          (list ,specifier))
       ,@body)))

(defmacro option-bind ((&rest option-names) options &body body)
  (a:once-only (options)
    (let ((binding-forms
            (mapcar (lambda (option-name)
                      `(cdr (assoc ,(a:make-keyword option-name) ,options)))
                    option-names)))
      `(let ,(mapcar #'list option-names binding-forms)
         ,@body))))

(defmacro wltype-case (keyform &body cases)
  "Evaluate the clause selected by the matching wayland type."
  (a:once-only (keyform)
    `(case (if (listp ,keyform)
               (first ,keyform)
               ,keyform)
       ,@(mapcar
           (lambda (case)
             (destructuring-bind (type &rest form) case
               `(,(if (listp type)
                      (first type)
                      type)
                  ,@(if (listp type)
                        `((destructuring-bind ,(rest type) (when (listp ,keyform)
                                                             (rest ,keyform))
                            ,@form))
                        form))))
           cases))))

(defmacro wltype-ecase (keyform &body cases)
  "Evaluate the clause selected by the matching wayland type."
  (a:once-only (keyform)
    `(ecase (if (listp ,keyform)
                (first ,keyform)
                ,keyform)
       ,@(mapcar
           (lambda (case)
             (destructuring-bind (type &rest form) case
               `(,(if (listp type)
                      (first type)
                      type)
                  ,@(if (listp type)
                        `((destructuring-bind ,(rest type) (when (listp ,keyform)
                                                             (rest ,keyform))
                            ,@form))
                        form))))
           cases))))

(defmacro read-arg (type sender buffer)
  "Read an object from the Wayland buffer depending on the given TYPE."
  (wltype-ecase type
    (:int `(wire:read-wl-int ,buffer))
    (:uint `(wire:read-wl-uint ,buffer))
    (:fixed `(wire:read-wl-fixed ,buffer))
    (:string `(wire:read-wl-string ,buffer))
    (:object `(find-proxy (wl-proxy-display ,sender)
                          (wire:read-wl-uint ,buffer)))
    ((:new-id interface)
     `(make-proxy ',interface
                  (wl-proxy-display ,sender)
                  (wire:read-wl-uint ,buffer)))
    (:array `(wire:read-wl-array ,buffer))
    (:fd `(iolib:receive-file-descriptor
            (io:input-buffer-stream ,buffer)))))

(defmacro write-arg (obj type sender buffer)
  "Write an object to the Wayland buffer depending on the given TYPE."
  (wltype-ecase type
    (:int `(wire:write-wl-int ,obj ,buffer))
    (:uint `(wire:write-wl-uint ,obj ,buffer))
    (:fixed `(wire:write-wl-fixed ,obj ,buffer))
    (:string `(wire:write-wl-string ,obj ,buffer))
    (:object `(wire:write-wl-uint (wl-proxy-id ,obj) ,buffer))
    (:new-id `(wire:write-wl-uint (wl-proxy-id ,obj) ,buffer))
    (:array `(wire:write-wl-array ,obj ,buffer))
    (:fd `(iolib:send-file-descriptor
            (display-socket (wl-proxy-display ,sender))
            ,obj))))

(defmacro define-interface (name () &body options)
  "Define a wl-proxy CLOS subclass an associated wl-event CLOS subclass, and assign the interface's name to the interface table, accessible via #'FIND-INTERFACE-NAMED."
  (option-bind (documentation event-class skip-defclass interface-name) options
    `(progn
       ;; wl-proxy class
       ,@(unless (first skip-defclass)
          `((defclass ,name (wl-proxy) ()
              (:documentation ,@documentation))))

       ;; wl-event class
       ,@(when event-class
           `((defclass ,(first event-class) (wl-event) ()
               (:documentation ,(format nil "Event sent from a ~A proxy."
                                        name)))))

       ;; Associate wl-proxy class w/ interface name
       ,@(when interface-name
           `((setf (gethash ,(first interface-name) *interface-table*)
                  (find-class ',name))))

       ',name)))

(defmacro define-enum (name () &body (entry-specifiers &rest options))
  "Define a parameter that associates each entry keyword with an index in the array."
  (declare (ignore name entry-specifiers options)))

(defmacro define-request ((name interface opcode) &body (arg-specifiers &rest options))
  "Define a function implementing the wl request.

DEFINE-REQUEST currently only supports up to one :NEW-ID argument per request."
  (option-bind (documentation) options
    (a:with-gensyms (proxy buffer new-proxy)
      (let* ((new-proxy-interface
               ;; If a new proxy is being created, in this request, the form to
               ;; evaluate to the proxy's interface. If the type is fixed, it's
               ;; that type, quoted. Otherwise, it's the interface's name given in
               ;; the lambda list.
               (dolist (specifier arg-specifiers)
                 (let ((type (getf (rest specifier) :type)))
                   (wltype-case type
                     ((:new-id &optional type-interface)
                      (return (if type-interface
                                  `(quote ,type-interface)
                                  (gensym "INTERFACE"))))))))
             (lambda-list-tail
               (mapcan (lambda (specifier)
                         (specifier-bind (name &key type &allow-other-keys) specifier
                           (wltype-case type
                             ((:new-id &optional type-interface)
                              (unless type-interface
                                (list new-proxy-interface)))
                             (t (list name)))))
                       arg-specifiers))
             (output-form
               `(wire:with-output-as-message (,buffer (wl-proxy-id ,proxy)
                                                      ,opcode
                                                      (display-socket (wl-proxy-display ,proxy)))
                  ,@(mapcar (lambda (specifier)
                              (specifier-bind (name &key type &allow-other-keys) specifier
                                `(write-arg ,(wltype-case type
                                               (:new-id new-proxy)
                                               (t name))
                                            ,type
                                            ,proxy
                                            ,buffer)))
                            arg-specifiers))))
        `(defun ,name ,(list* proxy lambda-list-tail)
           ,@documentation
           (check-type ,proxy ,interface)
           ,(if new-proxy-interface
                `(let ((,new-proxy (make-proxy ,new-proxy-interface (wl-proxy-display ,proxy))))
                   ,output-form
                   ,new-proxy)
                output-form))))))

(defmacro define-event ((name interface opcode) &body (arg-specifiers &rest options))
  "Define a wl-event class and a READ-EVENT method to support WL-DISPLAY-DISPATCH-EVENT."
  (option-bind (documentation event-superclasses) options
    (a:with-gensyms (proxy opcode-sym buffer)
      (let ((slot-specifiers
              (mapcar (lambda (arg-specifier)
                        (specifier-bind (arg-name &key initarg documentation &allow-other-keys)
                                        arg-specifier
                          `(,arg-name :reader ,arg-name
                                  :initarg ,initarg
                                  ,@(when documentation
                                      `(:documentation ,@documentation)))))
                      arg-specifiers))
            (initargs
              (mapcan
                (lambda (arg-specifier)
                  (specifier-bind (arg-name &key initarg type &allow-other-keys)
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
             (make-instance ',name ,@initargs)))))))

