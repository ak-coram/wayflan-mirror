;;; client.lisp -- fun

(defpackage #:xyz.shunter.wayhack.client
  (:use #:cl)
  (:local-nicknames (#:io #:fast-io)
                    (#:wire #:xyz.shunter.wayhack.wire))
  (:export #:wl-proxy
           #:wl-proxy-id
           #:wl-proxy-display
           #:wl-proxy-listeners
           #:wl-deleted-proxy
           #:wl-event
           #:wl-event-listener
           #:read-event
           #:handle-event

           #:find-proxy
           #:make-proxy
           #:display-pathname
           #:wl-display-connect
           #:wl-display-disconnect
           #:wl-display-listen
           #:wl-display-dispatch-event

           ;; Wayland protocol
           #:wl-display
           #:wl-display-event
           #:wl-display-error-event
           #:wl-event-proxy
           #:wl-event-code
           #:wl-event-message
           #:wl-display-delete-id-event
           #:wl-event-id
           #:wl-display-sync
           #:wl-display-get-registry

           #:wl-callback
           #:wl-callback-event
           #:wl-callback-done-event
           #:wl-event-callback-data))

(in-package #:xyz.shunter.wayhack.client)



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

(defgeneric read-event (sender opcode buffer)
  (:documentation "Read an event sent from PROXY with the given OPCODE and fast-io BUFFER."))

(defgeneric dispatch-event (sender event)
  (:documentation "Inform a proxy about an event of interest. Usually, emit the event across its listeners."))

(defgeneric handle-event (listener sender event)
  (:documentation "Notify a listener about an event of interest."))

(defmethod print-object ((object wl-proxy) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S ~S (~D)"
            :id (wl-proxy-id object)
            :listeners (length (wl-proxy-listeners object)))))

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
;; Display utilities

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
  (let (sender
        event)
    (wire:with-input-from-message (buffer sender-id opcode
                                          nil (display-socket display))
      (setf sender (find-proxy display sender-id)
            event (read-event sender opcode buffer))
      (dispatch-event sender event))))

;; Event handling

(defmethod dispatch-event (sender event)
  "Inform all proxy's listeners of the event."
  (dolist (listener (wl-proxy-listeners sender))
    (handle-event listener sender event)))

;; Stub out event for defmethod, will be redefined later.
(defclass wl-display-delete-id-event (wl-event) ())

(defmethod dispatch-event :before (display (event wl-display-delete-id-event))
  "Mark the object as deleted and remove it from the object table."
  (remove-proxy display (wl-event-id event)))



;; WL protocol macros

(eval-when (:load-toplevel :compile-toplevel)
  ;; Macro helpers
  (defun hyphenize (&rest strings)
    (format nil "~{~A~^-~}" (mapcar #'string strings)))

  (defun name-and-options-name (name-and-options)
    (if (symbolp name-and-options)
        name-and-options
        (first name-and-options)))

  (defun name-and-options-options (name-and-options)
    (and (listp name-and-options)
         (rest name-and-options))))

(defmacro define-interface (name-and-options &rest class-options)
  "Define a wayland interface, implementing its proxy CLOS class and event CLOS class."
  (let* ((name (name-and-options-name name-and-options))
         (options (name-and-options-options name-and-options))
        (event-name (intern (hyphenize name '#:event))))
    `(progn
       ,(unless (cadr (assoc :skip-defclass options))
          `(defclass ,name (wl-proxy) () ,@class-options))
       (defclass ,event-name (wl-event) ()
         (:documentation ,(format nil "Event sent from a ~A object."
                                  (symbol-name name)))))))

(defmacro define-event (interface name-and-options args &rest options)
  "Define a wayland event, implementing its proxy CLOS class and (TODO) its read-event method."
  (let* ((name (name-and-options-name name-and-options))
         (full-name (intern (hyphenize interface name '#:event)))
         (parent-name (intern (hyphenize interface '#:event)))
         (slot-specifiers
           (mapcar (lambda (arg)
                     (let ((initarg-name (intern (string arg) :keyword))
                           (reader-name (intern (hyphenize '#:wl-event arg))))
                       `(,arg :initarg ,initarg-name
                              :reader ,reader-name)))
                   args)))
    `(defclass ,full-name (,parent-name)
       ,slot-specifiers
       ,@options)))

(defmacro define-enum (interface name-and-options entries)
  (let* ((name (name-and-options-name name-and-options))
         (full-name (intern (format nil "+~A+" (hyphenize interface name)))))
    `(defparameter ,full-name
       ,(make-array (length entries)
                    :initial-contents entries))))



;; wl-display

(define-interface (wl-display (:skip-defclass t)))

(define-event wl-display (#:error :opcode 0)
  (proxy code message))

(define-event wl-display (#:delete-id :opcode 0)
  (id))

(define-enum wl-display (#:errors :bitfield nil)
  (:invalid-object :invalid-method :no-memory :implementation))

;; wl-display requests

(defun wl-display-sync (display)
  (let ((new-proxy (make-proxy 'wl-callback display)))
    (wire:with-output-as-message (buffer (wl-proxy-id display)
                                         0
                                         (display-socket display))
      (wire:write-wl-uint (wl-proxy-id new-proxy) buffer))
    new-proxy))

(defun wl-display-get-registry (display)
  (let ((new-proxy (make-proxy 'wl-registry display)))
    (wire:with-output-as-message (buffer (wl-proxy-id display)
                                         1
                                         (display-socket display))
      (wire:write-wl-uint (wl-proxy-id new-proxy) buffer))
    new-proxy))

;; wl-display events

(defmethod read-event ((sender wl-display) (opcode (eql 0)) buffer)
  "Read the wl-display error event."
  (make-instance 'wl-display-error-event
                 :sender sender
                 :object-id (find-proxy (wl-proxy-display sender)
                                        (wire:read-wl-uint buffer))
                 :code (wire:read-wl-uint buffer)
                 :message (wire:read-wl-string buffer)))

(defmethod read-event ((sender wl-display) (opcode (eql 1)) buffer)
  "Read the wl-display delete-id event."
  (make-instance 'wl-display-delete-id-event
                 :sender sender
                 :id (wire:read-wl-uint buffer)))

;; wl-callback

(define-interface wl-callback)

(define-event wl-callback (#:done :opcode 0)
  (callback-data))

;; wl-callback events

(defmethod read-event ((sender wl-callback) (opcode (eql 0)) buffer)
  "Read the wl-callback done event."
  (make-instance 'wl-callback-done-event
                 :sender sender
                 :callback-data (wire:read-wl-uint buffer)))
