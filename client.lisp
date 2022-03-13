;;; client.lisp -- fun

(defpackage #:xyz.shunter.wayhack.client
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:io #:fast-io)
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

           #:find-interface-named
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

;; Placeholder definition, will be redefined later
(defclass wl-callback-done-event (wl-event) ())

(defmethod handle-event ((listener roundtrip-listener) sender (event wl-callback-done-event))
  (funcall (slot-value listener 'callback)))

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

(defmethod dispatch-event (sender event)
  "Inform all proxy's listeners of the event."
  (dolist (listener (wl-proxy-listeners sender))
    (handle-event listener sender event)))

;; Stub out event for defmethod, will be redefined later.
(defclass wl-display-delete-id-event (wl-event) ())

(defmethod dispatch-event :before (display (event wl-display-delete-id-event))
  "Mark the object as deleted and remove it from the object table."
  (remove-proxy display (wl-event-id event)))



;; Wl protocol macros

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; Macro helpers
  (defun hyphenize (&rest strings)
    (format nil "~{~A~^-~}" (mapcar #'string strings))))

(defmacro specifier-bind (lambda-list specifier &body body)
  "Bind the given LAMBDA-LIST to SPECIFIER, which is transformed to a list of itself if it's not already a list."
  (a:once-only (specifier)
    `(destructuring-bind ,lambda-list (if (listp ,specifier)
                                          ,specifier
                                          (list ,specifier))
       ,@body)))

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
    (:uint `(wire:read-wl-uint ,buffer))
    (:int `(wire:read-wl-int ,buffer))
    (:string `(wire:read-wl-string ,buffer))
    (:array `(wire:read-wl-array ,buffer))
    (:object `(find-proxy (wl-proxy-display ,sender)
                          (wire:read-wl-uint ,buffer)))
    ((:new-id interface)
     `(make-proxy ,interface
                      (wl-proxy-display ,sender)
                      (wire:read-wl-uint ,buffer)))))

(defmacro write-arg (obj type buffer)
  "Write an object to the Wayland buffer depending on the given TYPE."
  (wltype-ecase type
    (:uint `(wire:write-wl-uint ,obj ,buffer))
    (:int `(wire:write-wl-int ,obj ,buffer))
    (:string `(wire:write-wl-string ,obj ,buffer))
    (:array `(wire:write-wl-array ,obj ,buffer))
    (:object `(wire:write-wl-uint (wl-proxy-id ,obj) ,buffer))
    (:new-id `(wire:write-wl-uint (wl-proxy-id ,obj) ,buffer))))

(defmacro define-interface (name &body options)
  "Define a wl-proxy CLOS subclass an associated wl-event CLOS subclass, and assign the interface's name to the interface table, accessible via #'FIND-INTERFACE-NAMED."
  (let ((event-name (intern (hyphenize name '#:event))))
    `(progn
       ;; wl-proxy class. The :skip-defclass option is given specifically for
       ;; wl-display.
       ,(unless (cadr (assoc :skip-defclass options))
          `(defclass ,name (wl-proxy) ()))
       ;; wl-event class
       (defclass ,event-name (wl-event) ()
         (:documentation ,(format nil "Event sent from a ~A object."
                                  (symbol-name name))))
       ;; Associate wl-proxy class w/ interface name
       ,(when (assoc :name options)
          `(setf (gethash ,(cadr (assoc :name options))
                          *interface-table*)
                 (find-class ',name))))))

(defmacro define-enum ((interface name &rest options) &body entry-specifiers)
  "Define a parameter that associates each entry keyword with an index in the array."
  (declare (ignore options))
  ;; TODO implement #'decode-X and #'encode-X, sensitive to :bitfield
  (let ((full-name (intern (format nil "+~A+" (hyphenize interface name)))))
    `(defparameter ,full-name
       ,(make-array (length entry-specifiers)
                    :initial-contents (mapcar #'first entry-specifiers)))))

(defmacro define-request ((interface name &rest options) &body arg-specifiers)
  "Define a function implementing the wl request.

DEFINE-REQUEST currently only supports up to one :NEW-ID argument per request."
  (destructuring-bind (&key opcode &allow-other-keys) options
    (let ((full-name (intern (hyphenize interface name)))
          (new-proxy-interface
            (dolist (specifier arg-specifiers)
              (let ((type (getf (rest specifier) :type)))
                (wltype-case type
                  ((:new-id &optional interface)
                   (return (if interface `',interface 'interface)))))))
          (lisp-args
            (mapcan (lambda (specifier)
                      (specifier-bind (name &key type &allow-other-keys) specifier
                        (wltype-case type
                          ((:new-id &optional interface)
                           (if interface () (list 'interface)))
                          (t (list name)))))
                    arg-specifiers))
          (output-form
            `(wire:with-output-as-message (buffer (wl-proxy-id proxy)
                                                  ,opcode
                                                  (display-socket (wl-proxy-display proxy)))
               ,@(mapcar (lambda (specifier)
                           (specifier-bind (name &key type &allow-other-keys) specifier
                             `(write-arg ,(wltype-case type
                                            (:new-id 'new-proxy)
                                            (t name))
                                         ,type
                                         buffer)))
                         arg-specifiers))))
      `(defun ,full-name (proxy ,@lisp-args)
         ,(if new-proxy-interface
              `(let ((new-proxy (make-proxy ,new-proxy-interface (wl-proxy-display proxy))))
                 ,output-form
                 new-proxy)
              output-form)))))

(defmacro define-event ((interface name &rest options) &body arg-specifiers)
  "Define a wl-event class and a READ-EVENT method to support WL-DISPLAY-DISPATCH-EVENT."
  (destructuring-bind (&key opcode &allow-other-keys) options
    (let ((full-name (intern (hyphenize interface name '#:event)))
          (parent-name (intern (hyphenize interface '#:event)))
          (slot-specifiers
            (mapcar (lambda (arg-specifier)
                      (specifier-bind (name &key &allow-other-keys) arg-specifier
                        (let ((initarg (a:make-keyword name))
                              (reader (intern (hyphenize '#:wl-event name))))
                          `(,name :initarg ,initarg
                                  :reader ,reader))))
                    arg-specifiers)))
      `(progn
         ;; Event class
         (defclass ,full-name (,parent-name)
           ,slot-specifiers)
         ;; Event reader
         (defmethod read-event ((proxy ,interface) (opcode (eql ,opcode)) buffer)
           (make-instance
             ',full-name
             ,@(mapcan
                 (lambda (arg-specifier)
                   (specifier-bind (name &key type) arg-specifier
                     (list (a:make-keyword name)
                           `(read-arg ,type proxy buffer))))
                 arg-specifiers)))))))



;; wl-display

(define-interface wl-display
  (:skip-defclass t)
  (:name "wl_display"))

(define-enum (wl-display #:errors :bitfield nil)
  (:invalid-object)
  (:invalid-method)
  (:no-memory)
  (:implementation))

(define-request (wl-display #:sync :opcode 0)
  (callback :type (:new-id wl-callback)))

(define-request (wl-display #:get-registry :opcode 1)
  (registry :type (:new-id wl-registry)))

(define-event (wl-display #:error :opcode 0)
  (object-id :type :object)
  (code :type :uint)
  (message :type :string))

(define-event (wl-display #:delete-id :opcode 1)
  (id :type :uint))

;; wl-registry

(define-interface wl-registry
  (:name "wl_registry"))

(define-request (wl-registry #:bind :opcode 0)
  (name :type :string)
  (id :type :new-id))

(define-event (wl-registry #:global :opcode 0)
  (name :type :uint)
  (interface :type :string)
  (version :type :uint))

(define-event (wl-registry #:global-remove :opcode 1)
  (name :type :uint))

;; wl-callback

(define-interface wl-callback
  (:name "wl_callback"))

(define-event (wl-callback #:done :opcode 0)
  (callback-data :type :uint))

;; wl-compositor

(define-interface wl-compositor
  (:name "wl_compositor"))

(define-request (wl-compositor #:create-surface :opcode 0)
  (id :type (:new-id wl-surface)))

(define-request (wl-compositor #:create-region :opcode 1)
  (id :type (:new-id wl-region)))

;; wl-shm-pool

(define-interface wl-shm-pool
  (:name "wl_shm_pool"))

(define-request (wl-shm-pool #:create-buffer :opcode 0)
  (id :type (:new-id wl-buffer))
  (offset :type :int)
  (width :type :int)
  (height :type :int)
  (stride :type :int)
  ;; TODO support automatic enum decoding
  (format :type (:uint wl-shm-format)))

(define-request (wl-shm-pool #:destroy
                             :opcode 2
                             :type :destructor))

(define-request (wl-shm-pool #:resize :opcode 2)
  (size :type :int))

;; wl-shm

(define-interface wl-shm
  (:name "wl_shm"))

;(define-request (wl-shm #:create-pool :opcode 0)
;  (id :type (:new-id wl-shm-pool))
;  (fd :type :fd)
;  (size :type :int))

(define-event (wl-shm #:format :opcode 0)
  (format :type (:uint wl-shm-format)))

(define-enum (wl-shm #:error)
  (:invalid-format)
  (:invalid-stride)
  (:invalid-fd))

(define-enum (wl-shm #:format)
  (:argb8888)
  (:xrgb8888)
  (:c8)
  (:rgb332)
  (:bgr233)
  (:xrgb4444)
  (:xbgr4444)
  (:rgbx4444)
  (:bgrx4444)
  (:argb4444)
  (:abgr4444)
  (:rgba4444)
  (:bgra4444)
  (:xrgb1555)
  (:xbgr1555)
  (:rgbx5551)
  (:bgrx5551)
  (:argb1555)
  (:abgr1555)
  (:rgba5551)
  (:bgra5551)
  (:rgb565)
  (:bgr565)
  (:rgb888)
  (:bgr888)
  (:xbgr8888)
  (:rgbx8888)
  (:bgrx8888)
  (:abgr8888)
  (:rgba8888)
  (:bgra8888)
  (:xrgb2101010)
  (:xbgr2101010)
  (:rgbx1010102)
  (:bgrx1010102)
  (:argb2101010)
  (:abgr2101010)
  (:rgba1010102)
  (:bgra1010102)
  (:yuyv)
  (:yvyu)
  (:uyvy)
  (:vyuy)
  (:ayuv)
  (:nv12)
  (:nv21)
  (:nv16)
  (:nv61)
  (:yuv410)
  (:yvu410)
  (:yuv411)
  (:yvu411)
  (:yuv420)
  (:yvu420)
  (:yuv422)
  (:yvu422)
  (:yuv444)
  (:yvu444)
  (:r8)
  (:r16)
  (:rg88)
  (:gr88)
  (:rg1616)
  (:gr1616)
  (:xrgb16161616f)
  (:xbgr16161616f)
  (:argb16161616f)
  (:abgr16161616f)
  (:xyuv8888)
  (:vuy888)
  (:vuy101010)
  (:y210)
  (:y212)
  (:y216)
  (:y410)
  (:y412)
  (:y416)
  (:xvyu2101010)
  (:xvyu12_16161616)
  (:xvyu16161616)
  (:y0l0)
  (:x0l0)
  (:y0l2)
  (:x0l2)
  (:yuv420_8bit)
  (:yuv420_10bit)
  (:xrgb8888_a8)
  (:xbgr8888_a8)
  (:rgbx8888_a8)
  (:bgrx8888_a8)
  (:rgb888_a8)
  (:bgr888_a8)
  (:rgb565_a8)
  (:bgr565_a8)
  (:nv24)
  (:nv42)
  (:p210)
  (:p010)
  (:p012)
  (:p016)
  (:axbxgxrx106106106106)
  (:nv15)
  (:q410)
  (:q401)
  (:xrgb16161616)
  (:xbgr16161616)
  (:argb16161616)
  (:abgr16161616)
  (:argb8888)
  (:xrgb8888)
  (:c8)
  (:rgb332)
  (:bgr233)
  (:xrgb4444)
  (:xbgr4444)
  (:rgbx4444)
  (:bgrx4444))

;; wl-buffer

(define-interface wl-buffer
  (:name "wl_buffer"))

(define-request (wl-buffer #:destroy
                           :opcode 0
                           :type :destructor))

(define-event (wl-buffer #:release :opcode 0))



(defclass test-listener (wl-event-listener) ())

(defmethod handle-event ((listener test-listener) proxy (event wl-registry-global-event))
  (format t "#x~8,'0X  ~35A  v~D~%"
          (wl-event-name event)
          (wl-event-interface event)
          (wl-event-version event)))

(defun test ()
  (let ((display (wl-display-connect)))
    (unwind-protect
      (let ((registry (wl-display-get-registry display)))
        (push (make-instance 'test-listener)
              (wl-proxy-listeners registry))
        (wl-display-roundtrip display))
      (wl-display-disconnect display))))
