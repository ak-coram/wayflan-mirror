;;; sockets.lisp - local sockets for Wayflan
;;;
;; Copyright (c) 2022 Samuel Hunter <samuel@shunter.xyz>
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.sockets)



;; Ring Buffers

(deftype buffer-data () 'cffi:foreign-pointer)
(deftype buffer-offset () '(unsigned-byte 32))
(deftype octet () '(unsigned-byte 8))
(deftype octet-sarray (&optional (size '*))
  `(simple-array octet (,size)))
(deftype octet-vector (&optional (size '*))
  `(vector octet ,size))

(defstruct ring-buffer
  (data  (cffi:null-pointer) :type buffer-data)
  (cap   0                   :type buffer-offset)
  (start 0                   :type buffer-offset)
  (end   0                   :type buffer-offset))

;; This magic number comes from libwayland. TODO: Figure out why
(defconstant +max-fds-out+ 28)

;; Both sizes must be powers of 2, so that LOGAND can be used to mask the
;; length of the buffer.
(defconstant +iobuf-size+ (* 4 4096)
             "Number of octets to store in a ring buffer")
(defconstant +fdbuf-size+
             ;; Lowest power of 2 that fits +max-fds-out+
             (expt 2 (ceiling (log +max-fds-out+ 2)))
             "Number of file descriptors to store in a fd buffer")

(defun allocate-ring-buffer (type size)
  "Allocate a ring buffer to hold SIZE elements of TYPE. Free with FREE-RING-BUFFER."
  (make-ring-buffer :data (cffi:foreign-alloc type :count size)
                    :cap size))

(defun free-ring-buffer (buf)
  (cffi:foreign-free (ring-buffer-data buf))
  (setf (ring-buffer-data buf) (cffi:null-pointer)))

(defun clear-ring-buffer (buf)
  "Reset the ring buffer's head and tail."
  (setf (ring-buffer-start buf) 0
        (ring-buffer-end buf) 0)
  (values))

(declaim (inline ring-buffer-length ring-buffer-empty?
                 ring-buffer-freespace ring-buffer-full?))
(defun ring-buffer-length (buf)
  (- (ring-buffer-end buf) (ring-buffer-start buf)))

(defun ring-buffer-empty? (buf)
  (= (ring-buffer-end buf) (ring-buffer-start buf)))

(defun ring-buffer-freespace (buf)
  (- (ring-buffer-cap buf) (ring-buffer-length buf)))

(defun ring-buffer-full? (buf)
  (= (ring-buffer-cap buf) (ring-buffer-length buf)))

(defun ring-buffer-can-fit? (buf sarray start end)
  "Return whether a simple octet array can fit inside BUF."
  (declare (type ring-buffer buf)
           (type octet-sarray sarray)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (setf end (or end (length sarray)))
  (>= (ring-buffer-freespace buf)
      (- end start)))

(defun shiftin-ring-buffer (buf count)
  "Shift the start of the ring buffer by COUNT items."
  (assert (<= count (ring-buffer-length buf)))
  (if (= count (ring-buffer-length buf))
      (clear-ring-buffer buf)
      (incf (ring-buffer-start buf) count))
  (values))

(defun shiftout-ring-buffer (buf count)
  "Shift the end of the ring buffer by COUNT items."
  (incf (ring-buffer-end buf) count))

(defun ring-buffer-push (buf type obj)
  "Add a single item to the ring buffer."
  (assert (not (ring-buffer-full? buf)))
  (with-accessors ((data ring-buffer-data)
                   (end ring-buffer-end)
                   (cap ring-buffer-cap)) buf
    (prog1
      (setf (cffi:mem-aref data type (logand end (1- cap))) obj)
      (incf end))))

(defun ring-buffer-pop (buf type)
  "Remove and return a single item from the ring buffer."
  (assert (not (ring-buffer-empty? buf)))
  (with-accessors ((data ring-buffer-data)
                   (start ring-buffer-start)
                   (cap ring-buffer-cap)) buf
    (prog1
      (cffi:mem-aref data type (logand start (1- cap)))
      (incf start))))

(defun ring-buffer-put-sarray (buf sarray start end)
  "Put the contents of a simple octet array into BUF."
  (declare (type ring-buffer buf)
           (type octet-sarray sarray)
           (type (integer 0) start)
           (type (or null (integer 0)) end))
  (let* ((count (- end start))
         (data (ring-buffer-data buf))
         (cap (ring-buffer-cap buf))
         (buf-end (logand (ring-buffer-end buf) (1- cap))))
    (cffi:with-pointer-to-vector-data (carray sarray)
      (if (<= (+ buf-end count) cap)
          (ffi:memcpy (cffi:inc-pointer data buf-end) carray count)
          (let ((size (- cap buf-end)))
            (ffi:memcpy (cffi:inc-pointer data buf-end) carray size)
            (ffi:memcpy data (cffi:inc-pointer carray size) (- count size))))
      (shiftout-ring-buffer buf count)))
  (values))

(defun ring-buffer-pull-sarray (buf sarray start end)
  "Pull the contents of BUF into the simple octet array."
  (declare (type ring-buffer buf)
           (type octet-sarray sarray)
           (type (integer 0) start)
           (type (or null (integer 0)) end))
  (let* ((data (ring-buffer-data buf))
         (cap (ring-buffer-cap buf))
         (buf-start (logand (ring-buffer-start buf) (1- cap)))
         (count (- end start)))
    (cffi-sys:with-pointer-to-vector-data (carray sarray)
      (if (<= (+ buf-start count) cap)
          (ffi:memcpy (cffi:inc-pointer carray start)
                      (cffi:inc-pointer data buf-start)
                      count)
          (let ((size (- cap buf-start)))
            (ffi:memcpy (cffi:inc-pointer carray start)
                        (cffi:inc-pointer data buf-start)
                        size)
            (ffi:memcpy (cffi:inc-pointer carray (+ start size))
                        data (- count size))))
      (shiftin-ring-buffer buf count))))

(defun make-gather-iovec (iov buf)
  "Fill up to 2 elements of an unsized iovec array with filled buffer data, intended for a write operation.
Return the number of elements filled."
  (let* ((cap (ring-buffer-cap buf))
         (start (logand (ring-buffer-start buf) (1- cap)))
         (end (logand (ring-buffer-end buf) (1- cap)))
         (data (ring-buffer-data buf)))
    (cond
      ((< start end)
       ;; 1 vector from start to end
       (setf (ffi:iovec.iov-base iov) (cffi:mem-aptr data :uint8 start)
             (ffi:iovec.iov-len iov) (- end start))
       1)
      ((zerop end)
       ;; 1 vector from start to cap
       (setf (ffi:iovec.iov-base iov) (cffi:mem-aptr data :uint8 start)
             (ffi:iovec.iov-len iov) (- cap start))
       1)
      (t
       ;; 2 vectors from start to cap, then 0 to end
       (setf (ffi:iovec.iov-base iov) (cffi:mem-aptr data :uint8 start)
             (ffi:iovec.iov-len iov) (- cap start)
             (ffi:iovec.iov-base (autowrap:c-aref iov 1 'ffi:iovec)) data
             (ffi:iovec.iov-len (autowrap:c-aref iov 1 'ffi:iovec)) end)
       2))))

;; Iovec operations are solely for octet ring buffers.

(defun make-scatter-iovec (iov buf)
  "Fill up to 2 elements of an unsized iovec array with unset buffered data, intended for a read operation.
Return the number of elements filled."
  (let* ((cap (ring-buffer-cap buf))
         (start (logand (ring-buffer-start buf) (1- cap)))
         (end (logand (ring-buffer-end buf) (1- cap)))
         (data (ring-buffer-data buf)))
    (cond
      ((< end start)
       ;; 1 vector from end to start
       (setf (ffi:iovec.iov-base iov) (cffi:mem-aptr data :uint8 end)
             (ffi:iovec.iov-len iov) (- start end))
       1)
      ((zerop start)
       ;; 1 vector from end to cap
       (setf (ffi:iovec.iov-base iov) (cffi:mem-aptr data :uint8 end)
             (ffi:iovec.iov-len iov) (- cap end))
       1)
      (t
       ;; 2 vectors from end to cap, then 0 to start
       (setf (ffi:iovec.iov-base iov) (cffi:mem-aptr data :uint8 end)
             (ffi:iovec.iov-len iov) (- cap end)
             (ffi:iovec.iov-base (autowrap:c-aref iov 1 'ffi:iovec)) data
             (ffi:iovec.iov-len (autowrap:c-aref iov 1 'ffi:iovec)) start)
       2))))



;; Class, Constructor & FD Extensions

(defclass local-socket-stream (gray:fundamental-binary-input-stream
                               gray:fundamental-binary-output-stream)
  ((fd :initarg :fd :type (or (integer 0) null))
   (input-iobuf :initform (allocate-ring-buffer :uint8 +iobuf-size+)
                :type (or ring-buffer null))
   (output-iobuf :initform (allocate-ring-buffer :uint8 +iobuf-size+)
                 :type (or ring-buffer null))
   (input-fdbuf :initform (allocate-ring-buffer :int +fdbuf-size+)
                :type (or ring-buffer null))
   (output-fdbuf :initform (allocate-ring-buffer :int +fdbuf-size+)
                 :type (or ring-buffer null))
   (dirty? :initform nil :type boolean :reader %dirty?))
  (:documentation
    "A binary local socket stream that can send file descriptors."))

(defclass passive-local-socket-stream (local-socket-stream) ())
(defclass active-local-socket-stream (local-socket-stream) ())

(defun make-socket ()
  "Create a endpoint for local stream communication."
  (let ((sockfd (ffi:socket ffi:+af-unix+ ffi:+sock-stream+ 0)))
    (if (minusp sockfd)
        (error 'socket-error :errno ffi:*errno*)
        (make-instance 'passive-local-socket-stream :fd sockfd))))

;; TODO transform hardcoded magic number to CFFI expression
(defconstant +sun-path-length+ 108)
(defun fill-sockaddr-un (sun string)
  (declare (type string string))
  (assert (< (length string) (1- +sun-path-length+)))
  (ffi:bzero sun (autowrap:sizeof '(:struct (ffi:sockaddr-un))))
  (setf (ffi:sockaddr-un.sun-family sun) ffi:+af-unix+)
  (cffi:with-foreign-string (c-string string :null-terminated-p t)
    (ffi:memcpy
      (ffi:sockaddr-un.sun-path[]& sun)
      c-string
      (1- +sun-path-length+))))

(defun connect (socket address-string)
  "Initiate a connection on a local socket."
  (check-type address-string string)
  (autowrap:with-alloc (sun '(:struct (ffi:sockaddr-un)))
    (fill-sockaddr-un sun address-string)
    (when (minusp (ffi:connect (slot-value socket 'fd) sun
                               (autowrap:sizeof '(:struct (ffi:sockaddr-un)))))
      (error 'socket-stream-error :errno ffi:*errno* :stream socket)))

  (change-class socket 'active-local-socket-stream))

;; TODO bind
;; TODO accept



;; IO Helper Writers

(defun %flush-obufs (socket nonblocking?)
  "Flush the output buffers to the socket.
Return two values: the number of octets written, and whether the call would have been blocked if NONBLOCKING? was NIL."
  (with-slots (fd output-iobuf output-fdbuf) socket
    (declare (ignore output-fdbuf))
    (autowrap:with-calloc (msghdr '(:struct (ffi:msghdr)))
      (autowrap:with-alloc (iov 'ffi:iovec 2)
        (setf (ffi:msghdr.msg-iov msghdr) (autowrap:ptr iov)
              (ffi:msghdr.msg-iovlen msghdr) (make-gather-iovec iov output-iobuf))
        ;; TODO fill in cmsg with fdbufs here
        (let ((nread (ffi:sendmsg fd msghdr (if nonblocking? ffi:+msg-dontwait+ 0))))
          (when (minusp nread)
            (let ((errno ffi:*errno*))
              (if (or (= errno ffi:+ewouldblock+)
                      (= errno ffi:+eagain+))
                  (return-from %flush-obufs (values 0 t))
                  (error 'socket-stream-error :errno ffi:*errno* :stream socket))))
          (shiftin-ring-buffer output-iobuf nread)
          (values nread nil))))))

(defun %flush-obufs-if-needed (socket)
  (with-slots (output-iobuf dirty?) socket
    (unless (or dirty? (ring-buffer-full? output-iobuf))
      (%flush-obufs socket nil)
      (setf dirty? nil))))

(defun %write-sarray (socket sarray start end)
  (declare (type local-socket-stream socket)
           (type octet-sarray sarray)
           (type fixnum start)
           (type (or fixnum null) end))
  (with-slots (output-iobuf) socket
    (unless (ring-buffer-can-fit? output-iobuf sarray start end)
      (%flush-obufs socket nil))
    (ring-buffer-put-sarray output-iobuf sarray start end)))

(defun %write-vector (socket vector start end)
  (%write-sarray socket (coerce vector 'octet-sarray) start end))



;; IO Helper Readers

(defun %read-once (socket nonblocking?)
  "Read data from the socket into the buffer.
Return two values: the number of octets read, and whether the call would have been blocked in NONBLOCKING? was NIL."
  (with-slots (fd input-iobuf input-fdbuf) socket
    (declare (ignore input-fdbuf))
    (autowrap:with-calloc (msghdr '(:struct (ffi:msghdr)))
      (autowrap:with-alloc (iov 'ffi:iovec 2)
        (setf (ffi:msghdr.msg-iov msghdr) (autowrap:ptr iov)
              (ffi:msghdr.msg-iovlen msghdr) (make-scatter-iovec iov input-iobuf))
        ;; TODO fill in cmsg to read fd's here

        (let ((nread (ffi:recvmsg fd msghdr (if nonblocking? ffi:+msg-dontwait+ 0))))
          (when (minusp nread)
            (let ((errno ffi:*errno*))
              (if (or (= errno ffi:+ewouldblock+)
                      (= errno ffi:+eagain+))
                  (return-from %read-once (values 0 t))
                  (error 'socket-stream-error :errno ffi:*errno* :stream socket))))
          (shiftout-ring-buffer input-iobuf nread)
          (values nread nil))))))

(defun %read-into-sarray (socket sarray start end)
  (declare (type local-socket-stream socket)
           (type octet-sarray sarray)
           (type (integer 0) start)
           (type (or null (integer 0)) end))
  (loop :with iobuf := (slot-value socket 'input-iobuf)
        :with octets-left := (- end start)
        :with offset := start
        :for size := (min octets-left (ring-buffer-length iobuf))
        :when (plusp size)
        :do (ring-buffer-pull-sarray iobuf sarray offset (+ offset size))
            (incf offset size)
            (decf octets-left size)
        :when (or (zerop octets-left)
                  (zerop (%read-once socket nil)))
        :do (loop-finish)
        :finally (return offset)))

(defun %read-into-vector (socket vector start end)
  (declare (type local-socket-stream socket)
           (type vector vector)
           (type (integer 0) start)
           (type (or null (integer 0)) end))
  (loop :for i :from start :below end
        :for octet := (gray:stream-read-byte socket)
        :until (eq octet :eof)
        :do (setf (aref vector i) octet)
        :finally (return i)))



;; Gray Stream Methods

(defmethod gray:stream-finish-output ((stream local-socket-stream))
  "Send all buffered output to the socket and wait until it's written."
  (%flush-obufs stream nil)
  (with-slots (dirty?) stream
    (setf dirty? nil))
  nil)

(defmethod gray:stream-force-output ((stream local-socket-stream))
  "Send some buffered output to the socket, but don't wait."
  (%flush-obufs stream t)
  (with-slots (output-iobuf dirty?) stream
    (unless (ring-buffer-empty? output-iobuf)
      (setf dirty? t)))
  nil)

(defmethod gray:stream-clear-output ((stream local-socket-stream))
  "Clear all outstanding output data."
  (with-slots (output-iobuf dirty?) stream
    (clear-ring-buffer output-iobuf)
    (setf dirty? nil)))

(defmethod gray:stream-clear-input ((stream local-socket-stream))
  "Clear all buffered input."
  (with-slots (input-iobuf) stream
    (clear-ring-buffer input-iobuf)))

(defmethod close ((socket local-socket-stream) &key abort)
  (when (and (%dirty? socket) (not abort))
    (finish-output socket))
  (with-slots (fd input-iobuf output-iobuf input-fdbuf output-fdbuf) socket
    (when input-iobuf (free-ring-buffer input-iobuf))
    (when output-iobuf (free-ring-buffer output-iobuf))
    (when input-fdbuf (free-ring-buffer input-fdbuf))
    (when output-fdbuf (free-ring-buffer output-fdbuf))
    (setf input-iobuf nil output-iobuf nil
          input-fdbuf nil output-fdbuf nil)

    (when (minusp (ffi:close fd))
      (error 'socket-stream-error :errno ffi:*errno* :stream socket))
    (setf fd nil))

  (change-class socket 'passive-local-socket-stream)
  t)

(defmethod stream-element-type ((stream local-socket-stream))
  '(unsigned-byte 8))

(defmethod gray:stream-write-byte ((stream local-socket-stream) byte)
  (check-type byte octet)
  (%flush-obufs-if-needed stream)
  (with-slots (output-iobuf) stream
    (ring-buffer-push output-iobuf :uint8 byte)))

(defun flush-with-sequence (socket sequence start end)
  (declare (type local-socket-stream socket)
           (type octet-sarray sequence)
           (type fixnum start)
           (type (or fixnum null) end))
  (autowrap:with-many-alloc ((iov 'ffi:iovec 3)
                             (msghdr '(:struct (ffi:msghdr))))
    (cffi:with-foreign-array (c-arr sequence '(:array :uint8 (length sequence)))
      (with-slots (output-iobuf fd) socket
        (let ((iobuf-nvecs (make-gather-iovec iov output-iobuf)))
          (setf (ffi:iovec.iov-base (autowrap:c-aref iov iobuf-nvecs 'ffi:iovec))
                (cffi:mem-aptr c-arr :uint8 start)
                (ffi:iovec.iov-len (autowrap:c-aref iov iobuf-nvecs 'ffi:iovec))
                (- (or end (length sequence)) start)
                (ffi:msghdr.msg-iovlen msghdr) (1+ iobuf-nvecs)))
        (when (minusp (ffi:sendmsg fd msghdr 0))
          (error 'socket-stream-error :errno ffi:*errno* :stream socket))
        (clear-ring-buffer output-iobuf)))))

(defmethod gray:stream-write-sequence ((stream local-socket-stream) sequence start end &key)
  (etypecase sequence
    ;; Fast-io uses simple octet arrays, so let's specialize on that, and
    ;; fallback to generic vectors as a workaround.
    ;; This socket is binary-only, so no strings or characters here.
    (octet-sarray (%write-sarray stream sequence start end))
    (vector (%write-vector stream sequence start end))))

(defmethod gray:stream-read-byte ((stream local-socket-stream))
  (with-slots (input-iobuf) stream
    (when (ring-buffer-empty? input-iobuf)
      (let ((nread (%read-once stream nil)))
        (when (zerop nread)
              (return-from sb-gray:stream-read-byte :eof))))

    (ring-buffer-pop input-iobuf :uint8)))

(defmethod gray:stream-read-sequence ((stream local-socket-stream) sequence start end &key)
  (etypecase sequence
    (octet-sarray (%read-into-sarray stream sequence start end))
    (vector (%read-into-vector stream sequence start end))))

(defmethod gray:stream-listen ((stream local-socket-stream))
  (with-slots (input-iobuf) stream
    (or (ring-buffer-empty? input-iobuf)
        (plusp (%read-once stream t)))))

;; TODO write-fd
;; TODO read-fd
