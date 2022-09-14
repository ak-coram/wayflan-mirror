;;; sockets.lisp - local sockets for Wayflan
;;;
;; Copyright (c) 2022 Samuel Hunter <samuel@shunter.xyz>
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.sockets)



(defmacro when-debug (&body body)
  (declare (ignore body)))



;; Ring Buffers

;; This magic number comes from libwayland. TODO Figure out why.
;; I have a suspicion that this has something to do with the fact
;; that CMSG_LEN(sizeof(int) * 28) is exactly 128 bytes, but without comments
;; within the source code, I'll have to ask one of the devs for confirmation.
(defconstant +max-fds-out+ 28)

;; Both sizes must be powers of 2, so that LOGAND can be used to mask the
;; length of the buffer.
(defconstant +iobuf-size+ (* 4 4096)
             "Number of octets to store in a ring buffer")
(defconstant +fdbuf-size+
             ;; Lowest power of 2 that fits +max-fds-out+
             (ash 1 (ceiling (log +max-fds-out+ 2)))
             "Number of file descriptors to store in a fd buffer")

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

(defun ring-buffer-pull-foreign-array (buf carray count)
  "Move the contents of the ring buffer into the foreign array."
  (declare (type ring-buffer buf)
           (type cffi:foreign-pointer carray)
           (type (integer 0) count))
  (let* ((data (ring-buffer-data buf))
         (cap (ring-buffer-cap buf))
         (buf-start (logand (ring-buffer-start buf) (1- cap))))
    (if (<= (+ buf-start count) cap)
        (ffi:memcpy carray (cffi:inc-pointer data buf-start) count)
        (let ((size (- cap buf-start)))
          (ffi:memcpy carray (cffi:inc-pointer data buf-start) size)
          (ffi:memcpy (cffi:inc-pointer carray size) data (- count size)))))
  (shiftin-ring-buffer buf count))

(defun ring-buffer-pull-sarray (buf sarray start end)
  "Pull the contents of the ring buffer into the simple octet array."
  (declare (type ring-buffer buf)
           (type octet-sarray sarray)
           (type (integer 0) start)
           (type (integer 0) end))
  (cffi-sys:with-pointer-to-vector-data (carray sarray)
    (ring-buffer-pull-foreign-array buf (cffi:inc-pointer carray start)
                                    (- end start))))

(defun initialize-gather-iovec (iov buf)
  "Fill up to 2 elements of an unsized iovec array with filled buffer data, intended for a write operation.
Return the number of elements filled."
  (let* ((cap (ring-buffer-cap buf))
         (start (logand (ring-buffer-start buf) (1- cap)))
         (end (logand (ring-buffer-end buf) (1- cap)))
         (data (ring-buffer-data buf)))
    (cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                              iov (:struct ffi:iovec))
      (cond
        ((<= start end)
         ;; 1 vector from start to end
         (setf ffi:iov-base (cffi:mem-aptr data :uint8 start)
               ffi:iov-len (- end start))
         1)
        ((zerop end)
         ;; 1 vector from start to cap
         (setf ffi:iov-base (cffi:mem-aptr data :uint8 start)
               ffi:iov-len (- cap start))
         1)
        (t
         ;; 2 vectors from start to cap, then 0 to end
         (setf ffi:iov-base (cffi:mem-aptr data :uint8 start)
               ffi:iov-len (- cap start)
               (cffi:foreign-slot-value
                 (cffi:mem-aptr iov '(:struct ffi:iovec) 1)
                 '(:struct ffi:iovec) 'ffi:iov-base) data
               (cffi:foreign-slot-value
                 (cffi:mem-aptr iov '(:struct ffi:iovec) 1)
                 '(:struct ffi:iovec) 'ffi:iov-len) end)
         2)))))

;; Iovec operations are solely for octet ring buffers.

(defun initialize-scatter-iovec (iov buf)
  "Fill up to 2 elements of an unsized iovec array with unset buffered data, intended for a read operation.
Return the number of elements filled."
  (let* ((cap (ring-buffer-cap buf))
         (start (logand (ring-buffer-start buf) (1- cap)))
         (end (logand (ring-buffer-end buf) (1- cap)))
         (data (ring-buffer-data buf)))
    (cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                              iov (:struct ffi:iovec))
      (cond
        ((< end start)
         ;; 1 vector from end to start
         (setf ffi:iov-base (cffi:mem-aptr data :uint8 end)
               ffi:iov-len (- start end))
         1)
        ((zerop start)
         ;; 1 vector from end to cap
         (setf ffi:iov-base (cffi:mem-aptr data :uint8 end)
               ffi:iov-len (- cap end))
         1)
        (t
         ;; 2 vectors from end to cap, then 0 to start
         (setf ffi:iov-base (cffi:mem-aptr data :uint8 end)
               ffi:iov-len (- cap end)
               (cffi:foreign-slot-value
                 (cffi:mem-aptr iov '(:struct ffi:iovec) 1)
                 '(:struct ffi:iovec) 'ffi:iov-base) data
               (cffi:foreign-slot-value
                 (cffi:mem-aptr iov '(:struct ffi:iovec) 1)
                 '(:struct ffi:iovec) 'ffi:iov-len) start)
         2)))))



;; CMSG helpers

;; TODO: these functions were handmade from
;; </usr/src/linux-headers-5.18.0-2-common/include/linux/socket.h>.
;; The cmsg(3) NOTES section states that "For portability, ancillary data
;; should be accessed using only the macros described here."
;;
;; I ought to find a way to wrap the macros in the future, somehow. I tried using
;; CFFI's wrapper files, however it erases the type (:pointer (:struct foobar)) to
;; a void*, such as in:
;;
;; (defwrapper "CMSG_FIRSTHDR" (:pointer (:struct cmsghdr))
;;   (msghdr (:pointer (:struct msghdr))))

(declaim (inline cmsg-align cmsg-firsthdr cmsg-nxthdr
                 cmsg-space cmsg-len cmsg-data))
(defun cmsg-align (length)
  (declare (type fixnum length))
  (logand (+ length (load-time-value (1- (cffi:foreign-type-size :long))))
          (load-time-value (lognot (1- (cffi:foreign-type-size :long))))))

(defun cmsg-firsthdr (msgh)
  (cffi:with-foreign-slots ((ffi:msg-controllen ffi:msg-control)
                            msgh (:struct ffi:msghdr))
    (when (plusp ffi:msg-controllen)
      ffi:msg-control)))

;; TODO cmsg-nxthdr for reading

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cmsg-space (length)
    (declare (type fixnum length))
    (+ (load-time-value (cffi:foreign-type-size '(:struct ffi:cmsghdr)))
       (cmsg-align length))))

(defun cmsg-len (length)
  (declare (type fixnum length))
  (+ (load-time-value (cffi:foreign-type-size '(:struct ffi:cmsghdr)))
     length))

(defun cmsg-data (cmsg)
  (cffi:inc-pointer
    cmsg
    (load-time-value (cffi:foreign-type-size '(:struct ffi:cmsghdr)))))

(defconstant +cspace+
  (cmsg-space (* +max-fds-out+ (cffi:foreign-type-size :int))))



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
  (let ((sockfd (ffi:socket ffi:+af-local+ ffi:+sock-stream+ 0)))
    (if (minusp sockfd)
        (error 'socket-error :errno ffi:*errno*)
        (make-instance 'passive-local-socket-stream :fd sockfd))))

(defconstant +sun-path-length+
             (* (cffi:foreign-slot-count '(:struct ffi:sockaddr-un) 'ffi:sun-path)
                (cffi:foreign-type-size
                  (cffi:foreign-slot-type '(:struct ffi:sockaddr-un)
                                          'ffi:sun-path))))
(defmacro sun-path& (ptr)
  `(cffi:inc-pointer ,ptr (cffi:foreign-slot-offset '(:struct ffi:sockaddr-un)
                                                    'ffi:sun-path)))

(defun fill-sockaddr-un (sun string)
  (declare (type string string))
  (assert (< (length string) (1- +sun-path-length+)))
  (ffi:bzero sun (cffi:foreign-type-size '(:struct ffi:sockaddr-un)))
  (cffi:with-foreign-slots ((ffi:sun-family ffi:sun-path)
                            sun (:struct ffi:sockaddr-un))
    (setf ffi:sun-family ffi:+af-local+)
    (cffi:with-foreign-string (c-string string :null-terminated-p t)
      (ffi:memcpy (sun-path& sun) c-string
                  (1- +sun-path-length+)))))

(defun connect (socket address-string)
  "Initiate a connection on a local socket."
  (check-type address-string string)
  (cffi:with-foreign-object (sun '(:struct ffi:sockaddr-un))
    (fill-sockaddr-un sun address-string)
    (when (minusp (ffi:connect (slot-value socket 'fd) sun
                               (cffi:foreign-type-size '(:struct ffi:sockaddr-un))))
      (error 'socket-stream-error :errno ffi:*errno* :stream socket)))

  (change-class socket 'active-local-socket-stream))

;; TODO bind
;; TODO accept



;; IO Helper Writers

(defun initialize-gather-cmsg (cmsg output-fdbuf)
  "Build the control message and return the real length."
  (let* ((cmsghdr cmsg)
         (cmsgdata (cmsg-data cmsg))
         (nfds (ring-buffer-length output-fdbuf))
         (length (* nfds (cffi:foreign-type-size :int))))
    (if (plusp nfds)
        (progn
          (cffi:with-foreign-slots ((ffi:cmsg-level ffi:cmsg-type ffi:cmsg-len)
                                    cmsghdr (:struct ffi:cmsghdr))
            (setf ffi:cmsg-level ffi:+sol-socket+
                  ffi:cmsg-type ffi:+scm-rights+
                  ffi:cmsg-len (+ length (cffi:foreign-type-size '(:struct ffi:cmsghdr)))))
          ;; TODO rig RING-BUFFER-PULL-x to handle fd-sized buffers
          (dotimes (i nfds)
            (setf (cffi:mem-aref cmsgdata :int i)
                  (ring-buffer-pop output-fdbuf :int)))
          (cmsg-len length))
        0)))

(defun %flush-obufs (socket nonblocking?)
  "Flush the output buffers to the socket.
Return two values: the number of octets written, and whether the call would have been blocked if NONBLOCKING? was NIL."
  (declare (optimize debug))
  (with-slots (fd output-iobuf output-fdbuf) socket
    (when (ring-buffer-empty? output-iobuf)
      ;; TODO FIXME: If there's no iovec data, sendmsg() essentially no-ops.
      ;; The same issue is found in iolib. This shouldn't be a problem for
      ;; Wayland since every fd sent is part of a greater message, but if this
      ;; is moved to a general-purpose library, this surprising behavior
      ;; *needs* to be addressed.
      (unless (ring-buffer-empty? output-fdbuf)
        (error "Flushing ring buffer without I/O data."))
      (return-from %flush-obufs (values 0 nil)))

    (cffi:with-foreign-objects ((msghdr '(:struct ffi:msghdr))
                                (iov '(:struct ffi:iovec) 2)
                                (cmsg :uint8 +cspace+))
      (ffi:bzero msghdr (cffi:foreign-type-size '(:struct ffi:msghdr)))
      (cffi:with-foreign-slots ((ffi:msg-iov ffi:msg-iovlen ffi:msg-control
                                             ffi:msg-controllen)
                                msghdr (:struct ffi:msghdr))
        (setf ffi:msg-iov iov
              ffi:msg-iovlen (initialize-gather-iovec iov output-iobuf)
              ffi:msg-control cmsg
              ffi:msg-controllen (initialize-gather-cmsg cmsg output-fdbuf)))

      (let ((nread (ffi:sendmsg fd msghdr (if nonblocking? '(:dontwait) ())))
            (errno ffi:*errno*))
        (when (minusp nread)
          (if (or (eq errno :ewouldblock)
                  (eq errno :eagain))
              (return-from %flush-obufs (values 0 t))
              (error 'socket-stream-error :errno ffi:*errno* :stream socket)))
        (shiftin-ring-buffer output-iobuf nread)
        (values nread nil)))))

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
    (cffi:with-foreign-objects ((msghdr '(:struct ffi:msghdr))
                                (iov '(:struct ffi:iovec) 2))
      (cffi:with-foreign-slots ((ffi:msg-iov ffi:msg-iovlen)
                                iov (:struct ffi:msghdr))
        (setf ffi:msg-iov iov
              ffi:msg-iovlen (initialize-scatter-iovec iov input-iobuf)))
      ;; TODO fill in cmsg to read fd's here

      (let ((nread (ffi:recvmsg fd msghdr (if nonblocking? '(:dontwait) ()))))
        (when (minusp nread)
          (let ((errno ffi:*errno*))
            (if (or (eq errno :ewouldblock)
                    (eq errno :eagain))
                (return-from %read-once (values 0 t))
                (error 'socket-stream-error :errno ffi:*errno* :stream socket))))
        (shiftout-ring-buffer input-iobuf nread)
        (values nread nil)))))

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
        :until (or (zerop octets-left)
                   (zerop (%read-once socket nil)))
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
  "Clear all outstanding output operations, including byte and fd I/O."
  (with-slots (output-iobuf output-fdbuf dirty?) stream
    (clear-ring-buffer output-iobuf)
    (clear-ring-buffer output-fdbuf)
    (setf dirty? nil)))

(defmethod gray:stream-clear-input ((stream local-socket-stream))
  "Clear all buffered I/O and file descriptor input."
  (with-slots (input-iobuf input-fdbuf) stream
    (loop :until (ring-buffer-empty? input-fdbuf)
          :for fd := (ring-buffer-pop input-fdbuf :int)
          :do (ffi:close fd))
    (clear-ring-buffer input-iobuf)))

(defmethod close ((socket local-socket-stream) &key abort)
  (when (and (%dirty? socket) (not abort))
    (finish-output socket))
  (with-slots (fd input-iobuf output-iobuf input-fdbuf output-fdbuf) socket
    (gray:stream-clear-input socket)
    (gray:stream-clear-output socket)

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
    (or (not (ring-buffer-empty? input-iobuf))
        (plusp (%read-once stream t)))))

(defun write-fd (socket fd)
  "Write a file descriptor to the socket stream.
Like binary I/O, fd's may be buffered and affected by FINISH-OUTPUT, FORCE-OUTPUT, and CLEAR-OUTPUT."
  (check-type socket local-socket-stream)
  (check-type fd (integer 0))
  (with-slots (output-fdbuf) socket
    (when (= (ring-buffer-length output-fdbuf) +max-fds-out+)
      (%flush-obufs socket nil))
    (ring-buffer-push output-fdbuf :int fd)))

(defun read-fd (socket)
  "Pop a buffered file descriptor or return NIL.
Like binary I/O, buffered fd's may be affected by CLEAR-INPUT."
  (check-type socket local-socket-stream)
  (with-slots (input-fdbuf) socket
    (unless (ring-buffer-empty? input-fdbuf)
      (ring-buffer-pop input-fdbuf :int))))
