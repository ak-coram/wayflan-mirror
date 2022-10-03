;;; sockets.lisp - local sockets for Wayflan
;;;
;; Copyright (c) 2022 Samuel Hunter <samuel@shunter.xyz>
;;; All rights reserved.

(in-package #:xyz.shunter.wayflan.sockets)
(declaim (optimize debug))



(deftype octet ()
  '(unsigned-byte 8))

(deftype simple-octet-sarray (&optional (size '*))
  `(simple-array octet (,size)))

(deftype c-sized-int ()
  `(signed-byte #.(* 8 (cffi:foreign-type-size :int))))

;; Some older versions of libwayland reserved a small 128-byte buffer to
;; receive cmsg data. This can hold at most 28 fds, and as there's no way to
;; check for limits, and no man page documenting how to recover from a
;; MSG_CTRUNC, we need to flush once we reach this limit.
;;
;; See https://gitlab.freedesktop.org/wayland/wayland commit 73d4a536
(defconstant +max-fds-out+ 28
             "Maximum # of file descriptors buffered before each flush")
(defconstant +buf-size+ 4096
             "Size, in octets, of all circular buffers")

;; The circular buffer size must be a power of 2, such that LOGAND can be used
;; to mod values by the circular buffer length.
(assert (zerop (logand +buf-size+ (1- +buf-size+))))

;; The circular buffer size must be able to fit the necessary fd's
(assert (>= +buf-size+
            (the fixnum (* +max-fds-out+ (the fixnum (cffi:foreign-type-size :int))))))



;; Circular Buffers

;; These circular buffers store a pointer to a foreign array (to be either
;; shipped out via ffi:sendmsg or filled in via ffi:recvmsg), always of size
;; +buf-size+, and has heads and tails stored as offsets to the pointer.

(defstruct (circular-buffer (:conc-name #:cb-)
                            (:print-function nil)
                            (:predicate nil)
                            (:copier nil))
  (ptr (cffi:foreign-alloc :uint8 :count +buf-size+)
       :type cffi:foreign-pointer)
  ;; START and END may be incremented beyond +BUF-SIZE+.
  ;; The real offset is CB-START* and CB-END*.
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(declaim (inline cb-start* cb-end* cb-length cb-free-space
                 cb-clear))
(defun cb-start* (cb) (logand (cb-start cb) (1- +buf-size+)))
(defun cb-end* (cb) (logand (cb-end cb) (1- +buf-size+)))
(defun cb-length (cb)
  (declare (type circular-buffer cb))
  (the fixnum (- (cb-end cb) (cb-start cb))))
(defun cb-free-space (cb)
  (declare (type circular-buffer cb))
  (the fixnum (+ +buf-size+ (cb-start cb) (- (cb-end cb)))))

(defun cb-clear (cb)
  (setf (cb-start cb) 0
        (cb-end cb) 0)
  (values))

(defun free-circular-buffer (cb)
  (declare (type circular-buffer cb))
  (cffi:foreign-free (cb-ptr cb))
  (setf (cb-ptr cb) (cffi:null-pointer)
        (cb-start cb) 0
        (cb-end cb) 0)
  (values))

(defun cb-shiftout (cb nbytes)
  "Mark that the given number of bytes were consumed."
  (declare (type circular-buffer cb)
           (type fixnum nbytes))
  (with-accessors ((start cb-start)
                   (end cb-end)) cb
    (assert (<= (+ start nbytes) end))
    (incf start nbytes)
    (when (= start end)
      (setf start 0 end 0))
    (values)))

(defun cb-shiftin (cb nbytes)
  "Mark that the given nubmer of bytes were written to the buffer."
  (declare (type circular-buffer cb)
           (type fixnum nbytes))
  (with-accessors ((end cb-end)
                   (length cb-length)) cb
    (assert (<= (+ length nbytes) +buf-size+))
    (incf end nbytes)
    (values)))

;; TODO refactor duplicate code in cb-push-octets, cb-push-foreign-objects
(defun cb-push-octets (cb sarray start end)
  "Push octets from the array to the circular buffer."
  (declare (type circular-buffer cb)
           (type simple-octet-sarray sarray)
           (type fixnum start end))
  (when (zerop (length sarray))
    (return-from cb-push-octets))
  (let ((n (- end start)))
    (assert (plusp n))
    (assert (>= (cb-free-space cb) n))
    (cffi:with-pointer-to-vector-data (carray sarray)
      (if (<= (+ (cb-start* cb) n) +buf-size+)
          (ffi:memcpy (cffi:inc-pointer (cb-ptr cb) (cb-end* cb))
                      (cffi:inc-pointer carray start) n)
          (let ((first-seg (- +buf-size+ (cb-start* cb))))
            (ffi:memcpy (cffi:inc-pointer (cb-ptr cb) (cb-end* cb))
                        (cffi:inc-pointer carray start)
                        first-seg)
            (ffi:memcpy (cb-ptr cb)
                        (cffi:inc-pointer carray (+ start first-seg))
                        (- n first-seg)))))
    (cb-shiftin cb n)))

(defun cb-push-foreign-octets (cb carray n)
  (declare (type circular-buffer cb)
           (type cffi:foreign-pointer carray)
           (type fixnum size))
  (unless (plusp n)
    (return-from cb-push-foreign-octets))
  (assert (>= (cb-free-space cb) n))
  (if (<= (+ (cb-start* cb) n) +buf-size+)
      (ffi:memcpy (cffi:inc-pointer (cb-ptr cb) (cb-end* cb))
                  carray n)
      (let ((first-seg (- +buf-size+ (cb-start* cb))))
        (ffi:memcpy (cffi:inc-pointer (cb-ptr cb) (cb-end* cb))
                    carray
                    first-seg)
        (ffi:memcpy (cb-ptr cb)
                    (cffi:inc-pointer carray first-seg)
                    (- n first-seg))))
  (cb-shiftin cb n))

(defun cb-pull-octets (cb sarray start end)
  "Pull octets from the circular buffer to the array."
  (declare (type circular-buffer cb)
           (type simple-octet-sarray sarray)
           (type fixnum start end))
  (let ((n (- end start)))
    (assert (plusp n))
    (assert (>= (cb-length cb) n))
    (cffi:with-pointer-to-vector-data (carray sarray)
      (if (<= (+ (cb-start* cb) n) +buf-size+)
          (ffi:memcpy carray (cffi:inc-pointer (cb-ptr cb) (cb-start* cb))
                      n)
          (let ((first-seg (- +buf-size+ (cb-start* cb))))
            (ffi:memcpy carray (cffi:inc-pointer (cb-ptr cb) (cb-start* cb))
                        first-seg)
            (ffi:memcpy (cffi:inc-pointer carray first-seg)
                        (cb-ptr cb)
                        (- n first-seg)))))
    (cb-shiftout cb n)))

(defun cb-push-int (cb int)
  (declare (type circular-buffer cb)
           (type c-sized-int int))
  (setf (cffi:mem-ref (cb-ptr cb) :int (cb-end* cb)) int)
  (cb-shiftin cb (cffi:foreign-type-size :int)))

(defun cb-pull-int (cb)
  (assert (>= (cb-length cb) (cffi:foreign-type-size :int)))
  (prog1
    (cffi:mem-ref (cb-ptr cb) :int (cb-start* cb))
    (cb-shiftout cb (cffi:foreign-type-size :int))))

(defun cb-push-octet (cb octet)
  (declare (type circular-buffer cb)
           (type octet octet))
  (setf (cffi:mem-ref (cb-ptr cb) :uint8 (cb-end* cb)) octet)
  (cb-shiftin cb 1))

(defun cb-pull-octet (cb)
  (assert (plusp (cb-length cb)))
  (prog1
    (cffi:mem-ref (cb-ptr cb) :int (cb-start* cb))
    (cb-shiftout cb 1)))

(defun cb-copy (cb dest-ptr)
  "Copy the entire contents of the circular buffer into dest-ptr"
  (let ((start* (cb-start* cb))
        (length (cb-length cb)))
    (if (<= (+ start* length)
            +buf-size+)
        (ffi:memcpy dest-ptr (cffi:inc-pointer (cb-ptr cb) start*)
                    length)
        (let ((first-seg (- +buf-size+ start*)))
          (ffi:memcpy dest-ptr (cffi:inc-pointer (cb-ptr cb) start*)
                      first-seg)
          (ffi:memcpy (cffi:inc-pointer dest-ptr first-seg)
                      (cb-ptr cb) (- length first-seg)))))
  (values))

(defun cb-prepare-gather-iovec (cb iov)
  "Given a pointer to at least two iovecs, point them to filled spaces
in the circular buffer and return the number of iovecs used."
  (let ((start (cb-start cb))
        (end (cb-end cb))
        (start* (cb-start* cb))
        (end* (cb-end* cb)))
    (cond
      ((= start end)
       0)
      ((and (>= start* end*)
            (plusp start))
       (cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                                 iov (:struct ffi:iovec))
         (setf ffi:iov-base (cffi:inc-pointer (cb-ptr cb) start*)
               ffi:iov-len (- +buf-size+ start*)))
       (cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                                 (cffi:mem-aptr iov '(:struct ffi:iovec) 1)
                                 (:struct ffi:iovec))
         (setf ffi:iov-base (cb-ptr cb)
               ffi:iov-len end*))
       2)
      ((cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                                 iov (:struct ffi:iovec))
         (setf ffi:iov-base (cffi:inc-pointer (cb-ptr cb) start*)
               ffi:iov-len (- (if (plusp end*) end* +buf-size+) start*)))
       1))))

(defun cb-prepare-scatter-iovec (cb iov)
  "Given a pointer to at least two iovecs, point them to empty spaces
in the circular buffer and return the number of iovecs used."
  (let ((start (cb-start cb))
        (end (cb-end cb))
        (start* (cb-start* cb))
        (end* (cb-end* cb)))
    (cond
      ((and (< start end)
            (= start* end*))
       0)
      ((or (> start* end*)
           (zerop start))
       (cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                                 iov (:struct ffi:iovec))
         (setf ffi:iov-base (cffi:inc-pointer (cb-ptr cb) end*)
               ffi:iov-len (- (if (plusp start*) start* +buf-size+) end*)))
       1)
      ((cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                                 iov (:struct ffi:iovec))
         (setf ffi:iov-base (cffi:inc-pointer (cb-ptr cb) end*)
               ffi:iov-len (- +buf-size+ end*)))
       (cffi:with-foreign-slots ((ffi:iov-base ffi:iov-len)
                                 (cffi:mem-aptr iov '(:struct ffi:iovec) 1)
                                 (:struct ffi:iovec))
         (setf ffi:iov-base (cb-ptr cb)
               ffi:iov-len start*))
       2))))



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

(defun cmsg-firsthdr (msghdr)
  (cffi:with-foreign-slots ((ffi:msg-controllen ffi:msg-control)
                            msghdr (:struct ffi:msghdr))
    (when (plusp ffi:msg-controllen)
      ffi:msg-control)))

(defun cmsg-nxthdr (msghdr cmsghdr)
  (cffi:with-foreign-slots ((ffi:msg-control ffi:msg-controllen)
                            msghdr (:struct ffi:msghdr))
    (let ((ptr (cffi:inc-pointer
                 cmsghdr
                 (cmsg-align (cffi:foreign-slot-value
                               cmsghdr '(:struct ffi:cmsghdr)
                               'ffi:cmsg-len)))))
      (unless (> (+ (cffi:pointer-address ptr) 1
                    (- (cffi:pointer-address ffi:msg-control)))
                 ffi:msg-controllen)
        ptr))))

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
   (input-iobuf :initform (make-circular-buffer)
                :type (or circular-buffer null))
   (output-iobuf :initform (make-circular-buffer)
                 :type (or circular-buffer null))
   (input-fdbuf :initform (make-circular-buffer)
                :type (or circular-buffer null))
   (output-fdbuf :initform (make-circular-buffer)
                 :type (or circular-buffer null))
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
         (length (cb-length output-fdbuf)))
    (if (plusp length)
        (progn
          (cffi:with-foreign-slots ((ffi:cmsg-level ffi:cmsg-type ffi:cmsg-len)
                                    cmsghdr (:struct ffi:cmsghdr))
            (setf ffi:cmsg-level ffi:+sol-socket+
                  ffi:cmsg-type ffi:+scm-rights+
                  ffi:cmsg-len (+ length (cffi:foreign-type-size '(:struct ffi:cmsghdr)))))
          (cb-copy output-fdbuf cmsgdata)
          (cmsg-len length))
        0)))

(defun %flush-obufs (socket nonblocking?)
  "Flush the output buffers to the socket.
Return two values: the number of octets written, and whether the call would have been blocked if NONBLOCKING? was NIL."
  (declare (optimize debug))
  (with-slots (fd output-iobuf output-fdbuf) socket
    (when (zerop (cb-length output-iobuf))
      ;; FIXME: If there's no iovec data, sendmsg() essentially no-ops.
      ;; The same issue is found in iolib. This shouldn't be a problem for
      ;; Wayland since every fd sent is part of a greater message, but if this
      ;; is moved to a general-purpose library, this surprising behavior
      ;; *needs* to be addressed.
      (when (plusp (cb-length output-fdbuf))
        (error "Flushing buffer without I/O data."))
      (return-from %flush-obufs (values 0 nil)))

    (cffi:with-foreign-objects ((msghdr '(:struct ffi:msghdr))
                                (iov '(:struct ffi:iovec) 2)
                                (cmsg :uint8 +cspace+))
      (ffi:bzero msghdr (cffi:foreign-type-size '(:struct ffi:msghdr)))
      (cffi:with-foreign-slots ((ffi:msg-iov ffi:msg-iovlen ffi:msg-control
                                             ffi:msg-controllen)
                                msghdr (:struct ffi:msghdr))
        (setf ffi:msg-iov iov
              ffi:msg-iovlen (cb-prepare-gather-iovec output-iobuf iov)
              ffi:msg-control cmsg
              ffi:msg-controllen (initialize-gather-cmsg cmsg output-fdbuf)))

      (let ((nread (ffi:sendmsg fd msghdr (if nonblocking? '(:dontwait) ()))))
        (when (minusp nread)
          (let ((errno ffi:*errno*))
            (if (or (eq errno :ewouldblock)
                    (eq errno :eagain))
                (return-from %flush-obufs (values 0 t))
                (error 'socket-stream-error :errno ffi:*errno* :stream socket))))
        (cb-clear output-fdbuf)
        (cb-shiftout output-iobuf nread)
        (values nread nil)))))

(defun %flush-obufs-if-needed (socket)
  (with-slots (output-iobuf dirty?) socket
    (unless (or dirty? (= (cb-length output-iobuf)
                          +buf-size+))
      (%flush-obufs socket nil)
      (setf dirty? nil))))

(defun %write-sarray (socket sarray start end)
  (declare (type local-socket-stream socket)
           (type simple-octet-sarray sarray)
           (type fixnum start)
           (type (or fixnum null) end))
  (setf end (or end (length sarray)))
  (with-slots (output-iobuf) socket
    (unless (>= (cb-free-space output-iobuf)
                (- end start))
      (%flush-obufs socket nil))
    (cb-push-octets output-iobuf sarray start end)))

(defun %write-vector (socket vector start end)
  (%write-sarray socket (coerce vector 'simple-octet-sarray) start end))



;; IO Helper Readers

(defun %read-once (socket nonblocking?)
  "Read data from the socket into the buffer.
Return two values: the number of octets read, and whether the call would have been blocked in NONBLOCKING? was NIL."
  (with-slots (fd input-iobuf input-fdbuf) socket
    (cffi:with-foreign-objects ((msghdr '(:struct ffi:msghdr))
                                (iov '(:struct ffi:iovec) 2)
                                (cmsg :uint8 +cspace+))
      (cffi:with-foreign-slots ((ffi:msg-name ffi:msg-namelen
                                 ffi:msg-iov ffi:msg-iovlen
                                 ffi:msg-control ffi:msg-controllen
                                 ffi:msg-flags)
                                msghdr (:struct ffi:msghdr))
        (setf ffi:msg-name (cffi:null-pointer)
              ffi:msg-namelen 0
              ffi:msg-iov iov
              ffi:msg-iovlen (cb-prepare-scatter-iovec input-iobuf iov)
              ffi:msg-control cmsg
              ffi:msg-controllen +cspace+
              ffi:msg-flags 0))

      (let ((nread (ffi:recvmsg fd msghdr (if nonblocking? '(:dontwait) ()))))
        (when (minusp nread)
          (let ((errno ffi:*errno*))
            (if (or (eq errno :ewouldblock)
                    (eq errno :eagain))
                (return-from %read-once (values 0 t))
                (error 'socket-stream-error :errno ffi:*errno* :stream socket))))
        (cb-shiftin input-iobuf nread)

        ;; Read ctl messages
        (do ((cmsghdr (cmsg-firsthdr msghdr)
                      (cmsg-nxthdr msghdr cmsghdr))
             overflow?
             size max)
            ((null cmsghdr)
             (when overflow?
               (error 'socket-stream-error :errno :eoverflow :stream socket)))
          (cffi:with-foreign-slots ((ffi:cmsg-len ffi:cmsg-level ffi:cmsg-type)
                                    cmsghdr (:struct ffi:cmsghdr))
            (when (and (= ffi:cmsg-level ffi:+sol-socket+)
                       (= ffi:cmsg-type ffi:+scm-rights+))
              (setf size (- ffi:cmsg-len (cmsg-len 0))
                    max (cb-free-space input-fdbuf))
              (if (or (> size max) overflow?)
                  (progn
                    (setf overflow? t)
                    (dotimes (i (floor size (load-time-value (cffi:foreign-type-size :int))))
                      (ffi:close (cffi:mem-aref (cmsg-data cmsghdr) :int i))))
                  (cb-push-foreign-octets
                    input-fdbuf (cmsg-data cmsghdr) size)))))
        (values nread nil)))))

(defun %read-into-sarray (socket sarray start end)
  (declare (type local-socket-stream socket)
           (type simple-octet-sarray sarray)
           (type (integer 0) start)
           (type (or null (integer 0)) end))
  (setf end (or end (length sarray)))
  (loop :with iobuf := (slot-value socket 'input-iobuf)
        :with octets-left := (- end start)
        :with offset := start
        :for size := (min octets-left (cb-length iobuf))
        :when (plusp size)
        :do (cb-pull-octets iobuf sarray offset (+ offset size))
            (incf offset size)
            (decf octets-left size)
        :until (or (zerop octets-left)
                   (zerop (%read-once socket nil)))
        :finally (return offset)))



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
    (when (plusp (cb-length output-iobuf))
      (setf dirty? t)))
  nil)

(defmethod gray:stream-clear-output ((stream local-socket-stream))
  "Clear all outstanding output operations, including byte and fd I/O."
  (with-slots (output-iobuf output-fdbuf dirty?) stream
    (cb-clear output-iobuf)
    (cb-clear output-fdbuf)
    (setf dirty? nil)))

(defmethod gray:stream-clear-input ((stream local-socket-stream))
  "Clear all buffered I/O and file descriptor input."
  (with-slots (input-iobuf input-fdbuf) stream
    (loop :while (plusp (cb-length input-fdbuf))
          :for fd := (cb-pull-int input-fdbuf)
          :do (ffi:close fd))
    (cb-clear input-iobuf)))

(defmethod close ((socket local-socket-stream) &key abort)
  (when (and (%dirty? socket) (not abort))
    (finish-output socket))
  (with-slots (fd input-iobuf output-iobuf input-fdbuf output-fdbuf) socket
    (gray:stream-clear-input socket)
    (gray:stream-clear-output socket)

    (when input-iobuf (free-circular-buffer input-iobuf))
    (when output-iobuf (free-circular-buffer output-iobuf))
    (when input-fdbuf (free-circular-buffer input-fdbuf))
    (when output-fdbuf (free-circular-buffer output-fdbuf))
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
    (cb-push-octet output-iobuf byte)))

(defmethod gray:stream-write-sequence ((stream local-socket-stream) sequence start end &key)
  (etypecase sequence
    ;; Fast-io uses simple octet arrays, so let's specialize on that, and
    ;; fallback to generic vectors as a workaround.
    ;; This socket is binary-only, so no strings or characters here.
    (simple-octet-sarray (%write-sarray stream sequence start end))
    (vector (%write-vector stream sequence start end))))

(defmethod gray:stream-read-byte ((stream local-socket-stream))
  (with-slots (input-iobuf) stream
    (unless (plusp (cb-length input-iobuf))
      (let ((nread (%read-once stream nil)))
        (when (zerop nread)
          (return-from gray:stream-read-byte :eof))))
    (cb-pull-octet input-iobuf)))

(defmethod gray:stream-read-sequence ((stream local-socket-stream) sequence start end &key)
  (check-type sequence simple-octet-sarray)
  (%read-into-sarray stream sequence start end))

(defmethod gray:stream-listen ((stream local-socket-stream))
  (with-slots (input-iobuf) stream
    (or (plusp (cb-length input-iobuf))
        (plusp (%read-once stream t)))))

(defun write-fd (socket fd)
  "Write a file descriptor to the socket stream.
Like binary I/O, fd's may be buffered and affected by FINISH-OUTPUT, FORCE-OUTPUT, and CLEAR-OUTPUT."
  (check-type socket local-socket-stream)
  (check-type fd (integer 0))
  (with-slots (output-fdbuf) socket
    (when (= (cb-length output-fdbuf)
             (* +max-fds-out+ (cffi:foreign-type-size :int)))
      (%flush-obufs socket nil))
    (cb-push-int output-fdbuf fd)))

(defun read-fd (socket)
  "Pop a buffered file descriptor or return NIL.
Like binary I/O, buffered fd's may be affected by CLEAR-INPUT."
  (check-type socket local-socket-stream)
  (with-slots (input-fdbuf) socket
    (when (plusp (cb-length input-fdbuf))
      (cb-pull-int input-fdbuf))))
