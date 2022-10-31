;;; src/wire.lisp -- Wayland socket and wl-arrayire format marshalling
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.wire)



#-(or little-endian big-endian)
(error "No idea what endianness the host is; expecting the feature ~S or ~S."
       :little-endian :big-endian)

(defconstant +wl-word-size+ (cffi:foreign-type-size :uint32))

;; Used in conjunction with either the message size or body length to derive
;; the other value.
(defconstant +header-length+ (* 2 +wl-word-size+))

(deftype c-sized-int ()
  '(signed-byte #.(* 8 (cffi:foreign-type-size :int))))

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
            (* +max-fds-out+ (cffi:foreign-type-size :int))))

(declaim (inline padded-size))
(defun padded-size (size)
  (* 4 (ceiling size 4)))



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

(defun cb-pull-foreign-octets (cb carray offset size)
  (declare (type circular-buffer cb)
           (type fixnum offset size))
  (if (<= (+ (cb-start* cb) size) +buf-size+)
      (ffi:memcpy carray (cffi:inc-pointer (cb-ptr cb) (cb-start* cb)) size)
      (let ((first-seg (- +buf-size+ (cb-start* cb))))
        (ffi:memcpy (cffi:inc-pointer carray offset)
                    (cffi:inc-pointer (cb-ptr cb) (cb-start* cb))
                    first-seg)
        (ffi:memcpy (cffi:inc-pointer carray (+ offset first-seg))
                    (cb-ptr cb) (- size first-seg))))
  (cb-shiftout cb size))

(defun cb-push-number (cb number ctype)
  (declare (type circular-buffer cb))
  (assert (>= (cb-free-space cb) (cffi:foreign-type-size ctype)))
  (setf (cffi:mem-ref (cb-ptr cb) ctype (cb-end* cb)) number)
  (cb-shiftin cb (cffi:foreign-type-size ctype)))

(defun cb-pull-number (cb ctype)
  (declare (type circular-buffer cb))
  (assert (>= (cb-length cb) (cffi:foreign-type-size ctype)))
  (prog1
    (cffi:mem-ref (cb-ptr cb) ctype (cb-start* cb))
    (cb-shiftout cb (cffi:foreign-type-size ctype))))

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



(defclass %socket () ())

(defclass data-socket (%socket)
  ((%fd :initarg :fd :type (or c-sized-int null))
   (%input-iobuf :initform (make-circular-buffer)
                :type (or circular-buffer null))
   (%output-iobuf :initform (make-circular-buffer)
                 :type (or circular-buffer null))
   (%input-fdbuf :initform (make-circular-buffer)
                :type (or circular-buffer null))
   (%output-fdbuf :initform (make-circular-buffer)
                 :type (or circular-buffer null)))
  (:documentation "A binary binary local socket connected to a Wayland compositor"))

(defun make-socket ()
  (let ((sockfd (ffi:socket ffi:+af-local+ ffi:+sock-stream+ 0)))
    (when (minusp sockfd)
      (error "Can't create socket: ~A" (ffi:strerror ffi:*errno*)))
    (make-instance 'data-socket :fd sockfd)))

(defconstant +sun-path-length+
             (* (cffi:foreign-slot-count
                  '(:struct ffi:sockaddr-un) 'ffi:sun-path)
                (cffi:foreign-type-size
                  (cffi:foreign-slot-type
                    '(:struct ffi:sockaddr-un) 'ffi:sun-path))))

(defun connect (socket address-string)
  (cffi:with-foreign-object (sun '(:struct ffi:sockaddr-un))
    (ffi:bzero sun (cffi:foreign-type-size '(:struct ffi:sockaddr-un)))
    (setf (cffi:foreign-slot-value
            sun '(:struct ffi:sockaddr-un) 'ffi:sun-family)
          ffi:+af-local+)
    (cffi:lisp-string-to-foreign
      address-string sun +sun-path-length+
      :offset (cffi:foreign-slot-offset '(:struct ffi:sockaddr-un) 'ffi:sun-path))

    (when (minusp (ffi:connect (slot-value socket '%fd) sun
                               (cffi:foreign-type-size '(:struct ffi:sockaddr-un))))
      (error "Can't connect socket: ~A" (ffi:strerror ffi:*errno*)))))

(defun %clear-socket-input (socket)
  (with-slots (%input-iobuf %input-fdbuf) socket
    (loop :while (plusp (cb-length %input-fdbuf))
          :do (ffi:close (cb-pull-number %input-fdbuf :int)))
    (cb-clear %input-iobuf)))

(defgeneric close-socket (socket)
  (:method ((socket data-socket))
   (with-slots (%fd %input-iobuf %input-fdbuf %output-iobuf %output-fdbuf) socket
     (%clear-socket-input socket)
     (when %input-iobuf (free-circular-buffer %input-iobuf))
     (when %input-fdbuf (free-circular-buffer %input-fdbuf))
     (when %output-iobuf (free-circular-buffer %output-iobuf))
     (when %output-fdbuf (free-circular-buffer %output-fdbuf))

     (when (minusp (ffi:close %fd))
       (error "Cannot close stream: ~A" (ffi:strerror ffi:*errno*)))
     (setf %fd nil)
     t)))

(defgeneric listen-socket (socket)
  (:method ((socket data-socket))
   (with-slots (%input-iobuf) socket
     (or (plusp (cb-length %input-iobuf))
         (plusp (%read-once socket t))))))

(defun %prepare-gather-cmsg (cmsg output-fdbuf)
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


(defun %flush-output (socket)
  (with-slots (%fd %output-iobuf %output-fdbuf) socket
    ;; KLUDGE If there's no iovec data, sendmsg() essentially no-ops. The
    ;; same issue is found in iolib. This shouldn't be a problem for Wayland
    ;; since every fd sent is part of a greater message, fortunately.
    (when (zerop (cb-length %output-iobuf))
      (assert (zerop (cb-length %output-fdbuf)))
      (return-from %flush-output))

    (cffi:with-foreign-objects ((msgh '(:struct ffi:msghdr))
                                (iov '(:struct ffi:iovec) 2)
                                (cmsgh :uint8 +cspace+))
      (ffi:bzero msgh (cffi:foreign-type-size '(:struct ffi:msghdr)))
      (cffi:with-foreign-slots ((ffi:msg-iov ffi:msg-iovlen
                                             ffi:msg-control ffi:msg-controllen)
                                msgh (:struct ffi:msghdr))
        (setf ffi:msg-iov iov
              ffi:msg-iovlen (cb-prepare-gather-iovec %output-iobuf iov)
              ffi:msg-control cmsgh
              ffi:msg-controllen (%prepare-gather-cmsg cmsgh %output-fdbuf)))

      (let ((nread (ffi:sendmsg %fd msgh ())))
        (when (minusp nread)
          (let ((errno ffi:*errno*))
            (if (or (eq errno :ewouldblock)
                    (eq errno :eagain))
                (return-from %flush-output)
                (error "Can't write to stream: ~A" (ffi:strerror errno)))))

        (cb-clear %output-fdbuf)
        (cb-shiftout %output-iobuf nread)))))

(defgeneric %write-fd (socket fd)
  (:method ((socket data-socket) fd)
   (with-slots (%output-fdbuf) socket
     (when (= (cb-length %output-fdbuf)
              (* +max-fds-out+ (cffi:foreign-type-size :int)))
       (%flush-output socket))
     (cb-push-number %output-fdbuf fd :int))))

(defgeneric read-fd (socket)
  (:method ((socket data-socket))
   (with-slots (%input-fdbuf) socket
     (when (plusp (cb-length %input-fdbuf))
       (cb-pull-number %input-fdbuf :int)))))

(defgeneric %write-message (socket carray size)
  (:method ((socket data-socket) carray size)
   (with-slots (%output-iobuf) socket
     (unless (>= (cb-free-space %output-iobuf) size)
       (%flush-output socket))
     ;; TODO slice and send out messages that are larger than 4096 bytes
     (assert (>= (cb-free-space %output-iobuf) size))
     (cb-push-foreign-octets %output-iobuf carray size))
   (%flush-output socket)))

;; Read data from the socket into thebuffer.
(defun %read-once (socket nonblocking?)
  (with-slots (%fd %input-iobuf %input-fdbuf) socket
    (cffi:with-foreign-objects ((msgh '(:struct ffi:msghdr))
                                (iov '(:struct ffi:iovec) 2)
                                (cmsg :uint8 +cspace+))
      (cffi:with-foreign-slots ((ffi:msg-name ffi:msg-namelen
                                 ffi:msg-iov ffi:msg-iovlen
                                 ffi:msg-control ffi:msg-controllen
                                 ffi:msg-flags)
                                msgh (:struct ffi:msghdr))
        (setf ffi:msg-name (cffi:null-pointer)
              ffi:msg-namelen 0
              ffi:msg-iov iov
              ffi:msg-iovlen (cb-prepare-scatter-iovec %input-iobuf iov)
              ffi:msg-control cmsg
              ffi:msg-controllen +cspace+
              ffi:msg-flags 0))

      (let ((nread (ffi:recvmsg %fd msgh (when nonblocking? '(:dontwait)))))
        (when (minusp nread)
          (let ((errno ffi:*errno*))
            (if (or (eq errno :ewouldblock)
                    (eq errno :eagain))
                (return-from %read-once 0)
                (error "Can't read from socket: ~A"
                       (ffi:strerror ffi:*errno*)))))
        (cb-shiftin %input-iobuf nread)

        ;; Read ctl messages
        (do ((cmsgh (cmsg-firsthdr msgh)
                    (cmsg-nxthdr msgh cmsgh))
             overflow?
             size
             max)
            ((null cmsgh)
             (when overflow?
               (error "Overflow: too many unread file descriptors")))
            (cffi:with-foreign-slots ((ffi:cmsg-len ffi:cmsg-level ffi:cmsg-type)
                                      cmsgh (:struct ffi:cmsghdr))
              (when (and (= ffi:cmsg-level ffi:+sol-socket+)
                         (= ffi:cmsg-type ffi:+scm-rights+))
                (setf size (- ffi:cmsg-len (cmsg-len 0))
                      max (cb-free-space %input-fdbuf))
                (if (or (> size max) overflow?)
                    (progn
                      (setf overflow? t)
                      (dotimes (i (floor size (cffi:foreign-type-size :int)))
                        (ffi:close (cffi:mem-aref (cmsg-data cmsgh) :int i))))
                    (cb-push-foreign-octets
                      %input-fdbuf (cmsg-data cmsgh) size)))))
        nread))))

(defgeneric %call-with-message (socket function)
  (:method ((socket data-socket) function)
   (with-slots (%input-iobuf) socket
     ;; Fetch until the header is available
     (loop :while (< (cb-length %input-iobuf) +header-length+)
           :do (%read-once socket nil))

     ;; Decode the header
     (let* ((sender-id (cb-pull-number %input-iobuf :uint32))
            (length-and-opcode (cb-pull-number %input-iobuf :uint32))
            (msg-length (ldb (byte 16 16) length-and-opcode))
            (opcode (ldb (byte 16 0) length-and-opcode)))
       (cffi:with-foreign-pointer (buffer (- msg-length +header-length+) body-size)
         ;; Continuously fetch input and load it into the body buffer until
         ;; it's full.
         (loop :with octets-left := (- msg-length +header-length+)
               :with offset := 0
               :for size := (min octets-left (cb-length %input-iobuf))
               :when (plusp size)
                 :do (cb-pull-foreign-octets %input-iobuf buffer offset size)
                     (incf offset size)
                     (decf octets-left size)
               :until (or (zerop octets-left)
                          (zerop (%read-once socket nil)))
               :finally (assert (zerop octets-left)))
         (funcall function sender-id opcode buffer body-size))))))



;; Serialization

(declaim (inline %write-wl-uint %write-wl-int %write-wl-string
                 %write-wl-array %write-wl-string))

(defmacro %wl-primitive-size (type object)
  (ecase type
    (:fd 0)
    ((:int :uint :fixed) +wl-word-size+)
    (:array (with-gensyms (size)
              `(let ((,size (length ,object)))
                 (+ +wl-word-size+ (padded-size ,size)))))
    (:string (with-gensyms (size)
               `(let ((,size (1+ (length ,object))))
                  (+ +wl-word-size+ (padded-size ,size)))))))

;; With respect to all objects' types, return the size required to transmit a
;; message containing the header and all included arguments. Compound argument
;; types like :new-id must be broken down to their primitive components.
(defmacro %wl-message-size (types &rest objects)
  (assert (= (length types) (length objects)))
  (let ((object-syms (loop :repeat (length objects)
                           :collect (gensym))))
    `(let ,(mapcar #'list object-syms objects)
       (declare (ignorable ,@object-syms))
       (+ +header-length+
          ,@(mapcar (lambda (type obj) `(%wl-primitive-size ,type ,obj))
                      types object-syms)))))

;; Write an unsigned 32-bit integer to a foreign array
(defun %write-wl-uint (n cptr offset)
  (setf (cffi:mem-ref cptr :uint32 offset) n))

;; Write a signed 32-bit integer to a foreign array
(defun %write-wl-int (n cptr offset)
  (setf (cffi:mem-ref cptr :int32 offset) n))

;; Write a signed 24.8 decimal number to a foreign array
(defun %write-wl-fixed (n cptr offset)
  (multiple-value-bind (integer-part decimal-part) (floor n)
    (setf (cffi:mem-ref cptr :uint32 offset)
          (dpb integer-part (byte 24 8) (floor decimal-part #x1/100)))))

;; Write an octet vector to a foreign array
(defun %write-wl-array (octet-vector cptr offset)
  (let ((length (length octet-vector)))
    (cffi:with-pointer-to-vector-data (carray octet-vector)
      (setf (cffi:mem-ref cptr :uint32 offset) length)
      (ffi:memcpy (cffi:inc-pointer cptr (+ offset 4)) carray length))))

;; Write a null-terminated UTF-8 encoded string
(defun %write-wl-string (string cptr offset)
  (cffi:with-foreign-string ((cstr size) string :encoding :utf-8)
    (setf (cffi:mem-ref cptr :uint32 offset) size)
    (ffi:memcpy (cffi:inc-pointer cptr (+ offset 4)) cstr size)))

(defmacro send-wl-message ((socket sender-id opcode) types &rest objects)
  (with-gensyms (sock offset cptr size)
    (let ((object-syms (loop :repeat (length objects)
                             :collect (gensym))))
      `(let ((,sock ,socket)
             ,@(when objects
                `((,offset 8)))
             ,@(mapcar #'list object-syms objects))
         (declare (type wl-uint ,offset))
         (cffi:with-foreign-pointer (,cptr (%wl-message-size ,types ,@object-syms) ,size)
           (%write-wl-uint ,sender-id ,cptr 0)
           (%write-wl-uint (dpb ,size (byte 16 16) ,opcode) ,cptr 4)
           ,@(mapcar (lambda (obj type)
                       `(progn
                          (check-type ,obj
                                      ,(ecase type
                                         (:uint 'wl-uint)
                                         (:int 'wl-int)
                                         (:fixed 'wl-fixed)
                                         (:array 'wl-array)
                                         (:string 'string)
                                         (:fd 'c-sized-int)))
                          ,(ecase type
                                 (:uint `(%write-wl-uint ,obj ,cptr ,offset))
                                 (:int `(%write-wl-int ,obj ,cptr ,offset))
                                 (:fixed `(%write-wl-fixed ,obj ,cptr ,offset))
                                 (:array `(%write-wl-array ,obj ,cptr ,offset))
                                 (:string `(%write-wl-string ,obj ,cptr ,offset))
                                 (:fd `(%write-fd ,sock ,obj)))
                          (incf ,offset (%wl-primitive-size ,type ,obj))))
                     object-syms types)
           (%write-message ,sock ,cptr ,size))))))



;; Deserialization

(declaim (inline make-buf buf-ptr buf-offset buf-size incf-buf))
(defun make-buf (ptr size) (list* ptr 0 size))
(defun buf-ptr (buffer) (car buffer))
(defun buf-offset (buffer) (cadr buffer))
(defun buf-size (buffer) (cddr buffer))
(defun incf-buf (buffer delta)
  (incf (cadr buffer) delta))

(defun check-size (buffer size)
  (assert (>= (- (buf-size buffer) (buf-offset buffer)) size)))

(defun read-wl-int (buffer)
  (prog2
    (check-size buffer +wl-word-size+)
    (cffi:mem-ref (buf-ptr buffer) :int32 (buf-offset buffer))
    (incf-buf buffer +wl-word-size+)))

(defun read-wl-uint (buffer)
  (prog2
    (check-size buffer +wl-word-size+)
    (cffi:mem-ref (buf-ptr buffer) :uint32 (buf-offset buffer))
    (incf-buf buffer +wl-word-size+)))

(defun read-wl-fixed (buffer)
  (let* ((fixed (read-wl-uint buffer))
         (integer-part (let ((unsigned (ldb (byte 24 8) fixed)))
                         (if (>= unsigned #x800000)
                             (- unsigned #x1000000)
                             unsigned)))
         (decimal-part (ldb (byte 8 0) fixed)))
    (+ integer-part (/ decimal-part #x100))))

(defun read-wl-string (buffer)
  (let* ((nul-length (read-wl-uint buffer))
         (body-size (padded-size nul-length)))
    (prog2
      (check-size buffer body-size)
      (cffi:foreign-string-to-lisp
        (buf-ptr buffer)
        :offset (buf-offset buffer)
        :max-chars (1- nul-length)
        :encoding :utf-8)
      (incf-buf buffer body-size))))

(defun read-wl-array (buffer)
  (let* ((length (read-wl-uint buffer))
         (body-size (padded-size length)))
    (prog2
      (check-size buffer body-size)
      (cffi:foreign-array-to-lisp
        (cffi:inc-pointer (buf-ptr buffer) (buf-offset buffer))
        `(:array :uint8 ,length)
        :element-type 'xyz.shunter.wayflan::octet)
      (incf-buf buffer body-size))))

(defmacro with-incoming-message ((socket sender-id opcode buf) &body body)
  (with-gensyms (carray size)
    `(%call-with-message
       ,socket
       (lambda (,sender-id ,opcode ,carray ,size)
         (let ((,buf (make-buf ,carray ,size)))
           ,@body
           (assert (= (buf-offset ,buf) (buf-size ,buf))))))))
