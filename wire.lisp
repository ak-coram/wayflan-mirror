;;; wire.lisp - Wayland primitive data communication

(defpackage #:xyz.shunter.wayhack.wire
  (:use #:cl)
  (:local-nicknames (#:io #:fast-io))
  (:export #:wl-int
           #:wl-uint

           #:read-wl-int
           #:read-wl-uint
           #:read-wl-fixed
           #:read-wl-string
           #:read-wl-array

           #:write-wl-int
           #:write-wl-uint
           #:write-wl-fixed
           #:write-wl-string
           #:write-wl-array)
  (:documentation "Wayland primitive data communication.

Wayland is a protocol for a compositor to talk to its clients.
The compositor can be a standalone display server running on Linux kernel modesetting and evdev input devices, or an X application, or a Wayland client itself.
The clients can be traditional applications, X servers (rootless or fullscreen), or other display servers.

The wayland-wire package defines utilities for communicating primitive data through a fast-io buffer connected to a Wayland UNIX socket stream."))

(in-package #:xyz.shunter.wayhack.wire)



(defconstant +header-length+ 8
  "Used in conjunction with either the message size (in READ-MESSAGE) or body length (in WRITE-MESSAGE) to derive the other value.")

#-(or little-endian big-endian)
(error "No idea what endianness the host is; expecting the feature ~S or ~S."
       :little-endian :big-endian)

(deftype wl-int ()
  '(signed-byte 32))

(deftype wl-uint ()
  '(unsigned-byte 32))

(declaim (inline make-octet-vector padding))

(defun make-octet-vector (size)
  (make-array size :element-type 'io:octet))

(defun padding (length)
  (mod (- length) 4))



;; Reading

(declaim (inline read-wl-int read-wl-uint))

(defun read-wl-int (buffer)
  "Read a signed integer from a Wayland fast-io buffer."
  #+little-endian (io:read32-le buffer)
  #+big-endian (io:read32-be buffer))

(defun read-wl-uint (buffer)
  "Read a unsigned integer from a Wayland fast-io buffer."
  #+little-endian (io:readu32-le buffer)
  #+big-endian (io:readu32-be buffer))

(defun read-wl-fixed (buffer)
  "Read a signed 24.8 decimal number from a Wayland fast-io buffer."
  (let* ((fixed (read-wl-uint buffer))
         (integer-part (let ((unsigned (ash fixed -8)))
                         (if (>= unsigned #x800000)
                             (- unsigned #x1000000)
                             unsigned)))
         (decimal-part (logand fixed #xff)))
    (+ integer-part (/ decimal-part #x100))))

(defun read-wl-string (buffer)
  "Read a string from a Wayland fast-io buffer."
  ;; NOTE: This assumes strings are ASCII-encoded. I can't find anything in
  ;; Wayland saying what the character encoding actually is -- unless wayland
  ;; wants everyone to assume it's a "string" of octets?
  (let* ((length (read-wl-uint buffer))
         (padding (padding length))
         (octets (make-octet-vector (+ length padding)))
         (octets-trimmed
           (make-array (1- length) ;; -1 for null byte
                       :element-type 'io:octet
                       :displaced-to octets)))
    (assert (= (+ length padding)
               (io:fast-read-sequence octets buffer)))
    (map 'string #'code-char octets-trimmed)))

(defun read-wl-array (buffer)
  "Read a array from a Wayland fast-io buffer."
  (let* ((length (read-wl-uint buffer))
         (padding (padding length))
         (octets (make-octet-vector (+ length padding)))
         (octets-trimmed
           (make-array length
                       :element-type 'io:octet
                       :displaced-to octets)))
    (assert (= (+ length padding)
               (io:fast-read-sequence octets buffer)))
    octets-trimmed))

(defun read-wl-message (buffer)
  "Reads three values from a Wayland fast-io buffer: the sender's object ID, the opcode, and the message body as an octet vector."
  ;; The message header contains two 32-bit words:
  ;; - The sender's Object ID,
  ;; - Two 16-bit numbers:
  ;;   - The message size in bytes, including the header
  ;;     (upper 16 bits),
  ;;   - The request/event opcode (lower 16 bits).
  ;;
  ;; The order of the message size is host-endianness-dependent, so read
  ;; the whole 32-bit uint and bit-shift the original numbers out.
  (let* ((object-id (read-wl-uint buffer))
         (length-and-opcode (read-wl-uint buffer))
         (message-length (ash length-and-opcode -16))
         (opcode (logand length-and-opcode (1- (ash 1 16))))
         (body-length (- message-length +header-length+))
         (body (make-octet-vector body-length)))
    (assert (= (io:fast-read-sequence body buffer)
               body-length))
    (values
      object-id
      opcode
      body)))



;; Writing

(declaim (inline write-wl-int write-wl-uint))

(defun write-wl-int (n buffer)
  "Write a signed integer to a Wayland fast-io buffer."
  #+little-endian (io:write32-le n buffer)
  #+big-endian (io:write32-ge n buffer))

(defun write-wl-uint (n buffer)
  "Write a unsigned integer to a Wayland fast-io buffer."
  #+little-endian (io:write32-le n buffer)
  #+big-endian (io:write32-ge n buffer))

(defun write-wl-fixed (n buffer)
  "Write a signed 24.8 decimal number to a Wayland fast-io buffer."
  (multiple-value-bind (integer-part decimal-part) (floor n 1)
    (write-wl-uint (logior (ash integer-part 8)
                           (mod (floor decimal-part #x1/100) #x100))
                   buffer)))

(defun write-wl-string (string buffer)
  "Write a string to a Wayland fast-io buffer."
  ;; NOTE: This assumes strings are ASCII-encoded. I can't find anything in
  ;; Wayland saying what the character encoding actually is -- unless wayland
  ;; wants everyone to assume it's a "string" of octets?
  (let* ((octets (map '(vector io:octet) #'char-code string))
         (length (1+ (length octets)))) ;; +1 for null byte
    ;; length and characters
    (write-wl-uint length buffer)
    (io:fast-write-sequence octets buffer)
    ;; null terminator and pad to 4 bytes
    (dotimes (i (1+ (padding length)))
      (io:writeu8 0 buffer))))

(defun write-wl-array (octet-vector buffer)
  "Write an octet vector to a Wayland fast-io buffer."
  (let* ((length (length octet-vector)))
    ;; length and contents
    (write-wl-uint length buffer)
    (io:fast-write-sequence octet-vector buffer)
    ;; pad to 4 bytes
    (dotimes (i (padding length))
      (io:writeu8 0 buffer)))
  (values))

(defun write-wl-message (object-id opcode body buffer)
  "Write a message including an octet vector body to a Wayland fast-io buffer."
  (check-type object-id (unsigned-byte 32))
  (check-type opcode (unsigned-byte 16))
  (let* ((message-length (+ (length body) +header-length+))
         (length-and-opcode (logior (ash message-length 16)
                                    opcode)))
    (write-wl-uint object-id buffer)
    (write-wl-uint length-and-opcode buffer)
    (assert (= (io:fast-write-sequence body buffer)
               (length body)))))
