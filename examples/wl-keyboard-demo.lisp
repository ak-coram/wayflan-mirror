;;; examples/wl-keyboard-demo.lisp
;;;
;;; This wl-keyboard demo uses cl-xkb to process keycodes and modifiers
;;; into the appropriate keysym. Type keys on the screen, view the WASD
;;; key state, and explore the code in wl-keyboard.
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.examples.wl-keyboard-demo
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.wl-keyboard-demo)



(defclass app ()
  (;; Globals
   (wl-display :initarg :display)
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base
   wl-seat

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel
   (wl-keyboard :initform nil)

   ;; State
   (width :type wl-int)
   (height :type wl-int)
   (xkb-context :initform (cffi:null-pointer))
   (xkb-keymap :initform (cffi:null-pointer))
   (xkb-state :initform (cffi:null-pointer))
   (messages :initform (make-ring-buffer))
   (press-map :initform (make-array 4 :element-type 'bit)
              :documentation "Simple bit-vector to hold WASD key states")))

;; For use with app slot press-map
(defconstant +w+ 0)
(defconstant +a+ 1)
(defconstant +s+ 2)
(defconstant +d+ 3)

;; Ring buffer rotates out a constant number of messages, to show on the
;; screen.
(defconstant +rb-size+ 10)
(defstruct (ring-buffer (:conc-name #:rb-))
  (messages (make-array +rb-size+ :initial-element ""))
  (start 0))

(defun rb-push (message rb)
  (with-accessors ((messages rb-messages)
                   (start rb-start)) rb
    (setf (aref messages start) message
          start (mod (1+ start) +rb-size+)))
  rb)

(defun rb-to-string (rb)
  (with-accessors ((messages rb-messages)
                   (start rb-start)) rb
    (with-output-to-string (out)
      (dotimes (i +rb-size+)
        (princ (aref messages (mod (+ i start) +rb-size+)) out)
        (terpri out)))))

(defun draw-key (letter left top pressed?)
  (cairo:set-source-color
    (if pressed?
        (cl-colors:rgb .7 .2 0)
        (cl-colors:rgb 1 .25 0)))
  (let ((width 80)
        (height 80)
        (text-size 30))
    (cairo:rectangle left top width height)
    (cairo:fill-path)

    (cairo:set-source-rgb 0 0 0)
    (cairo:set-line-width 5)
    (cairo:rectangle left top width height)
    (cairo:stroke)

    (cairo:move-to left (+ top (/ (- height text-size) 2)))
    (pango:print-with-attributes
      (letter :alignment :pango_align_center
              :width width)
      `((:absolute-size ,text-size)
        (:family "Monospace")))))

(defun draw-frame (app)
  (with-slots (wl-shm width height press-map messages) app
    (let* ((stride (* width 4))
           (size (* stride height))
           buffer)
      (shm:with-open-shm-and-mmap* (shm pool-data (:direction :io) (size))
        (with-proxy (pool (wl-shm.create-pool wl-shm (shm:shm-fd shm) size))
          (setf buffer (wl-shm-pool.create-buffer
                         pool 0 width height stride :xrgb8888)))

        ;; Draw the surface
        (cairo:with-surface-and-context (surf (cairo:create-image-surface-for-data
                                                pool-data :rgb24 width height stride))
          ;; Draw a white background
          (cairo:set-source-rgb 1 1 1)
          (cairo:paint)

          (draw-key "W" (- width 300) 100 (plusp (sbit press-map +w+)))
          (draw-key "A" (- width 400) 200 (plusp (sbit press-map +a+)))
          (draw-key "S" (- width 300) 200 (plusp (sbit press-map +s+)))
          (draw-key "D" (- width 200) 200 (plusp (sbit press-map +d+)))

          (cairo:move-to 0 0)
          (pango:print-with-attributes
            ((format nil "Keyboard Events:~%~A" (rb-to-string messages))
             :width width)
            `((:absolute-size ,(min 30 (floor height 15)))
              (:family "Monospace")))))

      (push (evelambda
              (:release ()
               (destroy-proxy buffer)))
            (wl-proxy-hooks buffer))
      buffer)))

(defun update-press-map (press-map keysym key-state)
  (a:switch ((code-char (xkb:tolower keysym)) :test #'char=)
    (#\w (setf (sbit press-map +w+) (if (eq key-state :pressed) 1 0)))
    (#\a (setf (sbit press-map +a+) (if (eq key-state :pressed) 1 0)))
    (#\s (setf (sbit press-map +s+) (if (eq key-state :pressed) 1 0)))
    (#\d (setf (sbit press-map +d+) (if (eq key-state :pressed) 1 0)))))

(defun handle-keyboard (app &rest event)
  (with-slots (xkb-context xkb-keymap xkb-state press-map messages wl-surface) app
    (event-case event
      ;; wl-keyboard's :KEYMAP event provides a key map as a file and size,
      ;; alongside the format used.
      (:keymap (format fd size)
       (let ((shm (shm:make-shm fd)))
         (unwind-protect
           (progn
             ;; wl_keyboard::keyboard_format exists in case a new
             ;; keyboard format comes along, but for now, :XKB-V1 should
             ;; be the only format compositors for any PC may send.
             (assert (eq format :xkb-v1))
             (shm:with-mmap (ptr shm size :flags '(:private))
               (let* ((keymap (xkb:xkb-keymap-new-from-string
                                xkb-context ptr :text-v1 ()))
                      (state (xkb:xkb-state-new keymap)))
                 (xkb:xkb-keymap-unref xkb-keymap)
                 (xkb:xkb-state-unref xkb-state)
                 (setf xkb-keymap keymap
                       xkb-state state))))
           (shm:close-shm shm))))

      ;; Update changed state of keyboard modifiers
      (:modifiers (serial depressed latched locked group)
       (declare (ignore serial))
       (xkb:xkb-state-update-mask
         xkb-state
         depressed latched locked
         0 0 group))

      ;; Notify keyboard focus entered the surface, and an array of keycodes
      ;; already entered.
      (:enter (serial surface keys)
       (declare (ignore serial surface))
       (rb-push "Keyboard entered" messages)
       (format t "Keyboard entered -- keys pressed are:~%")
       (dotimes (i (length keys))
         (let* ((keycode (+ 8 (aref keys i)))
                (sym (xkb:xkb-state-key-get-one-sym xkb-state keycode))
                (msg (format nil "DOWN ~12A (~D), utf8: ~A"
                             (xkb:xkb-keysym-get-name sym)
                             sym
                             (xkb:xkb-state-key-get-utf8 xkb-state keycode))))
           (update-press-map press-map sym :pressed)
           (rb-push msg messages)
           (princ msg) (terpri))))

      ;; Notify a key was pressed or released
      (:key (serial time-ms key state)
       (declare (ignore serial time-ms))
       (let* ((keycode (+ 8 key))
              (sym (xkb:xkb-state-key-get-one-sym xkb-state keycode))
              (msg (format nil "~4A ~12A (~D), utf8: ~A"
                           (if (eq state :pressed) "DOWN" "UP")
                           (xkb:xkb-keysym-get-name sym)
                           sym
                           (xkb:xkb-state-key-get-utf8 xkb-state keycode))))
         (update-press-map press-map sym state)
         (rb-push msg messages)
         (princ msg) (terpri)))

      ;; Notify leave of focus
      (:leave (serial surface)
       (declare (ignore serial surface))
       (rb-push "Keyboard leave" messages)
       (format t "Keyboard leave~%"))

      ;; Provides information on the user's preferred key repetition settings.
      ;; Implementation is left to the client.
      (:repeat-info (rate delay)
       (format t "Preferred repeat: ~D keys/sec after ~Dms~%"
               rate delay)))

    ;; Redraw the surface
    (let ((buffer (draw-frame app)))
      (wl-surface.attach wl-surface buffer 0 0)
      (wl-surface.damage
        wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
      (wl-surface.commit wl-surface))))

(defun handle-seat (app &rest event)
  (with-slots (wl-seat wl-keyboard) app
    (event-case event
      (:name (name)
       (format t "Seat name: ~S~%" name))
      (:capabilities (capabilities)
       (format t "Wl-seat capabilities: ~S~%" capabilities)
       (if (member :keyboard capabilities)
           (unless wl-keyboard
             (setf wl-keyboard (wl-seat.get-keyboard wl-seat))
             (push (a:curry 'handle-keyboard app)
                   (wl-proxy-hooks wl-keyboard)))
           (when wl-keyboard
             (destroy-proxy wl-keyboard)
             (setf wl-keyboard nil)))))))

(defun handle-registry (app registry &rest event)
  (with-slots (wl-shm wl-compositor wl-seat xdg-wm-base) app
    (event-case event
      (:global (name interface version)
       (declare (ignore version))
       (case (a:when-let ((it (find-interface-named interface)))
               (class-name it))
         (wl-shm
           (setf wl-shm (wl-registry.bind
                          registry name 'wl-shm 1)))
         (wl-compositor
           (setf wl-compositor (wl-registry.bind
                                 registry name 'wl-compositor 4)))
         (wl-seat
           (setf wl-seat (wl-registry.bind
                           registry name 'wl-seat 4))
           (push (a:curry 'handle-seat app)
                 (wl-proxy-hooks wl-seat)))
         (xdg-wm-base
           (setf xdg-wm-base (wl-registry.bind
                               registry name 'xdg-wm-base 1))
           (push (evelambda
                   (:ping (serial)
                    (xdg-wm-base.pong xdg-wm-base serial)))
                 (wl-proxy-hooks xdg-wm-base))))))))

(defun run ()
  (with-open-display (display)
    (let ((app (make-instance 'app :display display)))
      (with-slots (wl-shm wl-registry wl-compositor xdg-wm-base wl-seat
                          wl-surface xdg-surface xdg-toplevel
                          width height
                          xkb-context xkb-keymap xkb-state) app
        (unwind-protect
          (progn
            ;; Create xkb-context
            (setf xkb-context (xkb:xkb-context-new ()))

            ;; Get the registry and bind all globals
            (setf wl-registry (wl-display.get-registry display))
            (push (a:curry 'handle-registry app wl-registry)
                  (wl-proxy-hooks wl-registry))
            (wl-display-roundtrip display)

            ;; Make a surface and give it the toplevel desktop role.
            (setf wl-surface (wl-compositor.create-surface wl-compositor)
                  xdg-surface (xdg-wm-base.get-xdg-surface xdg-wm-base wl-surface)
                  xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
            (push (evelambda
                    (:configure (serial)
                     (xdg-surface.ack-configure xdg-surface serial)
                     (let ((buffer (draw-frame app)))
                       (wl-surface.attach wl-surface buffer 0 0)
                       (wl-surface.commit wl-surface))))
                  (wl-proxy-hooks xdg-surface))
            (push (evlambda
                    (:configure (new-width new-height state)
                     (declare (ignore state))
                     (setf width (if (zerop new-width) 800 new-width)
                           height (if (zerop new-height) 600 new-height)))
                    (:close ()
                     ;; Close the app
                     (return-from run)))
                  (wl-proxy-hooks xdg-toplevel))
            (xdg-toplevel.set-title xdg-toplevel "Wayflan wl-keyboard Demo")
            (wl-surface.commit wl-surface)

            ;; Keep handling events until we get a close signal
            (loop (wl-display-dispatch-event display)))

          (xkb:xkb-state-unref xkb-state)
          (xkb:xkb-keymap-unref xkb-keymap)
          (xkb:xkb-context-unref xkb-context))))))
