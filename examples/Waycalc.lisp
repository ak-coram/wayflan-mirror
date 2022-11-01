;;; examples/Waycalc.lisp - Example calculator application
;;;
;;; Waycalc demonstrates a semi-practical application using
;;; pointer and keyboard input.
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.examples.waycalc
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.waycalc)



;; #K reader macro to read in keysym names into integers, e.g.
;; #K"Return" => 65293
#.(set-dispatch-macro-character
    #\# #\K (lambda (s c n)
              (declare (ignore c n))
              (xkb:xkb-keysym-from-name
                (read s) '(:no-flags))))

(defparameter +bg-color+ (cl-colors:hsv 30 .5 1))
(defparameter +fg-color+ (cl-colors:hsv 0 0 .2))
(defparameter +active-fg-color+ (cl-colors:hsv 0 0 .9))
(defparameter +border-color+ (cl-colors:hsv 30 1 .8))

(defparameter +margin+ 10)
(defparameter +border-width+ 5)
(defparameter +cells-x+ 4)
(defparameter +cells-y+ (1- 5)) ;; 1- for the display

(defclass waycalc-app ()
  (;; Globals
   wl-registry
   wl-compositor
   wl-shm
   wl-seat
   xdg-wm-base

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel
   (wl-pointer :initform nil)
   (wl-keyboard :initform nil)

   ;; State
   (state :initform (list 0))
   (buttons :initform (make-array
                        (list +cells-x+ +cells-y+)
                        :initial-contents `((("7" . ,(a:curry 'push-digit 7))
                                             ("8" . ,(a:curry 'push-digit 8))
                                             ("9" . ,(a:curry 'push-digit 9))
                                             ("+" . ,(a:curry 'push-op #'+)))
                                            (("4" . ,(a:curry 'push-digit 4))
                                             ("5" . ,(a:curry 'push-digit 5))
                                             ("6" . ,(a:curry 'push-digit 6))
                                             ("-" . ,(a:curry 'push-op #'-)))
                                            (("1" . ,(a:curry 'push-digit 1))
                                             ("2" . ,(a:curry 'push-digit 2))
                                             ("3" . ,(a:curry 'push-digit 3))
                                             ("*" . ,(a:curry 'push-op #'*)))
                                            (("0" . ,(a:curry 'push-digit 0))
                                             ("C" . ,#'clear-calc)
                                             ("=" . ,#'eval-calc)
                                             ("/" . ,(a:curry 'push-op #'floor))))))
   (width :type fixnum)
   (height :type fixnum)

   (pointer-x :initform nil)
   (pointer-y :initform nil)
   (hover-cell :initform nil)
   (hover-active? :initform nil)

   (xkb-context :initform (xkb:xkb-context-new ()))
   (xkb-keymap :initform (cffi:null-pointer))
   (xkb-state :initform (cffi:null-pointer))))

(defun cleanup-app (app)
  (with-slots (xkb-context xkb-keymap xkb-state) app
    (xkb:xkb-context-unref xkb-context)
    (xkb:xkb-keymap-unref xkb-keymap)
    (xkb:xkb-state-unref xkb-state)))

(defmacro with-app ((app) &body body)
  `(let ((,app (make-instance 'waycalc-app)))
     (unwind-protect
       (progn ,@body)
       (cleanup-app ,app))))

(defmacro do-cells ((x y) &body body)
  `(dotimes (,x +cells-x+)
     (dotimes (,y +cells-y+)
       ,@body)))

(declaim (inline cell-size cell-region display-region text-height))
(defun cell-size (vw vh)
  (values (/ vw +cells-x+)
          (/ vh (1+ +cells-y+))))

(defun cell-region (x y vw vh)
  (multiple-value-bind (cell-width cell-height) (cell-size vw vh)
    (values (+ +margin+ (* x cell-width))
            (+ +margin+ (* (1+ y) cell-height))
            (- cell-width +margin+ +margin+)
            (- cell-height +margin+ +margin+))))

(defun display-region (vw vh)
  (values +margin+
          +margin+
          (- vw +margin+ +margin+)
          (- (nth-value 1 (cell-size vw vh)) +margin+ +margin+)))

(defun text-height (vh)
  (min 40 (- (floor (nth-value 1 (cell-size 0 vh))) +margin+ +margin+)))

(defun eval-calc (app)
  (with-slots (state) app
    (when (= 3 (length state))
      (destructuring-bind (x2 fn x1) state
        (setf state (list (funcall fn x1 x2)))))))

(defun clear-calc (app)
  (setf (slot-value app 'state) (list 0)))

(defun push-digit (digit app)
  (with-slots (state) app
    (if (numberp (first state))
        (setf (first state) (+ (* 10 (first state)) digit))
        (push digit state))))

(defun pop-digit (app)
  (with-slots (state) app
    (when (numberp (first state))
      (setf (first state) (floor (first state) 10)))))

(defun push-op (fn app)
  (eval-calc app)
  (with-slots (state) app
    (if (functionp (first state))
        (setf (first state) fn)
        (push fn state))))

(defun draw-frame (app)
  (with-slots (wl-shm width height state buttons
                      hover-cell hover-active?) app
    (let* ((stride (* width 4))
           (size (* stride height))
           (text-height (text-height height))
           buffer)
      (shm:with-open-shm-and-mmap* (shm pool-data (:direction :io) (size))
        (with-proxy (pool (wl-shm.create-pool wl-shm (shm:shm-fd shm) size))
          (setf buffer (wl-shm-pool.create-buffer pool 0 width height stride
                                                  :xrgb8888)))

        ;; Draw white
        (cairo:with-surface-and-context (surf (cairo:create-image-surface-for-data
                                                pool-data :rgb24 width height stride))
          ;; Draw background
          (cairo:set-source-color +bg-color+)
          (cairo:paint)

          ;; Draw text display
          (multiple-value-bind (dx dy dw dh) (display-region width height)
            (cairo:set-source-color +bg-color+)
            (cairo:rectangle dw dy dw dh)
            (cairo:fill-path)

            (cairo:set-source-color +fg-color+)
            (cairo:move-to dx (+ dy (/ (- dh text-height) 2)))
            (pango:print-with-attributes
              ((write-to-string
                 (or (find-if #'numberp state) 0))
               :width dw
               :alignment :pango_align_left)
              `((:absolute-size ,text-height)
                (:family "Monospace")))

            (cairo:set-line-width +border-width+)
            (cairo:set-source-color +border-color+)
            (cairo:rectangle dx dy dw dh)
            (cairo:stroke))

          ;; Draw buttons
          (do-cells (x y)
            (multiple-value-bind (cx cy cw ch) (cell-region x y width height)
              (let ((hover? (equal (list x y) hover-cell)))
                (cairo:set-source-color
                  (if (and hover? hover-active?)
                      +border-color+ +bg-color+))
                (cairo:rectangle cx cy cw ch)
                (cairo:fill-path)

                (cairo:set-source-color
                  (if (and hover? hover-active?)
                      +active-fg-color+
                      +fg-color+))
                (cairo:move-to cx (+ cy (/ (- ch text-height) 2)))
                (pango:print-with-attributes
                  ((car (aref buttons y x))
                   :width cw
                   :alignment :pango_align_center)
                  `((:absolute-size ,text-height)
                    (:family "Monospace")))

                (cairo:set-line-width
                  (if hover?
                      (* 2 +border-width+)
                      +border-width+))
                (cairo:set-source-color +border-color+)
                (cairo:rectangle cx cy cw ch)
                (cairo:stroke))))))

      ;; Destroy the proxy when it's no longer needed
      (push (evelambda
              (:release () (destroy-proxy buffer)))
            (wl-proxy-hooks buffer))
      buffer)))

(defun draw-and-commit (app)
  (with-slots (wl-surface) app
    (let ((buffer (draw-frame app)))
      (wl-surface.attach wl-surface buffer 0 0)
      (wl-surface.damage
        wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
      (wl-surface.commit wl-surface))))


(defun handle-pointer (app &rest event)
  (with-slots (width height pointer-x pointer-y
                     buttons hover-cell hover-active?) app
    (event-case event
      ;; Update pointer position
      (:enter (serial surface x y)
       (declare (ignore serial surface))
       (setf pointer-x x pointer-y y))
      (:leave (serial surface)
       (declare (ignore serial surface))
       (setf pointer-x nil pointer-y nil))
      (:motion (time-ms x y)
       (declare (ignore time-ms))
       (setf pointer-x x pointer-y y))
      ;; Update active cell state based on mousedown location
      (:button (serial time-ms button state)
       (declare (ignore serial time-ms))
       (when (= button input-event-codes:+btn-left+)
         (setf hover-active? (eq state :pressed))
         (when (and hover-cell (eq state :pressed))
           (destructuring-bind (x y) hover-cell
             (funcall (cdr (aref buttons y x)) app)))))
      ;; Update hover cell
      (:frame ()
       (block cells
         (when hover-active?
           (return-from cells))
         (when (and pointer-x pointer-y)
           (do-cells (x y)
             (multiple-value-bind (cx cy cw ch) (cell-region x y width height)
               (when (and (<= cx pointer-x (+ cx cw))
                          (<= cy pointer-y (+ cy ch)))
                 (setf hover-cell (list x y))
                 (return-from cells)))))
         (setf hover-cell nil))
       (draw-and-commit app)))))

(defun handle-keydown (app sym)
  (cond
    ((<= #K"0" sym #K"9") (push-digit (- sym #K"0") app))
    ((= sym #K"BackSpace") (pop-digit app))
    ((= sym #K"plus") (push-op #'+ app))
    ((= sym #K"minus") (push-op #'- app))
    ((= sym #K"asterisk") (push-op #'* app))
    ((= sym #K"slash") (push-op #'floor app))
    ((= sym #K"Escape") (clear-calc app))
    ((or (= sym #K"equal")
         (= sym #K"Return"))
     (eval-calc app))))

(defun handle-keyboard (app &rest event)
  (with-slots (xkb-context xkb-keymap xkb-state) app
    (event-case event
      ;; Set or update the keyboard key-map
      (:keymap (format fd size)
       (let ((shm (shm:make-shm fd)))
         (unwind-protect
           (progn
             (assert (eq format :xkb-v1))
             (shm:with-mmap (ptr shm size :prot '(:read) :flags '(:private))
               (let* ((keymap (xkb:xkb-keymap-new-from-string
                                xkb-context ptr :text-v1 ()))
                      (state (xkb:xkb-state-new keymap)))
                 (xkb:xkb-keymap-unref xkb-keymap)
                 (xkb:xkb-state-unref xkb-state)
                 (setf xkb-keymap keymap
                       xkb-state state))))
           (shm:close-shm shm))))

      ;; Pass thru mod key updates to the xkb state machine
      (:modifiers (serial depressed latched locked group)
       (declare (ignore serial))
       (xkb:xkb-state-update-mask
         xkb-state depressed latched locked 0 0 group))

      ;; Handle keys as focus enters the surface
      (:enter (serial surface keys)
       (declare (ignore serial surface))
       (dotimes (i (length keys))
         (let* ((keycode (+ 8 (aref keys i)))
                (sym (xkb:xkb-state-key-get-one-sym xkb-state keycode)))
           (when (plusp sym)
             (handle-keydown app sym))))
       (draw-and-commit app))

      ;; Handle keys as they're pressed
      (:key (serial time-ms key state)
       (declare (ignore serial time-ms))
       (let* ((keycode (+ 8 key))
              (sym (xkb:xkb-state-key-get-one-sym xkb-state keycode)))
         (when (and (plusp sym) (eq state :pressed))
           (handle-keydown app sym)))
       (draw-and-commit app)))))

(defun handle-seat (app &rest event)
  (with-slots (wl-seat wl-pointer wl-keyboard) app
    (event-case event
      (:capabilities (capabilities)
       (if (member :pointer capabilities)
           (unless wl-pointer
             (setf wl-pointer (wl-seat.get-pointer wl-seat))
             (push (a:curry 'handle-pointer app)
                   (wl-proxy-hooks wl-pointer)))
           (when wl-pointer
             (destroy-proxy wl-pointer)
             (setf wl-pointer nil)))
       (if (member :keyboard capabilities)
           (unless wl-keyboard
             (setf wl-keyboard (wl-seat.get-keyboard wl-seat))
             (push (a:curry 'handle-keyboard app)
                   (wl-proxy-hooks wl-keyboard)))
           (when wl-keyboard
             (destroy-proxy wl-keyboard)
             (setf wl-keyboard nil)))))))

(defun handle-registry (app &rest event)
  (with-slots (wl-registry wl-compositor wl-shm wl-seat xdg-wm-base) app
    (event-case event
      (:global (name interface version)
       (declare (ignore version))
       (case (a:when-let ((it (find-interface-named interface)))
               (class-name it))
         (wl-compositor
           (setf wl-compositor (wl-registry.bind
                                 wl-registry name 'wl-compositor 4)))
         (wl-shm
           (setf wl-shm (wl-registry.bind
                          wl-registry name 'wl-shm 1)))
         (wl-seat
           (setf wl-seat (wl-registry.bind
                           wl-registry name 'wl-seat 5))
           (push (a:curry 'handle-seat app)
                 (wl-proxy-hooks wl-seat)))
         (xdg-wm-base
           (setf xdg-wm-base (wl-registry.bind
                               wl-registry name 'xdg-wm-base 2))
           (push (evelambda
                   (:ping (serial)
                    (xdg-wm-base.pong xdg-wm-base serial)))
                 (wl-proxy-hooks xdg-wm-base))))))))

(defun run ()
  (with-open-display (display)
    (with-app (app)
      (with-slots (wl-registry wl-compositor xdg-wm-base
                   wl-surface xdg-surface xdg-toplevel
                   width height) app
        ;; Grab the registry to bind all globals
        (setf wl-registry (wl-display.get-registry display))
        (push (a:curry 'handle-registry app) (wl-proxy-hooks wl-registry))
        (wl-display-roundtrip display)

        ;; Create the surface and give it the xdg-toplevel role
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
                (:configure (new-width new-height states)
                 (declare (ignore states))
                 ;; Adjust the height and draw a new frame
                 (if (or (zerop new-width) (zerop new-height))
                     (setf width 300 height 400)
                     (setf width new-width height new-height)))
                (:close ()
                 (return-from run)))
              (wl-proxy-hooks xdg-toplevel))
        (with-proxy (region (wl-compositor.create-region wl-compositor))
          (wl-region.add
            region 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
          (wl-surface.set-opaque-region wl-surface region))
        (xdg-toplevel.set-title xdg-toplevel "Waycalc")
        (xdg-toplevel.set-app-id xdg-toplevel "xyz.shunter.wayflan.waycalc")
        (xdg-toplevel.set-min-size xdg-toplevel 230 280)
        (wl-surface.commit wl-surface)

        ;; Handle events forever
        (loop (wl-display-dispatch-event display))))))
