;;; examples/wl-pointer-demo.lisp
;;;
;;; This wl-pointer demo manages a wl-pointer and demonstrates
;;; tracking its location, button and scroll state.
;;; Move your mouse around, scroll, and click the left, right, and middle mouse
;;; buttons.
;;; Release the middle mouse button to toggle a custom pointer image
;;; instead of the system default.
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.examples.wl-pointer-demo
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.wl-pointer-demo)



(defparameter +min-radius+ 30)
(defparameter +max-radius+ 150)

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
   (wl-pointer :initform nil)
   cursor-surface

   ;; State
   (width :type wl-int)
   (height :type wl-int)
   (ptr-x :initform nil)
   (ptr-y :initform nil)
   (btn-left? :initform nil)
   (btn-right? :initform nil)
   (btn-mid? :initform nil)
   (radius :initform 50)
   (messages :initform (make-ring-buffer))
   latest-pointer-serial
   (custom-cursor-on? :initform nil)))

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

(defun draw-frame (app)
  (with-slots (wl-shm width height
                      radius ptr-x ptr-y
                      btn-left? btn-right? btn-mid? messages) app
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

          ;; Draw partial arcs that reflect each pointer button recorded.
          ;; The left button reflects the bottom-left arc, the
          ;; right button the bottom-right, and the middle button
          ;; (shared with the scroll wheel on my and many common mouses)
          ;; the top.
          (when (and ptr-x ptr-y)
            (cairo:set-source-rgb 0 0 1)
            (when btn-left?
              (cairo:arc ptr-x ptr-y radius
                         (* 1/2 pi) (* 4/3 pi))
              (cairo:line-to ptr-x ptr-y)
              (cairo:fill-path))
            (when btn-right?
              (cairo:arc ptr-x ptr-y radius
                         (* 5/3 pi) (* 1/2 pi))
              (cairo:line-to ptr-x ptr-y)
              (cairo:fill-path))
            (when btn-mid?
              (cairo:arc ptr-x ptr-y radius
                         (* 4/3 pi) (* 5/3 pi))
              (cairo:line-to ptr-x ptr-y)
              (cairo:fill-path)))

          ;; Draw a red circle outline and fork at the pointer
          (when ptr-x
            (cairo:set-source-rgb 1 00 0)

            (cairo:set-line-width 20)
            (cairo:arc ptr-x ptr-y radius 0 (* 2 pi))
            (cairo:stroke)
            (cairo:set-line-width 5)

            (cairo:move-to (+ ptr-x (* radius (cos (* 4/3 pi))))
                           (+ ptr-y (* radius (sin (* 4/3 pi)))))
            (cairo:line-to ptr-x ptr-y)
            (cairo:line-to (+ ptr-x (* radius (cos (* 5/3 pi))))
                           (+ ptr-y (* radius (sin (* 5/3 pi)))))
            (cairo:stroke)

            (cairo:move-to ptr-x ptr-y)
            (cairo:line-to ptr-x (+ ptr-y radius))
            (cairo:stroke))

          ;; Draw pointer data at top-left.
          (cairo:set-source-rgb 0 0 0)
          (pango:print-with-attributes
            ((format nil "Pointer X: ~D/256~%Pointer Y: ~D/256~%Latest events:~%~A"
                     (when ptr-x (* 256 ptr-x))
                     (when ptr-x (* 256 ptr-y))
                     (rb-to-string messages))
             :width width)
            `((:absolute-size ,(min 30 (floor height 15)))
              (:family "Sans Serif")))))

      (push (evelambda
              (:release ()
               (destroy-proxy buffer)))
            (wl-proxy-hooks buffer))
      buffer)))

(defun handle-pointer (app &rest event)
  (with-slots (wl-surface wl-pointer ptr-x ptr-y radius
                          btn-left? btn-right? btn-mid?
                          messages latest-pointer-serial
                          cursor-surface custom-cursor-on?) app
    (rb-push (write-to-string event) messages)
    (event-case event
      ;; Save the pointer position whenever it enters and moves the surface.
      ;; Since we only have one surface open, we ignore this value -- but
      ;; multi-surface apps can save this value to associate the pointer
      ;; with the relevant surface.
      (:enter (serial surface x y)
       (declare (ignore surface))
       (setf latest-pointer-serial serial)
       (setf ptr-x x
             ptr-y y))
      (:motion (time-ms x y)
       (declare (ignore time-ms))
       (setf ptr-x x
             ptr-y y))
      (:leave (serial surface)
       ;; Hide the pointer when it leaves the surface.
       (declare (ignore serial surface))
       (setf ptr-x nil
             ptr-y nil))
      (:button (serial time-ms button state)
       (declare (ignore serial time-ms))
       ;; Wayland sends the rising and falling edges of button presses.
       ;; For the app, we want to show the current state of the button,
       ;; so save each button's state as it changes.
       (block nil
              (setf (slot-value
                      app
                      (a:switch (button :test #'=)
                        (input-event-codes:+btn-left+ 'btn-left?)
                        (input-event-codes:+btn-right+ 'btn-right?)
                        (input-event-codes:+btn-middle+ 'btn-mid?)
                        (t (return))))
                    (eq state :pressed)))

       (when (and (= button input-event-codes:+btn-middle+)
                  (eq state :released))
         (setf custom-cursor-on? (not custom-cursor-on?))
         (if custom-cursor-on?
             (wl-pointer.set-cursor wl-pointer latest-pointer-serial
                                    cursor-surface 13 13)
             (wl-pointer.set-cursor wl-pointer latest-pointer-serial
                                    nil 0 0))))
      (:axis (time-ms axis delta)
       ;; The value of the axis's motion is in the same coordinate space as
       ;; :MOTION events.
       (declare (ignore time-ms))
       (when (eq axis :vertical-scroll)
         (setf radius (a:clamp (- radius delta) +min-radius+ +max-radius+))))
      (:frame ()
       ;; Intentionally left blank.
       ;;
       ;; The wl-pointer FRAME event groups separate events together, by
       ;; signifying the end of a logical group -- for example, an :AXIS-SOURCE
       ;; showing where a scroll is coming from, two :AXIS events for
       ;; :HORIZONTAL-SCROLL and :VERTICAL-SCROLL signifying a vertical scroll,
       ;; or a :LEAVE followed by an :ENTER to signify the pointer moved from
       ;; one surface to the other.
       ;;
       ;; This application is simple enough that it doesn't need to understand
       ;; logical event groups. Applications with more complex event processing
       ;; ought to accumulate each event, and then process them as a group once
       ;; :FRAME is heard.
       ))

    ;; Redraw the surface
    (let ((buffer (draw-frame app)))
      (wl-surface.attach wl-surface buffer 0 0)
      (wl-surface.damage
        wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
      (wl-surface.commit wl-surface))))

(defun handle-seat (app &rest event)
  (with-slots (wl-seat wl-pointer) app
    (event-case event
      (:name (name)
       (format t "Seat name: ~S~%" name))
      (:capabilities (capabilities)
       (format t "Wl-seat capabilities: ~S~%" capabilities)
       (if (member :pointer capabilities)
           (unless wl-pointer
             (setf wl-pointer (wl-seat.get-pointer wl-seat))
             (push (a:curry 'handle-pointer app)
                   (wl-proxy-hooks wl-pointer)))
           (when wl-pointer
             (destroy-proxy wl-pointer)
             (setf wl-pointer nil)))))))

(defun make-cursor-surface (app)
  "Create a static, pre-drawn surface to use as the pointer's image, or cursor."
  (with-slots (wl-shm wl-compositor) app
    (let* ((width 25)
           (height 25)
           (stride (* width 4))
           (size (* stride height))
           (surface (wl-compositor.create-surface wl-compositor))
           buffer)

      (shm:with-open-shm-and-mmap* (shm pool-data (:direction :io) (size))

        ;; Create a buffer out of the shm memory
        (with-proxy (pool (wl-shm.create-pool wl-shm (shm:shm-fd shm) size))
          (setf buffer (wl-shm-pool.create-buffer
                         pool 0 width height stride
                         :argb8888)))

        ;; Draw the surface
        (cairo:with-surface-and-context (surf (cairo:create-image-surface-for-data
                                                pool-data :argb32 width height stride))
          ;; Draw a green square with a black border
          (cairo:set-source-rgb 0 1 0)
          (cairo:paint)

          (cairo:set-source-rgb 0 0 0)
          (cairo:set-line-width 1)
          (cairo:rectangle 0 0 25 25)
          (cairo:stroke)))

      ;; Destroy the buffer when the server mentions it's released.
      ;; Though in theory, it shouldn't be released, as the cursor surface
      ;; lives for the duration of the application.
      (push (evelambda
              (:release ()
               (destroy-proxy buffer)))
            (wl-proxy-hooks buffer))

      (wl-surface.attach surface buffer 0 0)
      (wl-surface.damage-buffer
        surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
      (wl-surface.commit surface)
      surface)))

(defun run ()
  (with-open-display (display)
    (let ((app (make-instance 'app :display display)))
      (with-slots (wl-shm wl-registry wl-compositor xdg-wm-base wl-seat
                          wl-surface xdg-surface xdg-toplevel cursor-surface
                          width height) app
        ;; Get the registry and bind all globals
        (setf wl-registry (wl-display.get-registry display))
        (push (evlambda
                (:global (name interface version)
                 (declare (ignore version))
                 (case (a:when-let ((it (find-interface-named interface)))
                         (class-name it))
                   (wl-shm
                     (setf wl-shm (wl-registry.bind
                                    wl-registry name 'wl-shm 1)))
                   (wl-compositor
                     (setf wl-compositor (wl-registry.bind
                                           wl-registry name 'wl-compositor 4)))
                   (wl-seat
                     (setf wl-seat (wl-registry.bind
                                     wl-registry name 'wl-seat 5))
                     (push (a:curry 'handle-seat app)
                           (wl-proxy-hooks wl-seat)))
                   (xdg-wm-base
                     (setf xdg-wm-base (wl-registry.bind
                                         wl-registry name 'xdg-wm-base 1))
                     (push (evelambda
                             (:ping (serial)
                              (xdg-wm-base.pong xdg-wm-base serial)))
                           (wl-proxy-hooks xdg-wm-base))))))
              (wl-proxy-hooks wl-registry))
        (wl-display-roundtrip display)

        ;; Prepare a custom cursor surface to toggle on middle-click.
        (setf cursor-surface (make-cursor-surface app))

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
        (xdg-toplevel.set-title xdg-toplevel "Wayflan wl-pointer Demo")
        (wl-surface.commit wl-surface)

        ;; Keep handling events until we get a close signal
        (loop (wl-display-dispatch-event display))))))
