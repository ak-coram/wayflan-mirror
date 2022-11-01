;;; examples/wl-touch-demo.lisp
;;;
;;; This wl-touch demo maintains a list of touch points
;;; and displays them on the surface.
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.examples.wl-touch-demo
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:shm #:posix-shm))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.wl-touch-demo)



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
   (wl-touch :initform nil)

   ;; State
   (width :type wl-int)
   (height :type wl-int)
   (points :initform (make-hash-table))))

(defstruct point
  x y
  first-x first-y
  major
  minor
  orientation)

(defun draw-frame (app)
  (with-slots (wl-shm width height points) app
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

          ;; Draw blue crosses for each point
          (cairo:set-line-width 20)
          (loop :with point-radius := 50
                :with minor-radius := 25
                :for point :being :the :hash-value :of points
                :do (with-accessors ((x point-x)
                                     (y point-y)
                                     (first-x point-first-x)
                                     (first-y point-first-y)) point
                      ;; Draw the tail, which is a line from its first x,y
                      ;; to the current, ended with a smaller cross.
                      (progn
                        (cairo:set-source-rgb 0 0 0.5)
                        (cairo:move-to first-x first-y)
                        (cairo:line-to x y)
                        (cairo:move-to (- first-x minor-radius)
                                       (- first-y minor-radius))
                        (cairo:line-to (+ first-x minor-radius)
                                       (+ first-y minor-radius))
                        (cairo:move-to (+ first-x minor-radius)
                                       (- first-y minor-radius))
                        (cairo:line-to (- first-x minor-radius)
                                       (+ first-y minor-radius))
                        (cairo:stroke))
                      ;; TODO if I ever get a touchscreen with
                      ;; shape support, draw ellipses.
                      (progn
                        (cairo:set-source-rgb 0 0 1)
                        (cairo:move-to (- x point-radius) (- y point-radius))
                        (cairo:line-to (+ x point-radius) (+ y point-radius))
                        (cairo:move-to (+ x point-radius) (- y point-radius))
                        (cairo:line-to (- x point-radius) (+ y point-radius))
                        (cairo:stroke))))

          ;; Draw touch data at top-left.
          (cairo:set-source-rgb 0 0 0)
          (pango:print-with-attributes
            ((format nil "Touch Points:~{~%(~A)~}"
                     (loop :for p :being :the :hash-values :of points
                           :collect (format nil ":X ~15D/256 :Y ~15D/256"
                                            (* 256 (point-x p))
                                            (* 256 (point-y p)))))
             :width width)
            `((:absolute-size ,(min 30 (floor height 15)))
              (:family "Sans Serif")))))

      (push (evelambda
              (:release ()
               (destroy-proxy buffer)))
            (wl-proxy-hooks buffer))
      buffer)))

(defun handle-touch (app &rest event)
  (with-slots (points wl-surface) app
    (event-ecase event
      (:down (serial time-ms surface id x y)
       (declare (ignore serial time-ms surface))
       (setf (gethash id points)
             (make-point :x x :y y :first-x x :first-y y)))
      (:up (serial time-ms id)
       (declare (ignore serial time-ms))
       (remhash id points))
      (:motion (time-ms id new-x new-y)
       (declare (ignore time-ms))
       (with-accessors ((x point-x)
                        (y point-y)) (gethash id points)
         (setf x new-x
               y new-y)))
      (:frame ()
       ;; Intentionally left blank.
       ;;
       ;; The wl-touch FRAME event groups separate events together, by
       ;; signifying the end of a logical group -- for example, an :UP event
       ;; followed by a :DOWN event may signify touch input moving from
       ;; one surface to another.
       ;;
       ;; This application is simple enough that it doesn't need to understand
       ;; logical event groups. Applications with more complex event processing
       ;; ought to accumulate each event, and then process them as a group once
       ;; :FRAME is heard.
       )
      (:shape (id new-major new-minor)
       (with-accessors ((major point-major)
                        (minor point-minor)) (gethash id points)
         (setf major new-major
               minor new-minor)))
      (:orientation (id orientation)
       (setf (point-orientation (gethash id points)) orientation)))

    ;; Redraw the surface
    (let ((buffer (draw-frame app)))
      (wl-surface.attach wl-surface buffer 0 0)
      (wl-surface.damage
        wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
      (wl-surface.commit wl-surface))))

(defun handle-seat (app &rest event)
  (with-slots (wl-seat wl-touch) app
    (event-case event
      (:name (name)
       (format t "Seat name: ~S~%" name))
      (:capabilities (capabilities)
       (format t "Wl-seat capabilities: ~S~%" capabilities)
       (if (member :touch capabilities)
           (unless wl-touch
             (setf wl-touch (wl-seat.get-touch wl-seat))
             (push (a:curry 'handle-touch app)
                   (wl-proxy-hooks wl-touch)))
           (when wl-touch
             (destroy-proxy wl-touch)
             (setf wl-touch nil)))))))

(defun run ()
  (with-open-display (display)
    (let ((app (make-instance 'app :display display)))
      (with-slots (wl-shm wl-registry wl-compositor xdg-wm-base wl-seat
                          wl-surface xdg-surface xdg-toplevel
                          width height) app
        ;; Get the registry and bind all globals
        (setf wl-registry (wl-display.get-registry display))
        (push (evlambda
                (:global (name interface version)
                 (case (a:when-let ((it (find-interface-named interface)))
                         (class-name it))
                   (wl-shm
                     (setf wl-shm (wl-registry.bind
                                    wl-registry name 'wl-shm 1)))
                   (wl-compositor
                     (setf wl-compositor (wl-registry.bind
                                           wl-registry name 'wl-compositor 4)))
                   (wl-seat
                     ;; wl-touch :SHAPE and :ORIENTATION requires at least version
                     ;; 6.
                     (setf wl-seat (wl-registry.bind
                                     wl-registry name 'wl-seat
                                     (a:clamp version 3 6)))
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
        (xdg-toplevel.set-title xdg-toplevel "Wayflan wl-touch Demo")
        (wl-surface.commit wl-surface)

        ;; Keep handling events until we get a close signal
        (loop (wl-display-dispatch-event display))))))
