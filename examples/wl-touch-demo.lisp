;;; examples/wl-touch-demo.lisp
;;;
;;; This wl-touch demo maintains a list of touch points
;;; and displays them on the surface.
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>
;;; All rights reserved.

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
   cursor-surface

   ;; State
   width
   height
   (points :initform (make-hash-table))))

(defstruct point
  x y
  major
  minor
  orientation)

(defun draw-frame (app)
  (with-slots (wl-shm width height points) app
    (let* ((stride (* width 4))
           (size (* stride height))
           buffer)
      (shm:with-open-shm-and-mmap* (shm pool-data (:direction :io :permissions '(:user-all))
                                        ((cffi:null-pointer) size '(:read :write) 0))
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
          (cairo:set-source-rgb 0 0 1)
          (cairo:set-line-width 20)
          (loop :with point-radius := 50
                :for point :being :the :hash-value :of points
                :do (with-accessors ((x point-x)
                                     (y point-y)) point
                      ;; TODO if I ever get a touchscreen with
                      ;; shape support, draw ellipses.
                      (progn
                        (cairo:move-to (- x point-radius) (- y point-radius))
                        (cairo:line-to (+ x point-radius) (+ y point-radius))
                        (cairo:move-to (+ x point-radius) (- y point-radius))
                        (cairo:line-to (- x point-radius) (+ y point-radius)))
                      (cairo:stroke)))

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

(defun handle-frame-callback (app callback &rest event)
  (event-ecase event
    (:done (time-ms)
     (declare (ignore time-ms))
     (with-slots (wl-surface last-frame) app
       ;; Destroy this callback
       (destroy-proxy callback)

       ;; Request another frame
       (setf callback (wl-surface.frame wl-surface))
       (push (a:curry 'handle-frame-callback app callback)
             (wl-proxy-hooks callback))

       ;; Submit a new frame for this event
       (let ((wl-buffer (draw-frame app)))
         (wl-surface.attach wl-surface wl-buffer 0 0)
         (wl-surface.damage-buffer
           wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
         (wl-surface.commit wl-surface))))))

(defun handle-touch (app &rest event)
  (with-slots (points) app
    (event-ecase event
      (:down (serial time-ms surface id x y)
       (declare (ignore serial time-ms surface))
       (setf (gethash id points)
             (make-point :x x :y y)))
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
       ;; The Wayland spec says the client should accumulate all state and
       ;; start processing the events as a logical group once the :FRAME
       ;; event is hit, signalling the frame is finished.
       ;;
       ;; This app doesn't process anything at :FRAME since state accumulation
       ;; Is the entire nature of the event logic here.
       )
      (:cancel ()
       ;; TODO figure out exactly how to trigger this, to demonstrate it in
       ;; the demo.
       )
      (:shape (id new-major new-minor)
       (with-accessors ((major point-major)
                        (minor point-minor)) (gethash id points)
         (setf major new-major
               minor new-minor)))
      (:orientation (id orientation)
       (setf (point-orientation (gethash id points)) orientation)))))

(defun handle-seat (app &rest event)
  (with-slots (wl-seat wl-touch) app
    (event-case event
      (:name (name)
       (format t "Seat name: ~S~%" name))
      (:capabilities (capabilities)
       (format t "Wl-seat capabilities: ~S~%" capabilities)
       (cond
         ((and (member :touch capabilities)
               (null wl-touch))
          (setf wl-touch (wl-seat.get-touch wl-seat))
          (push (a:curry 'handle-touch app)
                (wl-proxy-hooks wl-touch)))
         ((and (not (member :touch capabilities))
               wl-touch)
          (destroy-proxy wl-touch)
          (setf wl-touch nil)))))))

(defun make-cursor-surface (app)
  "Create a static, pre-drawn surface to use as the pointer's image, or cursor."
  (with-slots (wl-shm wl-compositor) app
    (let* ((width 25)
           (height 25)
           (stride (* width 4))
           (size (* stride height))
           (surface (wl-compositor.create-surface wl-compositor))
           buffer)

      (posix-shm:with-open-shm-and-mmap* (shm pool-data (:direction :io :permissions '(:user-all))
                                              ((cffi:null-pointer) size '(:read :write) 0))

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
        (xdg-toplevel.set-title xdg-toplevel "Wayflan Cairo Demo")
        (wl-surface.commit wl-surface)

       (let ((cb (wl-surface.frame wl-surface)))
         (push (a:curry 'handle-frame-callback app cb)
               (wl-proxy-hooks cb)))

        ;; Keep handling events until we get a close signal
        (loop (wl-display-dispatch-event display))))))
