;;; examples/cairo-demo.lisp - A hello world demo using Cairo
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.
;;;
;;; The Lisp Alien Logo at #P"examples/lisplogo_256.png" is
;;; Copyright (c) Conrad Barski, licensed under the Public Domain.
;;; Unaltered image accessed at <https://www.lisperati.com/logo.html>

(defpackage #:xyz.shunter.wayflan.examples.cairo-demo
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria)
                    (#:c #:cl-cairo2)
                    (#:shm #:posix-shm))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.cairo-demo)



(defclass app ()
  (;; Globals
   (wl-display :initarg :display)
   wl-registry
   wl-shm
   wl-compositor
   xdg-wm-base

   ;; Objects
   wl-surface
   xdg-surface
   xdg-toplevel

   ;; State
   width
   height
   (last-frame :initform 0)
   (hue :initform 0.0)))

(defun draw-frame (app)
  (with-slots (wl-shm width height hue) app
    (let* ((stride (* width 4))
           (size (* stride height))
           buffer)
      (shm:with-open-shm-and-mmap* (shm pool-data (:direction :io) (size))
        (with-proxy (pool (wl-shm.create-pool wl-shm (shm:shm-fd shm) size))
          (setf buffer (wl-shm-pool.create-buffer
                         pool 0 width height stride
                         :xrgb8888)))

        ;; Draw the surface
        (c:with-surface-and-context (surf (c:create-image-surface-for-data
                                            pool-data :rgb24 width height stride))
          ;; Draw a rainbow background
          (c:set-source-color (cl-colors:hsv hue 0.9 1))
          (c:paint)

          ;; Add an image on the bottom
          (c:with-surface (image (c:image-surface-create-from-png
                                   #.(asdf:component-pathname
                                       (asdf:find-component
                                         '#:wayflan-client/examples
                                         "lisplogo_256.png")))
                                     :destroy t)
            (c:set-source-surface
              image
              (/ (- width (c:image-surface-get-width image)) 2)
              (- height (c:image-surface-get-height image) 10))
            (c:paint))

          ;; Print "Hello World" on top
          (c:set-source-rgb 0 0 0)
          (pango:print-with-attributes
            ("Hello, world!" :alignment :pango_align_center :width width)
            `((:absolute-size ,(min 120 (max 60 (floor height 5))))
              (:family "Sans Serif"))))

        (push (evelambda
                (:release ()
                 (destroy-proxy buffer)))
              (wl-proxy-hooks buffer))
        buffer))))

(defun handle-frame-callback (app callback &rest event)
  (event-ecase event
    (:done (time-ms)
     (with-slots (wl-surface last-frame hue) app
       ;; Destroy this callback
       (destroy-proxy callback)

       ;; Request another frame
       (setf callback (wl-surface.frame wl-surface))
       (push (a:curry 'handle-frame-callback app callback)
             (wl-proxy-hooks callback))

       ;; Update hue at 360 deg / 10 seconds
       (unless (zerop last-frame)
         (incf hue (* (/ (- time-ms last-frame) 1000.0) 360/10))
         (setf hue (mod hue 360)))

       ;; Submit a new frame for this event
       (let ((wl-buffer (draw-frame app)))
         (wl-surface.attach wl-surface wl-buffer 0 0)
         (wl-surface.damage-buffer
           wl-surface 0 0 +most-positive-wl-int+ +most-positive-wl-int+)
         (wl-surface.commit wl-surface))

       (setf last-frame time-ms)))))

(defun run ()
  (with-open-display (display)
    (let ((app (make-instance 'app :display display)))
      (with-slots (wl-shm wl-registry wl-compositor xdg-wm-base
                          wl-surface xdg-surface xdg-toplevel
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
        (xdg-toplevel.set-title xdg-toplevel "Wayflan Cairo Demo")
        (wl-surface.commit wl-surface)

        ;; Report we're ready for the next frame
        (let ((cb (wl-surface.frame wl-surface)))
          (push (a:curry 'handle-frame-callback app cb)
                (wl-proxy-hooks cb)))

        ;; Keep handling events until we get a close signal
        (loop (wl-display-dispatch-event display))))))
