;;; Wayruler.lisp -- Virtual ruler in Wayland.
;;;
;;; Demonstrates gathering monitor metadata from wl-output by making a virtual
;;; ruler. The physical dimensions precision is to the mm, and it isn't
;;; completely accurate -- on my screen, the ruler's 20cm was ~19.9cm in real
;;; life -- but is a fair approximation
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.examples.wayruler
  (:use #:cl #:wayflan-client #:wayflan-client.xdg-shell)
  (:local-nicknames (#:a #:alexandria))
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.wayruler)



(deftype ruler-type ()
  '(member :vertical :horizontal))

(defparameter +ruler-color+ (cl-colors:hsv 50 .7 1))
(defparameter +ruler-thickness+ 200)
(defparameter +major-height+ 100)
(defparameter +minor-height+ 50)
(defparameter +major-thickness+ 5)
(defparameter +minor-thickness+ 2)
(defparameter +minor-step+ 5/2)
(defparameter +major-span+ 4)
(defparameter +ruler-text-width+ 20)
(defparameter +ruler-text-height+ 20)

(defclass wayruler-app ()
  (;; Globals
   wl-registry
   wl-compositor
   wl-shm
   wl-output
   xdg-wm-base

   ;; Surface
   wl-surface
   xdg-surface
   xdg-toplevel

   ;; State
   (physical-width :type wl-int)
   (physical-height :type wl-int)
   (output-width :type wl-int)
   (output-height :type wl-int)
   (output-scale :type wl-int :initform 1)
   (width :type wl-int)
   (height :type wl-int)))

(defun ruler-size-mm (app)
  (with-slots (physical-width output-width output-scale width) app
    (* output-scale physical-width width (/ output-width))))

(defun mm-to-x (app mm)
  (with-slots (physical-width output-width output-scale) app
    (/ (* mm output-width) physical-width output-scale)))

(defun draw-frame (app)
  (with-slots (wl-shm width height) app
    (let* ((stride (* width 4))
           (size (* stride height))
           buffer)
      (posix-shm:with-open-shm-and-mmap* (shm ptr (:direction :io) (size))
        (with-proxy (pool (wl-shm.create-pool wl-shm (posix-shm:shm-fd shm) size))
          (setf buffer (wl-shm-pool.create-buffer pool 0 width height stride :xrgb8888)))

        (cairo:with-surface-and-context (surf (cairo:create-image-surface-for-data
                                                ptr :rgb24 width height stride))
          ;; Fill with background
          (cairo:set-source-color +ruler-color+)
          (cairo:paint)

          ;; Put up a line every 10mm or so
          (cairo:set-source-rgb 0 0 0)
          (do* ((width-mm (ruler-size-mm app))
                (mm +minor-step+ (+ mm +minor-step+))
                (n 1 (1+ n))
                (major? (zerop (mod n +major-span+))
                        (zerop (mod n +major-span+)))
                (x (mm-to-x app mm)
                   (mm-to-x app mm))
                (top (- height (if major? +major-height+ +minor-height+))
                     (- height (if major? +major-height+ +minor-height+))))
            ((> mm width-mm))
            (cairo:set-line-width
              (if major? +major-thickness+ +minor-thickness+))
            (cairo:move-to x height)
            (cairo:line-to x top)
            (cairo:stroke)
            ;; If on a major stroke, also print out the length so far
            (when major?
              (cairo:move-to (+ x (/ +ruler-text-height+ 2)) 0)
              (cairo:save)
              (cairo:rotate (/ pi 2))

              (pango:print-with-attributes
                ((format nil "~D cm" (/ mm 10))
                 :alignment :pango_align_right
                 :width (- top 10))
                `((:absolute-size ,+ruler-text-height+)
                  (:family "Sans Serif")))
              (cairo:restore)))))

      ;; Destroy the buffer when the compositor releases it
      (push (evelambda
              (:release ()
               (destroy-proxy buffer)))
            (wl-proxy-hooks buffer))

      buffer)))

(defun handle-wl-output (app &rest event)
  (with-slots (physical-width physical-height
                              output-width output-height output-scale) app
    (event-case event
      ;; The geometry event gives us our coordinates in the global space,
      ;; physical dimensions in mm, and some misc metadata.
      (:geometry (x y phys-width phys-height subpixel make model transform)
       (format t "Geometry: +~D+~D:~DmmX~Dmm ~S ~S ~S ~S~%"
               x y phys-width phys-height subpixel make model transform)
       (setf physical-width phys-width
             physical-height phys-height))
      ;; Our output has a list of modes with flags indicating which is
      ;; preferred, and which is currently set.
      (:mode (flags width height refresh)
       (when (member :current flags)
         (format t "Current mode: ~Dx~D@~SHz~%" width height (/ refresh 1000.0))
         (setf output-width width
               output-height height)))
      ;; On HiDPI displays particularly, the output may scale up our surfaces
      ;; by a factor, so we should take this into consideration.
      ;; TODO: are the mode dimensions pre-scale, or post-scale? If post-scale,
      ;; we don't need this.
      (:scale (factor)
       (format t "Scale: ~D~%" factor)
       (setf output-scale factor))
      ;; And some content for human eyes
      (:name (name)
       (format t "Dislay name: ~A~%" name))
      (:description (desc)
       (format t "Description: ~A~%" desc))
      (:done ()
       (format t "Done!~%")))))

(defun handle-registry (app &rest event)
  (with-slots (wl-registry wl-compositor wl-shm wl-output
                           xdg-wm-base) app
    (event-ecase event
      (:global (name interface version)
       (declare (ignore version))
       (case (a:when-let ((class (find-interface-named interface)))
               (class-name class))
         (wl-compositor
           (setf wl-compositor (wl-registry.bind
                                 wl-registry name 'wl-compositor 4)))
         (wl-shm
           (setf wl-shm (wl-registry.bind wl-registry name 'wl-shm 1)))
         (wl-output
           (setf wl-output (wl-registry.bind wl-registry name 'wl-output 4))
           (push (a:curry 'handle-wl-output app)
                 (wl-proxy-hooks wl-output)))
         (xdg-wm-base
           (setf xdg-wm-base (wl-registry.bind wl-registry name 'xdg-wm-base 2))
           (push (evelambda
                   (:ping (serial)
                    (xdg-wm-base.pong xdg-wm-base serial)))
                 (wl-proxy-hooks xdg-wm-base))))))))

(defun run ()
  (with-open-display (display)
    (let ((app (make-instance 'wayruler-app)))
      (with-slots (wl-registry wl-compositor wl-shm wl-output xdg-wm-base
                               wl-surface xdg-surface xdg-toplevel
                               width height) app
        ;; Register and bind all globals
        (setf wl-registry (wl-display.get-registry display))
        (push (a:curry 'handle-registry app)
              (wl-proxy-hooks wl-registry))
        (wl-display-roundtrip display)

        ;; Setup the toplevel surface
        (setf wl-surface (wl-compositor.create-surface wl-compositor)
              xdg-surface (xdg-wm-base.get-xdg-surface xdg-wm-base wl-surface)
              xdg-toplevel (xdg-surface.get-toplevel xdg-surface))
        (push (evelambda
                (:configure (serial)
                 (xdg-surface.ack-configure xdg-surface serial)
                 ;; Draw a new surface
                 (let ((buffer (draw-frame app)))
                   (wl-surface.attach wl-surface buffer 0 0)
                   (wl-surface.commit wl-surface))))
              (wl-proxy-hooks xdg-surface))
        (push (evlambda
                (:configure (new-width new-height states)
                 (declare (ignore states))
                 (if (and (plusp new-width) (plusp new-height))
                     (setf width new-width
                           height new-height)
                     (setf width 800
                           height +ruler-thickness+)))
                (:close ()
                 (return-from run)))
              (wl-proxy-hooks xdg-toplevel))
        (xdg-toplevel.set-title xdg-toplevel "Wayruler")
        (xdg-toplevel.set-app-id xdg-toplevel "xyz.shunter.wayflan.wayruler")
        (xdg-toplevel.set-min-size xdg-toplevel 0 +ruler-thickness+)
        (xdg-toplevel.set-max-size xdg-toplevel 0 +ruler-thickness+)
        (wl-surface.commit wl-surface)

        ;; Keep handling events forever
        (loop (wl-display-dispatch-event display))))))
