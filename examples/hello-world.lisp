;;; examples/hello-world.lisp - Grab info of all registry globals
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(defpackage #:xyz.shunter.wayflan.examples.hello-world
  (:use #:cl #:wayflan-client)
  (:export #:run))

(in-package #:xyz.shunter.wayflan.examples.hello-world)



(defun run ()
  ;; Try to connect to a compositor socket at $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY.
  ;; If $WAYLAND_DISPLAY describes an absolute path, connect to that directly.
  ;;
  ;; The wl-display is a special wl-proxy whose ID is always #1 and whose
  ;; #'wl-proxy-display is itself.
  (with-open-display (display)
    (format t "Hello, Wayland! Connected to ~S~%~%" display)

    ;; Send the request wl_display::get_registry to the compositor.
    ;; <https://wayland.app/protocols/wayland#wl_display:request:get_registry>.
    ;; The sole "parameter" is a new id for a wl_registry, which is generated
    ;; and a proxy is returned as a sole return value.
    (let ((registry (wl-display.get-registry display)))
      ;; #'wl-proxy-hooks is an accessor to a list of functions
      ;; that are called by the display whenever it reads an event
      ;; tied to this proxy.
      ;;
      ;; Callback functions accept the event name as a first arguments,
      ;; followed by a variadic number of event args.
      ;;
      ;; EVLAMBDA (and sister macros EVCLAMBDA and EVELAMBDA) is a
      ;; lambda-returning macro that dispatches events in a destructuring
      ;; case -like format.
      (push (evlambda
              (:global (name interface version)
               (format t "#x~8,'0X ~32S v~D ~S~%"
                       name interface version
                       (find-interface-named interface))))
            (wl-proxy-hooks registry))

      (format t "wl-registry globals:~%")
      ;; Ask the compositor to submit a "done" event and keep
      ;; consuming events until the callback is received.
      ;;
      ;; This emulates libwayland wl_display_roundtrip, and
      ;; ensures all previous requests and resulting events have
      ;; been handled.
      (wl-display-roundtrip display))))
