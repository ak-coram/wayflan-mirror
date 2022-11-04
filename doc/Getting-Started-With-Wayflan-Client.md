# Getting Started with Wayflan Client

This document target those familiar with Wayland fundamentals, and aims to
explain how each Wayland concept integrates into Wayflan. It starts with a
refresher, and then a more in-depth look at how each Wayland concept maps to
Lisp code.

To help with brevity in the snippets, I'm assuming all forms are evaluated
under `(use-package :wayflan-client)`.

## Wayland Refresher

The Wayland protocol is an object-oriented, asynchronous protocol between a
client and the compositor it's connected to. There are a few concepts both
actors understand when talking to each other:

- **Interfaces** -- describes the API of a protocol object, including requests,
  events, and enums. Interfaces in Wayflan are instances of
  **wl-interface-class**, and are subclasses of **wl-proxy**.
- **Protocol objects** -- Objects in Wayland that implement some interface.
  Clients have references to protocol objects called **proxies**. Proxies in
  Wayflan are instances of **wl-proxy**. Proxies have ID's that are all
  positive numbers.
- **Requests and Events** -- Clients send Requests. Servers send Events.
  Interfaces define both. Both are associated with an *opcode* that Wayflan
  keeps invisible for its users. Some requests are *destructors*, and are sent
  to notify the compositor that a client has destroyed the proxy.
- **Enums** -- Sets of integers each associated with meaningful names. For
  bitfield enums, the bits in the values themselves are meaningful. Wayflan
  converts these integers into keywords for you, but also accepts the integers
  themselves as arguments.
- **Display** - A special object. Represents a connection to the compositor,
  and manages internal protocol logic. The display starts with an ID of 1.
- **Registry** -- A special object. Communicates the global objects that the
  compositor can provide to the client.

These datatypes are available within Requests and Events:

- **int**, **uint** -- Signed and unsigned 32-bit integers. These may be
  associated with an enum, in which case events return the associated keyword
  (or keywords in the case of bitfield enums), and requests accept both keyword
  and integer.
- **fixed** -- 24.8 bit signed integers. Wayflan handles them as rational
  numbers with `1/256` precision.
- **string** -- Wayland does not specify the encoding, but UTF-8 is used in
  practice.
- **object** -- A protocol object's ID. The type may be known or unknown in the
  protocol.
- **new-id** -- ID of a fresh protocol object created by the display. Requests
  with this ID return the proxy instance as its value. If the type is unknown
  within the protocol, the request also sends the new object's interface and
  version.
- **array** -- blob of arbitrary data. Wayflan handles arrays as Common Lisp
  vectors of (unsigned-byte 8) (i.e. octet vectors).
- **fd** -- Integer file descriptors. These aren't sent as mundane I/O, but
  part of the ancillary data system for UNIX local sockets (cmsg).

## The Display

Connecting to a Wayland server is straightforward on Wayflan:

```lisp
* (wl-display-connect)
#<WL-DISPLAY :ON #P"/run/user/1000/wayland-1" :ID 1 :VERSION 1 :HOOKS (0) ...>
* (wl-display-disconnect *)
T
```

After connecting to a Wayland socket, a wl-display is instantiated and is
responsible for internal Wayland features.

**Wl-display-connect** aims to emulate libwayland's algorithm to find the most
appropriate path to the socket:

- If a string is provided to `wl-display-connect`, use that as the socket name.
  Otherwise, use `$WAYLAND_DISPLAY` as the primary name.
- If the primary name is an absolute path, connect to the socket at that
  location. Otherwise, connect to `$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY`.

Wayflan provides a utility macro to close the display whenever execution leaves the body:

```lisp
(with-open-display (display)
  ;; do things with display...
  )
```

## Sending Requests

Requests under Wayflan are functions with the naming schema `<lispified
interface name>  .  <lispified request name>`. For example, the function that
sends the request `wl_display::get_registry`, is named
`wl-display.get-registry`.

Request functions always take the sender as the first arg, followed by any
request arguments. If a request parameter has the type `new_id` with a known
type, then the new id is provided, and a proxy with that id is given to the
caller as the return value:

```lisp
;; wl_display::get_registry(registry: new_id<wl_registry>)
* (wl-display.get-registry display)
#<WL-REGISTRY :VERSION 1 ...>
```

If a request parameter has the type `new_id` with
an unknown type (such as the `id` parameter in `wl_registry::bind`), then the
caller needs to provide the class name of the interface and version of the new proxy:

```
;; wl_registry::bind(name: uint, id: new_id)
* (wl-registry.bind registry name 'wl-seat 8)
#<WL-SEAT :VERSION 8 ...>
```

Proxies are destroyed by the client via the generic function **destroy-proxy**. *Destructor* requests adds a method so that the proxy sends its respective message before it is marked as destroyed. The `<interface>.<request>` style function still exists, but only as a synonym to **destroy-proxy**:

```lisp
;; Implicitly sends the destructor message wl_shm_pool::destroy().
* (destroy-proxy shm-pool)
* shm-pool
#<WL-DESTROYED-PROXY ...>
```

## Handling Events

Libwayland handles events by associating proxies with Listeners, structs with a
callback function for each type of event, including a `void*` parameter to
accept relevant data, a workaround for the lack of closures in C.

Instead, Wayflan's "listeners" are just a list of functions directly accessible
by the accessor **wl-proxy-hooks**. The function's first argument is the event
name, followed by a variadic number of event arguments:

```lisp
* (push (lambda (event-name &rest event-args)
          (format t "Proxy received event ~S with args ~S~%"
                  event-name event-args))
        (wl-proxy-hooks proxy))
(#<FUNCTION (LAMBDA (EVENT-NAME &REST EVENT-ARGS))> ...)
```

The sister macros `evlambda`, `evclambda`, and `evelambda` return a function that dispatches events based on each given clause:

```lisp
(push (evlambda
        (:down (serial time surface id x y)
          (declare (ignore serial time id))
          (format t "Pointer down on surface ~S at ~D ~D~%" surface x y))
        (t (name &rest args)
          (format t "Misc event: ~S ~S~%" name args)))
      (wl-proxy-hooks touch))

;; Is the same as...
(push (lambda (&rest event)
        (case (first event)
          (:down
           (destructuring-bind (serial time surface id x y) (rest event)
             (declare (ignore serial time id))
             (format t "Pointer down on surface ~S at ~D ~D~%" surface x y)))
          (t
           (destructuring-bind (name &rest args) event
             (format t "Misc event: ~S ~S~%" name args)))))
      (wl-proxy-hooks touch))
```

Aimed for `defun`s, Another triplet, `event-case`, `event-ccase`, and
`event-ecase`, accesses an event directly and can live under a lambda. Currying
a function may preserve some benefits of closures, plus recompiling toplevel
functions will immediately affect all proxies it lives under:

```lisp
(defun handle-pointer (app recipient &rest event)
  (event-case event
    (:down
     (destructuring-bind (serial time surface id x y) (rest event)
       (declare (ignore serial time id))
       (format t "Pointer down on surface ~S at ~D ~D~%" surface x y)))
    (t
     (destructuring-bind (name &rest args) event
       (format t "Misc event: ~S ~S~%" name args)))))

...

(push (alexandria:curry 'handle-pointer my-app pointer)
      (wl-proxy-hooks pointer))
```

## From Zero to Globals

This code snippet connects to a Wayland server, grabs the registry, and then
listens for a global event that broadcasts a `wl_compositor` global:

```lisp
(with-open-display (display)
  (let (registry compositor)
    (setf registry (wl-display.get-registry display))
    ;; Add a closure that listens for wl_registry.global events
    (push (evlambda
            (:global (name interface version)
             (when (string= interface "wl_compositor")
               (setf compositor (wl-registry.bind
                                  registry name 'wl-compositor 1)))))
          (wl-proxy-hooks registry))

    ;; Process all events up to this point
    (wl-display-roundtrip display)

    ;; !! Use the compositor and any other globals here
    (print compositor)))
```

Alternatively instead of matching for the string name, you can match on the
interface class name instead:

```lisp
(push (evlambda
        (:global (name interface version)
         (when (find-interface-named interface)
           (case (class-name (find-interface-named interface))
             (wl-compositor
               (setf compositor (wl-registry.bind
                                  registry name 'wl-compositor 5)))
             (wl-shm
               (setf shm (wl-registry.bind
                           registry name 'wl-shm 1)))))))
      (wl-proxy-hooks registry))
```

## Marshalling Enums

In Wayflan, enums live as keywords rather than numeric values:

```lisp
;; wl_output::transform::flipped = 4
(wl-surface.set-buffer-transform surface :flipped)

(push (evlambda
        (:format (format)
          (when (eq format :argb8888)
            (format t "Shm supports 32-bit ARGB format~%"))))
      (wl-proxy-hooks wl-shm))
```

Sometimes it's required to marshal enums manually, for example when passing
around arrays of enum values. Wayflan provides two functions to do that:

```lisp
(wl-enum-value 'wl-shm.format :c8) ;; => #x20203843
(wl-enum-keyword 'wl-shm.format 1) ;; => :XRGB8888
```

Bitfield enums emphasize that the bits of a value are significant, rather than
the numeric equivalence of a value. Wayflan interprets bitfield enums to mean
that, if all the bits in an enum member are set in a value, that keyword is a
member of the list. If the enum member's value is 0, however, that keyword is
only included when all bits are clear, returning a singleton list of that
keyword.

**Keyword => Number:**
```lisp
(wl-enum-value 'wl-shell-surface.resize (:none))                 ;; => 0
(wl-enum-value 'wl-shell-surface.resize ())                      ;; => 0
(wl-enum-value 'wl-shell-surface.resize '(:top))                 ;; => 1
(wl-enum-value 'wl-shell-surface.resize '(:top :left))           ;; => 5
(wl-enum-value 'wl-shell-surface.resize '(:top :left :top-left)) ;; => 5
(wl-enum-value 'wl-shell-surface.resize '(:top :left :top-left)) ;; => 5
```

**Number => Keyword:**
```lisp
(wl-enum-keyword 'wl-shell-surface.resize 0) ;; => (:none)
(wl-enum-keyword 'wl-shell-surface.resize 1) ;; => (:top)
(wl-enum-keyword 'wl-shell-surface.resize 5) ;; => (:top :left :top-left)
```

### [WIP] Adding New Protocols

Outside the core Wayland protocol, Wayflan bundles 3 other stable protocols
from libwayland in their own separate packages:

- `wayflan-client.presentation-time`
- `wayflan-client.viewporter`
- `wayflan-client.xdg-shell`

Wayflan provides the ASDF component `:wayflan-client-impl` to add protocol
bindings to a package:

```lisp
(defsystem foo
  :defsystem-depends-on (#:wayflan-client)
  :components ((:file "package")
               ;; :wayflan-client-impl expects an XML document called "so-and-so-unstable-v1.xml"
               ;; and generates bindings within the package #:FOO-PACKAGE.
               (:wayflan-client-impl "so-and-so-unstable-v1"
                                     :in-package "foo-package"
                                     :depends-on ("package"))))
```

**WIP:** This is currently an early pass at including protocols. I want to
iterate on this in the future.

## Managing Shared Memory and File Descriptors

Wayflan passes fd's around as integers, and leaves it to the user to determine
how to create shm objects, map fd's to memory, and the like.

I developed the [posix-shm](https://git.sr.ht/~shunter/posix-shm/) project to
shm objects, and is developed in lock-step to the Wayflan examples.
