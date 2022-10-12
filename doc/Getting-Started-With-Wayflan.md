# Getting Started with Wayflan

This document target those familiar with Wayland fundamentals, and aims to
explain how each Wayland concept integrates into Wayflan.

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

Bitfield enums emphasize that the bits of a value are significant, rather than
the numeric equivalence of a value. Wayflan interprets bitfield enums to mean
that, if all the bits in an enum member are set in a value, that keyword is a
member of the list. If the enum member's value is 0, however, that keyword is
only included when all bits are clear, returning a singleton list of that
keyword.

**Keyword => Number:**
```lisp
;; wl_shell_surface::resize
(:none)                => 0
()                     => 0
(:top)                 => 1
(:top :left)           => 5
(:top-left)            => 5
(:top :left :top-left) => 5
```

**Number => Keyword:**
```lisp
;; wl_shell_surface::resize
0 => (:none)
1 => (:top)
5 => (:top :left :top-left)
```

### [WIP] Adding New Protocols

Outside the core Wayland protocol, Wayflan bundles 3 other stable protocols
from libwayland in their own separate packages:

- `wayflan-client.presentation-time`
- `wayflan-client.viewporter`
- `wayflan-client.xdg-shell`

The package `wayflan-autowrap` enables users to integrate other protocols with
Wayflan via XML files:

```lisp
(defpackage #:super-protocol
  (:use #:cl #:wayflan-demo))
(in-package #:super-protocol)

(wayflan-autowrap:wl-client-include
  '(#:super-system "zsuper-protocol-v1.xml"))
```

There are four possible input types for `wl-client-include`:

- (**Recommended**) an ASDF component pathname, starting with the system name,
  followed by each (sub)component name ending with a `:static-file` XML file
  component.
- A pathname to an XML file.
- An String containing an XML document.
- A input stream to an XML document.

**WIP:** I ultimately want to move this API to be primarily an ASDF component,
but to keep this as a separate living API to let lisp hackers add protocols
without fiddling with defsystem files.

## Managing Shared Memory and File Descriptors

Wayflan passes fd's around as integers, and leaves it to the user to determine
how to create shm objects, map fd's to memory, and the like.

I developed the [posix-shm](https://git.sr.ht/~shunter/posix-shm/) project to
shm objects, and is developed in lock-step to the Wayflan examples.
