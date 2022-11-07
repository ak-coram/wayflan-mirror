# API Reference - Wayflan Protocol

This API reference describes all exported symbols for public use that lives
within the package `#:wayflan`. All symbols from this package are used and
re-exported in the package `#:wayflan-client`.

The Wayflan package contains (1) A collection of types and constants related to arguments to Wayland messages, (2) a collection of CLOS classes that describe a Wayland protocol, and (3) the function **wl-parse** that reads an XML document into a __wl-protocol__.

# Types and Constants

- **wl-int** (integers between -2^31 and 2^31-1 inclusive)
- **wl-uint** (integers between 0 and 2^32-1 inclusive)
- **wl-fixed** (reals between -0x800000 and 0x80000000/256 inclusive). When
  sent across the wire, the real is floored to the nearest 1/256.
- **wl-array** (vector of 8-bit unsigned bytes)

- **+most-positive-wl-uint+** = 2^32-1
- **+most-positive-wl-int+** = 2^31-1
- **+most-negative-wl-int+** = 2^31
- **+most-positive-wl-fixed+** = -0x800000
- **+most-negative-wl-fixed+** = 0x80000000/256

# Conditions

## [Condition] __wl-error__ (__error__)

**Description:**

Signaled when a fatal error has happened during Wayland communication.

The conditions __wl-socket-error__, __wl-message-error__, and __wl-server-error__ are all disjoint conditions of __wl-error__.

## [Condition] __wl-socket-error__ (__wl-error__)

**Description:**

Signaled due to an issue in a Wayland connection's underlying socket.

## [Condition] __wl-message-error__ (__wl-error__)

**Description:**

Signaled due to a malformed Wayland message read within a Wayland session.

## [Condition] __wl-server-error__ (__wl-error__)

**Description:**

Signaled due to a Wayland display reading a
[wl_display::error](https://wayland.freedesktop.org/docs/html/apa.html#protocol-spec-wl_display)
event from the server.

# Protocol Classes

## [Class] __wl-protocol__

**Description:**

A protocol (as opposed to *the* Wayland protocol) is a collection of interfaces
and their requests and events all meant to accomplish some shared aim.
Protocols are typically defined in XML documents and are defined by
libwayland's wayland-scanner program.

**Readers:**

- **Wl-name** => *string*
- **wl-copyright** => *string or null*
- **wl-description** => *wl-description or null*
- **wl-interfaces** => *wl-interface list*

## [Class] __wl-interface__

**Description:**

Interfaces consist of requests that a client can invoke as a method, and
requests that a server can emit. All Wayland objects implement one interface.

Interfaces are message-based. Requests are actuated as server-bound messages,
while events are client-bound. Both requests and events have opcodes set by the
order each was defined, and identify which request or event to act on.

**Readers:**

- **wl-name** => *string*
- **wl-version** => *wl-uint*
- **description** => *wl-description or null*
- **requests** => *wl-request sequence*
- **events** => *wl-event sequence*
- **enums** => *wl-enum sequence*

## [Class] __wl-request__

**Description:**

Represents a message from a client to the server

**Readers:**

- **wl-name** => *string*
- **wl-type** => *:destructor or null*
- **wl-since** => *wl-uint* -- The version of the interface since the request was introduced
- **wl-description** => *wl-description or null*
- **wl-args** => *wl-arg list*

## [Class] __wl-event__

**Description:**

Represents a message from the server to a client

**Readers:**

- **wl-name** => *string*
- **wl-type** => *:destructor or null*
- **wl-since** => *wl-uint* -- The version of the interface since the event was introduced
- **wl-description** => *wl-description or null*
- **wl-args** => *wl-arg sequence*

## [Class] __wl-enum__

**Readers:**

- **wl-name** => *string*
- **wl-since** => *wl-uint* -- The version of the interface since the enum was introduced
- **wl-bitfield** => *boolean*
- **wl-description** => *wl-description or null*
- **wl-entries** => *wl-entry sequence*

## [Class] __wl-entry__

**Readers:**

- **wl-name** => *string*
- **wl-value** => *wl-uint*
- **wl-summary** => *string or null*
- **wl-since** => *wl-uint* -- The version of the interface since the entry was introduced
- **wl-description** => *wl-description or null*

## [Class] __wl-arg__

**Readers:**

- **wl-name** => *string*
- **wl-type**
- **wl-summary** => *string or null*

## [Class] __wl-description__

**Readers:**

- **wl-summary** => *string or null*
- **wl-text** => *string*
