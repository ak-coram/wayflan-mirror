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

# Protocol Classes

## [Class] __wl-protocol__

**Readers:**

- **Wl-name** => *string*
- **wl-copyright** => *string or null*
- **wl-description** => *wl-description or null*
- **wl-interfaces** => *wl-interface list*

## [Class] __wl-interface__

**Readers:**

- **wl-name** => *string*
- **wl-version** => *wl-uint*
- **description** => *wl-description or null*
- **requests** => *wl-request sequence*
- **events** => *wl-event sequence*
- **enums** => *wl-enum sequence*

## [Class] __wl-request__

**Description:**

Represents a message from a client to the compositor

**Readers:**

- **wl-name** => *string*
- **wl-type** => *:destructor or null*
- **wl-since** => *wl-uint* -- The version of the interface since the request was introduced
- **wl-description** => *wl-description or null*
- **wl-args** => *wl-arg list*

## [Class] __wl-event__

**Description:**

Represents a message from the compositor to a client

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
