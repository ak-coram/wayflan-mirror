# API Reference - Wayflan Client

## Constants

These constants represent the ranges within Wayland's numeric types: signed and
unsigned integers, and fixed-precision numbers.

- **+most-positive-wl-uint+**
- **+most-positive-wl-int+**
- **+most-positive-wl-fixed+**
- **+most-negative-wl-int+**
- **+most-negative-wl-fixed+**

## [Metaclass] __wl-interface-class__ (__standard-class__)

**Description:**

All interface classes, such as **wl-display**, are an instance of this class.

**Readers:**

- **wl-interface-version** => *number* - The latest supported version
- **wl-interface-name** => *string* - The name as known by the __wl-registry__

## [Function] **find-interface-named** *name* => *interface-class*

**Arguments and Values:**

- *name* -- a __string__.
- *interface-class* -- a __wl-interface-class__ or **nil**.

**Description:**

Return the interface class known by *name* by the __wl-registry__. If there is
no value, return **nil**.

## [Class] __wl-proxy__ ()

**Description:**

A representation of a resource in the Wayland compositor. All subclasses are an
instance of __wl-interface-class__ except __wl-destroyed-proxy__.

**Readers:**

- **wl-proxy-id** => *number* -- the proxy's numeric ID.
- **wl-proxy-display** => *display* -- the display the proxy is connected by.
- **wl-proxy-version** => *number* -- the proxy's bound version.

## [Accessor] **wl-proxy-hooks** *proxy* => *list*

**Arguments and Values:**

- *proxy* - a __wl-proxy__.
- *list* - a __list__ of __functions__.

**Description:**

A list of functions that are called whenever an event is dispatched to this
proxy.

A valid function accepts an event name as the first parameter, followed by a
variadic number of arguments. The number of total arguments depends on the type
of event.

## [Class] __wl-destroyed-proxy__ (__wl-proxy__)

**Description:**

An invalid proxy. Represents a proxy that has been **proxy-destroy**ed and should no longer be operated on.

## [Class] __wl-display__ (__wl-proxy__)

**Description:**

The core global object. This is a special singleton object. It is used for
internal Wayland protocol features.

**Readers:**

- **wl-display-pathname** - The pathname to the socket the display is connected
  to.

## [Function] **find-proxy** *display id* => *proxy*

**Arguments and Values:**

- *display* -- a __wl-display__.
- *id* -- a __positive integer__.
- *proxy* -- a __wl-proxy__, or **nil**.

**Description:**

Find and return a proxy known by the Wayland compositor by that name.

## [Generic Function] **destroy-proxy** *proxy*

**Arguments and Values:**

- *proxy* -- a __wl-proxy__.

**Description:**

Mark the proxy for destruction and reclaim its ID for use by another proxy.
Destructor requests specialize on **destroy-proxy** to send the compositor the
request message.

## [Condition] __wl-error__ (__error__)

**Description:**

Signaled when a fatal error has occurred.

- **wl-error-object** => *number* -- the object id
- **wl-error-code** => *number* -- the error code
- **wl-error-message** => *string* -- the error message

## [Function] **display-pathname** *&optional name* => *pathname*

Return the absolute pathname to a Wayland compositor socket given the name of
the connection.

## [Function] **wl-display-connect** *&optional name* => *display*

Connect to a Wayland compositor and return a __wl-display__ owning the
connection.

## [Function] **wl-display-listen** *display* => *boolean*

Return whether there is a (partial) message available from the display.

## [Function] **wl-display-dispatch-event** *display*

Read and dispatch the display's next event.

## [Function] **wl-display-roundtrip** *display*

Block and dispatch events until all requests up to this point have been
finalized.

## [Macro] **with-open-display** *(display &rest options) &body body* => *result*

**Arguments and Values:**

- *display* -- a __symbol__.
- *options* -- __forms__, evaluated
- *body* -- an __implicit progn__.
- *result* -- the __values__ returned by *body*.

**Description:**

Run *body* under a display, return a value, and then close the display.

When execution exits the *body*, the display is closed, no matter whether the
exit was normal or abnormal.

## [Macro] **with-proxy** *(var proxy) &body body* => *result*

**Arguments and Values:**

- *var* -- a __symbol__.
- *proxy* -- a __form__, evaluated
- *body* -- an __implicit progn__.
- *result* -- the __values__ returned by *body*.

**Description:**

Bind *var* to *proxy*, evaluate the *body*, return a value, and destroy
*proxy*.

The *proxy* is destroyed when execution exits the *body*, no matter whether the
exit was normal or abnormal.

## [Macro] **event-case**, **event-ccase**, **event-ecase** *event &body clauses*

`clause := (event-name (destructuring-lambda-list...) body...)`

**Description:**

Select the clause by matching the *event-name* with the event type. If a clause
is selected, bind the event arguments by the *destructuring-lambda-list* and
run the clause *body*.

## [Macro] **evlambda**, **evclambda**, **evelambda** *&body clauses*

`clause := (event-name (destructuring-lambda-list...) body...)`

**Description:**

Return a lambda that accepts an event as specified in **wl-proxy-hooks**.

The lambda selects the clause by matching the *event-name* with the event type.
If a clause is selected, bind the event arguments by the
*destructuring-lambda-list* and run the clause *body*.
