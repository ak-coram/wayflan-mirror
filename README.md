# Wayflan

Wayland implementation for clients (TODO: and servers).

## Dependencies

Wayflan requires any Common Lisp implementation with support for ASCII char-codes, gray streams, and CFFI. Just about any "serious" CL system should have this already provided.

Wayflan uses UNIX domain sockets. Since the Wayland protocol's targeted audience is UNIX already, this shouldn't be too much of an issue.
