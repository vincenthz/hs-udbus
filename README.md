hs-udbus
========

The udbus haskell package provides a BSD implementation of the DBus protocol in
pure Haskell, giving access to a simple lowlevel (Network.DBus.Actions) interface
and a simple high level interface with a simple mainloop dispatcher (Network.DBus)

The interfaces are coherent and basic, sacrificing some ease of use (in using
more appropriate haskell types for dbus array for example) in favor of being
able to write generated code easily.

The code doesn't check if the object values (objectpath, interface, etc)
are appropriates on sending, nor on receiving. The dbus daemon should have
prevented invalid values to be received, and for sending, the user need to take
extra care of the values sent.

TODO & Caveats
--------------

Dbus endianness is not handle properly to serialize message if the user choose
big endian. User should always select little endian regardless of the actual
machine architecture. However, receiving message in both endianness is supported.

Double are expected to be handled as IEEE754 by the compiler. Serialization of double
is handled in a GHC specific way (making the package GHC specific), expecting
that any architecture represent double the IEEE754 way. So far, this seems to be
true for every architecture supported by GHC.

Stability
---------

While the low level interfaces should remains the same, it's not unexpected that
some high level interface would be grafted on top of the actual interfaces.

Example
-------

A simple example on how to use the interfaces is available at the root of the
package as DBus.hs
