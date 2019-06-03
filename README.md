
# ripple-binary-codec

This is essentially a remix of the Ripple Javascript code for binary-codec  in Scala 12.X

Minimal amount of third party libraries, should allow cross-compiling to ScalaJS and maybe Scala Native.

Best place to start on documentation is: https://developers.ripple.com/serialization.html

Main use case is just signing transaction, but should handle more although not tested as much.



External API is in RippleCodecAPI and returns encoded values now, and byte array form.

