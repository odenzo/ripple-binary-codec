
# ripple-binary-codec
[ ![Download](https://api.bintray.com/packages/odenzooss/maven/ripple-binary-codec/images/download.svg
) ](https://bintray.com/odenzoos/maven/ripple-binary-codec/0.1.3/link)
[![Build Status](https://travis-ci.com/odenzo/ripple-binary-codec.svg?branch=master)](https://travis-ci.com/odenzo/ripple-binary-codec)
[![codecov](https://codecov.io/gh/odenzo/ripple-binary-codec/branch/master/graph/badge.svg)](https://codecov.io/gh/odenzo/ripple-binary-codec)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/0ec6db4a57fc4de98a9f52f80a39dc1a)](https://www.codacy.com/app/odenzo/ripple-binary-codec?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=odenzo/ripple-binary-codec&amp;utm_campaign=Badge_Grade)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

This library is for binary encoding Ripple transactions, with some decoding for debugging purposes.
The primary use-case is to implement local signing, multi-signing and verification of Ripple transactions.

It is a developer library, most users will want to use https://github.com/odenzo/ripple-local-signing to do the signing.

Cross-compiled to Scala 2.12 and Scala 2.13 currently, migrating to JDK11

1. JSON and serializes into Ripple binary format, e.g. TxBlob
2. De-serializes Ripple binary encoded – back to a TxBlob style or broken down to hex fields and subfields for debugging

  
Best place to start on documentation is:
https://xrpl.org/serialization.html

Note: Please "star" on GitHub if you are using, and I will switch to semantic versioning and keeping past versions
 around.
 
 

### Status

Works well enough, and plus/minus is that it is written very simply, so serves as a good example/reference to the
 gotchas. 

- This is "production" ready in that it works reliably and correcltly.
- Serializes for signing (and serialization) on the order of 30 tx_json / ms on a slow machine
- Any exceptions generated are captured, so API is "pure function" in Scala sense.
- Functional serialization, works on a good set of test cases.
- Deserialization useful for development and debugging work
- Code is getting cleaner;  but not optimized for speed and some debris left. 




### Testing

There are some unit tests, but primary testing is done using trace logs of requests / responses from Ripple TestNet
 and production server.

## Quick Start

Published under Bintray for now, so in sbt use via:

```
   // Where version is the value in the badge above, e.g. "0.1.3" 
   resolvers in ThisBuild += Resolver.bintrayRepo("odenzooss", "maven"),
   libraryDependencies += "com.odenzo" %% "ripple-binary-codec" % version
```

The API is in   `com.odenzo.ripple.bincodec.RippleCodecAPI` and provides the routines to 

- Serializize a JsonObject (String form) to non HashPrefixed
    * TxBlob  (aka serialize ll serialization fields)
    * SigningTxBlob (serialize for Signing, this is binary that is signed to TxnSignature)
    
- Serialize a Ripple address in Base58Check format, no Variable Length encoding. This 
is useful for implementing SignFor / Multi-signing.

e.g. 
```scala          
     val jsonTxt   =  "..."   // Just the tx_json field value
     for {
      json    <- io.circe.parser.parse(jsonTxt)
      txblob  <-  com.odenzo.ripple.bincodec.RippleCodecAPI.signingTxBlob(json)
     } yield txblob        

```
Other API are useful but have more specific requirements, e.g. using Circe JsonObject.
The returned errors are of typer RippleCodecError which extends Throwable.
There are also some Cats "show" implicits defined in `bincodec.syntax` package.
These are likely to move to their companian objects. 

Other than that the API should be stable, additional Java, ScalaJS and perhaps Scala Native may be added.
  
    


## Differences from Ripple Encoding

### Issued Amounts (aka Fiat Amount, IOU Amount)


https://github.com/odenzo/ripple-binary-codec/issues/11

XRPL rounds/truncates some amounts with more than 15 significant digits, currently I reject these with error.



## TODOs

1) Cross Compile to ScalaJS 
4) Internal: clean-up Scribe logging and other internal test utils
6) Clean-up code on rainy days
