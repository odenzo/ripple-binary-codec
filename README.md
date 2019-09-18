
# ripple-binary-codec
[ ![Download](https://api.bintray.com/packages/odenzooss/maven/ripple-binary-codec/images/download.svg
) ](https://bintray.com/odenzoos/maven/ripple-binary-codec/0.1.0/link)
[![Build Status](https://travis-ci.com/odenzo/ripple-binary-codec.svg?branch=master)](https://travis-ci.com/odenzo/ripple-binary-codec)
[![codecov](https://codecov.io/gh/odenzo/ripple-binary-codec/branch/master/graph/badge.svg)](https://codecov.io/gh/odenzo/ripple-binary-codec)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/0ec6db4a57fc4de98a9f52f80a39dc1a)](https://www.codacy.com/app/odenzo/ripple-binary-codec?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=odenzo/ripple-binary-codec&amp;utm_campaign=Badge_Grade)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

This library is for binary encoding Ripple transactions, with some decoding for debugging purposes.
The primary use-case is to implement local signing, multi-signing and verification of Ripple transactions.

It is a developer library, most users will want to use https://github.com/odenzo/ripple-local-signing to do the signing.



### Status

Works well enough, and plus/minus is that it is written very simply, so serves as a good example/reference to the
 gotchas. 

- This is "production" ready in that it works reliably, and throws no exceptions.
- Functional serialization, works on a good set of test cases.
- Deserialization useful for development and debugging work
- Code is getting cleaner;  but not optimized for speed and some debris left. 


### Environment
Cross-compiled to Scala 2.12.9 and Scala 2.13.1 currently, tested with OpenJDK9
It takes 

1. JSON and serializes into Ripple binary format, e.g. TxBlob
2. De-serializes Ripple binary encoded – back to a TxBlob style or broken down to hex fields and subfields for debugging

  
Best place to start on documentation is:
https://xrpl.org/serialization.html

### Testing

There are some unit tests, but primary testing is done using trace logs of requests / responses from Ripple TestNet
 and production server.

## Quick Start

Published under Bintray for now, so in sbt use via:

```
   // Where version is the value in the badge above, e.g. "0.3.0" 
   resolvers in ThisBuild += Resolver.bintrayRepo("odenzooss", "maven"),
   libraryDependencies += "com.odenzo" %% "ripple-binary-codec" % version
```

The API is in   `com.odenzo.ripple.bincodec.RippleCodecAPI` and provides the routines to 

- Serializize a JsonObject (String form) to non HashPrefixed
    * TxBlob  (aka serialize ll serialization fields)
    * SigningTxBlob (serialize for Signing, this is binary that is signed to TxnSignature)
    
- Serialize a Ripple address in Base58Check format, no Variable Length encoding. This 
is useful for implementing SignFor / Multi-signing.

Other API are useful but have more specific requirements, e.g. using Circe JsonObject.
The returned errors are of typer RippleCodecError which extends Throwable.
There are also some Cats "show" implicits defined in `bincodec.syntax` package.
These are likely to move to their companian objects. 

Other than that the API should be stable, additional Java, ScalaJS and perhaps Scala Native may be added.
  
    


## Differences from Ripple Encoding

All the test cases pass, but the following are "problem" areas that may arise.

### Issued Amounts (aka Fiat Amount, IOU Amount)
https://xrpl.org/currency-formats.html#issued-currency-math defines the "spec" for these.

Defined with 15-digits of precision and min and max ranges.
Testing has shown that messages with more than 15 digits of precision are round/truncated.
This is unlikely to occur in most cases, but I am not comfortable with it.
All amounts with more than 15 digits of precision are rejected by the binary-encoder for now.
If I figure out the precise behaviour of RippleD may change this.
For now a hack with BigDecimal precision which treated 1234.00 as precision 6 (trailing zeros are significant digits)s 

Examples:
 - 9223372036854775807 is SignRq is returned as 9223372036854775e3 in response)
 - 18014398509481989 =>   1801439850948198e1 

Mantissa is 54 bit unsigned, everything after that seems to be truncated.

Tidy up and correct error messages pending for Issued Amounts, see IssuedAmountCodec

### 160-bit hex encoded Currencies not tested or supported



## TODOs

1) Cross Compile to ScalaJS 
2) After that start benchmark and optimization
    * Optimize data structures perhaps (currently List[UByte] everywhere) and concatenation.
3) Adding Hex encoded currencies
4) Internal: clean-up Scribe logging and other internal test utils
5) Low Priority: Re-implement decoding, perhaps with scodec
