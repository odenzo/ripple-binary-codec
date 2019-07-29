
# ripple-binary-codec

[ ![Download](https://api.bintray.com/packages/odenzoorg/odenzooss/ripple-binary-codec/images/download.svg) ](https://bintray.com/odenzoorg/odenzooss/ripple-binary-codec/_latestVersion)
[![Build Status](https://travis-ci.com/odenzo/ripple-binary-codec.svg?branch=master)](https://travis-ci.com/odenzo/ripple-binary-codec)
[![codecov](https://codecov.io/gh/odenzo/ripple-binary-codec/branch/master/graph/badge.svg)](https://codecov.io/gh/odenzo/ripple-binary-codec)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/0ec6db4a57fc4de98a9f52f80a39dc1a)](https://www.codacy.com/app/odenzo/ripple-binary-codec?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=odenzo/ripple-binary-codec&amp;utm_campaign=Badge_Grade)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

This project is a library written in Scala for use with Ripple XRP ledger.
Cross-compiled to Scala 2.12.8 and Scala 2.13.0 currently.
It takes 

1. JSON and serialization into Ripple binary format, e.g. TxBlob
2. De-serializes Ripple binary encoded – back to a TxBlob style or broken down to hex fields and subfields for 
debugging

  
Best place to start on documentation is:
 https://developers.ripple.com/serialization.html

Main use case is just signing and multi-signing transactions locally.

External API is in RippleCodecAPI and returns encoded values now, and byte array form.

## Differences from Ripple Signing

### Issued Amounts (aka Fiat Amount, IOU Amount)
  [[https://xrpl.org/currency-formats.html#issued-currency-math]] defines
This is defined with 15-digits of precision and min and max ranges.
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
On the two do list - low priority.

## Setup

Binaries are hosted at BinTray, using SBT, update the version based on download badge above.

```
     resolvers +=   Resolver.bintrayRepo("odenzooss", "maven")
     libraryDependencies +=  "com.odenzo" %% "ripple-binary-codec" % "0.2.4" // Or whatever is in the badge above :-) 
        
```

## Status

- Functional serialization, works on a pretty good set of test cases.
- Deserialization useful for development and debugging work
- Code is getting cleaner, but not optimized. 



## TODOs

* Add more test cases, particularly of Ripple transactions
* Cross-compile to Javascript
* Maven publish



# Release Notes

## 0.2.6
Added cross-scala build

## 0.2.5

+ Test routines with SignFor multisign serialization
+ Addition of RippleCodecAPI.serializeAddress for use by local-signing for multisignature signing. 
