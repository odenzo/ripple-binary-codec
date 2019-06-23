
# ripple-binary-codec

[ ![Download](https://api.bintray.com/packages/odenzoorg/odenzooss/ripple-binary-codec/images/download.svg) ](https://bintray.com/odenzoorg/odenzooss/ripple-binary-codec/_latestVersion)
[![Build Status](https://travis-ci.com/odenzo/ripple-binary-codec.svg?branch=master)](https://travis-ci.com/odenzo/ripple-binary-codec)
[![codecov](https://codecov.io/gh/odenzo/ripple-binary-codec/branch/master/graph/badge.svg)](https://codecov.io/gh/odenzo/ripple-binary-codec)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/0ec6db4a57fc4de98a9f52f80a39dc1a)](https://www.codacy.com/app/odenzo/ripple-binary-codec?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=odenzo/ripple-binary-codec&amp;utm_campaign=Badge_Grade)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

This project is a library written in Scala for use with Ripple XRP ledger.
It takes 

1. JSON and serialization into Ripple binary format, e.g. TxBlob
2. De-serializes Ripple binary encoded – back to a TxBlob style or broken down to hex fields and subfields for 
debugging

  
Best place to start on documentation is:
 https://developers.ripple.com/serialization.html

Main use case is just signing transaction, but should handle more although not tested as much.

External API is in RippleCodecAPI and returns encoded values now, and byte array form.


## Setup

Binaries are hosted at BinTray, using SBT, update the version based on download badge above.

```
     resolvers +=   Resolver.bintrayRepo("odenzooss", "maven")
     resolvers +=   Resolver.jcenterRepo   // Will be moving there soon
        
     libraryDependencies +=  "com.odenzo" %% "ripple-binary-codec" % "0.2.1" 
        
```

## Status

- Functional serialization, works on a pretty good set of test cases.
- Deserialization useful for development and debugging work
- Code is getting cleaner, but not optimized. 



## TODOs

* Add more test cases, particularly of Ripple transactions
* Cross-compile to Javascript
* Maven publish
