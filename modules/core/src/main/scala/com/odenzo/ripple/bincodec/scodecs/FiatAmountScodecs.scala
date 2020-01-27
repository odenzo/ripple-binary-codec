package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._

import com.odenzo.ripple.bincodec.{BCLibErr, BinCodecLibError}
import cats.implicits._
import scodec.bits.{BitVector, ByteVector, _}
import scodec.codecs._

/**
  * https://xrpl.org/currency-formats.html#issued-currency-math
  * ULong max is 18,446,744,073,709,551,615
  * First bit is taken to indicate its not XRP.
  *
  *  54-bit mantissa normalized to (10^15 ,10^16-1)
  *  8-bit exponent which is math exponent encoded +97 (uint)
  */
