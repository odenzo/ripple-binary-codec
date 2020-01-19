package com.odenzo.ripple.bincodec

import cats.data._
import cats.implicits._
import cats.Show
import cats._
import scodec.bits.ByteVector
import scodec.interop.cats._

trait Encoded {

  /**
    *
    * @return Linearized bytes from this and all nested objects rolled up
    */
  val encoded: List[ByteVector]

}

case class RawValues[T](bytes: List[ByteVector], meta: Option[T] = None) extends Encoded {
  lazy val encoded: List[ByteVector] = bytes
}
case class RawValue[T](ubytes: ByteVector, meta: Option[T] = None) extends Encoded {
  lazy val encoded: List[ByteVector] = List(ubytes)
}

object RawValue {
  implicit def showRaw[T]: Show[RawValue[T]] = Show.show(v => s" [${v.ubytes.toHex}]")
}

object RawValues {
  implicit def showEncNested[T]: Show[RawValues[T]] = Show.show { nev =>
    "\n[Nested]: " +
      nev.bytes.map(bv => bv.show).mkString("\n\t", "\n\t", "\n\n") +
      s"<--[NestdEnd]\n"

  }
}
