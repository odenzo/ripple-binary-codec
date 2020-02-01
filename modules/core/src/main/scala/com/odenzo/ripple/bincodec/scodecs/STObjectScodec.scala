package com.odenzo.ripple.bincodec.scodecs

import cats.implicits._
import io.circe.Json
import scodec.bits._
import scodec._
import scodec.codecs._

/**
  * Essentially this is just going done the list of fields, and dealing with any prefix/suffix needed
  * e.g. end of object marker
  * The problem seems to be that the end of array and end of object markers can occur in fields/data
  * So we have to take the multiplexed approach
  */
trait STObjectScodec {

  import FieldScodec.xrpfield
  import _root_.cats.data._
  import _root_.cats.implicits._
  // @todo When encoding make sure to place objects in canonical order (prior to feeding json?)
  // Thing to test here is wether the objectEndMarker can be eagerly searched for, or can be present in the fields.
  // Not sure what variableSizeDelimited or vectorDelimited()
  // The vector may have veriable sized members. Read the source code.

  case class MyState(bv: BitVector, acc: List[(Json, Json)])
  // /I think we have to make a

  /** The arrays are not delimeted, the contents are just fieldId ~ fieldValue. The Fields may contain the delimeter of end or array
    * field */
  val xrparrayDec = Decoder[List[(Json, Json)]](delimitedDynamicList(_, constant(hex"f1")))

  /** This decodes an object which is the contents of a field. Similar to array each entry is a field */
  val xrpobjectDec = Decoder[List[(Json, Json)]](delimitedDynamicList(_, constant(hex"e1")))

  val xrpobjectEnc: Encoder[List[(Json, Json)]] = fail(Err("ST Object Encoder Not Done")).asEncoder
  val xrparrayEnc: Encoder[List[(Json, Json)]]  = fail(Err("ST Array Encoder Not Done")).asEncoder

  val xrpstarray: Codec[List[(Json, Json)]]  = Codec(xrparrayEnc, xrparrayDec)
  val xrpstobject: Codec[List[(Json, Json)]] = Codec(xrpobjectEnc, xrpobjectDec)

  /** This is for a List of A where the *entire* list is delimited but the individual A variable size/type.
    * Each A is decoded to a (Json,Json) tyuple to avoid type-dependant functions. */
  def delimitedDynamicList(bv: BitVector, delimiter: scodec.Decoder[Unit]): Attempt[DecodeResult[List[(Json, Json)]]] = {
    // This is really a loop of get  END OF ARRAY | field

    // DecodeResult[A,BitVector]
    val initialState: MyState = MyState(bv, List.empty)
    val stateFn: State[MyState, Option[(Json, Json)]] = State[MyState, Option[(Json, Json)]](s =>
      getNextField(delimiter)(s.bv) match {
        case Left(remainder) => (s.copy(bv = remainder), None)
        case Right(dcr)      => (MyState(dcr.remainder, dcr.value :: s.acc), dcr.value.some)
      }
    )
    val result: (MyState, Option[(Json, Json)]) =
      stateFn.run(initialState).iterateWhile((a: (MyState, Option[(Json, Json)])) => a._2.isDefined).value

    val finalState: MyState = result._1
    val ans                 = Attempt.successful(DecodeResult(finalState.acc, finalState.bv))
    ans
  }

  /** Returns the next field in the array or Left is the delimeter is found outside a field
    * To avoided dependant types the decoding is done to JSON This consumes the delimeter too. */
  def getNextField[A](delimiter: Decoder[A])(bv: BitVector): Either[BitVector, DecodeResult[(Json, Json)]] = {
    delimiter.decode(bv) match {
      case Attempt.Successful(res) => Left(res.remainder)
      case Attempt.Failure(cause)  => xrpfield.decode(bv).require.asRight
    }

  }

}

object STObjectScodec extends STObjectScodec
