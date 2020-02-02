package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._
import cats.implicits._
import cats.implicits._
import io.circe.Json
import scodec.bits._
import scodec.{cats, _}
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

  val xrpstarray: Codec[List[(Json, Json)]] = Codec(xrparrayEnc, xrparrayDec)

  // This does an infinite loop on first field
  val xrpstobject: Codec[List[(Json, Json)]] = Codec(xrpobjectEnc, xrpobjectDec)

  /** This is for a List of A where the *entire* list is delimited but the individual A variable size/type.
    * Each A is decoded to a (Json,Json) tyuple to avoid type-dependant functions. */
  // DecodeResult[A,BitVector]

  def delimitedDynamicList(bv: BitVector, delimiter: scodec.Codec[Unit]): Attempt[DecodeResult[List[(Json, Json)]]] = {
    // This is really a loop of get  END OF ARRAY | field
    scribe.debug(s"Doing dynamic delimited list with delimeter ${delimiter.encode(())} ")

    val initialState: MyState = MyState(bv, List.empty)
    val stateFn: State[MyState, Option[(Json, Json)]] = State[MyState, Option[(Json, Json)]](state => {
      scribe.debug(s"Current State => $state")
      val (newState, out) = getNextField(delimiter)(state.bv) match {
        case Left(remainder) => (state.copy(bv = remainder), None)
        case Right(dcr)      => (MyState(dcr.remainder, dcr.value :: state.acc), dcr.value.some)
      }
      scribe.debug(s"New State $newState")
      scribe.debug(s"New Out $out")
      (newState, out)
    })
    val state0 = stateFn
    scribe.debug(s"Initial State $state0")
    val result: (MyState, Option[(Json, Json)]) = stateFn.iterateWhile(_.isDefined).run(initialState).value

    val finalState: MyState = result._1
    val ans                 = Attempt.successful(DecodeResult(finalState.acc, finalState.bv))
    ans

  }

  /** Returns the next field in the array or Left is the delimeter is found outside a field
    * To avoided dependant types the decoding is done to JSON This consumes the delimeter too. */
  def getNextField[A](delimiter: Decoder[A])(fromBV: BitVector): Either[BitVector, DecodeResult[(Json, Json)]] = {
    scribe.info(s"Getting Next Field in Delimeted Object, bv = ${fromBV.toHex}")
    delimiter.decode(fromBV) match {
      case Attempt.Successful(res) => Left(res.remainder)
      case Attempt.Failure(cause)  => xrpfield.decode(fromBV).require.asRight
    }

  }

}

object STObjectScodec extends STObjectScodec
