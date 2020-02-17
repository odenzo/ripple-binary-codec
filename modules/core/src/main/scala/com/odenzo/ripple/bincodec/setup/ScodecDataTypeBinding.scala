package com.odenzo.ripple.bincodec.setup

import cats._
import cats.data._
import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}
import scodec.{Attempt, DecodeResult}
import scodec.bits.BitVector
import _root_.scodec.codecs.variableSizeBytes
import com.odenzo.ripple.bincodec.models.{XRPLAmount, XRPLPathSet}
import com.odenzo.ripple.bincodec.scodecs.VL
import shapeless.HMap
import spire.math.ULong
//import io.circe.spire._
import com.odenzo.circe.spire.SpireCodecs._

/** All of the known XRPL data types, bound to some scodec */
object ScodecDataTypeBinding {
  import com.odenzo.ripple.bincodec.scodecs.BasicScodecs._
  import com.odenzo.ripple.bincodec.scodecs.PathSetScodecs._
  import com.odenzo.ripple.bincodec.scodecs.AccountScodecs._
  import com.odenzo.ripple.bincodec.scodecs.AmountScodecs._
  import com.odenzo.ripple.bincodec.scodecs.AdditionalScodecs._
  import com.odenzo.ripple.bincodec.scodecs.STObjectScodec._

//
//  // Going to need an HList I think
//  def binding =
//    HMap(
//      "UInt16"      -> xrpuint16,
//      "Transaction" -> xrpError("Transaction NIMP"),
//      "PathSet"     -> xrplPathSet,
//      "Validation"  -> xrpError("Validation NIMP"),
//      "LedgerEntry" -> xrpError("LedgerEntry NIMP"),
//      "STArray"     -> xrpError("LedgerEntry NIMP"),
//      "Vector256"   -> xrpvectorhash256,
//      "NotPresent"  -> xrpError("NotPresent Data Type"),
//      "AccountID"   -> xrplAccount,
//      "UInt8"       -> xrpuint8,
//      "UInt32"      -> xrpuint32,
//      "Hash128"     -> xrphash128,
//      "Blob"        -> xrpblob,
//      "Done"        -> xrpError("DONE datatype not understood"),
//      "Amount"      -> xrpamount, // XRP or Fiat Amount
//      "Hash256"     -> xrphash256,
//      "Unknown"     -> xrpError("Unknown Data Type"),
//      "Hash160"     -> xrphash160,
//      "UInt64"      -> xrpulong64,
//      "STObject"    -> xrpError("STOBject NIMP")
//    )

  // Goig to need Aux and Poly functions for encoding.
  // The alternative is binding JSON codecsto xrpXXX scodecs.
  // First decoding because shapeless makes my head hurt
  // Explicit x=> needed for type inference
  def dynamicDecode(bv: BitVector, typename: String): Attempt[DecodeResult[Json]] = {

    typename match {
      case "UInt16"        => xrpuint16.decode(bv).map(x => transform2Json(x)) // Int
      case "Transaction"   => xrplTransactionType.decode(bv).map(x => transform2Json(x)) // String/Int
      case "PathSet"       => xrplPathSet.decode(bv).map(x => transform2Json(x))
      case "Vector256"     => xrpvectorhash256.decode(bv).map(x => transform2Json(x)) // String
      case "AccountIdNoVL" => xrplAccount.decode(bv).map(x => transform2Json(x))
      case "AccountIdVL"   => variableSizeBytes(VL.xrpvl, xrplAccount).decode(bv).map(x => transform2Json(x))
      case "UInt8"         => xrpuint8.decode(bv).map(x => transform2Json(x))
      case "UInt32"        => xrpuint32.decode(bv).map(x => transform2Json(x))
      case "Hash128"       => xrphash128.decode(bv).map(x => transform2Json(x))
      case "Blob"          => xrpblob.decode(bv).map(x => transform2Json(x)) // String
      case "Amount"        => xrplAmount.decode(bv).map(x => transform2Json(x)) // XRP or Fiat Amount
      case "Hash256"       => xrphash256.decode(bv).map(x => transform2Json(x)) // String
      case "Hash160"       => xrphash160.decode(bv).map(x => transform2Json(x)) // String
      case "UInt64"        => xrpulong64.decode(bv).map(x => transform2Json(x)) // ULong
      case "STObject"      => xrpstobject.decode(bv).map(x => transform2Json(x)) // List(Json->Json) not JSonObjectYet
      case "STArray"       => xrpstarray.decode(bv).map(x => transform2Json(x)) // List (Json->Json)

      case "Unknown"     => xrpError[Int]("Unknown Data Type").decode(bv).map(x => transform2Json(x)) // Dummy
      case "Validation"  => xrpError[Int]("Validation NIMP").decode(bv).map(x => transform2Json(x)) // Int
      case "LedgerEntry" => xrpError[Int]("LedgerEntry NIMP").decode(bv).map(x => transform2Json(x)) // Int
      case "Done"        => xrpError[Int]("DONE datatype not understood").decode(bv).map(x => transform2Json(x))
      case "NotPresent"  => xrpError[Int]("NotPresent Data Type").decode(bv).map(x => transform2Json(x))

    }
  }

  def dynamicEncode(json: Json, typename: String): Attempt[BitVector] = {

    typename match {
      case "UInt16"        => xrpuint16.encode(fromJson[Int](json))
      case "Transaction"   => xrplTransactionType.encode(fromJson[String](json))
      case "PathSet"       => xrplPathSet.encode(fromJson[XRPLPathSet](json))
      case "Vector256"     => xrpvectorhash256.encode(fromJson[String](json)) // String
      case "AccountIdNoVL" => xrplAccount.encode(fromJson[String](json))
      case "AccountIdVL"   => variableSizeBytes(VL.xrpvl, xrplAccount).encode(fromJson[String](json))
      case "UInt8"         => xrpuint8.encode(fromJson[Int](json))
      case "UInt32"        => xrpuint32.encode(fromJson[Int](json))
      case "Hash128"       => xrphash128.encode(fromJson[String](json))
      case "Blob"          => xrpblob.encode(fromJson[String](json)) // String
      case "Amount"        => xrplAmount.encode(fromJson[XRPLAmount](json)) // XRP or Fiat Amount
      case "Hash256"       => xrphash256.encode(fromJson[String](json)) // String
      case "Hash160"       => xrphash160.encode(fromJson[String](json)) // String
      case "UInt64"        => xrpulong64.encode(fromJson[ULong](json)) // ULong
      case "STObject"      => xrpstobject.encode(fromJson[JsonObject](json)) // List(Json->Json) not JSonObjectYet
      // case "STArray"     => xrpstarray.encode(fromJson[Int](json)) // List (Json->Json)

      // case "Validation"  => xrpError[Int]("Validation NIMP").encode(json2(json)) // Int
      // case "LedgerEntry" => xrpError[Int]("LedgerEntry NIMP").encode(json2(json)) // Int
      //  case "Done"        => xrpError[Int]("DONE datatype not understood").encode(json2(json))

      case other => throw new IllegalArgumentException(s"Encoding Type [$other] not known.")

    }
  }

  def fromJson[T: Decoder](json: Json): T = json.as[T] match {
    case Right(v) => v
    case Left(err) =>
      scribe.error(s"Could not decode JSON ${json.spaces4}", err)
      throw new Exception(s"Decoding $json ", err)
  }

  def transform2Json[T: Encoder](rs: DecodeResult[T]): DecodeResult[Json] = {
    import io.circe.syntax._
    rs.map { x: T =>
      val j = x.asJson
      scribe.info(s"Decoded Model Value $x as Json \n ${j.spaces4}")
      j
    }
  }

}
