package com.odenzo.ripple.bincodec.codecs

import scala.annotation.tailrec

import cats.data.NonEmptyList
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{DefinitionData, FieldData, FieldMetaData}
import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait PathCodecs extends JsonUtils {

  import PathCodecs._

  /** These is really a container. Inside is a list of  datasteps and delimeters. Equalivalent to paths **/
  def encodePathSet(data: Json): Either[BinCodecLibError, EncodedPathSet] = {

    scribe.debug(s"Encoding PathSet/Path:  : \n ${data.spaces4}")
    val pathList: Either[BinCodecLibError, List[Json]] = json2array(data)

    val encodedPaths: Either[BinCodecLibError, List[EncodedNestedValue]] =
      pathList.flatMap((path: List[Json]) => path.traverse(encodePath))

    // PathSet has delimeters betweend the paths

    val withDelimeters: Either[BinCodecLibError, EncodedPathSet] = encodedPaths.fmap { listOfPaths: List[EncodedNestedValue] =>
      if (listOfPaths.nonEmpty) {
        val intercalated: List[Encoded] = listOfPaths.flatMap(a => List(a, anotherPathMarker)).dropRight(1)
        val delimited: List[Encoded]    = intercalated ::: List(endOfPathsMarker) // appeneded 2.13 only
        EncodedPathSet(delimited)
      } else {
        // Not sure what this should be, don't think it will ever occur
        EncodedPathSet(List.empty[Encoded])
      }
    }
    withDelimeters.flatTap(v => scribe.debug(s"Encoded PathSet ${v.show}").asRight)
  }

  /**
    *  Path is an array with anonomous PathStep contents
    * @param json
    */
  def encodePath(json: Json): Either[BinCodecLibError, EncodedNestedValue] = {
    // There are no delimeters at the end of each pathstep in a path
    for {
      arr          <- json2array(json)
      steps        <- arr.traverse(json2object)
      encodedSteps <- steps.traverse(encodePathStep)
      _ = scribe.debug(s"Encoded Path: ${encodedSteps.show}")
    } yield EncodedNestedValue(encodedSteps)
  }

  /** @param json Object containing the account/currency/issuer
    *            @return Encoded Fields, but no pathstep delimineters
    */
  def encodePathStep(json: JsonObject): Either[BinCodecLibError, EncodedNestedValue] = {
    import PathCodecs._
    scribe.debug(s"Encoding Path Step\n ${json.asJson.spaces2}")

    // TODO: Validate currency is not XRP , with special currency encoding TBC

    val fields: List[Option[Json]] = List("account", "currency", "issuer").map(k => json(k))
    import AccountIdCodecs._
    val res: Either[BinCodecLibError, List[RawValue]] = fields match {
      case Some(account) :: None :: None :: Nil => encodeAccountNoVL(account).fmap(List(accountPrefix, _))
      case None :: Some(curr) :: None :: Nil    => MoneyCodecs.encodeCurrency(curr).fmap(List(currencyPrefix, _))
      case None :: None :: Some(issuer) :: Nil  => encodeAccountNoVL(issuer).fmap(List(issuerPrefix, _))

      case None :: Some(curr) :: Some(issuer) :: Nil =>
        for {
          c <- MoneyCodecs.encodeCurrency(curr)
          i <- AccountIdCodecs.encodeAccountNoVL(issuer)
        } yield List(currencyAndIssuerPrefix, c, i)

      case _ => BinCodecLibError("Illegal Path", json.asJson).asLeft
    }

    res.map { v =>
      scribe.debug(s"PathStep Encoded: ${v.show}")
      EncodedNestedValue(v)
    }

  }

  // not tested
  def decodePathSet(v: List[UByte], info: FieldMetaData): Either[Nothing, (DecodedNestedField, List[UByte])] = {
    // Array of Arrays

    def loop(v: List[UByte], acc: List[List[RawValue]]): (List[List[RawValue]], List[UByte]) = {
      val (path, remaining) = decodePath(v)
      if (path.head.ubytes.head === UByte.MinValue) {
        (path.reverse :: acc, remaining)
      } else {
        loop(v, path.reverse :: acc)
      }
    }

    val (lld: List[List[RawValue]], tail) = loop(v, List.empty[List[RawValue]])
    val flat: List[RawValue]              = lld.flatten // Not ideal for no Nested w/o Field yet
    (com.odenzo.ripple.bincodec.DecodedNestedField(info, flat), tail).asRight
  }

  /**
    *
    * @param path
    *
    * @return A list of decoded ubytes for the path, in reverse order.
    */
  def decodePath(path: List[UByte]): (List[RawValue], List[UByte]) = {
    // Composed of N Path Steps , first always has implied account
    val endPathWithMorePaths = UByte(0xFF)
    val endPathAndPathSet    = UByte(0x00)

    @tailrec
    def loop(v: List[UByte], acc: List[RawValue]): (List[RawValue], List[UByte]) = {
      v match {
        case h :: tail if h === endPathWithMorePaths => (RawValue(List(h)) :: acc, tail)
        case h :: tail if h === endPathAndPathSet    => (RawValue(List(h)) :: acc, tail)
        case step =>
          val (decoded, rest) = decodePathStep(v)
          loop(rest, decoded :: acc)
      }
    }

    loop(path, List.empty[RawValue])
  }

  def decodePathStep(v: List[UByte]): (RawValue, List[UByte]) = {
    // Since just going to Hex and address currency and issuer all 20 we cheat
    val stepType = v.head

    val numBits = List(
      (stepType & PathCodecs.kAddressStep) > PathCodecs.kZERO,
      (stepType & PathCodecs.kCurrencyStep) > PathCodecs.kZERO,
      (stepType & PathCodecs.kIssuerStep) > PathCodecs.kZERO
    ).filterNot(_ === true).length

    val len             = 1 + (numBits * 20) // Including the marker
    val (decoded, tail) = v.splitAt(len)
    (RawValue(decoded), tail)
  }

}

object PathCodecs extends PathCodecs {
  val kZERO         = UByte(0)
  val kAddressStep  = UByte(1)
  val kCurrencyStep = UByte(16)
  val kIssuerStep   = UByte(32)

  val accountType: UByte  = UByte(0x01)
  val currencyType: UByte = UByte(0x10)
  val issuerType: UByte   = UByte(0x20)

  val accountPrefix           = RawValue(List(accountType))
  val currencyPrefix          = RawValue(List(currencyType))
  val issuerPrefix            = RawValue(List(issuerType))
  val currencyAndIssuerPrefix = RawValue(List(currencyType | issuerType))

  val anotherPathMarker: RawValue = DefinitionData.pathSetAnother
  val endOfPathsMarker: RawValue  = DefinitionData.pathSetEnd

}
