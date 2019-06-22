package com.odenzo.ripple.bincodec.codecs

import scala.annotation.tailrec

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import spire.math.UByte

import com.odenzo.ripple.bincodec.reference.{DefinitionData, FieldData, FieldInfo}
import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError
import com.odenzo.ripple.bincodec.{DecodedNestedField, EncodedNestedVals, RawValue}

trait PathCodecs  extends JsonUtils {

  /** These is really a container. Inside is a list of  datasteps and delimeters **/
  def encodePathSet(data: FieldData): Either[RippleCodecError, EncodedNestedVals] = {

    // Another array of arrays. List of PathSet, each PathSet has Paths, each Path has  PathSteps

    val pathList: Either[RippleCodecError, List[Json]] = json2array(data.v)

    val another = DefinitionData.pathSetAnother
    val end = DefinitionData.pathSetEnd

    val encodedPaths = pathList.flatMap{ path: List[Json] ⇒
      path.traverse(encodePathStep)
    }

    // No for each of the paths we need to put in deliminers
    encodedPaths.map{ listOfPaths: List[RawValue] ⇒
      val rest: List[RawValue] = listOfPaths.dropRight(1).flatMap(path ⇒ List(path, another))

      val lastPath: RawValue = listOfPaths.takeRight(1).head
      val endList: List[RawValue] = List(lastPath, end)

      val subFields: List[RawValue] = rest ::: endList

      EncodedNestedVals(subFields)

    }
  }

  /** @param json The array surrounding the object **/
  def encodePathStep(json: Json): Either[RippleCodecError, RawValue] = {
    /*
      account by itself
      currency by itself
      currency and issuer as long as the currency is not XRP
      issuer by itself
     */
    scribe.debug(s"Encoding Path Step\n ${json.spaces2}")
    // Another array of arrays
    val arr: Either[RippleCodecError, JsonObject] = json2array(json).map(_.head).flatMap(json2object)

    // In a step the following fields are serialized in the order below
    // FIXME: Move to reference data
    val accountType: UByte = UByte(1)
    val currencyType: UByte = UByte(16)
    val issuerType: UByte = UByte(32)
    val currencyAndIssuer: UByte = currencyType | issuerType

    def combine2(amtType: UByte, amt: RawValue): RawValue = {
      RawValue(amtType +: amt.ubytes)
    }

    def combine3(amtType: UByte, curr: RawValue, issuer: RawValue): RawValue = {
      RawValue(amtType +: (curr.ubytes ++ issuer.ubytes))
    }

    val ans = arr.flatMap{ obj: JsonObject ⇒
      scribe.debug(s"JOBJ: ${obj.asJson.spaces2}")

      val fields: Seq[Option[Json]] = Seq("account", "currency", "issuer").map(k ⇒ obj(k))
      fields match {
        case Seq(Some(account), None, None) ⇒
          AccountIdCodecs.encodeAccountNoVL(account).map(ac ⇒ combine2(accountType, ac))

        case Seq(None, Some(curr), None) ⇒
          MoneyCodecs.encodeCurrency(curr).map(combine2(currencyType, _))

        case Seq(None, None, Some(issuer)) ⇒
          AccountIdCodecs.encodeAccountNoVL(issuer).map(combine2(issuerType, _))

        case Seq(None, Some(curr), Some(issuer)) ⇒
          // TODO: Validate currency is not XRP , with special currency encoding TBC
          (MoneyCodecs.encodeCurrency(curr), AccountIdCodecs.encodeAccountNoVL(issuer))
            .mapN(combine3(currencyAndIssuer, _, _))

      }
    }

    ans

  }


  def decodePathSet(v: List[UByte], info: FieldInfo): Either[Nothing, (DecodedNestedField, List[UByte])] = {
    // Array of Arrays

    def loop(v: List[UByte], acc: List[List[RawValue]]): (List[List[RawValue]], List[UByte]) = {
      val (path, tail) = decodePath(v)
      if (path.head.ubytes.head === UByte.MinValue) {
        (path.reverse :: acc, tail)
      } else {
        loop(v, path.reverse :: acc)
      }
    }

    val (lld: List[List[RawValue]], tail) = loop(v, List.empty[List[RawValue]])
    val flat: List[RawValue] = lld.flatten // Not ideal for no Nested w/o Field yet
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
    val endPathAndPathSet = UByte(0x00)

    @tailrec
    def loop(v: List[UByte], acc: List[RawValue]): (List[RawValue], List[UByte]) = {
      v match {
        case h :: tail if h === endPathWithMorePaths ⇒ (RawValue(List(h)) :: acc, tail)
        case h :: tail if h === endPathAndPathSet    ⇒ (RawValue(List(h)) :: acc, tail)
        case step                                    ⇒
          val (decoded, rest) = decodePathStep(v)
          loop(rest, decoded :: acc)
      }
    }

    loop(path, List.empty[RawValue])
  }

  def decodePathStep(v: List[UByte]): (RawValue, List[UByte]) = {
    val kZERO = UByte(0)
    val kAddressStep = UByte(1)
    val kCurrencyStep = UByte(16)
    val kIssuerStep = UByte(32)

    // Since just going to Hex and address currency and issuer all 20 we cheat
    val stepType = v.head

    val numBits = List(
                        (stepType & kAddressStep) > kZERO,
                        (stepType & kCurrencyStep) > kZERO,
                        (stepType & kIssuerStep) > kZERO
                        ).filterNot(_ === true).length

    val len = 1 + (numBits * 20) // Including the marker
    val (decoded, tail) = v.splitAt(len)
    (RawValue(decoded), tail)
  }


}

object PathCodecs extends PathCodecs
