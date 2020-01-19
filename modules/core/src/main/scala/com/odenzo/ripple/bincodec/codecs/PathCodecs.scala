package com.odenzo.ripple.bincodec.codecs

import cats._
import cats.data._
import cats.implicits._
import cats.data.NonEmptyList
import cats.implicits._
import io.circe.syntax._
import io.circe.Json
import io.circe.JsonObject
import scodec.interop.cats._
import scodec.bits._

import com.odenzo.ripple.bincodec.reference.DefinitionData
import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.utils.JsonUtils

trait PathCodecs extends JsonUtils {

  import com.odenzo.ripple.bincodec.codecs.PathCodecs._

  /** These is really a container. Inside is a list of  datasteps and delimeters. Equalivalent to paths **/
  def encodePathSet(data: Json): Either[BinCodecLibError, ByteVector] = {
    scribe.debug(s"Encoding PathSet/Path:  : \n ${data.spaces4}")
    val pathList = for {
      pathArr <- json2array(data)
      paths   <- pathArr.traverse(encodePath)
    } yield paths

    pathList.flatMap {
      case lp if lp.isEmpty => BinCodecLibError("Seems there were no paths in the PathSet!").asLeft
      case lp               => lp.foldSmash(ByteVector.empty, anotherPathMarker, endOfPathsMarker).asRight
    }

  }

  /**
    *  Path is an array with anonomous PathStep contents
    * @param json
    */
  private[codecs] def encodePath(json: Json): Either[BinCodecLibError, ByteVector] = {
    // There are no delimeters at the end of each pathstep in a path
    for {
      arr       <- json2array(json)
      steps     <- arr.traverse(json2object)
      pathsteps <- steps.traverse(encodePathStep)
    } yield pathsteps.reduce(_ ++ _)
  }

  /** @param json Object containing the account/currency/issuer
    *            @return Encoded Fields, but no pathstep delimineters
    */
  private[codecs] def encodePathStep(json: JsonObject): Either[BinCodecLibError, ByteVector] = {
    import com.odenzo.ripple.bincodec.codecs.PathCodecs._
    scribe.debug(s"Encoding Path Step\n ${json.asJson.spaces2}")

    // TODO: Validate currency is not XRP , with special currency encoding TBC

    val fields: List[Option[Json]] = List("account", "currency", "issuer").map(k => json(k))
    import com.odenzo.ripple.bincodec.codecs.AccountIdCodecs._
    val res = fields match {
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

    res.map(v => v.reduce(_ ++ _))

  }

}

object PathCodecs extends PathCodecs {
  val kZERO                    = hex"00"
  val kAddressStep             = hex"01"
  val kCurrencyStep            = hex"10"
  val kIssuerStep              = hex"20"
  val accountType: ByteVector  = hex"0x01"
  val currencyType: ByteVector = hex"0x10"
  val issuerType: ByteVector   = hex"0x20"
  val accountPrefix            = accountType
  val currencyPrefix           = currencyType
  val issuerPrefix             = issuerType
  val currencyAndIssuerPrefix  = currencyType | issuerType

  val anotherPathMarker: ByteVector = DefinitionData.pathSetAnother
  val endOfPathsMarker: ByteVector  = DefinitionData.pathSetEnd

}
