package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data.{NonEmptyList, _}
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import scodec.interop.cats._
import scodec.bits._

import com.odenzo.ripple.bincodec._
import com.odenzo.ripple.bincodec.utils.JsonUtils

/** These should fall under the delimited fields stuff */
trait PathsetScodecs extends JsonUtils {
//
//  import com.odenzo.ripple.bincodec.codecs.PathCodecs._
//
//  // Better to decode data into a Model object list PathSet(p:List[Path])  Path(steps:List[PathSte],
//  // and maybe PathSetp is one of AccountPathStep, CurrencyPathStep, IssuerPathStep, CurrencyAndIsserPathStep
//  // The json encoding is kinda odd...
//  /** These is really a container. Inside is a list of  datasteps and delimeters. Equalivalent to paths **/
//  def encodePathSet(data: Json): Either[BinCodecLibError, ByteVector] = {
//    scribe.debug(s"Encoding PathSet/Path:  : \n ${data.spaces4}")
//    // So, a pathset starts with no special marker, each path and DEL_A except last is DEL_B see the foldSmash
//    val pathList = for {
//      pathArr <- json2array(data)
//      paths   <- pathArr.traverse(encodePath)
//    } yield paths
//
//    pathList.flatMap {
//      case lp if lp.isEmpty => BinCodecLibError("Seems there were no paths in the PathSet!").asLeft
//      case lp               => lp.foldSmash(ByteVector.empty, anotherPathMarker, endOfPathsMarker).asRight
//    }
//
//  }
//
//  /**
//    *  Path is an array with anonomous PathStep contents
//    * @param json
//    */
//  private[codecs] def encodePath(json: Json): Either[BinCodecLibError, ByteVector] = {
//    // There are no delimeters at the end of each pathstep in a path
//    // So this is just pure packing
//    for {
//      arr       <- json2array(json)
//      steps     <- arr.traverse(json2object)
//      pathsteps <- steps.traverse(encodePathStep)
//    } yield pathsteps.reduce(_ ++ _)
//  }
//
//  /** @param json Object containing the account/currency/issuer
//    *            @return Encoded Fields, but no pathstep delimineters
//    */
//  private[codecs] def encodePathStep(json: JsonObject): Either[BinCodecLibError, ByteVector] = {
//    import com.odenzo.ripple.bincodec.codecs.PathCodecs._
//    scribe.debug(s"Encoding Path Step\n ${json.asJson.spaces2}")
//
//    // Break down to ADT case classes with Codec for each
//
//    // TODO: Validate currency is not XRP , with special currency encoding TBC
//
//    val fields: List[Option[String]] = List("account", "currency", "issuer").map(k => json(k)).map(_.flatMap(_.asString))
//    val res = fields match {
//      case Some(account) :: None :: None :: Nil      => accountPrefix ~ xrpaccount
//      case None :: Some(curr) :: None :: Nil         => currrencyPrefix ~ xrpcurrency
//      case None :: None :: Some(issuer) :: Nil       => issuerPrefix ~ xrpaccount
//      case None :: Some(curr) :: Some(issuer) :: Nil => currencyAndIssuerPrefix ~ xrpcurrency ~ xrpaccount // (for issuer)
//
//    }
//
//    res.map(v => v.reduce(_ ++ _))
//
//  }

}
