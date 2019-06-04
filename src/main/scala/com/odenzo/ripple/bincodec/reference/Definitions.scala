package com.odenzo.ripple.bincodec.reference

import java.io.InputStream
import scala.io.{BufferedSource, Source}

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging

import com.odenzo.ripple.bincodec.utils.caterrors.CodecError
import com.odenzo.ripple.bincodec.utils.{CirceUtils, JsonUtils}

/** Definitions are loading from the definitions.js file supplied by Ripple from their C++ for the JavaScript library.
  *     [[https://github.com/ripple/ripple-binary-codec/blob/master/src/enums/definitions.json]]
  *
  * @param data
  */
class Definitions(data: DefinitionData) {
//
//  val objectEndMarkerByte: Either[AppError, Byte] = data.findTypeForField(objectMarkerEndName).map(_.value.toByte)
//
//  val arrayEndMarkerByte: Either[AppError, Byte] = data.findTypeForField(arrayMarkerEndName).map(_.value.toByte)

}

/** Little module to wrap around the Ripple definitions.json for Binary Serialization for Signign */
object Definitions extends StrictLogging with JsonUtils {


  /** We insist on this succeeding or failing the whole shebang via exception */
  val fieldData: DefinitionData = loadDefaultData() match {
    case Left(err) ⇒ throw new IllegalStateException(s"Startup Phase Failed loading FieldData ${err.show}")
    case Right(v)  ⇒ v
  }



  logger.warn("Instanciating Definitions Object")
  final val objectMarkerEndName: String = "ObjectEndMarker"
  final val arrayMarkerEndName: String  = "ArrayEndMarker"

  /** Loads the default data
    * @return
    */
  def loadDefaultData(): Either[CodecError, DefinitionData] = {
    val resourceName = "/enums/definitions.json"
    logger.info(s"Loading Default Data from ${resourceName}")

    val rippleDefsUrl: InputStream = this.getClass.getResourceAsStream(resourceName)
    if (rippleDefsUrl == null) {CodecError(s"Couldn't Find Definitions Resource ${resourceName}").asLeft}
    else {

    val localSrc: BufferedSource   = Source.fromInputStream(rippleDefsUrl, "UTF-8")

    // In Case I want to fetch directly next time.
    val rippleSiteUrl: String = "https://github.com/ripple/ripple-binary-codec/blob/master/src/enums/definitions.json"
    val txt: String           = localSrc.getLines().mkString("\n")
    CirceUtils.parseAsJson(txt).flatMap(DefinitionsDecoding.decodeDefinitionFileJson)
    }
  }



}
