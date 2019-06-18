package com.odenzo.ripple.bincodec.reference

import java.io.InputStream
import scala.io.{BufferedSource, Source}

import cats.implicits._
import com.typesafe.scalalogging.StrictLogging

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.utils.caterrors.RippleCodecError

/** Definitions are loading from the definitions.js file supplied by Ripple from their C++ for the JavaScript library.
  *     [[https://github.com/ripple/ripple-binary-codec/blob/master/src/enums/definitions.json]]
  *
  * @param data
  */
class Definitions(data: DefinitionData) {


}

/** Little module to wrap around the Ripple definitions.json for Binary Serialization for Signign */
object Definitions extends StrictLogging with JsonUtils {




  /** We insist on this succeeding or failing the whole shebang via exception */
  val fieldData: DefinitionData = loadDefaultData() match {
    case Left(err) ⇒ throw new IllegalStateException(s"Startup Phase Failed loading FieldData ${err.show}")
    case Right(v)  ⇒ v
  }

  final val objectMarkerEndName: String = "ObjectEndMarker"
  final val arrayMarkerEndName: String  = "ArrayEndMarker"

  /** Loads the default data
    * @return
    */
  def loadDefaultData(): Either[RippleCodecError, DefinitionData] = {
    val resourceName = "/ripplereferencedata/definitions.json"
    logger.info(s"Loading Default Data from ${resourceName}")

    val rippleDefsUrl: InputStream = this.getClass.getResourceAsStream(resourceName)
    if (rippleDefsUrl == null) {RippleCodecError(s"Couldn't Find Definitions Resource ${resourceName}").asLeft}
    else {

    val localSrc: BufferedSource   = Source.fromInputStream(rippleDefsUrl, "UTF-8")

    // In Case I want to fetch directly next time.
    val rippleSiteUrl: String = "https://github.com/ripple/ripple-binary-codec/blob/master/src/enums/definitions.json"
    val txt: String           = localSrc.getLines().mkString("\n")
    JsonUtils.parseAsJson(txt).flatMap(DefinitionsDecoding.decodeDefinitionFileJson)
    }
  }



}
