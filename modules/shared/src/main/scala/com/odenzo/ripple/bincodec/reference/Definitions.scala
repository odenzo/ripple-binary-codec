package com.odenzo.ripple.bincodec.reference

import scala.io.Source

import cats.implicits._

import com.odenzo.ripple.bincodec.BinCodecLibError
import com.odenzo.ripple.bincodec.utils.JsonUtils

/** Definitions are loading from the definitions.js file supplied by Ripple from their C++ for the JavaScript library.
  *     [[https://github.com/ripple/ripple-binary-codec/blob/master/src/enums/definitions.json]]
  *
  *
  */
object Definitions extends JsonUtils {

  /** We insist on this succeeding or failing the whole shebang via exception */
  val fieldData: DefinitionData = loadDefaultData() match {
    case Left(err) => throw new IllegalStateException(s"Startup Phase Failed loading FieldData ${err.show}")
    case Right(v)  => v
  }

  def loadDefaultData(): Either[BinCodecLibError, DefinitionData] = {
    val resourceName = "/ripplereferencedata/definitions.json"
    scribe.info(s"Loading Default Data from $resourceName")

    BinCodecLibError.wrap(s"Couldn't Load Definitions Resource $resourceName") {
      val stream = this.getClass.getResourceAsStream(resourceName)
      val txt    = Source.fromInputStream(stream, "UTF-8").getLines().mkString("\n")
      JsonUtils.parseAsJson(txt).flatMap(DefinitionsDecoding.decodeDefinitions)
    }
    // In Case I want to fetch directly next time.
    // Current version is Mar 24, 2019
    // val rippleSiteUrl: String = "https://github.com/ripple/ripple-binary-codec/blob/master/src/enums/definitions.json"
  }

}
