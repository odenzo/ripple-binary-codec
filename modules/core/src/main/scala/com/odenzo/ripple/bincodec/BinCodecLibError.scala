package com.odenzo.ripple.bincodec

import scala.util.{Success, Failure, Try}

import cats._
import cats.implicits._
import io.circe.Decoder.Result
import io.circe.{ParsingFailure, Json, DecodingFailure}

/**
  *
  * All errors should instead this, and therefor be Throwable.
  * I let these be public, as they are returns from the RippleCodecAPI often as Throwable
  */
sealed trait BinCodecLibError extends Throwable {
  def msg: String
  def cause: Option[BinCodecLibError]
  def addMsg(s: String): BinCodecLibError = BCLibErr(s, this.some)
}

case class BCLibErr(msg: String = "<None>", cause: Option[BinCodecLibError] = None) extends BinCodecLibError

case class BCException(msg: String = "Wrapping Throwable", err: Throwable) extends BinCodecLibError {
  val cause: Option[BinCodecLibError] = Option.empty
}

case class BCJsonErr(msg: String, json: Json, cause: Option[BinCodecLibError] = None) extends BinCodecLibError

case class BCJsonParseErr(msg: String, raw: String, err: ParsingFailure) extends BinCodecLibError {
  val cause: Option[BinCodecLibError] = BCException(err.message, err).some
}

case class BCJsonDecodingErr(msg: String, json: Json, err: DecodingFailure) extends BinCodecLibError {
  val cause: Option[BinCodecLibError] = BCException("JSON Decoder", err).some
}

/**
  * Base Error is never instanciated, but the apply is up here as convenience
  * and delegates down. These will go away soon.
  */
object BinCodecLibError extends ErrorUtils {

  def apply(json: Json): BinCodecLibError                                 = BCJsonErr("Invalid Json", json)
  def apply(m: String): BCLibErr                                          = BCLibErr(m, None)
  def apply(m: String, e: BinCodecLibError): BCLibErr                     = BCLibErr(m, e.some)
  def apply(m: String, json: Json): BinCodecLibError                      = BCJsonErr(m, json)
  def apply(m: String, json: Json, e: BinCodecLibError): BinCodecLibError = BCJsonErr(m, json, e.some)
  def apply(m: String, ex: Throwable): BCException                        = BCException(m, ex)
  def apply(ex: Throwable): BCException                                   = BCException(err = ex)

  val NOT_IMPLEMENTED_ERROR: Either[BCLibErr, Nothing] = BinCodecLibError("Not Implemented").asLeft

  implicit val showBaseError: Show[BinCodecLibError] = Show.show[BinCodecLibError] {
    case err: BCLibErr          => err.show
    case err: BCJsonErr         => err.show
    case err: BCJsonParseErr    => err.show
    case err: BCJsonDecodingErr => err.show
    case err: BCException       => err.show
  }

}

object BCLibErr {

  /** Ignore the compimle error in IntelliJ, but not the crappy coding needs redo */
  implicit val show: Show[BCLibErr] = Show.show[BCLibErr] { (failure: BCLibErr) =>
    s"""
       |       BCLib Error: ${failure.msg}
       |       Cause:\n ${ErrorUtils.showCauseOrStack(failure)}
       |""".stripMargin

  }

}

object BCException extends ErrorUtils {

  /** Shows a Throwable, checking if its a subclass  */
  implicit val showThrowables: Show[Throwable] = Show.show[Throwable] {
    case err: BinCodecLibError => err.show
    case other =>
      s"""
         | BCThrowable:\n  ${other.toString}   // This has the stack trace
         |""".stripMargin
  }

  implicit val show: Show[BCException] = Show.show[BCException] { failure =>
    s""""
       | BCException -=>  ${failure.msg} \n\t\t
       |   Exception Class: \t${failure.err.getClass}
       |   Exception:    ${failure.err.show}
       |   StackTrace:\n ${stackAsString(failure.err)}
     """.stripMargin
  }
}

object BCJsonParseErr {
  implicit val show: Show[BCJsonParseErr] = Show.show[BCJsonParseErr] { failure: BCJsonParseErr =>
    s"""|
        |BCJsonParseErr -=> ${failure.msg}
        |ParseError:  ${failure.err.show}
        |FullText: ${failure.raw}
        |Cause:  ${ErrorUtils.showCauseOrStack(failure)}
        | """.stripMargin
  }

}
object BCJsonDecodingErr {
  implicit val show: Show[BCJsonDecodingErr] = Show.show[BCJsonDecodingErr] { failure: BCJsonDecodingErr =>
    s"""| CJsonDecodingErr         ${failure.msg}
        | DecodingFailure History:  ${failure.err.history}
        | Underlying:  \n${failure.cause.map(_.show).getOrElse("Shouldn't happen")}
        """.stripMargin
  }

  /**
    * Wrap the Decoding error if there was one, and return as Either
    */
  def lift[T](v: Result[T], json: Json, note: String = "No Clues"): Either[BinCodecLibError, T] = {
    v.leftMap(err => BCJsonDecodingErr(note, json, err))
  }

}

object BCJsonErr {

  implicit val show: Show[BCJsonErr] = Show.show { failure =>
    s"""
       | OErrorJson:
       | Error:\t ${failure.msg}
       | JSON :\t  ${failure.json.spaces2}
       | CAUSE:\t\n ${failure.cause
         .map((x: BinCodecLibError) => x.show)
         .getOrElse("<Nothing>")}""".stripMargin
  }

}

trait ErrorUtils {

  /** Catches non-fatal exceptions and places in Either context */
  def handlingM[A](msg: String)(fn: => Either[BinCodecLibError, A]): Either[BinCodecLibError, A] = {
    Try {
      fn
    } match {
      case Success(v)         => v
      case Failure(exception) => BCException(msg, exception).asLeft
    }
  }

  /** Catches non-fatal exceptions and places in Either context */
  def handling[A](msg: String)(fn: => A): Either[BinCodecLibError, A] = {
    Try {
      fn
    } match {
      case Success(v)         => v.asRight
      case Failure(exception) => BCException(msg, exception).asLeft
    }
  }

  def stackAsString(err: Throwable): String = {
    import java.io.{PrintWriter, StringWriter}
    val errors = new StringWriter
    err.printStackTrace(new PrintWriter(errors))
    errors.toString
  }

  def showCauseOrStack(base: BinCodecLibError): String = {
    base.cause match {
      case Some(err) => err.show
      case None      => ErrorUtils.stackAsString(base)
    }
  }
}

object ErrorUtils extends ErrorUtils

object ErrorOr {

  type ErrorOr[R] = Either[BinCodecLibError, R]

}
