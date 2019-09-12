package com.odenzo.ripple.bincodec

import scala.util.{Success, Failure, Try}

import cats._
import cats.implicits._
import io.circe.Decoder.Result
import io.circe.{ParsingFailure, Json, DecodingFailure}

/**
  * This stuff is and always was a mess :-). Maybe move to ZIO or clean up.
  * ZIO a bit fast moving to use now.
  * Base class that all errors (including OError) must extends directly or indirectly.
  * Not quite ready to move to case classes.
  *
  * RippleCodecError is the base that all other errors should derive from.
  * I let these be public, as they are returns from the RippleCodecAPI.
  */
sealed trait BinCodecLibError extends Throwable {
  def msg: String
  def cause: Option[BinCodecLibError]

  def addMsg(s: String) = BCLibErr(s, this.some)
}

case class BCLibErr(msg: String = "<None>", cause: Option[BinCodecLibError] = None) extends BinCodecLibError

/** This should be terminal node only, and typicall NonFatal throwable. */
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
       |       Cause:\n ${failure.cause.map(_.show)}
       |""".stripMargin

  }

}

object BCException extends ErrorUtils {

  /** Shows a Throwable, checking if its a subclass  */
  implicit val showThrowables: Show[Throwable] = Show.show[Throwable] {
    case err: BinCodecLibError => err.show
    case other =>
      s"""
         | BCThrowable:\n  ${other.toString}
         |""".stripMargin
  }

  implicit val show: Show[BCException] = Show.show[BCException] { errorException =>
    s""""
       | BCException -=>  ${errorException.msg} \n\t\t
       |   Exception Class: \t${errorException.err.getClass}
       |   Exception:    ${errorException.err.show}
       |   StackTrace As String: ${stackAsString(errorException.err)}
     """.stripMargin
  }
}

object BCJsonParseErr {
  implicit val show: Show[BCJsonParseErr] = Show.show[BCJsonParseErr] { failure: BCJsonParseErr =>
    s"""|
        |BCJsonParseErr -=> ${failure.msg}
        |ParseError:  ${failure.err.show}
        |FullText: ${failure.raw}
        |
        | """.stripMargin
  }

}
object BCJsonDecodingErr {
  implicit val show: Show[BCJsonDecodingErr] = Show.show[BCJsonDecodingErr] { failure: BCJsonDecodingErr =>
    val base  = s"BCJsonDecodingErr -=>  ${failure.err.show} \n\t\t On JSON: ${failure.json.spaces2}"
    val stack = "\n\nStack as String: " + ErrorUtils.stackAsString(failure.err)
    // val stackTrace = "\n\nStack Trace " + StackUtils.printStackTrace(failure.err)
    base + "\n DecodingFailure History: " + failure.err.history + stack
  }

  /**
    * Wrap the Decoding error if there was one, and return as Either
    */
  def wrapResult[T](v: Result[T], json: Json, note: String = "No Clues"): Either[BinCodecLibError, T] = {
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
  def wrap[A](msg: String)(fn: => Either[BinCodecLibError, A]): Either[BinCodecLibError, A] = {
    Try {
      fn
    } match {
      case Success(v)         => v
      case Failure(exception) => BCException(msg, exception).asLeft
    }
  }

  def wrapPure[A](msg: String)(fn: => A): Either[BinCodecLibError, A] = {
    Try {
      fn
    } match {
      case Success(v)         => v.asRight
      case Failure(exception) => BCException(msg, exception).asLeft
    }
  }

  // Functionally equivalent, I think this style better.
  def catchNonFatal[A](f: => A): Either[BinCodecLibError, A] = {
    Either.catchNonFatal(f).leftMap(e => new BCException("Wrapped Exception", e))
  }

  def stackAsString(err: Throwable): String = {
    import java.io.{PrintWriter, StringWriter}
    val errors = new StringWriter
    err.printStackTrace(new PrintWriter(errors))
    errors.toString
  }

  def printStackTrace(e: Throwable): String = {
    e.getStackTrace.slice(3, 19).map(_.toString).mkString("\n\t", "\n\t", "\n== .... ==\n")
  }

}

object ErrorUtils extends ErrorUtils

object ErrorOr {

  type ErrorOr[R] = Either[BinCodecLibError, R]

}
