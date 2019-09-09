package com.odenzo.ripple.bincodec.utils.caterrors

import scala.util.{Failure, Success, Try}

import cats._
import cats.implicits._
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json, ParsingFailure}

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr

/**
  * This stuff is and always was a mess :-). Maybe move to ZIO or clean up.
  * ZIO a bit fast moving to use now.
  * Base class that all errors (including OError) must extends directly or indirectly.
  * Not quite ready to move to case classes.
  *
  * RippleCodecError is the base that all other errors should derive from.
  * I let these be public, as they are returns from the RippleCodecAPI.
  */
sealed trait RippleCodecError extends Throwable {
  def msg: String
  def cause: Option[RippleCodecError]
}

case class OErrorRipple(val msg: String = "No Message", val cause: Option[RippleCodecError] = None)
    extends RippleCodecError

/** This should be terminal node only */
class BinCodecExeption(val msg: String = "Wrapping Root Exception", val err: Throwable) extends RippleCodecError {
  val cause: Option[RippleCodecError] = Option.empty
}

/**
  * Wraps an error that occurs on
  */
class AppJsonError(val msg: String, val json: Json, val cause: Option[RippleCodecError] = None) extends RippleCodecError

class AppJsonParsingError(val msg: String, val raw: String, val parser: ParsingFailure) extends RippleCodecError {
  val cause: Option[RippleCodecError] = new BinCodecExeption(parser.message, parser).some
}

/**
  * Represents a error in Circe Json decoding (Json => Model)
  *
  * @param json JSON that was input, generally the complete document in scope
  * @param err  The decoding failure from Circe.
  * @param note Informational message to enhance the exception, provides context.
  */
class AppJsonDecodingError(val json: Json, val err: DecodingFailure, note: String = "") extends RippleCodecError {
  val msg: String                     = note + ": " + err.message
  val base: String                    = s"\n OR: ${err.show}"
  val cause: Option[RippleCodecError] = None

}

/**
  * Base Error is never instanciated, but the apply is up here as convenience
  * and delegates down. These will go away soon.
  */
object RippleCodecError {

  type MonadAppError[F[_]] = MonadError[F, RippleCodecError]

  val NOT_IMPLEMENTED_ERROR: Either[OErrorRipple, Nothing] = RippleCodecError("Not Implemented").asLeft

  lazy implicit val show: Show[RippleCodecError] = Show.show[RippleCodecError] { failure: RippleCodecError =>
    nonImplShow(failure)
  }

  /** Ignore the compile error in IntelliJ due to recursive show definition
    * Also not the generic restriction/assumption for now that Throwable is bottom oh error stack.
    * */
  lazy implicit val showThrowables: Show[Throwable] = Show.show[Throwable] { t =>
    s"Showing Throwable ${t.getMessage} :" + Option(t.getCause)
      .map(_.toString)
      .getOrElse("<No Cause>")
  }

  def dump[A](err: Either[RippleCodecError, A]): Option[String] = {
    err match {
      case Left(e)  => Some(dumpErr(e))
      case Right(_) => None
    }
  }

  def dumpErr(err: RippleCodecError): String = {
    nonImplShow(err)
  }

  /**
    *
    * @param ee
    * @param msg
    */
  def log(ee: ErrorOr[_], msg: String = "Error: "): Unit = {
    dump(ee) match {
      case None       => scribe.debug("No Errors Found")
      case Some(emsg) => scribe.error(s"$msg\t=> $emsg ")
    }
  }

  def nonImplShow(failure: RippleCodecError): String = {
    val base                   = ShowHack.showBaseError.show(failure)
    val nested: Option[String] = failure.cause.map(sub => ShowHack.showBaseError.show(sub))
    val cause                  = nested.getOrElse("\tNo Nested Cause")

    base + "\n\t" + cause
  }

  def apply(json: Json): RippleCodecError = AppJsonError("Invalid Json", json)

  def apply(m: String, json: Json): RippleCodecError = AppJsonError(m, json)

  def apply(m: String, json: Json, e: RippleCodecError): RippleCodecError = AppJsonError(m, json, Some(e))

  def apply(m: String): OErrorRipple = OErrorRipple(m, None)

  def apply(m: String, ex: Throwable): BinCodecExeption = new BinCodecExeption(m, ex)

  /**
    * Produces a list of strings summarizing the error, going down the stack.
    */
  def summary(err: RippleCodecError): List[String] = {
    val quick = err.cause match {
      case None         => ("Solo Error: " + err.msg) :: Nil
      case Some(nested) => ("Nested Errors: " + err.msg) :: summary(nested)
    }
    val detail: String = RippleCodecError.nonImplShow(err)

    quick :+ detail
  }

}

object OErrorRipple {

  /** Ignore the compimle error in IntelliJ, but not the crappy coding needs redo */
  implicit val showOError: Show[OErrorRipple] = Show.show[OErrorRipple] { (failure: OErrorRipple) =>
    val top = s"OError => ${failure.msg}"
    val sub = failure.cause.map((x: RippleCodecError) => x.show)
    top + " Cause: " + sub
  }

  def catchNonFatal[A](f: => A): Either[RippleCodecError, A] = {
    val ex: Either[Throwable, A]         = Either.catchNonFatal(f)
    val res: Either[BinCodecExeption, A] = ex.leftMap(e => new BinCodecExeption("Wrapped Exception", e))
    res
  }

  def apply(m: String, e: RippleCodecError) = new OErrorRipple(m, Some(e))

  def apply(m: String, e: Option[RippleCodecError] = None) = new OErrorRipple(m, e)

}

object BinCodecExeption extends StackUtils {

  implicit val show: Show[BinCodecExeption] = Show.show[BinCodecExeption] { errorException =>
    s"OErrorException -=>  ${errorException.msg} \n\t\t " +
      s"Exception Message: ${errorException.err}\n\t\t" +
      s"Exception Class: \t${errorException.err.getClass}\n\t\t" +
      s"StackTrace As String: ${stackAsString(errorException.err)}"

  }

  def wrap[A](msg: String)(fn: => Either[RippleCodecError, A]): Either[RippleCodecError, A] = {
    Try {
      fn
    } match {
      case Success(v: Either[RippleCodecError, A]) => v
      case Failure(exception)                      => BinCodecExeption(msg, exception).asLeft
    }
  }

  def apply(msg: String): BinCodecExeption = new BinCodecExeption(err = new RuntimeException(msg))

  def apply(ex: Throwable): BinCodecExeption = new BinCodecExeption(err = ex)

  def apply(msg: String, err: Throwable) = new BinCodecExeption(msg, err)
}

object AppJsonDecodingError {
  implicit val show: Show[AppJsonDecodingError] = Show.show[AppJsonDecodingError] { failure: AppJsonDecodingError =>
    val base          = s"ODecodingError -=>  ${failure.err.show} \n\t\t On JSON: ${failure.json.spaces2}"
    val stackAsString = "\n\nStack as String: " + StackUtils.stackAsString(failure.err)
    // val stackTrace = "\n\nStack Trace " + StackUtils.printStackTrace(failure.err)
    base + "\n DecodingFailure History: " + failure.err.history + stackAsString
  }

  /**
    * Wrap the Decoding error if there was one, and return as Either
    */
  def wrapResult[T](v: Result[T], json: Json, note: String = "No Clues"): Either[RippleCodecError, T] = {
    v.leftMap { err: DecodingFailure =>
      new AppJsonDecodingError(json, err, note)
    }
  }

}

object AppJsonError {

  implicit val show: Show[AppJsonError] = Show.show { failure =>
    s"""
       | OErrorJson:
       | Error:\t ${failure.msg}
       | JSON :\t  ${failure.json.spaces2}
       | CAUSE:\t\n ${failure.cause
         .map((x: RippleCodecError) => x.show)
         .getOrElse("<Nothing>")}""".stripMargin
  }

  def apply(msg: String, json: Json): AppJsonError = new AppJsonError(msg, json)

  def apply(msg: String, json: Json, cause: RippleCodecError): AppJsonError = new AppJsonError(msg, json, Some(cause))

  def apply(msg: String, json: Json, cause: Option[RippleCodecError] = None): AppJsonError = {
    new AppJsonError(msg, json, cause)
  }

}

object ShowHack {
  implicit val showBaseError: Show[RippleCodecError] = Show.show[RippleCodecError] {
    case err: AppJsonError         => err.show
    case err: AppJsonDecodingError => err.show
    case err: BinCodecExeption     => err.show
    case err: OErrorRipple         => "\n --- " + err.show
    case other                     => "\n ****** Unknown Error " + other.toString
  }
}
