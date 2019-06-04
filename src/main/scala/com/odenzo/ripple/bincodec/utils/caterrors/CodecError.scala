package com.odenzo.ripple.bincodec.utils.caterrors

import scala.util.{Failure, Success, Try}

import cats._
import cats.implicits._
import com.typesafe.scalalogging.{Logger, StrictLogging}
import io.circe.Decoder.Result
import io.circe.{DecodingFailure, Json, ParsingFailure}

import com.odenzo.ripple.bincodec.utils.caterrors.ErrorOr.ErrorOr

/**
  * Base class that all errors (including OError) must extends directly or indirectly.
  * Not quite ready to move to case classes.
  */
sealed trait CodecError extends Throwable {
  def msg: String

  def cause: Option[CodecError]
}

object CatsErrorTypes {}

object ShowHack {
  implicit val showBaseError: Show[CodecError] = Show.show[CodecError]{
    case err: AppJsonError         => err.show
    case err: AppErrorRestCall     => err.show
    case err: AppJsonDecodingError => err.show
    case err: AppException         => err.show
    case err: OError               => "\n --- " + err.show
    case other                     => "\n ****** Unknown Error" + other.toString
  }
}

/**
  * Base Error is never instanciated, but the apply is up here as convenience
  * and delegates down. These will go away soon.
  */
object CodecError extends StrictLogging {

  type MonadAppError[F[_]] = MonadError[F, CodecError]

  lazy implicit val show: Show[CodecError] = Show.show[CodecError]{ failure: CodecError =>
    nonImplShow(failure)
  }
  /** Ignore the compile error in IntelliJ due to recursive show definition
    * Also not the generic restriction/assumption for now that Throwable is bottom oh error stack.
    * */
  lazy implicit val showThrowables: Show[Throwable] = Show.show[Throwable]{ t =>
    s"Showing Throwable ${t.getMessage}" + Option(t.getCause).map((v: Throwable) => v.toString).getOrElse("<No Cause>")
  }
  val NOT_IMPLEMENTED_ERROR: Either[OError, Nothing] = CodecError("Not Implemented").asLeft

  def dump[A](err: Either[CodecError, A]): Option[String] = {
    err match {
      case Left(e)  ⇒ Some(dumpErr(e))
      case Right(_) ⇒ None
    }
  }

  def dumpErr(err: CodecError) = {
    nonImplShow(err)
  }


  /**
    *
    * @param ee
    * @param msg
    * @param loggger
    */
  def log(ee: ErrorOr[_], msg: String = "Error: ", loggger: Logger = logger): Unit = {
    dump(ee) match {
      case None       ⇒ loggger.debug("No Errors Found")
      case Some(emsg) ⇒ loggger.error(s"$msg\t=> $emsg ")
    }
  }

  def nonImplShow(failure: CodecError): String = {
    val base = ShowHack.showBaseError.show(failure)
    val nested: Option[String] = failure.cause.map(sub ⇒ ShowHack.showBaseError.show(sub))
    val cause = nested.getOrElse("\tNo Nested Cause")

    base + "\n\t" + cause
  }

  def apply(json: Json): CodecError = AppJsonError("Invalid Json", json)

  def apply(m: String, json: Json): CodecError = AppJsonError(m, json)

  def apply(m: String, json: Json, e: CodecError): CodecError = AppJsonError(m, json, Some(e))

  def apply(m: String): OError = OError(m, None)

  def apply(m: String, ex: Throwable): AppException = new AppException(m, ex)

  /**
    * Produces a list of strings summarizing the error, going down the stack.
    */
  def summary(err: CodecError): List[String] = {
    val quick = err.cause match {
      case None         => ("Solo Error: " + err.msg) :: Nil
      case Some(nested) => ("Nested Errors: " + err.msg) :: summary(nested)
    }
    val detail: String = CodecError.nonImplShow(err)

    quick :+ detail
  }

}

/** SHould move to sealed module level hierarchies? */
/** The general error handling class, use instead of Throwables in EitherT etc .
  * Preferred method is to use the helper functions in StdContext.
  * These claseses may be made private in the future.
  */
class OError(val msg: String = "No Message", val cause: Option[CodecError] = None) extends CodecError

object OError {

  /** Ignore the compimle error in IntelliJ, but not the crappy coding needs redo */
  lazy implicit val showOError: Show[OError] = Show.show[OError]{ (failure: OError) =>
    val top = s"OError -> ${failure.msg}"
    val sub = failure.cause.map((x: CodecError) ⇒ x.show)
    top + sub
  }

  def catchNonFatal[A](f: => A): Either[CodecError, A] = {
    val ex: Either[Throwable, A] = Either.catchNonFatal(f)
    val res: Either[AppException, A] = ex.leftMap(e ⇒ new AppException("Wrapped Exception", e))
    res
  }

  def apply(m: String, e: CodecError) = new OError(m, Some(e))

  def apply(m: String, e: Option[CodecError] = None) = new OError(m, e)

}

/** This should be terminal node only */
class AppException(val msg: String = "Wrapping Root Exception", val err: Throwable) extends CodecError {
  val cause: Option[CodecError] = Option.empty
}

object AppException extends StackUtils {

  implicit val show: Show[AppException] = Show.show[AppException]{ errorException =>
    s"OErrorException -->  ${errorException.msg} \n\t\t " +
    s"Exception Message: ${errorException.err}\n\t\t" +
    s"Exception Class: \t${errorException.err.getClass}\n\t\t" +
    s"StackTrace As String: ${stackAsString(errorException.err)}"

  }

  def wrap[A](msg: String)(fn: ⇒ Either[CodecError, A]): Either[CodecError, A] = {
    Try{
      fn
    } match {
      case Success(v: Either[CodecError, A]) ⇒ v
      case Failure(exception)                => AppException(msg, exception).asLeft
    }
  }

  def apply(msg: String): AppException = new AppException(err = new RuntimeException(msg))

  def apply(ex: Throwable): AppException = new AppException(err = ex)

  def apply(msg: String, err: Throwable) = new AppException(msg, err)
}

/**
  * For use in Request/Response scenarios, or more generally when  a:Json => b:Json => transform(b) => answer
  * This can be used.
  * Prefer to use ORestCall when have source and response json at one place.
  */
class AppJsonError(val msg: String, val json: Json, val cause: Option[CodecError] = None) extends CodecError {}

class AppJsonParsingError(val msg: String, val raw: String, val parser: ParsingFailure) extends CodecError {
  val cause: Option[CodecError] = new AppException(parser.message, parser).some
}

/**
  * Special OError for JSON request / response errors which are generally wrapped.
  * Could just make a Throwable to suit but...
  */
class AppErrorRestCall(val msg: String,
                       val rawRq: Option[Json],
                       val rawRs: Option[Json],
                       val cause: Option[CodecError] = None)
  extends CodecError

/**
  * Represents a error in Circe Json decoding (Json => Model)
  *
  * @param json JSON that was input, generally the complete document in scope
  * @param err  The decoding failure from Circe.
  * @param note Informational message to enhance the exception, provides context.
  */
class AppJsonDecodingError(val json: Json, val err: DecodingFailure, val note: String = "") extends CodecError {
  val msg  : String             = note + ":" + err.message
  val base : String             = s"\n OR: ${err.show}"
  val cause: Option[CodecError] = None

}

object AppJsonDecodingError {
  implicit val show: Show[AppJsonDecodingError] = Show.show[AppJsonDecodingError]{ failure: AppJsonDecodingError =>
    val base = s"ODecodingError -->  ${failure.err.show} \n\t\t On JSON: ${failure.json.spaces2}"
    val stackAsString = "\n\nStack as String: " + StackUtils.stackAsString(failure.err)
    // val stackTrace = "\n\nStack Trace " + StackUtils.printStackTrace(failure.err)
    base + "\n DecodingFailure History: " + failure.err.history + stackAsString
  }

  /**
    * Wrap the Decoding error if there was one, and return as Either
    *
    * @param v
    * @param json
    * @param note
    * @tparam T
    *
    * @return
    */
  def wrapResult[T](v: Result[T], json: Json, note: String = "No Clues"): Either[CodecError, T] = {
    v.leftMap{ err: DecodingFailure ⇒
      new AppJsonDecodingError(json, err, note)
    }
  }

}

object AppJsonError {

  lazy implicit val show: Show[AppJsonError] = Show.show{ failure =>
    s"""
       | OErrorJson:
       | Error:\t ${failure.msg}
       | JSON :\t  ${failure.json.spaces2}
       | CAUSE:\t\n ${
      failure.cause
      .map((x: CodecError) ⇒ x.show)
      .getOrElse("<Nothing>")
    }""".stripMargin
  }

  def apply(msg: String, json: Json): AppJsonError = new AppJsonError(msg, json)

  def apply(msg: String, json: Json, cause: CodecError): AppJsonError = new AppJsonError(msg, json, Some(cause))

  def apply(msg: String, json: Json, cause: Option[CodecError] = None): AppJsonError = {
    new AppJsonError(msg, json, cause)
  }
}

object AppErrorRestCall {

  implicit val show: Show[AppErrorRestCall] = Show.show{ failure =>
    s"""
       | Error:   ${failure.msg}
       | JSON RQ: ${failure.rawRq.getOrElse(Json.Null).spaces2}
       | JSON RS: ${failure.rawRs.getOrElse(Json.Null).spaces2}
       | CAUSE:   ${
      failure.cause
      .map(v => v.show)
      .getOrElse("<Nothing>")
    }""".stripMargin
  }
}

