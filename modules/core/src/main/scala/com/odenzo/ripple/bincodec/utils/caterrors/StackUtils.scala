package com.odenzo.ripple.bincodec.utils.caterrors

/**
  * What to have some nice chopping to for nested stack traces.
  */
private[bincodec] trait StackUtils {

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

private[bincodec] object StackUtils extends StackUtils
