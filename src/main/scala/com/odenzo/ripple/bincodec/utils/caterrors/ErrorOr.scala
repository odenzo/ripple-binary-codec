package com.odenzo.ripple.bincodec.utils.caterrors

object ErrorOr {

  type ErrorOr[R] = Either[CodecError, R]

  

}
