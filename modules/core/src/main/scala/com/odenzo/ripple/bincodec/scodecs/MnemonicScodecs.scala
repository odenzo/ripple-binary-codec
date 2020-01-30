package com.odenzo.ripple.bincodec.scodecs

import scodec.Codec
import com.odenzo.ripple.bincodec.setup.Setup
import TrivialScodec._

trait MnemonicScodecs {

  val xrpltransactiontype: Codec[String] = xrpuint16.xmap(num => Setup.getTransactionType(num), str => Setup.getTransactionTypeCode(str))

}

object MnemonicScodecs extends MnemonicScodecs
