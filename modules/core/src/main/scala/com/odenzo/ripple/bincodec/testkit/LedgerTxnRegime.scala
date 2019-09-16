package com.odenzo.ripple.bincodec.testkit

import java.security.MessageDigest

import cats._
import cats.data._
import cats.implicits._
import io.circe.syntax._
import io.circe.JsonObject
import scribe.Logging

import com.odenzo.ripple.bincodec.{BCJsonErr, BinCodecLibError, RippleCodecAPI}
import com.odenzo.ripple.bincodec.reference.HashPrefix
import com.odenzo.ripple.bincodec.utils.ByteUtils

/** Helpers to make sure encoding is working on ledger txns, as source in LedgerRq or TxHistory */
trait LedgerTxnRegime extends RippleTestUtils with Logging with CodecTestCreators {}

object LedgerTxnRegime extends LedgerTxnRegime
