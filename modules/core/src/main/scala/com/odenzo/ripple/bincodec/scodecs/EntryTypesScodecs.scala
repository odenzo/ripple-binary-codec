package com.odenzo.ripple.bincodec.scodecs

import cats._
import cats.data._
import cats.implicits._
import scodec.bits._
import scodec.codecs._
import scodec._

import com.odenzo.ripple.bincodec.encoding.CodecUtils
import com.odenzo.ripple.bincodec.{BCLibErr, BinCodecLibError}
import com.odenzo.ripple.bincodec.reference.MnemonicType

trait EntryTypesScodecs extends CodecUtils {

  def mapEnc(map: Map[String, MnemonicType]): String => Attempt[BitVector] =
    (x: String) =>
      dd.getTransactionTypeMnemonic(x).map(_.value) match {
        case Left(err)    => Attempt.Failure(Err(err.msg))
        case Right(value) => uint16.encode(value.toInt)
      }

//  def mapDec(map: Map[String, MnemonicType]): BitVector => Attempt[String] = (y: BitVector) =>
//        y.
//
//
//
//
//  def mapDec(map: Map[String, MnemonicType]): String => Attempt[ByteVector] = (x: String) =>
//    dd.getTransactionTypeMnemonic(x).flatMap(_.encoded) match {
//      case Left(err)                => Attempt.Failure(Err(err.msg))
//      case Right(value: ByteVector) => Attempt.Successful(value)
//    }
//
//  def fromMap(map: Map[String, MnemonicType]): GenCodec[String, ByteVector] =
//    utf8.exmap[BitVector](mapEnc(map), )
//
//  val txnType: GenCodec[String, ByteVector] = fromMap(dd.txnTypes)
//  def ledgerEntryType                       = fromMap(dd.ledgerTypes)
//  def txnResultType                         = fromMap(dd.txnResultTypes)

}
