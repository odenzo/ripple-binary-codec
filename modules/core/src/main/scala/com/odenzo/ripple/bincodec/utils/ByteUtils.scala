package com.odenzo.ripple.bincodec.utils

import java.util.Locale
import scala.annotation.tailrec

import scribe.{Logging, Level}
import spire.implicits._
import spire.math.{UByte, ULong, UInt}
import cats._
import cats.data._
import cats.implicits._

import com.odenzo.ripple.bincodec.{BCLibErr, BinCodecLibError}

/** Helpers since I seldom use bits/bytes directly and Scala/Java sucks. Don't know a good lib
  * SDtarting to use Spire, and making sure these all work -- but more convenience than speed at this point
  * */
private[bincodec] trait ByteUtils extends Logging {

  // Set the logging threshold for this class only
  logger.withHandler(minimumLevel = Some(Level.Warn))

  val ZEROBYTE: Byte = 0.toByte

  def hex2Bytes(hex: String): Either[BinCodecLibError, List[Byte]] = Nested(hex2ubytes(hex)).map(_.toByte).value

  def byte2ubyte(b: Byte): UByte = UByte(b)

  def bytes2ubytes(bytes: Iterable[Byte]): Iterable[UByte] = bytes.map(byte2ubyte)

  def bytes2bigint(a: Array[Byte]): BigInt = BigInt(1, a)

  def bigint2bytes(bi: BigInt): Array[Byte] = {
    val bytes: Array[Byte] = bi.toByteArray // Not sure the left padding on this.
    bytes.dropWhile(_.equals(0))
  }

  /** WARNING: This doesn't check range problems */
  def bigInt2ulong(bi: BigInt): Either[BCLibErr, ULong] = {
    if (bi < BigInt(0) || (bi > ULong.MaxValue.toBigInt))
      BinCodecLibError(s"BigInt $bi out of ULong/UInt64 Range ").asLeft
    else ULong.fromBigInt(bi).asRight
  }

  /**
    * @return Formats unsigned byte as two hex characters, padding on left as needed (lowercase btw)
    */
  def ubyte2hex(v: UByte): String = zeroPadLeft(v.toHexString.toUpperCase, 2)

  def ubytes2hex(v: Seq[UByte]): String = v.map(ubyte2hex).mkString

  /**
    * Takes an arbitrary length string and returns an listed of unsigned bytes
    * If the number of hex digits is odd, is padded with zero on left.
    * FIX: 2.13 fix needed
    */
  def hex2ubytes(v: String): Either[BinCodecLibError, List[UByte]] = {
    val padded: String = v.length % 2 match {
      case 0 => v
      case 1 => '0' +: v
    }
    //  padded.sliding(2) Deprecated
    padded.grouped(2).map(hex2ubyte).toList.sequence
  }

  /**
    *   Unsafe converstion of Hex to list of Unsigned Bytes.
    *   If hex is invalid then it throw Exception
    * @param v
    * @return
    */
  def unsafeHex2ubytes(v: String): List[UByte] = {
    hex2ubytes(v) match {
      case Right(list) => list
      case Left(err)   => throw new IllegalArgumentException(s"Bad Hex $v ", err)
    }
  }

  /**
    *   Note for speed
    * @param v Must be a one or two character hex string not enforced
    * @return
    */
  def hex2ubyte(v: String): Either[BinCodecLibError, UByte] = {
    hex2byte(v).map(UByte(_))
  }

  /**
    * Not for speed!
    *
    * @param v Must be a one or two character hex string
    *
    * @return
    */
  def hex2byte(v: String): Either[BinCodecLibError, Byte] = {
    BinCodecLibError.handling(s"$v hex to Byte") {
      java.lang.Long.parseLong(v, 16).toByte
    }
  }

  def zeroPadLeft(v: String, len: Int): String = {
    val maxPad: String = "000000000000000000000000000000000000000000000000000000000000000000"
    val padding: Int   = len - v.length
    if (padding > 0) {
      maxPad.take(padding) + v
    } else {
      v
    }
  }

  def zeroEvenPadHex(hex: String): String = {
    hex.length % 2 match {
      case 1 => "0" + hex
      case 0 => hex
    }
  }

  def trimLeftZeroBytes(a: List[Byte]): List[Byte] = a.dropWhile((v: Byte) => v == ZEROBYTE)

  /** List of four unsigned bytes representing unsigned long get converted */
  def ubytes2ulong(bytes: Seq[UByte]): Either[BCLibErr, ULong] = {

    if (bytes.length != 8) BinCodecLibError("ulong requires exactly 4 ubytes").asLeft
    else {
      val ul: List[ULong]      = bytes.map(ub => ULong(ub.toLong)).toList.reverse
      val shifted: List[ULong] = ul.mapWithIndex((v: ULong, i: Int) => v << (i * 8))
      val res: ULong           = shifted.fold(ULong(0))(_ | _)
      res.asRight
    }
  }

  /**
    *
    * @param v
    * @return Hex padded with zeros out to full 8 bytes
    */
  def ulong2hex(v: ULong): String = zeroPadLeft(v.toHexString(), 16)

  /** Quicky to take 16 hex chars and turn into ULong. Hex prefixed with 0x if missing */
  def hex2ulong(hex: String): Either[BinCodecLibError, ULong] = {
    BinCodecLibError.handling(s"Parsing ULong from $hex") {
      val bi = BigInt(hex, 16)
      ULong.fromBigInt(bi)
    }
  }

  /** If there are 8 bytes then return as 64 bit  ULong otherwise error. */
  def longBytesToULong(bytes: List[UByte]): Either[BCLibErr, ULong] = {

    bytes.length match {
      case 8 =>
        val shifted: List[ULong] = bytes.mapWithIndex {
          case (b, indx) =>
            ULong(b.toLong) << ((7 - indx) * 8)
        }

        val ulong: ULong = shifted.foldLeft(ULong(0))(_ | _)
        ulong.asRight

      case x if x < 8 => BinCodecLibError(s"8 Bytes needed to convert to ulong but ${bytes.length}, pleaase pad").asLeft
      case _          => BinCodecLibError(s"8 Bytes needed to convert to ulong but ${bytes.length}").asLeft
    }

  }

  def ensureMaxLength(l: List[UByte], len: Int): Either[BinCodecLibError, List[UByte]] = {
    if (l.length > len) BinCodecLibError(s"Byte List length ${l.length} > $len").asLeft
    else l.asRight
  }

  def zeroPadBytes(l: List[UByte], len: Int): List[UByte] = {
    val padLen = len - l.length
    if (padLen > 0) {
      List.fill(padLen)(UByte(0)) ::: l
    } else {
      l
    }
  }

  def bytes2uint(bytes: Seq[Byte]): UInt = {
    val ints  = bytes.map(v => UInt(v.toLong))
    val shift = Seq(24, 16, 8, 0)

    (ints(0) << 24) + (ints(1) << 16) + (ints(2) << 8) + ints(3)
  }

  def bytes2ulong(bytes: Seq[Byte]): UInt = {
    val ints  = bytes.map(v => UInt(v.toLong))
    val shift = Seq(24, 16, 8, 0)

    (ints(0) << 24) + (ints(1) << 16) + (ints(2) << 8) + ints(3)
  }

  def uint2bytes(v: UInt): List[Byte] = {
    val mask     = UInt(255)
    val b4: UInt = mask & v
    val b3       = mask & (v >> 8)
    val b2       = mask & (v >> 16)
    val b1       = mask & (v >> 24)

    val longBytes: List[UInt] = List(b4, b3, b2, b1)
    val ans: List[Byte]       = longBytes.map(v => v.signed.toByte)
    ans
  }

  def bytes2hex(bytes: Iterable[Byte]): String = {
    bytes.map(byte2hex).mkString
  }

  /** Returns String with exactly two uppercased Hex digits */
  def byte2hex(byte: Byte): String = {
    val notPadded = UByte(byte).toHexString.toUpperCase
    zeroEvenPadHex(notPadded)
  }

}

object ByteUtils extends ByteUtils
