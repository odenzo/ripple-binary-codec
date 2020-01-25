package com.odenzo.ripple.bincodec.benchmark

import cats._
import cats.data._
import cats.implicits._
import java.util.concurrent.TimeUnit
import scala.util.Random

import io.circe.{Json, JsonObject, Decoder}
import io.circe.syntax._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import com.odenzo.ripple.bincodec.reference.{DefinitionData, Definitions}
import com.odenzo.ripple.bincodec.{BinCodecLibError, RippleCodecAPI}
import com.odenzo.ripple.bincodec.utils.JsonUtils

/**
  *  For trying to profie with Java Flight Recorder and IntelliJ
  *
  */
object ProfileCodecSignRq extends App {

//  case class SignData(v: Vector[Json])
//  case class LedgerData(v: Vector[Json])
//
//  val signs: SignData = {
//    val signes = for {
//      json    <- BenchmarkUtils.loadJsonResource("/sign_benchmarks_data.json")
//      rqs     <- JsonUtils.decode(json, Decoder[List[JsonObject]])
//      txjsons <- rqs.traverse(o => o.apply("tx_json").toRight(BinCodecLibError("Problem Parsing Rq", o.asJson)))
//    } yield txjsons.toVector
//    signes match {
//      case Left(err) => throw err
//      case Right(v)  => SignData(v)
//    }
//  }
//
//  val entries: Either[BinCodecLibError, LedgerData] = {
//    for {
//      json <- BenchmarkUtils.loadJsonResource("/ledger_benchmarks_data.json")
//      rqs  <- JsonUtils.decode(json, Decoder[Vector[Json]])
//    } yield LedgerData(rqs)
//  }
//
//  val loadingDefs = Definitions.fieldData
//  val expanded    = signs.v ++ signs.v ++ signs.v ++ signs.v ++ signs.v
//  val bigger      = expanded ++ expanded ++ expanded ++ expanded
//  scribe.info(s"Doing Signing for ${bigger.length}")
//  val res = signers(bigger)
//  scribe.info(s"TEst ${res.length}")
////   @Fork(value = 1, jvmArgsAppend = Array("-XX:+UnlockCommercialFeatures",
////    "-XX:+FlightRecorder", "-XX:+UnlockDiagnosticVMOptions", "-XX:+DebugNonSafepoints",
////    "-XX:FlightRecorderOptions=defaultrecording=true,dumponexit=true,dumponexitpath=/tmp/mapOnFunctionsAndList.jfr"))
//  def signers(data: Vector[Json]): Vector[Either[BinCodecLibError, Array[Byte]]] = {
//    val rs =
//      data.map {
//        RippleCodecAPI.signingTxBlob
//      }
//
//    rs
//  }
}
