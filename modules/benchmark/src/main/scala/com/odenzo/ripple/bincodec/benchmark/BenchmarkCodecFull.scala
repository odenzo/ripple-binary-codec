package com.odenzo.ripple.bincodec.benchmark

import cats._
import cats.data._
import cats.implicits._
import java.util.concurrent.TimeUnit
import scala.util.Random

import io.circe.{Json, JsonObject, Decoder}
import io.circe.syntax._
import org.openjdk.jmh.annotations.{State, _}
import org.openjdk.jmh.infra.Blackhole

import com.odenzo.ripple.bincodec.{BinCodecLibError, RippleCodecAPI}
import com.odenzo.ripple.bincodec.utils.JsonUtils

/**
  * Basic benchmarkuing, this is not really a microbenchmark but we abuse it.
  *
  * The following command will run the benchmarks with reasonable settings:
  *       benchmark/jmh:run  -i 10 -wi 10 -f1 -t1  BenchmarkCodecSignRq
  *
  */
@org.openjdk.jmh.annotations.State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class BenchmarkCodecSignRq {

  case class SignData(v: Vector[Json])
  case class LedgerData(v: Vector[Json])

  val signs: SignData = {
    val signes = for {
      json    <- BenchmarkUtils.loadJsonResource("/sign_benchmarks_data.json")
      rqs     <- JsonUtils.decode(json, Decoder[List[JsonObject]])
      txjsons <- rqs.traverse(o => o.apply("tx_json").toRight(BinCodecLibError("Problem Parsing Rq", o.asJson)))
    } yield txjsons.toVector
    signes match {
      case Left(err) => throw err
      case Right(v)  => SignData(v)
    }
  }

  def randomSign(): Json = {
    val signIndexRandom    = Random.nextInt(signs.v.length)
    val json: Option[Json] = signs.v.get(signIndexRandom)
    json.getOrElse(signs.v.head)
  }

  val entries: Either[BinCodecLibError, LedgerData] = {
    for {
      json <- BenchmarkUtils.loadJsonResource("/ledger_benchmarks_data.json")
      rqs  <- JsonUtils.decode(json, Decoder[Vector[Json]])
    } yield LedgerData(rqs)
  }

//   @Fork(value = 1, jvmArgsAppend = Array("-XX:+UnlockCommercialFeatures",
//    "-XX:+FlightRecorder", "-XX:+UnlockDiagnosticVMOptions", "-XX:+DebugNonSafepoints",
//    "-XX:FlightRecorderOptions=defaultrecording=true,dumponexit=true,dumponexitpath=/tmp/mapOnFunctionsAndList.jfr"))
  @Benchmark
  def singers(bh: Blackhole): Unit = {
    val rs = signTx(randomSign())
    bh.consume(rs)
  }

  @Benchmark
  def serializeSigners(bh: Blackhole): Unit = {
    val rs = serialTx(randomSign())
    bh.consume(rs)
  }

  @Benchmark
  def signLedgers(bh: Blackhole): Unit = {
    val rs = signTx(randomSign())
    bh.consume(rs)
  }
  @Benchmark
  def seralizeLedgers(bh: Blackhole): Unit = {
    val rs = serialTx(randomSign())
    bh.consume(rs)
  }

  def signTx(tx_json: Json): Either[BinCodecLibError, Array[Byte]] = RippleCodecAPI.signingTxBlob(tx_json)

  def serialTx(tx_json: Json): Either[BinCodecLibError, Array[Byte]] = RippleCodecAPI.serializedTxBlob(tx_json)

}
