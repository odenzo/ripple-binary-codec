package com.odenzo.ripple.bincodec.benchmark

import java.io.{BufferedWriter, FileWriter, File}

import cats._
import cats.data._
import cats.implicits._
import java.net.URL

import com.odenzo.ripple.bincodec.utils.JsonUtils
import com.odenzo.ripple.bincodec.BinCodecLibError
import scala.io.{Source, BufferedSource}

import io.circe.{JsonObject, Decoder, Json}
import io.circe.syntax._

/** Hack to prepare data for speed/benchmarks when we don't care about checking correctness (checked elsewhere) */
object DataPrep extends App {

  // cleanAndCombineLedgerTxn()
  cleanSignRqRs()
  def cleanAndCombineLedgerTxn(): Unit = {

    val direct: IndexedSeq[String] = (1 until 3)
      .map(i => s"./modules/benchmark/src/main/resources/ledger_direct_txn_good_$i.json")

    val outputTo = "ledger_benchmarks_data.json"

    val pruned: Either[BinCodecLibError, List[Json]] = direct.toList.flatTraverse { in =>
      for {
        json <- BenchmarkUtils.loadJsonFile(in)
        raw  <- JsonUtils.decode(json, Decoder[List[JsonObject]])
        nometa = raw.map(jo => jo.remove("metaData").asJson)
      } yield nometa
    }

    pruned match {
      case Left(err)   => scribe.error(err.show)
      case Right(objs) => BenchmarkUtils.writeToFile(outputTo, Json.fromValues(objs))
    }
  }

  def cleanSignRqRs(): Unit = {
    val dir                 = "./modules/benchmark/src/main/resources/"
    val files: List[String] = List("SignRqRs.json", "SignRqRs_Problems.json").map(dir + _)

    val outputTo = "sign_benchmarks_data.json"

    val pruned: Either[BinCodecLibError, List[Json]] = files.flatTraverse { in =>
      for {
        json <- BenchmarkUtils.loadJsonFile(in)
        raw  <- JsonUtils.decode(json, Decoder[List[JsonObject]])
        rq   <- raw.traverse(jo => jo.apply("rq").toRight(BinCodecLibError("Problem Parsing Rq", jo.asJson)))
      } yield rq
    }

    pruned match {
      case Left(err)   => scribe.error(err.show)
      case Right(objs) => BenchmarkUtils.writeToFile(outputTo, Json.fromValues(objs))

    }
  }

}

object BenchmarkUtils {
  def writeToFile(outFile: String, json: Json): Unit = {
    val file = new File(outFile)
    val bw   = new BufferedWriter(new FileWriter(file))
    val txt  = JsonUtils.droppingNullsPrinter.print(json)
    bw.write(txt)
    bw.close()
    scribe.info(s"Completed with output to ${file.getAbsolutePath}")
  }

  def loadJsonResource(in: String): Either[BinCodecLibError, Json] = {
    BinCodecLibError.handlingM(s"Getting Resource [$in]") {
      val resource: URL          = getClass.getResource(in)
      val source: BufferedSource = Source.fromURL(resource)
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }

  def loadJsonFile(in: String): Either[BinCodecLibError, Json] = {
    BinCodecLibError.handlingM(s"Getting File [$in]") {
      val resource: URL          = getClass.getResource(in)
      val source: BufferedSource = Source.fromFile(in, "UTF-8")
      val data: String           = source.getLines().mkString("\n")
      JsonUtils.parseAsJson(data)
    }
  }
}
