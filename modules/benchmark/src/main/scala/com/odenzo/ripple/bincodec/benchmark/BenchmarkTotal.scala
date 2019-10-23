package com.odenzo.ripple.bincodec.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/** Start of setting up JMH Benchmarking as part of the profiling as we move to ScalaJS */
/**
  * Compare the performance of various ways of folding JSON values.
  *
  * The following command will run the benchmarks with reasonable settings:
  *       jmh:run  -i 10 -wi 10 -f1 -t1  ByteUtilsBenchmark
  *
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class ByteUtilsBenchmark {

  def randomArray: Array[Byte] = {
    val a = Array.fill(300)(0.toByte)
    scala.util.Random.nextBytes(a)
    a
  }

  @Benchmark
  def formatter = toHexStringA(randomArray)

  @Benchmark
  def formatB = toHexStringB(randomArray)

  def toHexStringA(bytes: Array[Byte]) = bytes.map(b => f"$b%02x").mkString

  def toHexStringB(bytes: Array[Byte]) = {
    val hexArray: Array[Byte] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
    val hexChars              = Array.fill(bytes.length * 2)(0.toByte)
    for {
      j <- bytes.indices
      v = bytes(j) & 0xFF
    } {
      hexChars(j * 2)     = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0F)
    }
    new String(hexChars)
  }

}

/**
  *   jmh:run  -prof jmh.extras.JFR  -i 10 -wi 10 -f1 -t1  CaseVsIf
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class CaseVsIf() {

  def randomArray: Array[Byte] = {
    val a = Array.fill(300)(0.toByte)
    scala.util.Random.nextBytes(a)
    a
  }

  @Benchmark
  def formatter = toHexStringA(randomArray)

  @Benchmark
  def formatB = toHexStringB(randomArray)

  def toHexStringA(bytes: Array[Byte]) = bytes.map(b => f"$b%02x").mkString

  def toHexStringB(bytes: Array[Byte]) = {
    val hexArray: Array[Byte] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
    val hexChars              = Array.fill(bytes.length * 2)(0.toByte)
    for {
      j <- bytes.indices
      v = bytes(j) & 0xFF
    } {
      hexChars(j * 2)     = hexArray(v >>> 4)
      hexChars(j * 2 + 1) = hexArray(v & 0x0F)
    }
    new String(hexChars)
  }
}
