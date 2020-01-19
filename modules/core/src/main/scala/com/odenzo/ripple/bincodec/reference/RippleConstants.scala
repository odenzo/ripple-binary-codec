package com.odenzo.ripple.bincodec.reference

object RippleConstants {

  import cats.Show
  import scodec.bits._

  val pathSetAnother  = hex"FF" // indicates another path follows
  val pathSetEnd      = hex"00" // indicates the end of the PathSet
  val objDel          = hex"0F" // Object Delimeter in some packed fields forget which
  val objectEndMarker = hex"E1" // indicates end of object this is STObject not blob
  val arrDel          = hex"0D" // Array delimeter
  val arrayEndMarker  = hex"F1" // End of Array

  final val objectMarkerEndName: String = "ObjectEndMarker"
  final val arrayMarkerEndName: String  = "ArrayEndMarker"

}
