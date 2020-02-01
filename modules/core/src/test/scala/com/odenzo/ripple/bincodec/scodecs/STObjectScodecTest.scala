package com.odenzo.ripple.bincodec.scodecs

import com.odenzo.ripple.bincodec.OTestSpec
import com.odenzo.ripple.bincodec.setup.Setup
import scodec.bits._

class STObjectScodecTest extends OTestSpec {
  import STObjectScodec._

  val txblox =
    hex"12000022000000002400000003201BFFFFFFFF61D4C38D7EA4C680000000000000000000000000004E5A4400000000001A255086B5137A6E57079B1B4FFF4F75C61B4F7F68400000000000003269D4C71AFD498D00000000000000000000000000005553440000000000A4AB176547A22ED23E6D8C3138780526830081D27321EDF0B6D83E34724369D0B1064FE2B944843E8B5AF1EA0BBB889E0F70BEA2B3F771744099B1A429D1B48E82EC97D3D1B1DA2268BD5EB53F42C33FE50494853310B6885B4DC7E969AC42F9E5F4D1F069D01CA4641B805DCB6B0FBC2BD9BA355F360FEA078114E784C01C1F30F461ED92E6CBC96A177AA8822DB283144419B287060B8112B323AF730A8B1DF254DD6470" ++
      hex"E1"

  /** Test the same transaction object with a delimeter on the end, and the pathset removed. */
  test("Decoding") {
    val foo = Setup.config.fields.length
    xrpstobject.decode(txblox.bits).require
  }
}
