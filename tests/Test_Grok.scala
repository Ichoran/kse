package kse.tests

import kse.flow._
import kse.eio._

object Test_Grok extends Test_Kse {
  import kse.eio.{GrokErrorCodes => e}
  
  val mkGroks = Array[String => Grok](s => Grok(s), s => Grok text s.getBytes)
  
  def isNo[A](no: Ok[GrokError, A], who: Set[Int], why: Set[Int]): Boolean = no match {
    case No(ge: GrokError) if who(ge.whoError) && why(ge.whyError) => true
    case No(ge: GrokError) => println(ge); println(ge.whoError + " " + who); println(ge.whyError + " " + why); false
    case Yes(a) => println(a); println(who); println(why); false
  }
  def isNo[A](no: Ok[GrokError, A], who: Int, why: Int): Boolean = isNo(no, Set(who), Set(why))

  val validBool = Array("true", "false", "True", "False", "TRUE", "FALSE", "trUE", "fAlSe")
  val invalidBool = Array("treu", "t", "fals", "f")
  def test_Z = mkGroks.forall{ mkGrok =>
    validBool.forall{ s => val g = mkGrok(s); g{ implicit fail => g.Z } == Yes(s.toBoolean) } &&
    invalidBool.forall{ s => val g = mkGrok(s); isNo(g{ implicit fail => g.Z }, Set(e.Z), Set(e.wrong, e.end)) }
  }
  
  val trueAnyBool = Array("yes", "Y", "on", "T", "True", "TRUE", "yeS")
  val falseAnyBool = Array("no", "Off", "n", "falSe")
  val invalidAnyBool = Array("ye", "nay", "tr", "tru", "fa", "of", "uh-huh")
  def test_aZ = mkGroks.forall{ mkGrok =>
    trueAnyBool.forall{s => val g = mkGrok(s); g{ implicit fail => g.aZ } == Yes(true) } &&
    falseAnyBool.forall{s => val g = mkGrok(s); g{ implicit fail => g.aZ } == Yes(false) } && 
    validBool.forall{s => val g = mkGrok(s); g{ implicit fail => g.aZ } == Yes(s.toBoolean) } &&
    invalidAnyBool.forall{ s => val g = mkGrok(s).delimit(true); isNo(g{ implicit fail => g.aZ }, Set(e.aZ), Set(e.wrong, e.end, e.delim)) } &&
    invalidAnyBool.forall{ s => val g = mkGrok(s); val ans = g{implicit fail => g.aZ}; (ans.isOk && g.position == 1) || isNo(ans, Set(e.aZ), Set(e.wrong, e.end)) }
  }
  
  val validSignedBytes = Array[Byte](1, 47, -125, 0, 127, -128)
  val invalidSignedBytes = Array[Int](-129, 128, 19238758, 1515, 1111999)
  val notevenNumbers = Array[String]("fish", "true", "  2")
  def test_B = mkGroks.forall{ mkGrok =>
    validSignedBytes.forall{ b => val g = mkGrok(b.toString); g{ implicit fail => g.B } == Yes(b) } &&
    invalidSignedBytes.forall{ b => val g = mkGrok(b.toString); isNo( g{ implicit fail => g.B }, e.B, e.range ) } &&
    notevenNumbers.forall{ b => val g = mkGrok(b); isNo( g{ implicit fail => g.B }, e.B, e.wrong ) }
  }
  
  val invalidUnsignedBytes = Array[Int](-1, 256, 1111999)
  def test_uB = mkGroks.forall{ mkGrok =>
    validSignedBytes.forall{ b => val g = mkGrok((b&0xFF).toString); g{ implicit fail => g.uB } == Yes(b) } &&
    invalidUnsignedBytes.forall{ b => val g = mkGrok(b.toString); isNo(g{ implicit fail => g.uB }, Set(e.uB), Set(e.range, e.wrong)) } &&
    notevenNumbers.forall{ b => val g = mkGrok(b); isNo(g{ implicit fail => g.uB }, e.uB, e.wrong) }
  }
  
  val validSignedShorts = Array[Short](1, 47, -125, 0, -32768, 32767, 11441)
  val invalidSignedShorts = Array[Int](-1923587, -32769, 32768, 987519, 11111199)
  def test_S = mkGroks.forall{ mkGrok =>
    validSignedShorts.forall{ s => val g = mkGrok(s.toString); g{ implicit fail => g.S } == Yes(s) } &&
    invalidSignedShorts.forall{ s => val g = mkGrok(s.toString); isNo(g{ implicit fail => g.S }, e.S, e.range) } &&
    notevenNumbers.forall{ s => val g = mkGrok(s); isNo(g{ implicit fail => g.S }, e.S, e.wrong) }
  }
  
  val invalidUnsignedShorts = Array[Int](-1, 65536, 11111199)
  def test_uS = mkGroks.forall{ mkGrok =>
    validSignedShorts.forall{ s => val g = mkGrok((s&0xFFFF).toString); g{ implicit fail => g.uS } == Yes(s) } &&
    invalidUnsignedShorts.forall{ s => val g = mkGrok(s.toString); isNo(g{ implicit fail => g.uS }, Set(e.uS), Set(e.range, e.wrong)) } &&
    notevenNumbers.forall{ s => val g = mkGrok(s); isNo(g{ implicit fail => g.uS }, e.uS, e.wrong) }
  }
  
  val validCharacters = Array("x", "!", ".", "\u1235")
  val invalidCharacters = Array("", " x")
  def test_C = mkGroks.forall{ mkGrok =>
    validCharacters.forall{ c => val g = Grok(c); g{ implicit fail => g.C.toString } == Yes(c) } &&
    invalidCharacters.forall{ c => val g = Grok(c); isNo( g{ implicit fail => g.C } , e.C, if (c.isEmpty) e.end else e.wrong ) }
  }
  
  val validSignedInts = Array[Int](0, 1, -92751, 2519875, Int.MaxValue, Int.MinValue)
  val invalidSignedInts = Array[Long](11111111111199L, Int.MaxValue + 1L, Int.MinValue -1L)
  def test_I = mkGroks.forall{ mkGrok =>
    validSignedInts.forall{ i => val g = mkGrok(i.toString); g{ implicit fail => g.I } == Yes(i) } &&
    invalidSignedInts.forall{ i => val g = mkGrok(i.toString); isNo(g{ implicit fail => g.I }, e.I, e.range) } &&
    notevenNumbers.forall{ i => val g = mkGrok(i); isNo(g{ implicit fail => g.I }, e.I, e.wrong) }
  }
  
  val invalidUnsignedInts = Array[Long](-1L, 1L << 32, 11111111111199L)
  def test_uI = mkGroks.forall{ mkGrok =>
    validSignedInts.forall{ i => val g = mkGrok((i & 0xFFFFFFFFL).toString); g{ implicit fail => g.uI } == Yes(i) } &&
    invalidUnsignedInts.forall{ i => val g = mkGrok(i.toString); isNo(g{ implicit fail => g.uI }, Set(e.uI), Set(e.range, e.wrong)) } &&
    notevenNumbers.forall{ i => val g = mkGrok(i); isNo(g{ implicit fail => g.uI }, e.uI, e.wrong) }
  }
  
  val invalidHexInts = Array[String]("100000000", "-1", "ffffffffff")
  def test_xI = mkGroks.forall{ mkGrok =>
    validSignedInts.forall{ i => val g = mkGrok((i & 0xFFFFFFFFL).toHexString); g{ implicit fail => g.xI } == Yes(i) } &&
    invalidHexInts.forall{ i => val g = mkGrok(i); isNo(g{ implicit fail => g.xI }, Set(e.xI), Set(e.range, e.wrong)) } &&
    notevenNumbers.forall{ i => val g = mkGrok(i); val ans = g{ implicit fail => g.xI }; (ans == Yes(0xF) && g.position == 1) || isNo(ans, e.xI, e.wrong) }
  }
  
  val invalidAnyInts = Array[String]((Int.MinValue-1L).toString, (1L << 32).toString, "0x100000000", "0xFFFFFFFFF")
  def test_aI = mkGroks.forall{ mkGrok =>
    validSignedInts.forall{ i => val g = mkGrok(i.toString); g{ implicit fail => g.aI } == Yes(i) } &&
    validSignedInts.forall{ i => val g = mkGrok((i & 0xFFFFFFFFL).toString); g{ implicit fail => g.aI } == Yes(i) } &&
    validSignedInts.forall{ i => val g = mkGrok("0x" + (i & 0xFFFFFFFFL).toHexString); g{ implicit fail => g.aI } == Yes(i) } &&
    validSignedInts.forall{ i => val g = mkGrok("0X" + (i & 0xFFFFFFFFL).toHexString); g{ implicit fail => g.aI } == Yes(i) } &&
    invalidAnyInts.forall{ i => val g = mkGrok(i); isNo(g{ implicit fail => g.aI }, Set(e.aI, e.I, e.xI, e.uI), Set(e.range)) } &&
    notevenNumbers.forall{ i => val g = mkGrok(i); isNo(g{ implicit fail => g.aI }, e.aI, e.wrong) }
  }
  
  val validSignedLongs = Array[Long](0, 1, -9815, 198751, -9175498191732L, 12341982791571L, Long.MaxValue, Long.MinValue)
  val invalidSignedLongs = Array[BigInt](BigInt("1111111111111111111111111111199"), BigInt(Long.MinValue)-1, BigInt(Long.MaxValue)+1)
  def test_L = mkGroks.forall{ mkGrok =>
    validSignedLongs.forall{ l => val g = mkGrok(l.toString); g{ implicit fail => g.L } == Yes(l) } &&
    invalidSignedLongs.forall{ l => val g = mkGrok(l.toString); isNo(g{ implicit fail => g.L }, e.L, e.range) } &&
    notevenNumbers.forall{ l => val g = mkGrok(l); isNo(g{ implicit fail => g.L }, e.L, e.wrong) }
  }
  
  val invalidUnsignedLongs = Array[BigInt](BigInt("-1"), BigInt("11111111111111111111111199"), BigInt(1) << 64)
  def test_uL = mkGroks.forall{ mkGrok =>
    validSignedLongs.forall{ l => val g = mkGrok((BigInt(l) & ((BigInt(1)<<64)-1)).toString); g{ implicit fail => g.uL } == Yes(l) } &&
    invalidUnsignedLongs.forall{ l => val g = mkGrok(l.toString); isNo(g{ implicit fail => g.uL }, Set(e.uL), Set(e.range, e.wrong)) } &&
    notevenNumbers.forall{ l => val g = mkGrok(l); isNo(g{ implicit fail => g.uL }, e.uL, e.wrong) }
  }
  
  val invalidHexLongs = Array[String]("10000000000000000", "-1", "fffffffffffffffff")
  def test_xL = mkGroks.forall{ mkGrok =>
    validSignedLongs.forall{ l => val g = mkGrok((BigInt(l) & ((BigInt(1)<<64)-1)).toString(16)); g{ implicit fail => g.xL } == Yes(l) } &&
    invalidHexLongs.forall{ l => val g = mkGrok(l); isNo(g{ implicit fail => g.xL }, Set(e.xL), Set(e.range, e.wrong)) } &&
    notevenNumbers.forall{ l => val g = mkGrok(l); val ans = g{ implicit fail => g.xL }; (ans == Yes(0xF) && g.position == 1) || isNo(ans, e.xL, e.wrong) }
  }
  
  val invalidAnyLongs = Array[String]((BigInt(Long.MinValue)-1).toString, (BigInt(1) << 64).toString, "1111111111111111111111111111199", "0x10000000000000000", "0xFffffFFFFffffFFFF")
  def test_aL = mkGroks.forall{ mkGrok =>
    validSignedLongs.forall{ l => val g = mkGrok(l.toString); g{ implicit fail => g.aL } == Yes(l) } &&
    validSignedLongs.forall{ l => val g = mkGrok((BigInt(l) & ((BigInt(1)<<64)-1)).toString); g{ implicit fail => g.aL } == Yes(l) } &&
    validSignedLongs.forall{ l => val g = mkGrok("0x" + (BigInt(l) & ((BigInt(1)<<64)-1)).toString(16)); g{ implicit fail => g.aL } == Yes(l) } &&
    validSignedLongs.forall{ l => val g = mkGrok("0X" + (BigInt(l) & ((BigInt(1)<<64)-1)).toString(16)); g{ implicit fail => g.aL } == Yes(l) } &&
    invalidAnyLongs.forall{ l => val g = mkGrok(l); isNo(g{ implicit fail => g.aL }, Set(e.aL, e.L, e.xL, e.uL), Set(e.range)) } &&
    notevenNumbers.forall{ l => val g = mkGrok(l); isNo(g{ implicit fail => g.aL }, e.aL, e.wrong) }
  }  
  
  
  def main(args: Array[String]) { typicalMain(args) }
}
