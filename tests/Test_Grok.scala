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
  
  def main(args: Array[String]) { typicalMain(args) }
}
