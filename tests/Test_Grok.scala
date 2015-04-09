package kse.tests

import kse.flow._
import kse.eio._

object Test_Grok extends Test_Kse {
  val validBool = Array("true", "false", "True", "False", "TRUE", "FALSE", "trUE", "fAlSe")
  val invalidBool = Array("treu", "t", "fals", "f")
  
  def test_Z =
    validBool.forall{ s => val g = Grok(s); g{ implicit fail => g.Z } == Yes(s.toBoolean) } &&
    invalidBool.forall{ s => val g = Grok(s); !g{ implicit fail => g.Z }.isOk }
  
  val trueAnyBool = Array("yes", "Y", "on", "T", "True", "TRUE", "yeS")
  val falseAnyBool = Array("no", "Off", "n", "falSe")
  val invalidAnyBool = Array("ye", "nay", "tr", "tru", "fa", "of", "uh-huh")
  def test_aZ =
    trueAnyBool.forall{s => val g = Grok(s); g{ implicit fail => g.aZ } == Yes(true) } &&
    falseAnyBool.forall{s => val g = Grok(s); g{ implicit fail => g.aZ } == Yes(false) } && 
    validBool.forall{s => val g = Grok(s); g{ implicit fail => g.aZ } == Yes(s.toBoolean) } &&
    invalidAnyBool.forall{ s => val g = Grok(s).delimit(true); !g{ implicit fail => g.aZ }.isOk } &&
    invalidAnyBool.forall{ s => val g = Grok(s); !g{implicit fail => g.aZ}.isOk || g.position == 1 }
  
  val validSignedBytes = Array[Byte](1, 47, -125, 0, 127, -128)
  val invalidSignedBytes = Array[Int](-129, 128, 19238758, 1515, 1111999)
  def test_B =
    validSignedBytes.forall{ b => val g = Grok(b.toString); g{ implicit fail => g.B } == Yes(b) } &&
    invalidSignedBytes.forall{ b => val g = Grok(b.toString); !g{ implicit fail => g.B }.isOk }
  
  val invalidUnsignedBytes = Array[Int](-1, 256, 1111999)
  def test_uB =
    validSignedBytes.forall{ b => val g = Grok((b&0xFF).toString); g{ implicit fail => g.uB } == Yes(b) } &&
    invalidUnsignedBytes.forall{ b => val g = Grok(b.toString); !g{ implicit fail => g.uB }.isOk }
  
  def main(args: Array[String]) { typicalMain(args) }
}
