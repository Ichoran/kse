package kse.tests

import kse.flow._
import kse.eio._

object Test_Delimiter extends Test_Kse {
  val spacey = "This is  a   test    of     spaces"
  val spaced = spacey.getBytes()
  val whitey = "\tThis is\n\n\na test-of-whitespace"
  val whited = whitey.getBytes()
  val liney = "This\nis\ra\n\rtest\r\nof\n\nnewlines"
  val lined = liney.getBytes()
  val delimey = ",;: \t\u0000,::\u0000\t  ;;;\t\t\u0000\u0000 "
  val delimey = delimey.getBytes()
  
  def tokenize(d: Delimiter, s: String, n: Int, pre: Boolean): Seq[String] = {
    import kse.coll._
    val vs = Vector.newBuilder[String]
    var i = 0
    while (i < s.length) {
      if (pre) {
        val l = d._tok(s, i, s.length, n).inLong
        val a = l.i0
        val b = l.i1
        vs += s.substring(a,b)
        i = b
      }
      else {
        val l = d.tok_(s, i, s.length, n).inLong
        vs += s.substring(i,l.i0)
        i = l.i1
      }
    }
    vs.result()
  }
  
  def test_Delimiter: Boolean = {
    import Delimiter._
    if (space(spacey, 0, spacey.length, 1) != 0) return false
    if (space(spacey, spacey.indexOf("     "), spacey.length, 1) != spacey.indexOf("    s")) return false
    if (space(spacey, spacey.indexOf("     "), spacey.length, spacey.length) != spacey.indexOf("spaces")) return false
    if (tokenize(space, spacey, spacey.length, false) != spacey.split(" +").toVector) return false
    // TODO: finish
    true
  }

  def main(args: Array[String]) { typicalMain(args) }
}
