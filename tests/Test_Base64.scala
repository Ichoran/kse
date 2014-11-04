package kse.tests

import kse.flow._
import kse.eio.base64._

object Test_Base64 extends Test_Kse {
  def one_encoding(b: Base64, pairs: Array[(String, Array[Byte])] = Array.empty) = {
    val rng = new scala.util.Random(8173251)
    val lengths = Array(0, 1, 2, 3, 4, 5, 6, 15, 16, 17, 18, 19) ++ Array.fill(100)(20 + rng.nextInt(1000))
    lengths.forall{ l =>
      val a = new Array[Byte](l)
      (0 to 9).forall{ n =>
        for (i <- a.indices) a(i) = (rng.nextInt >>> 24).toByte
        probably{ implicit oops => b.decode( b.encode(a) ) } match {
          case None => false
          case Some(aa) if aa.length != a.length => false
          case Some(aa) =>
            var j = 0
            while (j < a.length && a(j) == aa(j)) j += 1
            if (j != a.length) println(s"Uhoj $l")
            j == a.length
        }
      }
    } &&
    pairs.forall{ case (txt,bin) =>
      txt == b.encode(bin) &&
      (probably{ implicit oops => b.decode(txt) } match {
        case None => false
        case Some(b) if b.length != bin.length => false
        case Some(b) =>
          var j = 0
          while (j < b.length && b(j) == bin(j)) j += 1
          j == b.length
      })
    }
  }
  
  // Example strings from Wikipedia.

  def test_mime64: Boolean = one_encoding(Mime64, Array(("c3VyZS4=","sure.".getBytes), ("YXN1cmUu","asure.".getBytes), ("ZWFzdXJlLg==", "easure.".getBytes)))

  def test_url64: Boolean = one_encoding(Url64, Array(("bGVhcw","leas".getBytes), ("bGVhc3U","leasu".getBytes), ("bGVhc3Vy", "leasur".getBytes)))

  def test_uucodeLine: Boolean = one_encoding(UucodeLine, Array(("0V%T", "Cat".getBytes), (":'1T<#HO+W=W=RYW:6MI<&5D:6$N;W)G#0H","http://www.wikipedia.org\r\n".getBytes)))

  def test_binhexCore: Boolean = one_encoding(BinhexCore)

  def main(args: Array[String]) { typicalMain(args) }
}
