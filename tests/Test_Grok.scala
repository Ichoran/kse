package kse.tests

import kse.flow._
import kse.eio._

object Test_Grok extends Test_Kse {
  def test_Delimiter: Boolean = {
    var c: Char = 0
    do {
      if (c.isWhitespace != Delimiter.whiteDelimiter(c)) return false
      if ((c == ' ') != Delimiter.spaceDelimiter(c)) return false
      if ((c == '\t') != Delimiter.tabDelimiter(c)) return false
      if ((c == java.io.File.separatorChar) != Delimiter.fileDelimiter(c)) return false
      if ((c == java.io.File.pathSeparatorChar) != Delimiter.pathDelimiter(c)) return false
      if ((c == '\r' || c == '\n') != Delimiter.lineDelimiter(c)) return false
      c = (c + 1).toChar
    } while (c != 0)
    true
  }

  def test_DelimByte: Boolean = {
    var b: Byte = 0
    do {
      val c = b.toChar
      if (c.isWhitespace != DelimByte.whiteDelimByte(b)) return false
      if ((c == ' ') != DelimByte.spaceDelimByte(b)) return false
      if ((c == '\t') != DelimByte.tabDelimByte(b)) return false
      if ((c == java.io.File.separatorChar) != DelimByte.fileDelimByte(b)) return false
      if ((c == java.io.File.pathSeparatorChar) != DelimByte.pathDelimByte(b)) return false
      if ((c == '\r' || c == '\n') != DelimByte.lineDelimByte(b)) return false
      b = (b + 1).toByte
    } while (b != 0)
    true
  }

  def test_grok_where: Boolean = {
    List("", "salmon cod herring perch   bass pike", "fish", "   flounder tilapia", "halibut patagonian-tooth-fish   ").forall{ s =>
      val g = Grok(s)
      g.inToken == (s.length > 0 && s(0) != ' ') && g.hasNext == (s.length > 0) && !g.hasPrev &&
      (if (!g.hasNext) true else {
        probably{ implicit oops =>
          val b = Vector.newBuilder[(Boolean, Boolean, Boolean)]
          while (g.hasNext) {
            g.tok
            b += ((g.inToken, g.hasNext, g.hasPrev))
          }
          val v = b.result
          v.dropRight(1).forall{ case (a,b,c) => !a && b && c } && v.takeRight(1).forall{ case (a,b,c) => !a && !b && c }
        }.getOrElse(false)
      })
    }
  }

  def test_grok_movement: Boolean = {
    List("", "pike", "salmon cod herring", "bass pike perch salmon cod herring  perch   halibut flounder","   minnow grouper","sardine sole    ").forall{ s =>
      val n = if (s.length == 0) 0 else s.split("\\s+").filter(_.length != 0).length
      val g = Grok(s)
      probably{ implicit oops => g.skip }.isEmpty == (n == 0) &&
      probably{ implicit oops => g.back }.isEmpty == (n == 0) &&
      probably{ implicit oops => val a = g.tok; g.back; val b = g.tok; val c = Grok(s).tok; a == b && a == c } == { if (n==0) None else Some(true) } &&
      (0 to 10).forall{ i =>
        probably{ implicit oops => Grok(s).skip(i).tok } == probably{ implicit oops => val g = Grok(s); List.fill(i+1)(g.tok).last } &&
        probably{ implicit oops => Grok(s).skip(i) }.isEmpty == (i > n) && 
        Grok(s).drop(i).hasNext == (i < n) &&
        (0 to 12).forall{ j =>
          probably{ implicit oops => Grok(s).skip(i).back(j).tok }.isEmpty == (i > n || j > i || (i == n && j == 0)) &&
          probably{ implicit oops => Grok(s).skip(i).back(j).tok == { val g = Grok(s); List.fill(1+i-j)(g.tok).last } }.forall(_ == true) && 
          Grok(s).drop(i).undrop(j).hasNext == (n > 0 && (i < n || j > 0)) &&
          Grok(s).drop(i).undrop(j).hasPrev == (n > 0 && j < math.min(i,n))
        }
      }
    }
  }

  def main(args: Array[String]) { typicalMain(args) }
}
