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

  def test_grok_primitives: Boolean = {
    val g = Grok("false true a \u2141 \u0000 0 1 -1 127 -128 0 1 -1 32767 -32768 0 1 -1 2147483647 -2147483648 0 1 -1 9223372036854775807 -9223372036854775808 0 1 -1 0.12785183 2e4 2.4e4 Inf NaN 0 1 -1 0.12957135291875189 2e100 2.4e100 -Inf NaN")
    probably{ implicit oops =>
      !g.Z && g.Z &&
      g.C == 'a' && g.C == '\u2141' && g.C == '\u0000' &&
      g.B == 0 && g.B == 1 && g.B == -1 && g.B == Byte.MaxValue && g.B == Byte.MinValue &&
      g.S == 0 && g.S == 1 && g.S == -1 && g.S == Short.MaxValue && g.S == Short.MinValue &&
      g.I == 0 && g.I == 1 && g.I == -1 && g.I == Int.MaxValue && g.I == Int.MinValue &&
      g.L == 0 && g.L == 1 && g.L == -1 && g.L == Long.MaxValue && g.L == Long.MinValue &&
      g.F == 0f && g.F == 1f && g.F == -1f && g.F == 0.12785183f && g.F == 2e4 && g.F == 2.4e4 && g.F == Float.PositiveInfinity && g.F.isNaN  &&
      g.D == 0d && g.D == 1d && g.D == -1d && g.D == 0.12957135291875189 && g.D == 2e100 && g.D == 2.4e100 && g.D == Double.NegativeInfinity && g.D.isNaN
    }.exists(_ == true)
  }

  def test_grok_double_transitions: Boolean = {
    def transitionally(pre: String, e: String, already: List[(String, String)], depth: Int): List[(String,String)] = if (depth <= 0) already else {
      val pres = ('0' to '9').map{ c => val s = pre + c.toString; (s, (s+e).toDouble, c-'0') }
      pres.find(_._2 != pres.head._2) match {
        case Some((_, _, i)) => transitionally(pres(i-1)._1, e, ((pres(i-1)._1, pres(i)._1)) :: already, depth-1)
        case None          => transitionally(pres.last._1, e, already, depth-1)
      }
    }
    val rng = new scala.util.Random(189516)
    probably{ implicit oops =>
      (Double.MaxValue :: (-Double.MaxValue) :: Double.MinValue :: -Double.MinValue :: List.fill(200000)(math.pow(rng.nextDouble - 0.5,rng.nextInt(614)-307))).filter(x => !x.isNaN && !x.isInfinity).forall{ d =>
        val s = if (rng.nextBoolean) f"$d%.8e" else f"$d%.16e"
        val pre = s.takeWhile(_ != 'e')
        val e = s.drop(pre.length)
        transitionally(pre, e, Nil, 20).map(x => (x._1+e, x._2+e)).reverse.forall{ case (a,b) =>
          (a.toDouble == Grok(a).D && b.toDouble == Grok(b).D).tap(x => if (!x) println(s"$a $b"))
        }
      }
    }.exists(_ == true)
  }

  def main(args: Array[String]) { typicalMain(args) }
}
