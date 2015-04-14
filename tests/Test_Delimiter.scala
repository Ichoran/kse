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
  val delimey = ",;: \t\u0000,::\u0000\t  ;\n \n\r;;\t\t\n\u0000\u0000 "
  val delimed = delimey.getBytes()
  
  def getDelim(c: Char) = c match {
    case ' ' => Delimiter.space
    case '\t' => Delimiter.tab
    case ',' => Delimiter.comma
    case ':' => Delimiter.colon
    case ';' => Delimiter.semi
    case _ => new CharDelim(c)
  }
  
  def stringDelim(s: String, delim: Delimiter): Seq[Seq[Int]] = {
    val r = 0 to s.length
    (for (i <- r) yield {
      Seq(delim(s, i, s.length, 1), delim(s, i, s.length, Int.MaxValue), delim.not(s, i, s.length))
    }).transpose
  }
  
  def bufferDelim(ab: Array[Byte], delim: Delimiter): Seq[Seq[Int]] = {
    val r = 0 to ab.length
    (for (i <- r) yield {
      Seq(delim(ab, i, ab.length, 1), delim(ab, i, ab.length, Int.MaxValue), delim.not(ab, i, ab.length))
    }).transpose
  }
  
  def canonSingle(s: String, p: Char => Boolean) = {
    val r = 0 to s.length
    (for (i <- r) yield {
      Seq(
        if (i >= s.length) -1 else if (p(s.charAt(i))) i+1 else i,
        if (i >= s.length) -1
          else if (!p(s.charAt(i))) i
          else { val rest = s.substring(i).dropWhile(p); if (rest.nonEmpty) i+s.substring(i).indexOf(rest) else s.length },
        if (i >= s.length) s.length
          else if (p(s.charAt(i))) i
          else { val rest = s.substring(i).dropWhile(c => !p(c)); if (rest.nonEmpty) i + s.substring(i).indexOf(rest) else s.length }
      )
    }).transpose
  }
  
  def stringCD(s: String, c: Char): Seq[Seq[Int]] = stringDelim(s, getDelim(c))
  def bufferCD(ab: Array[Byte], c: Char): Seq[Seq[Int]] = bufferDelim(ab, getDelim(c))
  def canonCD(s: String, c: Char): Seq[Seq[Int]] = canonSingle(s, _ == c)
  
  def stringSpace(s: String) = stringCD(s, ' ')
  def bufferSpace(ab: Array[Byte]) = bufferCD(ab, ' ')
  def canonSpace(s: String) = canonCD(s, ' ')
  
  def stringWhite(s: String) = stringDelim(s, Delimiter.white)
  def bufferWhite(ab: Array[Byte]) = bufferDelim(ab, Delimiter.white)
  def canonWhite(s: String) = canonSingle(s, _.isWhitespace)
  
  def canonLine(s: String): Seq[Seq[Int]] = {
    var idx = -1
    val aiss = s.linesWithSeparators.toVector.map(_.map(c => (c.toInt, { idx += 1; idx })))
    val bss = s.lines.toVector
    val ones = (aiss zip bss).map{ case (ais, bs) => for (i <- ais.indices) yield { if (i < bs.length) ais(i)._2 else ais.last._2+1 } }
    val nots = (aiss zip bss).map{ case (ais, bs) => for (i <- ais.indices) yield { if (i < bs.length) ais(bs.length-1)._2+1 else ais(i)._2 } }
    val alls = (aiss zip bss).zipWithIndex.map{ case ((ais, bs), n) => for (i <- ais.indices) yield {
      if (i < bs.length) ais(i)._2
      else if (n+1 >= bss.length || bss(n+1).length > 0) ais.last._2+1
      else {
        val m = n + bss.drop(n+1).takeWhile(_.isEmpty).length
        aiss(m).last._2 + 1
      }
    }}
    Seq(ones.flatten :+ -1, alls.flatten :+ -1, nots.flatten :+ s.length)
  }
  
  def stringDZero(s: String, delim: Delimiter) = stringDelim(s, delim terminatedBy Delimiter.zero)
  def bufferDZero(ab: Array[Byte], delim: Delimiter) = bufferDelim(ab, delim terminatedBy Delimiter.zero)
  
  def canonDZero(s: String, canon: String => Seq[Seq[Int]]) = {
    val Seq(onez, allz, notz) = canonSingle(s, _ == '\u0000')
    def chop(x: String, ns: Seq[Int], sub: Int = 0): Seq[String] = if (x.isEmpty) Seq() else {
      val i = math.min(x.length, ns(0)+1-sub)
      val y = x.substring(0,i)
      y +: chop(x.substring(i), ns.drop(i), sub+i)
    }
    val parts = chop(s, notz)
    val incs = parts.scanLeft(0)(_ + _.length)
    val subs = parts.map(s => if (s.endsWith("\u0000")) s.dropRight(1) else s)
    val anss = (subs.map(canon) zip incs).map{ case (xss, n) => xss.map(xs => xs.map{ case x => if (x >= 0) x + n else x }) }
    anss.transpose.map(_.flatten)
  } 
  
  def checkTrio(s: Seq[Seq[Int]], b: Seq[Seq[Int]], c: Seq[Seq[Int]]) = {
    (s, c).zipped.forall{ (x,y) => x =?= y } &&
    (b, c).zipped.forall{ (x,y) => x =?= y }
  }
  
  def checkAll(ss: Seq[Seq[Seq[Int]]]) = ss.headOption.forall{ s =>
    ss.drop(1).forall{ t => (s, t).zipped.forall{ (x,y) => x =?= y } }
  }

  def test_Spaces: Boolean = checkTrio(stringSpace(spacey), bufferSpace(spaced), canonSpace(spacey))
  
  def test_Whites: Boolean = checkTrio(stringWhite(whitey), bufferWhite(whited), canonWhite(whitey))
  
  def test_Newlines: Boolean = checkTrio(stringDelim(liney, Delimiter.newline), bufferDelim(lined, Delimiter.newline), canonLine(liney))
  
  def test_Various: Boolean = ":;,\t~".forall{ c =>
    checkTrio(stringCD(delimey, c), bufferCD(delimed, c), canonCD(delimey, c))
  }    
  
  def test_ZeroWhite: Boolean = checkTrio(stringDZero(delimey, Delimiter.white), bufferDZero(delimed, Delimiter.white), canonDZero(delimey, canonWhite))

  def test_ZeroNewline: Boolean = checkTrio(stringDZero(delimey, Delimiter.newline), bufferDZero(delimed, Delimiter.newline), canonDZero(delimey, canonLine))
  
  def test_ZeroVarious: Boolean = " :;,\t~".forall{ c =>
    checkTrio(stringDZero(delimey, new CharDelim(c)), bufferDZero(delimed, new CharDelim(c)), canonDZero(delimey, canonCD(_, c)))
  }
  
  def test_ConsistentDoubleDeep: Boolean = checkAll(Seq(
    stringDelim(delimey, Delimiter.colon.terminatedBy( Delimiter.tab terminatedBy Delimiter.zero )),
    bufferDelim(delimed, Delimiter.colon.terminatedBy( Delimiter.tab terminatedBy Delimiter.zero )),
    stringDelim(delimey, (Delimiter.colon terminatedBy Delimiter.tab).terminatedBy(Delimiter.zero)),
    bufferDelim(delimed, (Delimiter.colon terminatedBy Delimiter.tab).terminatedBy(Delimiter.zero))
  ))

  def test_switchWith: Boolean = {
    val s = "one\ntwo\tthree four\nfive"
    val d = new TerminatedDelim(Delimiter.tab, Delimiter.newline)
    val d2 = d.switchWith(Delimiter.space)
    d.not(s,0,s.length) == 3 &&
    d(s, 3, s.length, 1) == -1 &&
    d.not(s, 4, s.length) == 7 &&
    d(s, 7, s.length, 1) == 8 &&
    d2.not(s, 8, s.length) == 13 &&
    d2(s, 13, s.length, 1) == 14 &&
    d2.not(s, 14, s.length) == 18 &&
    d2(s, 18, s.length, 1) == -1
  }

  def main(args: Array[String]) { typicalMain(args) }
}
