// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse.eio

import language.postfixOps

import scala.annotation.tailrec
import scala.reflect.{ClassTag => Tag}
import kse.flow._
import kse.coll.packed._

object GrokCharacter {
  final def elevateCase(c: Char): Char = {
    if (c < 0x130 || c > 0x212B) Character.toUpperCase(c)
    else if (c == 0x130 || c == 0x3F4 || c == 0x2126 || c >= 0x212A) Character.toUpperCase(Character.toLowerCase(c))
    else Character.toUpperCase(c)
  }
  final def elevateCase(c: Int): Int = {
    if (c < 0x130 || c > 0x212B) Character.toUpperCase(c)
    else if (c == 0x130 || c == 0x3F4 || c == 0x2126 || c >= 0x212A) Character.toUpperCase(Character.toLowerCase(c))
    else Character.toUpperCase(c)
  }
}

object GrokNumber {
  final val maxULongPrefix = 1844674407370955161L
  final val maxULongLastDigit = 5

  final val parseErrorNaNBits = java.lang.Double.doubleToRawLongBits(Double.NaN) ^ 1
  final val negativeZeroDoubleBits = java.lang.Double.doubleToRawLongBits(-0.0)
  final val parseErrorNaN = java.lang.Double.longBitsToDouble(parseErrorNaNBits)
  def isParseError(d: Double) = parseErrorNaNBits == java.lang.Double.doubleToRawLongBits(d)
  final val smallPowersOfTen = Array(
    1L, 10L, 100L, 1000L, 10000L, 100000L, 1000000L, 10000000L, 100000000L, 1000000000L,
    10000000000L, 100000000000L, 1000000000000L, 10000000000000L, 100000000000000L,
    1000000000000000L, 10000000000000000L, 100000000000000000L, 1000000000000000000L
  )
  
  final val stringInfinity = "infinity"
  final val bytesInfinity = stringInfinity.getBytes
  final val stringNaN = "nan"
  final val bytesNaN = stringNaN.getBytes
}

abstract class Grok {
  import kse.eio.{GrokError => e}
  protected var i = 0
  protected var i0 = 0
  protected var iN = 0
  protected var nSep = 1
  protected var reqSep = false
  protected var error: Byte = 0
  protected var ready: Byte = 0
  protected var delim: Delimiter = null
  
  final def rawDecimalDigitsUnsigned(s: String, limit: Int): Long = {
    val N = math.min(i+limit, iN)
    if (i >= N) { error = e.end.toByte; return 0L }
    var ans = s.charAt(i).toLong-'0'
    if (ans < 0 || ans > 9) { error = e.wrong.toByte; return 0L }
    i += 1
    error = 0
    while (i < N) {
      var c = s.charAt(i)-'0'
      if (c < 0 || c > 9) return ans
      ans = ans*10 + c
      i += 1
    }
    ans
  }
  
  final def rawDecimalDigitsUnsigned(ab: Array[Byte], limit: Int): Long = {
    val N = math.min(i+limit, iN)
    if (i >= N) { error = e.end.toByte; return 0L }
    var ans = ab(i).toLong-'0'
    if (ans < 0 || ans > 9) { error = e.wrong.toByte; return 0L }
    i += 1
    error = 0
    while (i < N) {
      var c = ab(i)-'0'
      if (c < 0 || c > 9) return ans
      ans = ans*10 + c
      i += 1
    }
    ans
  }
  
  final def rawHexidecimalDigits(s: String, limit: Int): Long = {
    val N = math.min(i+limit, iN)
    if (i >= N) { error = e.end.toByte; return 0L }
    var ans = s.charAt(i).toLong-'0'
    if (ans < 0) { error = e.wrong.toByte; return 0L }
    if (ans > 9) {
      ans = (ans - 17)&0xDF
      if (ans < 0 || ans >= 6) { error = e.wrong.toByte; return 0L }
      ans += 10
    }
    i += 1
    error = 0
    while (i < N) {
      var c = s.charAt(i)-'0'
      if (c < 0) return ans
      if (c > 9) {
        val cc = (c-17)&0xDF
        if (cc < 0 || cc >= 6) return ans
        else ans = (ans << 4) + (cc+10)
      }
      else ans = (ans << 4) + c
      i += 1
    }
    ans
  }
  
  final def rawHexidecimalDigits(ab: Array[Byte], limit: Int): Long = {
    val N = math.min(i+limit, iN)
    if (i >= N) { error = e.end.toByte; return 0L }
    var ans = ab(i).toLong-'0'
    if (ans < 0) { error = e.wrong.toByte; return 0L }
    if (ans > 9) {
      ans = (ans - 17)&0xDF
      if (ans < 0 || ans >= 6) { error = e.wrong.toByte; return 0L }
      ans += 10
    }
    i += 1
    error = 0
    while (i < N) {
      var c = ab(i)-'0'
      if (c < 0) return ans
      if (c > 9) {
        val cc = (c-17)&0xDF
        if (cc < 0 || cc >= 6) return ans
        else ans = (ans << 4) + (cc+10)
      }
      else ans = (ans << 4) + c
      i += 1
    }
    ans
  }
  
  final def rawParseDoubleDigits(s: String, point: Char): Long = {
    import GrokNumber._
    if (i >= iN) { error = e.end.toByte; return 0 }
    
    // Initial +-
    var c = s.charAt(i)
    var negative = false
    if (c == '+' || c == '-') {
      i += 1
      if (i >= iN) { error = e.end.toByte; return 0 }
      negative = c == '-'
      c = s.charAt(i)
    }
    
    // Infinity / NaN
    c = (c | 0x20).toChar
    if (c == 'n' || c == 'i') {
      val keyword = if (c == 'n') stringNaN else stringInfinity
      i += 1
      var k = 1
      while (k < keyword.length && i < iN && keyword.charAt(k) == (s.charAt(i) | 0x20)) { k += 1; i += 1 }
      if (k == 3 || k == keyword.length) {
        if (i < iN && Character.isLetter(s.charAt(i))) { i += 1; error = e.wrong.toByte; return 0 }
        error = e.coded
        return if (c == 'n') 0 else if (negative) -1 else 1
      }
      else {
        if (i >= iN) error = e.end.toByte else error = e.wrong.toByte
        return 0
      }
    }

    // Digits before decimal
    val j0 = i
    while (i < iN && s.charAt(i) == '0') i += 1
    val ja = i
    while (i < iN && { c = s.charAt(i); c >= '0' && c <= '9' }) i += 1
    val jb = i
    if (c == point && i < iN) {
      c = s.charAt(i)
      i += 1
    }
    // Digits after decimal
    val jc = i
    if (jb - ja == 0) while (i < iN && s.charAt(i) == '0') i += 1
    val jcc = i
    while (i < iN && { c = s.charAt(i); c >= '0' && c <= '9' }) i += 1
    var jd = i
    // Need some digit somewhere
    if ((jd-jc) + (jb-j0) <= 0) { error = e.wrong.toByte; return 0 }
    // Throw away trailing zeros, if any
    if (jd > jcc) while (jd > jc && s.charAt(jd-1) == '0') jd -= 1
    if (jd == jc) { jd = jb; while (jd > ja && s.charAt(jd-1) == '0') jd -= 1 }
    
    // Exponent
    val exp =
      if ((c | 0x20) != 'e') 0
      else {
        if (i >= iN) { error = e.end.toByte; return 0 }
        c = s.charAt(i)
        i += 1
        val expneg =
          if (c == '+' || c == '-') {
            i += 1
            if (i >= iN) { error = e.end.toByte; return 0 }
            c == '-'
          }
          else false
        val ei = i
        while (i < iN && s.charAt(i) == '0') i += 1
        val x = rawDecimalDigitsUnsigned(s,11)  // Match this number to magic 11-digit Long constant below
        if (i == ei) { 
          if (i >= iN) error = e.end.toByte else error = e.wrong.toByte
          return 0
        }
        if (x >= 10000000000L) while (i < iN && { c = s.charAt(i); c >= '0' && c <= '9' }) i += 1  // Consume any extra digits--they won't matter
        if (expneg) -x else x
      }
    
    // Zero case is easy
    if (jb == ja && jd <= jcc) {
      error = 0
      if (negative) { error = 0; negativeZeroDoubleBits }
      else { error = e.whole.toByte; 0L }
    }
    else {
      val lead = if (jb > ja) jb - ja - 1 else jc - jcc -1
      val lex = lead + exp
      if (lex > 308) {
        error = e.coded.toByte
        if (negative) -1 else 1
      }
      else if (lex < -324) {
        if (negative) { error = 0; negativeZeroDoubleBits }
        else { error = e.whole.toByte; 0L }
      }
      else {
        val len = if (jd < jcc) jd - ja else if (jb == ja) jd - jcc else jd - ja - 1
        val fex = (lex - len + 1).toInt
        println(s"$ja $jb $jc $jcc $jd $lead $lex $len $fex")
        if (lex <= 18 && fex >= 0) {
          var ans = 0L
          var n = len
          var k = if (jb == ja) jcc else ja
          while (n > 0) {
            if (k != jb) { ans = 10*ans + (s.charAt(k) - '0'); n -= 1 }
            k += 1
          }
          if (fex > 0) ans *= smallPowersOfTen(fex)
          if (negative) ans = -ans
          if ((ans < 0) == negative) {
            error = e.whole.toByte
            println(s"All worked out $ans!")
            return ans
          }
        }
        error = 0
        0L
      }
    }
  }
  
  /*
  final def rawCheckDoubleDigits(ab: Array[Byte], point: Byte): Int = {
    import GrokNumber._
    if (i >= iN) { error = e.end.toByte; return i }
    var c = ab(i)
    val j0 =
      if (c == '+' || c == '-') { if (i+1 >= iN) { error = e.end.toByte; return i+1 }; c = ab(i+1); i+1 }
      else i
    c = (c | 0x20).toByte
    if (c == 'n') {
      if (j0+2 >= iN) { error = e.end.toByte; return iN }
      if ((ab(j0+1) | 0x20) != 'a') { error = e.wrong.toByte; return j0+2 }
      if ((ab(j0+2) | 0x20) != 'n') { error = e.wrong.toByte; return j0+3 }
      error = e.nan.toByte
      return j0+3
    }
    if (c == 'i') {
      var j = j0+1
      var k = 1
      while (k < stringInfinity.length && j < iN && stringInfinity.charAt(k) == (ab(j) | 0x20)) { k += 1; j += 1 }
      if (k == 3 || k == stringInfinity.length) {
        if (k == 3 && j < iN && Character.isLetter(ab(j))) { error = e.wrong.toByte; return j+1 }
        error = e.infinity.toByte
        return if (ab(i) == '-') -j else j
      }
      else {
        if (j >= iN) error = e.end.toByte else error = e.wrong.toByte
        return j
      }
    }
    var j = j0
    while (j < iN && { c = ab(j); c == '0' }) j += 1
    val jz = j
    while (j < iN && { c = ab(j); c >= '0' && c <= '9' }) j += 1
    val ji = j
    val jd =
      if (c != point || j >= iN) ji
      else {
        j += 1
        while (j <= iN && { c = ab(j); c >= '0' && c <= '9' }) j += 1
        j
      }
    if (ji - j0 == 0 && (jd-1 <= ji)) error = e.end.toByte; else error = 0
    if ((c | 0x20) != 'e') {
      if (error != 0) return j
      else if (ji == jd && c != point && ji - jz <= 19) return -j
      else return j
    }
    j += 1
    if (error != 0) { error = e.wrong.toByte; return j }
    if (j >= iN) { error = e.end.toByte; return j }
    c = ab(j)
    val je = if (c == '+' || c == '-') j+1 else j
    j = je
    while (j < iN && { c = ab(j); c >= '0' && c <= '9' }) j += 1
    if (j==je) { error = e.end.toByte; return j }
    error = 0
    j
  }
  */
  
  final def errorCode: Int = error
  
  def position: Long
  def isEmpty(implicit fail: Hop[Long, this.type]): Boolean
  def trim(implicit fail: Hop[Long, this.type]): this.type
  def skip(implicit fail: Hop[Long, this.type]): this.type
  def skip(n: Int)(implicit fail: Hop[Long, this.type]): this.type
  def Z(implicit fail: Hop[Long, this.type]): Boolean
  def aZ(implicit fail: Hop[Long, this.type]): Boolean
  def B(implicit fail: Hop[Long, this.type]): Byte
  def uB(implicit fail: Hop[Long, this.type]): Byte
  def S(implicit fail: Hop[Long, this.type]): Short
  def uS(implicit fail: Hop[Long, this.type]): Short
  def C(implicit fail: Hop[Long, this.type]): Char
  def I(implicit fail: Hop[Long, this.type]): Int
  def uI(implicit fail: Hop[Long, this.type]): Int
  def xI(implicit fail: Hop[Long, this.type]): Int
  def aI(implicit fail: Hop[Long, this.type]): Int
  def L(implicit fail: Hop[Long, this.type]): Long
  def uL(implicit fail: Hop[Long, this.type]): Long
  def xL(implicit fail: Hop[Long, this.type]): Long
  def aL(implicit fail: Hop[Long, this.type]): Long
  def F(implicit fail: Hop[Long, this.type]): Float
  def xF(implicit fail: Hop[Long, this.type]): Float
  def D(implicit fail: Hop[Long, this.type]): Double
  def xD(implicit fail: Hop[Long, this.type]): Double
  def peek(implicit fail: Hop[Long, this.type]): Int
  def peekTok(implicit fail: Hop[Long, this.type]): String
  def peekBinIn(n: Int, target: Array[Byte], start: Int)(implicit fail: Hop[Long, this.type]): Int
  def sub[A](delimiter: Delimiter, maxSkip: Int)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A
  final def sub[A](delimiter: Delimiter)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A = sub(delimiter, 1)(parse)
  final def sub[A](delimiter: Char, maxSkip: Int)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A = sub(new CharDelim(delimiter), maxSkip)(parse)
  final def sub[A](delimiter: Char)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A = sub(new CharDelim(delimiter), 1)(parse)
  def visit[A](s: String, start: Int, end: Int, delimiter: Delimiter, maxSkip: Int)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A
  final def visit[A](s: String, delimiter: Delimiter, maxSkip: Int)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A =
    visit(s, 0, s.length, delimiter, maxSkip)(parse)
  final def visit[A](s: String, delimiter: Delimiter)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A =
    visit(s, 0, s.length, delimiter, nSep)(parse)
  final def visit[A](s: String, delimiter: Char, maxSkip: Int)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A =
    visit(s, 0, s.length, new CharDelim(delimiter), maxSkip)(parse)
  final def visit[A](s: String, delimiter: Char)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A =
    visit(s, 0, s.length, new CharDelim(delimiter), nSep)(parse)
  final def visit[A](s: String)(parse: this.type => A)(implicit fail: Hop[Long, this.type]): A =
    visit(s, 0, s.length, delim, nSep)(parse)
  def tok(implicit fail: Hop[Long, this.type]): String
  def quoted(implicit fail: Hop[Long, this.type]): String
  def quotedBy(left: Char, right: Char, esc: Char)(implicit fail: Hop[Long, this.type]): String
  def qtok(implicit fail: Hop[Long, this.type]): String
  def qtokBy(left: Char, right: Char, esc: Char)(implicit fail: Hop[Long, this.type]): String
  def base64(implicit fail: Hop[Long, this.type]): Array[Byte]
  def base64in(target: Array[Byte], start: Int)(implicit fail: Hop[Long, this.type]): Int
  def exact(s: String)(implicit fail: Hop[Long, this.type]): this.type
  def exactNoCase(s: String)(implicit fail: Hop[Long, this.type]): this.type
  def oneOf(s: String*)(implicit fail: Hop[Long, this.type]): String
  def oneOfNoCase(s: String*)(implicit fail: Hop[Long, this.type]): String
  def binary(n: Int)(implicit fail: Hop[Long, this.type]): Array[Byte]
  def binaryIn(n: Int, target: Array[Byte], start: Int)(implicit fail: Hop[Long, this.type]): this.type
  
  def apply[A](f: Hop[Long, this.type] => A): Ok[Long, A] = {
    val hop = UnboundHopSupplier.ofLong[this.type]
    try { Yes(f(hop)) } catch { case t if hop is t => No(hop as t value) }
  }
  def manual[A](f: Hop[Long, this.type] => A): Ok[Long, A] = {
    val hop = UnboundHopSupplier.notLong[this.type]
    try{ Yes(f(hop)) } catch { case t if hop is t => No(hop as t value) }
  }
  def tryTo(f: this.type => Boolean)(implicit fail: Hop[Long, this.type]): Boolean
}


object Grok {
  def apply(s: String): Grok = new GrokString(s, 0, s.length, Delimiter.white)
  def apply(s: String, d: Delimiter): Grok = new GrokString(s, 0, s.length, d)
  def apply(s: String, c: Char): Grok = new GrokString(s, 0, s.length, new CharDelim(c))
}


object GrokError {
 // Must fit in 3 bits
  final val end = 1
  final val wrong = 2
  final val range = 3
  final val delim = 4
  final val imprecise = -1
  final val coded = -2
  final val whole = -3
  
  // Must fit in 5 bits
  final val Z = 1
  final val aZ = 2
  final val B = 3
  final val uB = 4
  final val S = 5
  final val uS = 6
  final val C = 7
  final val I = 8
  final val uI = 9
  final val xI = 10
  final val aI = 11
  final val L = 12
  final val uL = 13
  final val xL = 14
  final val aL = 15
  final val F = 16
  final val xF = 17
  final val D = 18
  final val xD = 19
  final val tok = 20
  final val quote = 21
  final val qBy = 22
  final val b64 = 23
  final val exact = 24
  final val oneOf = 25
  final val bin = 26
}
