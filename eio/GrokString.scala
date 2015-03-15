// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015 Rex Kerr and UCSF

package kse.eio

import language.postfixOps

import scala.annotation.tailrec
import scala.reflect.ClassTag
import kse.flow._
import kse.coll.packed._

final class GrokString(private[this] var string: String, initialStart: Int, initialEnd: Int, initialDelimiter: Delimiter, initialnSep: Int = 1, initialReqSep: Boolean = false) extends Grok {
  import kse.eio.{GrokErrorCodes => e}

  private[this] var t = 0
  i0 = math.max(0, math.min(initialStart, string.length))
  iN = math.min(string.length, math.max(initialEnd, i0))
  delim = initialDelimiter
  nSep = math.max(1, initialnSep)
  reqSep = initialReqSep
  ready = 1
  
  def input(newInput: String, start: Int = 0, end: Int = Int.MaxValue): this.type = {
    string = newInput
    i0 = math.max(0, math.min(start, string.length))
    iN = math.min(string.length, math.max(end, i0))
    i = i0
    t = 0
    this
  }
  
  private final def err(what: Int, who: Int) = GrokError(what.toByte, who.toByte, t, i)(string)
  private final def prepare(needed: Int, id: Int)(fail: GrokHop[this.type]): Boolean = {
    error = 0
    if (ready == 0) {
      val j = delim(string, i, iN, nSep)
      if (j < 0) { fail.on(err(e.end, id)); return false }
      i = j
      ready = 1
    }
    if (iN - i < needed) { fail.on(err(e.end, id)); false } else true
  }
  private final def wrapup(id: Int)(fail: GrokHop[this.type]): Boolean = {
    if (reqSep) {
      ready = 1
      val j = delim(string, i, iN, nSep)
      if (j == i) { fail.on(err(e.delim,id)); false }
      else if (j < 0) { t += 1; true }
      else { i = j; t += 1; true }
    }
    else { ready = 0; t += 1; true }
  }
  
  private final def smallNumber(dig: Int, lo: Long, hi: Long, id: Int)(fail: GrokHop[this.type]): Long = {
    if (!prepare(1, id)(fail)) return 0
    val negative = {
      if (lo >= 0) false
      else if (string.charAt(i) == '-') { i += 1; true }
      else false
    } 
    var j = i
    while (i < iN && string.charAt(i) == '0') i += 1
    val l = {
      if (j < i && (i >= iN || { val c = string.charAt(i); c < '0' || c > '9' })) { error = 0; 0 }
      else { j = i; rawDecimalDigitsUnsigned(string, dig+1) }
    }
    if (error > 0) { fail.on(err(error,id)); return 0 }
    if (i-j > dig) { fail.on(err(e.range,id)); return 0 }
    val ans = if (negative) -l else l
    if (ans < lo || ans > hi) { fail.on(err(e.range,id)); return 0 }
    if (!wrapup(id)(fail)) return 0
    ans
  }
  private final def longNumber(unsigned: Boolean, id: Int)(fail: GrokHop[this.type]): Long = {
    if (!prepare(1, id)(fail)) return 0
    val negative = {
      if (unsigned) false
      else if (string.charAt(i) == '-') { i += 1; true }
      else false
    }
    var j = i
    while (i < iN && string.charAt(i) == '0') i += 1
    val l = {
      if (j < i && (i >= iN || { val c = string.charAt(i); c < '0' || c > '9' })) { error = 0; 0 }
      else { j = i; rawDecimalDigitsUnsigned(string, 19) }
    }
    if (error > 0) { fail.on(err(error,id)); return 0 }
    val ans = 
      if (unsigned) {
        if (i-j < 19 || i >= iN) l
        else {
          val c = string.charAt(i)-'0'
          if (c < 0 || c > 9) l
          else if (l > GrokNumber.maxULongPrefix || c > GrokNumber.maxULongLastDigit) { fail.on(err(e.range, id)); return 0 }
          else {
            i += 1
            if (i < iN && { val c = string.charAt(i); c >= '0' && c <= '9'}) { fail.on(err(e.range, id)); return 0 }
            l*10 + c
          }
        }
      }
      else {
        if (i-j == 19 && i < iN && { val c = string.charAt(i); c >= '0' && c <= '9'}) { fail.on(err(e.range, id)); return 0 }
        val x = if (negative) -l else l
        if (x != 0 && (x < 0) != negative) { fail.on(err(e.range,id)); return 0 }
        x
      }
    if (!wrapup(id)(fail)) return 0
    ans
  }
  private final def hexidecimalNumber(dig: Int, id: Int)(fail: GrokHop[this.type]): Long = {
    if (!prepare(1, id)(fail)) return 0
    var j = i
    while (i < iN && string.charAt(i) == '0') i += 1
    val ans = {
      if (j < i && (i >= iN || { val c = string.charAt(i); c < '0' || (c > '9' && ((c&0xDF)<'A' || (c&(0xDF))>'F')) })) { error = 0; 0 }
      else { j = i; rawHexidecimalDigits(string, dig+1) }
    }
    if (error > 0) { fail.on(err(error,id)); return 0 }
    if (i-j > dig) { fail.on(err(e.range,id)); return 0 }
    if (!wrapup(id)(fail)) return 0
    ans
  }
  private final def matchAsciiInsensitive(lowered: String, id: Int)(fail: GrokHop[this.type]) {
    error = 0
    var j = 0
    while (j < lowered.length) {
      if (i >= iN) { fail.on(err(e.end, id)); return }
      if (lowered.charAt(j) != (string.charAt(i) | 0x20)) { fail.on(err(e.wrong, id)); return }
      i += 1; j += 1
    }
    ready = 0
  }
  
  final def customError = err(e.wrong, e.custom)
  
  final def position = {
    if (ready == 0) {
      ready = 1
      val j = delim(string, i, iN, nSep)
      i = if (j < 0) iN else j
    }
    i.toLong
  }
  final def isEmpty(implicit fail: GrokHop[this.type]) = {
    if (ready == 0) {
      ready = 1
      val j = delim(string, i, iN, nSep)
      i = if (j < 0) iN else j
    }
    i >= iN
  }
  final def trim(implicit fail: GrokHop[this.type]): this.type = {
    if (ready != 2 && i < iN) {
      ready = 2
      val j = delim(string, i, iN, Int.MaxValue)
      i = if (j < 0) iN else j
    }
    this
  }
  final def skip(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    val j = (if (ready != 0) delim.tok_(string, i, iN, 0) else delim._tok(string, i, iN, nSep)).inLong.i1
    ready = 0
    if (j < 0) { fail.on(err(e.end, e.tok)); return this }
    t += 1
    i = j
    this
  }
  final def skip(n: Int)(implicit fail: GrokHop[this.type]): this.type = { 
    error = 0
    var k = n
    while (k > 0 && error == 0) { skip(fail); k -= 1 }
    this
  }
  final def Z(implicit fail: GrokHop[this.type]): Boolean = {
    if (!prepare(4, e.Z)(fail)) return false
    var c = string.charAt(i)&0xDF
    val ans = 
      if (c == 'T') { matchAsciiInsensitive("true", e.Z)(fail); true }
      else if (c == 'F') { matchAsciiInsensitive("false", e.Z)(fail); false }
      else { fail.on(err(e.wrong, e.Z)); return false }
    if (!wrapup(e.Z)(fail)) return false
    ans
  }
  final def aZ(implicit fail: GrokHop[this.type]): Boolean = {
    if (!prepare(1, e.aZ)(fail)) return false
    val ans = (string.charAt(i)&0xDF) match {
      case 16 | 17 => smallNumber(1, 0, 1, e.aZ)(fail) == 1
      case 'T' =>
        i += 1
        if (i < iN && (string.charAt(i)&0xDF) == 'R') { i -= 1; matchAsciiInsensitive("true", e.aZ)(fail) }
        true
      case 'F' =>
        i += 1
        if (i < iN && (string.charAt(i)&0xDF) == 'A') { i -= 1; matchAsciiInsensitive("false", e.aZ)(fail) }
        false
      case 'Y' =>
        i += 1
        if (i < iN && (string.charAt(i)&0xDF) == 'E') { i -= 1; matchAsciiInsensitive("yes", e.aZ)(fail) }
        true
      case 'N' =>
        i += 1
        if (i < iN && (string.charAt(i)&0xDF) == 'O') i += 1
        false
      case 'O' =>
        i += 1
        if (i >= iN) { fail.on(err(e.end, e.aZ)); return false }
        val c = string.charAt(i) & 0xDF
        if (c == 'N') { i += 1; true }
        else if (c == 'F') {
          i += 1
          if (i >= iN) { fail.on(err(e.end, e.aZ)); return false }
          if ((string.charAt(i) & 0xDF) != 'F') { fail.on(err(e.wrong, e.aZ)); return false }
          i += 1
          false
        }
        else { fail.on(err(e.wrong, e.aZ)); return false }
      case _ =>
        fail.on(err(e.wrong, e.aZ)); return false
    }
    if (!wrapup(e.aZ)(fail)) return false
    ans
  }
  final def B(implicit fail: GrokHop[this.type]): Byte = smallNumber(3, Byte.MinValue, Byte.MaxValue, e.B)(fail).toByte
  final def uB(implicit fail: GrokHop[this.type]): Byte = smallNumber(3, 0, 0xFFL, e.uB)(fail).toByte
  final def S(implicit fail: GrokHop[this.type]): Short = smallNumber(5, Short.MinValue, Short.MaxValue, e.S)(fail).toShort
  final def uS(implicit fail: GrokHop[this.type]): Short = smallNumber(5, 0, 0xFFFFL, e.uS)(fail).toShort
  final def C(implicit fail: GrokHop[this.type]): Char = {
    if (!prepare(1, e.C)(fail)) return 0
    val ans = string.charAt(i)
    i += 1
    if (!wrapup(e.C)(fail)) return 0
    ans
  }
  final def I(implicit fail: GrokHop[this.type]): Int = smallNumber(10, Int.MinValue, Int.MaxValue, e.I)(fail).toInt
  final def uI(implicit fail: GrokHop[this.type]): Int = smallNumber(10, 0, 0xFFFFFFFFL, e.uI)(fail).toInt
  final def xI(implicit fail: GrokHop[this.type]): Int = hexidecimalNumber(8, e.xI)(fail).toInt
  final def aI(implicit fail: GrokHop[this.type]): Int = {
    if (!prepare(1, e.aI)(fail)) return 0
    val c = string.charAt(i)
    if (c == '-') I(fail)
    else if (c == '+') {
      i += 1
      if (i+1 >= iN) { fail.on(err(e.end, e.aI)); return 0 }
      val c = string.charAt(i)
      if (c >= '0' && c <= '9') I(fail)
      else { fail.on(err(e.wrong, e.aI)); return 0 }
    }
    else if (c >= '0' && c <= '9') {
      if (c == '0' && i+2 < iN && (string.charAt(i+1)|0x20)=='x' && { val c = string.charAt(i+2) | 0x20; (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') }) {
        i += 2
        xI(fail)
      }
      else uI(fail)
    }
    else { fail.on(err(e.wrong, e.aI)); return 0 }
  }
  final def L(implicit fail: GrokHop[this.type]): Long = longNumber(false, e.L)(fail)
  final def uL(implicit fail: GrokHop[this.type]): Long = longNumber(true, e.L)(fail)
  final def xL(implicit fail: GrokHop[this.type]): Long = hexidecimalNumber(16, e.xI)(fail)
  final def aL(implicit fail: GrokHop[this.type]): Long = {
    if (!prepare(1, e.aL)(fail)) return 0
    val c = string.charAt(i)
    if (c == '-') L(fail)
    else if (c == '+') {
      i += 1
      if (i+1 >= iN) { fail.on(err(e.end, e.aL)); return 0 }
      val c = string.charAt(i)
      if (c >= '0' && c <= '9') L(fail)
      else { fail.on(err(e.wrong, e.aL)); return 0 }
    }
    else if (c >= '0' && c <= '9') {
      if (c == '0' && i+1 < iN && (string.charAt(i+1)|0x20)=='x') {
        if (i+2 >= iN) { fail.on(err(e.end, e.aL)); return 0 }
        i += 2
        xL(fail)
      }
      else uL(fail)
    }
    else { fail.on(err(e.wrong, e.aI)); return 0 }
  }
  final def F(implicit fail: GrokHop[this.type]): Float = D(fail).toFloat
  final def xF(implicit fail: GrokHop[this.type]): Float = { fail(err(e.missing, e.xF)); 0 }
  final def D(implicit fail: GrokHop[this.type]): Double = {
    import GrokNumber._
    if (!prepare(1, e.D)(fail)) return parseErrorNaN
    val iOld = i
    val j = rawParseDoubleDigits(string, '.')
    if (error > 0) { ready = 0; fail(err(error, e.D)); return parseErrorNaN }
    else {
      ready = 0
      val ans = error match {
        case e.whole => j.toDouble
        case e.coded => if (j == 0) Double.NaN else if (j < 0) Double.NegativeInfinity else Double.PositiveInfinity
        case e.imprecise =>
          println("Imprecise!")
          try { java.lang.Double.parseDouble(string.substring(iOld, i)) }
          catch { case _: NumberFormatException => error = e.wrong.toByte; parseErrorNaN }
        case _ => java.lang.Double.longBitsToDouble(j)
      }
      if (error > 0) { fail(err(error, e.D)); return parseErrorNaN }
      error = 0
      if (!wrapup(e.D)(fail)) return parseErrorNaN
      ans
    }
  }
  final def xD(implicit fail: GrokHop[this.type]): Double = { fail(err(e.missing, e.xD)); 0 }
  final def peek(implicit fail: GrokHop[this.type]): Int = {
    if (ready == 0) {
      ready = 1
      val j = delim(string, i, iN, nSep)
      if (j < 0) return -1
      i = j
    }
    string.charAt(i)
  }
  final def peekTok(implicit fail: GrokHop[this.type]): String = {
    val l = (if (ready != 0) delim.tok_(string, i, iN, 0) else delim._tok(string, i, iN, nSep)).inLong
    val a = l.i0
    if (a == -1) null
    else if (ready != 0) string.substring(i,a)
    else { i = a; ready = 1; string.substring(a, l.i1) }
  }
  final def peekBinIn(n: Int, target: Array[Byte], start: Int)(implicit fail: GrokHop[this.type]): Int = {
    if (ready == 0) {
      val j = delim(string, i, iN, nSep)
      i = if (j < 0) iN else j
      ready = 1
      if (j < 0) return 0
    }
    var k = start
    val kN = start + n
    var j = i
    while (j < iN && k < kN) {
      val c = string.charAt(j)
      if (c > 0xFF) return (k - start)
      target(k) = c.toByte
      k += 1
      j += 1
    }
    k - start
  }
  final def tok(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.tok)(fail)) return null
    val a = delim.tok_(string, i, iN, 0).inLong.i0
    val ans = string.substring(i, a)
    ready = 0
    t += 1
    i = a
    ans
  }
  final def quoted(implicit fail: GrokHop[this.type]): String = quotedBy('"', '"', '\\')(fail)
  private def quotedByWithEscapes(iStart: Int, iEnd: Int, left: Char, right: Char, esc: Char, escaper: GrokEscape)(implicit fail: GrokHop[this.type]): String = {
    val buf = new Array[Char](iEnd - iStart)
    var j = 0
    var k = iStart
    while (k < iEnd) {
      val c = string.charAt(k)
      k += 1
      if (c != esc) { buf(j) = c; j += 1 }
      else {
        val c = string.charAt(k)
        val x = escaper.replace(c)
        if (x < 65536) {
          buf(j) = x.toChar
          j += 1
          k += 1
        }
        else {
          var n = (x >> 16) & 0xF
          if (k+n >= iEnd) { fail.on(err(e.wrong, e.quote)); return null }
          var l = 0L
          if ((x & 0x100000) != 0) {
            val iOld = i
            i = k+1
            l = rawHexidecimalDigits(string, k+n)
            if (error != 0) { fail.on(err(error, e.quote)); return null }
            i = iOld
          }
          else while (n > 0) { l = (l << 16) | string.charAt(k+n-1); n -= 1 }
          k += n+1
          val shift = if ((x&0xF0) != 0) 16 else 8
          val mask = if (shift == 8) 0xFF else 0xFFFF
          n = x & 0xF
          l = escaper.extended(c, l)
          while (n > 0) {
            buf(j) = (l & mask).toChar
            l = l >>> shift
            n -= 1
            j += 1
          }
        }
      }
    }
    val ans = new String(buf, 0, j)
    if (!wrapup(e.quote)(fail)) return null
    ans
  }
  private def quotedByDegenerately(q: Char)(implicit fail: GrokHop[this.type]): String = {
    val iStart = i
    var doublets = 0
    while (i < iN) {
      val c = string.charAt(i)
      if (c == q) {
        if (doublets == 2) doublets = 1
        else if (i+1 < iN && string.charAt(i+1) == q) doublets = 2
        else {
          val ans = 
            if (doublets > 0) {
              val buf = new Array[Char](i - iStart)
              var k = 0
              var j = iStart
              var skip = false
              while (j < i) {
                val c = string.charAt(j)
                if (!skip) {
                  buf(k) = c
                  k += 1
                  if (c == q) skip = true
                }
                else skip = false
                j += 1
              }
              new String(buf, 0, k)
            }
            else string.substring(iStart, i)
          i += 1
          if (!wrapup(e.quote)(fail)) return null
          return ans
        }
      }
      i += 1
    }
    fail.on(err(e.end, e.quote))
    null
  }
  final def quotedBy(left: Char, right: Char, esc: Char, escaper: GrokEscape = GrokEscape.standard)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(2, e.quote)(fail)) return null
    val c = string.charAt(i)
    if (c != left) { fail.on(err(e.wrong, e.quote)); return null }
    i += 1
    if (left == right && left == esc) return quotedByDegenerately(left)(fail)
    val iStart = i
    var depth = 1
    var escies = 0
    var esced = false
    while (i < iN && depth > 0) {
      val c = string.charAt(i)
      if (esced) esced = false
      else {
        if (c == right) depth -= 1
        else if (c == left) depth += 1
        else if (c == esc) { escies += 1; esced = true }
      }
      i += 1
    }
    if (depth != 0) { fail.on(err(e.wrong, e.quote)); return null }
    if (escies > 0) return quotedByWithEscapes(iStart, i-1, left, right, esc, escaper)(fail)
    val ans = string.substring(iStart,i-1)
    if (!wrapup(e.quote)(fail)) return null
    ans
  }
  final def qtok(implicit fail: GrokHop[this.type]): String = qtokBy('"', '"', '\\')(fail)
  final def qtokBy(left: Char, right: Char, esc: Char, escaper: GrokEscape = GrokEscape.standard)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.quote)(fail)) return null
    val c = string.charAt(i)
    if (c != left) tok(fail) else quotedBy(left, right, esc, escaper)(fail)
  }
  final def base64(implicit fail: GrokHop[this.type]): Array[Byte] = {
    if (!prepare(0, e.tok)(fail)) return null
    val a = delim.tok_(string, i, iN, 0).inLong.i0
    val buffer = new Array[Byte](((a - i).toLong*3/4).toInt)
    val n = kse.eio.base64.decodeFromBase64String(string, i, a, buffer, 0, kse.eio.base64.Url64.decoder)
    if (n < 0) { i = i - (n+1); fail.on(err(e.wrong, e.b64)); return null }
    i = a
    t += 1
    ready = 0
    if (n < buffer.length) java.util.Arrays.copyOf(buffer, n) else buffer
  }
  final def base64in(target: Array[Byte], start: Int)(implicit fail: GrokHop[this.type]): Int = {
    if (!prepare(0, e.tok)(fail)) return -1
    val a = delim.tok_(string, i, iN, 0).inLong.i0
    val n = kse.eio.base64.decodeFromBase64String(string, i, a, target, start, kse.eio.base64.Url64.decoder)
    if (n < 0) { i = i - (n+1); fail.on(err(e.wrong, e.b64)); return -1 }
    i = a
    t += 1
    ready = 0
    n
  }
  final def exact(c: Char)(implicit fail: GrokHop[this.type]): this.type = {
    if (!prepare(1, e.exact)(fail)) return null
    if (string.charAt(i) != c) { fail.on(err(e.wrong, e.exact)); return this }
    i += 1
    if (!wrapup(e.exact)(fail)) return null
    this
  }
  final def exact(s: String)(implicit fail: GrokHop[this.type]): this.type = {
    if (!prepare(0, e.exact)(fail)) return null
    var k = 0
    while (i < iN && k < s.length && string.charAt(i) == s.charAt(k)) { i += 1; k += 1 }
    if (k < s.length) { fail.on(err(e.wrong, e.exact)); return this }
    if (!wrapup(e.exact)(fail)) return null
    this
  }
  final def exactNoCase(s: String)(implicit fail: GrokHop[this.type]): this.type = {
    import GrokCharacter._
    if (!prepare(0, e.exact)(fail)) return null
    var k = 0
    while (i < iN && k < s.length && {
      val c = string.charAt(i)
      val cc = s.charAt(k)
      (c == cc) || {
        if (k > 0 && Character.isLowSurrogate(c)) Character.toUpperCase(string.codePointAt(i-1)) == Character.toUpperCase(s.codePointAt(k-1))
        else elevateCase(c) == elevateCase(cc)
      }
    }) { i += 1; k += 1 }
    if (k < s.length) { fail.on(err(e.wrong, e.exact)); return this }
    if (!wrapup(e.exact)(fail)) return null
    this
  }
  final def oneOf(s: String*)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.exact)(fail)) return null
    val a = delim.tok_(string, i, iN, 0).inLong.i0
    ready = 0
    var n = 0
    while (n < s.length) {
      if (a - i == s(n).length) {
        var j = 0
        var k = i
        val sn = s(n)
        while (k < a && sn.charAt(j) == string.charAt(k)) { k += 1; j += 1 }
        if (k == a) { i = a; t += 1; return sn }
      }
      n += 1
    }
    i = a
    fail.on(err(e.wrong, e.oneOf))
    null
  }
  final def oneOfNoCase(s: String*)(implicit fail: GrokHop[this.type]): String = {
    import GrokCharacter._
    if (!prepare(0, e.exact)(fail)) return null
    val a = delim.tok_(string, i, iN, 0).inLong.i0
    ready = 0
    var n = 0
    while (n < s.length) {
      if (a - i == s(n).length) {
        var j = 0
        var k = i
        val sn = s(n)
        while (k < a && {
          val c = string.charAt(k)
          val cc = sn.charAt(j)
          (c == cc) || {
            if (j > 0 && Character.isLowSurrogate(c)) Character.toUpperCase(sn.codePointAt(j-1)) == Character.toUpperCase(string.codePointAt(k-1))
            else elevateCase(c) == elevateCase(cc)
          }
        }) { k += 1; j += 1 }
        if (k == a) { i = a; t += 1; return sn }
      }
      n += 1
    }
    i = a
    fail.on(err(e.wrong, e.oneOf))
    null
  }
  final def binary(n: Int)(implicit fail: GrokHop[this.type]): Array[Byte] = {
    if (!prepare(n, e.bin)(fail)) return null
    val ans = {
      val buf = new Array[Byte](n)
      var j = 0
      while (j < n) {
        val c = string.charAt(i)
        if (c > 255) { fail.on(err(e.wrong, e.bin)); return null }
        buf(j) = c.toByte
        i += 1
        j += 1
      }
      buf
    }
    if (!wrapup(e.bin)(fail)) return null
    ans
  }
  final def binaryIn(n: Int, target: Array[Byte], start: Int)(implicit fail: GrokHop[this.type]): this.type = {
    if (!prepare(n, e.bin)(fail)) return null
    var j = start
    val end = start + n
    while (j < end) {
      val c = string.charAt(i)
      if (c > 255) { fail.on(err(e.wrong, e.bin)); return this }
      target(j) = c.toByte
      i += 1
      j += 1
    }
    if (!wrapup(e.bin)(fail)) return null
    this
  }
  
  def context[A](description: => String)(parse: => A)(implicit fail: GrokHop[this.type]): A = {
    val tOld = t
    val iOld = i
    try { parse }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(sub.whyError, e.sub.toByte, tOld, iOld, description, sub :: Nil)(string) ) }
  }
  
  def attempt[A](parse: => A)(implicit fail: GrokHop[this.type]): Ok[GrokError, A] = {
    var tToBe = t
    var iToBe = i
    var readyToBe = ready
    val i0Old = i0
    val iNOld = iN
    val delimOld = delim
    val nSepOld = nSep
    val reqSepOld = reqSep
    val stringOld = string
    try {
      val ans = parse
      if (string eq stringOld) {
        tToBe = t
        iToBe = i
        readyToBe = ready
      }
      Yes(ans)
    }
    catch { case t if fail is t => No( GrokError(e.wrong.toByte, e.sub.toByte, tToBe, iToBe, null, (fail as t value) :: Nil)(stringOld) ) }
    finally {
      string = stringOld
      reqSep = reqSepOld
      nSep = nSepOld
      delim = delimOld
      iN = iNOld
      i0 = i0Old
      ready = readyToBe
      i = iToBe
      t = tToBe
    }
  }
  
  def tangent[A](parse: => A)(implicit fail: GrokHop[this.type]): A = {
    val tOld = t
    val iOld = i
    val i0Old = i0
    val iNOld = iN
    val delimOld = delim
    val nSepOld = nSep
    val reqSepOld = reqSep
    val readyOld = ready
    val stringOld = string
    try { parse }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(e.delim.toByte, e.alt.toByte, tOld, iOld, null, sub :: Nil)(stringOld) ) }
    finally {
      string = stringOld
      ready = readyOld
      reqSep = reqSepOld
      nSep = nSepOld
      delim = delimOld
      iN = iNOld
      i0 = i0Old
      i = iOld
      t = tOld
    }
  }
  
  def each[A](parse: => A)(implicit fail: GrokHop[this.type], tag: ClassTag[A]): Array[A] = {
    val delimOld = delim
    val nSepOld = nSep
    val reqSepOld = reqSep
    val stringOld = string
    println(s"! $i $t $ready")
    try {
      val ans = Array.newBuilder[A]
      while (!isEmpty) {
        println(s"@ $i $t $ready")
        ans += parse
        println(s"# $i $t $ready")
        string = stringOld
        reqSep = reqSepOld
        nSep = nSepOld
        delim = delimOld
      }
      println(s"% $i $t $ready")      
      ans.result()
    }
    finally {
      string = stringOld
      reqSep = reqSepOld
      nSep = nSepOld
      delim = delimOld
      println(s"^ $i $t $ready")
    }
  }
  
  def grokEach[A](delimiter: Delimiter)(parse: => A)(implicit fail: GrokHop[this.type], tag: ClassTag[A]): Ok[Array[Ok[GrokError,A]], Array[A]] = {
    var tToBe = t
    var iToBe = i
    var readyToBe = ready
    val i0Old = i0
    val iNOld = iN
    val delimOld = delim
    val nSepOld = nSep
    val reqSepOld = reqSep
    val stringOld = string
    var successBuffer = Array.newBuilder[A]
    lazy val failureBuffer = Array.newBuilder[Ok[GrokError,A]]
    var hitEnd = false
    val delimNew = delimiter.terminatedBy(delim){ x => hitEnd = true; iN = x }
    while (!isEmpty) {
      println(s"a $i $t $ready")
      if (!prepare(0, e.exact)(fail)) return null
      try {
        hitEnd = false
        delim = delimNew
        val ans = parse
        println(s"b $i $t $ready")
        if (string eq stringOld) {
          tToBe = t
          iToBe = i
          ready = 0
        }
        string = stringOld
        reqSep = reqSepOld
        nSep = nSepOld
        iN = iNOld
        i0 = i0Old
        println(s"c $i $t $ready")
        if (!hitEnd) skip(1)
        readyToBe = ready
        if (successBuffer != null) successBuffer += ans else failureBuffer += Yes(ans)
        println(s"d $i $t $ready")
      }
      catch { case x if fail is x =>
        println(s"e $i $t $ready")
        if (successBuffer != null) {
          val buf = successBuffer.result()
          var i = 0
          while (i < buf.length) { failureBuffer += Yes(buf(i)); i += 1 }
          successBuffer = null
        }
        failureBuffer += No( GrokError(e.wrong.toByte, e.sub.toByte, tToBe, iToBe, null, (fail as x value) :: Nil)(stringOld) )
        string = stringOld
        reqSep = reqSepOld
        nSep = nSepOld
        iN = iNOld
        i0 = i0Old
        ready = readyToBe
        t = tToBe
        i = iToBe
        skip(1)
        readyToBe = ready
        println(s"f $i $t $ready")      
      }
      finally {
        delim = delimOld
      }
      if (!wrapup(e.sub)(fail)) return null
      println(s"g $i $t $ready")
    }
    if (successBuffer != null) Yes(successBuffer.result()) else No(failureBuffer.result())
  }
}
