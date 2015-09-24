// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015 Rex Kerr, UCSF, and Calico Labs.

package kse.eio

import language.postfixOps

import scala.annotation.tailrec
import scala.reflect.ClassTag
import kse.flow._
import kse.coll.packed._

final class GrokBuffer(private[this] var buffer: Array[Byte], initialStart: Int, initialEnd: Int, initialDelimiter: Delimiter, initialnSep: Int = 1, initialReqSep: Boolean = false)
extends Grok {
  import kse.eio.{GrokErrorCodes => e}

  private[this] var t = 0
  
  i0 = math.max(0, math.min(initialStart, buffer.length))
  iN = math.min(buffer.length, math.max(initialEnd, i0))
  delim = initialDelimiter
  nSep = math.max(1, initialnSep)
  reqSep = initialReqSep
  ready = 1
  
  private[eio] def adoptState(b: Array[Byte], j0: Int, jN: Int, j: Int, d: Delimiter, ns: Int, rs: Boolean) {
    buffer = b
    i0 = j0
    iN = jN
    i = j
    delim = d
    nSep = ns
    reqSep = rs
    ready = 1
    t = 0
  }
  
  private[eio] def bufferIsExactly(b: Array[Byte]) = (buffer eq b)
  
  // Ready states: 0 = consume whitespace if any; 1 = whitespace consumed; 2 = all whitespace consumed
  
  def input(newInput: Array[Byte], start: Int = 0, end: Int = Int.MaxValue): this.type = {
    buffer = newInput
    i0 = math.max(0, math.min(start, buffer.length))
    iN = math.min(buffer.length, math.max(end, i0))
    i = i0
    t = 0
    ready = 1
    this
  }
  
  // Important to keep everything after this point synced with GrokString.  No good way to do this right now, alas.
  private def err(fail: GrokHop[this.type], what: Int, who: Int) { error = what.toByte; if (fail != null) { fail(GrokError(what.toByte, who.toByte, t, i)(buffer)) } }
  private final def prepare(needed: Int, id: Int)(fail: GrokHop[this.type]): Boolean = {
    error = 0
    if (ready == 0) {
      val j = delim(buffer, i, iN, nSep)
      if (j < 0) { err(fail, e.end, id); iN = i; return false }
      i = j
      ready = 1
    }
    if (iN - i < needed) { err(fail, e.end, id); false } else true
  }
  private final def wrapup(id: Int)(fail: GrokHop[this.type]): Boolean = {
    if (reqSep) {
      val j = delim(buffer, i, iN, nSep)
      if (j == i) { err(fail,e.delim,id); false }
      else {
        t += 1
        if (j < 0) { iN = i; ready = 0 }
        else { i = j; ready = 1 }
        true
      }
    }
    else { ready = 0; t += 1; true }
  }
  
  private final def smallNumber(dig: Int, lo: Long, hi: Long, id: Int)(fail: GrokHop[this.type]): Long = {
    if (!prepare(1, id)(fail)) return 0
    val negative = {
      if (lo >= 0) false
      else if (buffer(i) == '-') { i += 1; true }
      else false
    } 
    var j = i
    while (i < iN && buffer(i) == '0') i += 1
    val l = {
      if (j < i && (i >= iN || { val c = buffer(i); c < '0' || c > '9' })) { error = 0; 0 }
      else { j = i; rawDecimalDigitsUnsigned(buffer, dig+1) }
    }
    if (error > 0) { err(fail,error,id); return 0 }
    if (i-j > dig) { err(fail,e.range,id); return 0 }
    val ans = if (negative) -l else l
    if (ans < lo || ans > hi) { err(fail,e.range,id); return 0 }
    if (!wrapup(id)(fail)) return 0
    ans
  }
  private final def longNumber(unsigned: Boolean, id: Int)(fail: GrokHop[this.type]): Long = {
    if (!prepare(1, id)(fail)) return 0
    val negative = {
      if (unsigned) false
      else if (buffer(i) == '-') { i += 1; true }
      else false
    }
    var j = i
    while (i < iN && buffer(i) == '0') i += 1
    val l = {
      if (j < i && (i >= iN || { val c = buffer(i); c < '0' || c > '9' })) { error = 0; 0 }
      else { j = i; rawDecimalDigitsUnsigned(buffer, 19) }
    }
    if (error > 0) { err(fail,error,id); return 0 }
    val ans = 
      if (unsigned) {
        if (i-j < 19 || i >= iN) l
        else {
          val c = buffer(i)-'0'
          if (c < 0 || c > 9) l
          else if (l > GrokNumber.maxULongPrefix || c > GrokNumber.maxULongLastDigit) { err(fail, e.range, id); return 0 }
          else {
            i += 1
            if (i < iN && { val c = buffer(i); c >= '0' && c <= '9'}) { err(fail, e.range, id); return 0 }
            l*10 + c
          }
        }
      }
      else {
        if (i-j == 19 && i < iN && { val c = buffer(i); c >= '0' && c <= '9'}) { err(fail, e.range, id); return 0 }
        val x = if (negative) -l else l
        if (x != 0 && (x < 0) != negative) { err(fail, e.range,id); return 0 }
        x
      }
    if (!wrapup(id)(fail)) return 0
    ans
  }
  private final def hexidecimalNumber(dig: Int, id: Int)(fail: GrokHop[this.type]): Long = {
    if (!prepare(1, id)(fail)) return 0
    var j = i
    while (i < iN && buffer(i) == '0') i += 1
    val ans = {
      if (j < i && (i >= iN || { val c = buffer(i); c < '0' || (c > '9' && ((c&0xDF)<'A' || (c&(0xDF))>'F')) })) { error = 0; 0 }
      else { j = i; rawHexidecimalDigits(buffer, dig+1) }
    }
    if (error > 0) { err(fail,error,id); return 0 }
    if (i-j > dig) { err(fail,e.range,id); return 0 }
    if (!wrapup(id)(fail)) return 0
    ans
  }
  private final def matchAsciiInsensitive(lowered: String, id: Int)(fail: GrokHop[this.type]) {
    error = 0
    var j = 0
    while (j < lowered.length) {
      if (i >= iN) { err(fail, e.end, id); error = e.end; return }
      if (lowered.charAt(j) != (buffer(i) | 0x20)) { err(fail, e.wrong, id); return }
      i += 1; j += 1
    }
    ready = 0
  }

  final def customError = GrokError(e.wrong.toByte, e.custom.toByte, t, i)(buffer)
  
  final def customError(message: String) = GrokError(e.wrong.toByte, e.custom.toByte, t, i, message)(buffer)
  
  def skip(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    if (!prepare(0, e.tok)(fail)) return this
    i = delim.not(buffer, i, iN)
    t += 1
    ready = 0
    this
  }
  def skip(n: Int)(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    var k = n
    while (k > 0 && error == 0) { skip(fail); k -= 1 }
    this
  }
  def Z(implicit fail: GrokHop[this.type]): Boolean = {
    if (!prepare(4, e.Z)(fail)) return false
    var c = buffer(i)&0xDF
    val ans = 
      if (c == 'T') { matchAsciiInsensitive("true", e.Z)(fail); true }
      else if (c == 'F') { matchAsciiInsensitive("false", e.Z)(fail); false }
      else { err(fail, e.wrong, e.Z); return false }
    if (!wrapup(e.Z)(fail)) return false
    ans
  }
  def aZ(implicit fail: GrokHop[this.type]): Boolean = {
    if (!prepare(1, e.aZ)(fail)) return false
    val ans = (buffer(i)&0xDF) match {
      case 16 | 17 => return smallNumber(1, 0, 1, e.aZ)(fail) == 1  // Return because smallNumber already handles wrapup
      case 'T' =>
        i += 1
        if (i < iN && (buffer(i)&0xDF) == 'R') { i -= 1; matchAsciiInsensitive("true", e.aZ)(fail) }
        true
      case 'F' =>
        i += 1
        if (i < iN && (buffer(i)&0xDF) == 'A') { i -= 1; matchAsciiInsensitive("false", e.aZ)(fail) }
        false
      case 'Y' =>
        i += 1
        if (i < iN && (buffer(i)&0xDF) == 'E') { i -= 1; matchAsciiInsensitive("yes", e.aZ)(fail) }
        true
      case 'N' =>
        i += 1
        if (i < iN && (buffer(i)&0xDF) == 'O') i += 1
        false
      case 'O' =>
        i += 1
        if (i >= iN) { err(fail, e.end, e.aZ); return false }
        val c = buffer(i) & 0xDF
        if (c == 'N') { i += 1; true }
        else if (c == 'F') {
          i += 1
          if (i >= iN) { err(fail, e.end, e.aZ); return false }
          if ((buffer(i) & 0xDF) != 'F') { err(fail, e.wrong, e.aZ); return false }
          i += 1
          false
        }
        else { err(fail, e.wrong, e.aZ); return false }
      case _ =>
        err(fail, e.wrong, e.aZ); return false
    }
    if (!wrapup(e.aZ)(fail)) return false
    ans
  }
  def B(implicit fail: GrokHop[this.type]): Byte = smallNumber(3, Byte.MinValue, Byte.MaxValue, e.B)(fail).toByte
  def uB(implicit fail: GrokHop[this.type]): Byte = smallNumber(3, 0, 0xFFL, e.uB)(fail).toByte
  def S(implicit fail: GrokHop[this.type]): Short = smallNumber(5, Short.MinValue, Short.MaxValue, e.S)(fail).toShort
  def uS(implicit fail: GrokHop[this.type]): Short = smallNumber(5, 0, 0xFFFFL, e.uS)(fail).toShort
  def C(implicit fail: GrokHop[this.type]): Char = {
    if (!prepare(1, e.C)(fail)) return 0
    if (delim.not(buffer, i, i+1) == i) { err(fail, e.wrong, e.C); return 0 }    
    var ans = buffer(i) & 0xFF
    i += 1
    if (!wrapup(e.C)(fail)) return 0
    ans.toChar
  }
  def I(implicit fail: GrokHop[this.type]): Int = smallNumber(10, Int.MinValue, Int.MaxValue, e.I)(fail).toInt
  def uI(implicit fail: GrokHop[this.type]): Int = smallNumber(10, 0, 0xFFFFFFFFL, e.uI)(fail).toInt
  def xI(implicit fail: GrokHop[this.type]): Int = hexidecimalNumber(8, e.xI)(fail).toInt
  def aI(implicit fail: GrokHop[this.type]): Int = {
    if (!prepare(1, e.aI)(fail)) return 0
    val c = buffer(i)
    if (c == '-') I(fail)
    else if (c == '+') {
      i += 1
      if (i+1 >= iN) { err(fail, e.end, e.aI); return 0 }
      val c = buffer(i)
      if (c >= '0' && c <= '9') I(fail)
      else { err(fail, e.wrong, e.aI); return 0 }
    }
    else if (c >= '0' && c <= '9') {
      if (c == '0' && i+2 < iN && (buffer(i+1)|0x20)=='x' && { val c = buffer(i+2) | 0x20; (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') }) {
        i += 2
        xI(fail)
      }
      else uI(fail)
    }
    else { err(fail, e.wrong, e.aI); return 0 }
  }
  def L(implicit fail: GrokHop[this.type]): Long = longNumber(false, e.L)(fail)
  def uL(implicit fail: GrokHop[this.type]): Long = longNumber(true, e.uL)(fail)
  def xL(implicit fail: GrokHop[this.type]): Long = hexidecimalNumber(16, e.xL)(fail)
  def aL(implicit fail: GrokHop[this.type]): Long = {
    if (!prepare(1, e.aL)(fail)) return 0
    val c = buffer(i)
    if (c == '-') L(fail)
    else if (c == '+') {
      i += 1
      if (i+1 >= iN) { err(fail, e.end, e.aL); return 0 }
      val c = buffer(i)
      if (c >= '0' && c <= '9') L(fail)
      else { err(fail, e.wrong, e.aL); return 0 }
    }
    else if (c >= '0' && c <= '9') {
      if (c == '0' && i+1 < iN && (buffer(i+1)|0x20)=='x') {
        if (i+2 >= iN) { err(fail, e.end, e.aL); return 0 }
        i += 2
        xL(fail)
      }
      else uL(fail)
    }
    else { err(fail, e.wrong, e.aL); return 0 }
  }
  def F(implicit fail: GrokHop[this.type]): Float = D(fail).toFloat
  def xF(implicit fail: GrokHop[this.type]): Float = xD(fail).toFloat
  def D(implicit fail: GrokHop[this.type]): Double = {
    import GrokNumber._
    if (!prepare(1, e.D)(fail)) return parseErrorNaN
    val iOld = i
    val j = rawParseDoubleDigits(buffer, '.')
    if (error > 0) { ready = 0; err(fail, error, e.D); return parseErrorNaN }
    else {
      ready = 0
      val ans = error match {
        case e.whole => j.toDouble
        case e.coded => if (j == 0) Double.NaN else if (j < 0) Double.NegativeInfinity else Double.PositiveInfinity
        case e.imprecise =>
          try { java.lang.Double.parseDouble(new String(buffer, iOld, i - iOld)) }
          catch { case _: NumberFormatException => error = e.wrong.toByte; parseErrorNaN }
        case _ => java.lang.Double.longBitsToDouble(j)
      }
      if (error > 0) { err(fail, error, e.D); return parseErrorNaN }
      error = 0
      if (!wrapup(e.D)(fail)) return parseErrorNaN
      ans
    }
  }
  def xD(implicit fail: GrokHop[this.type]): Double = {
    import GrokNumber._
    if (!prepare(7, e.xD)(fail)) return parseErrorNaN
    var neg = false
    buffer(i) match {
      case '-' => neg = true; i += 1
      case '+' => i += 1
      case _ =>
    }
    if (buffer(i) != '0') { err(fail, e.wrong, e.xD); return parseErrorNaN }
    i += 1
    if ((buffer(i)|0x20) != 'x') { err(fail, e.wrong, e.xD); return parseErrorNaN }
    i += 1
    val subnorm = buffer(i) match {
      case '0' => true
      case '1' => false
      case 'i' | 'I' => return (if (neg) -D(fail) else D(fail))
      case 'n' | 'N' => return D(fail)
      case _ => { err(fail, e.wrong, e.xD); return parseErrorNaN }
    }
    i += 1
    if (buffer(i) != '.') { err(fail, e.wrong, e.xD); return parseErrorNaN }
    i += 1
    val oldReqSep = reqSep
    reqSep = false
    ready = 1
    error = 0
    val bits = hexidecimalNumber(13, e.xD)(null)
    reqSep = oldReqSep
    if (error != 0) { err(fail, e.wrong, e.xD); return parseErrorNaN }
    if (i >= iN-1) { err(fail, e.end, e.xD); return parseErrorNaN }
    if ((buffer(i)|0x20) != 'p') { err(fail, e.wrong, e.xD); return parseErrorNaN }
    i += 1
    if (buffer(i) == '+') i += 1
    val iExp0 = i
    ready = 1
    reqSep = false
    val exp = smallNumber(6, -1022, 1023, e.xD)(null)
    reqSep = oldReqSep
    if (error != 0) { err(fail, e.wrong, e.xD); return parseErrorNaN }
    if ((i - iExp0) > 5) { err(fail, e.wrong, e.xD); return parseErrorNaN }
    if (!wrapup(e.xD)(fail)) return parseErrorNaN
    java.lang.Double.longBitsToDouble(bits | (exp+1022L + (if (subnorm) 0 else 1)) << 52)
  }
  def aD(implicit fail: GrokHop[this.type]): Double = {
    import GrokNumber._
    if (!prepare(1, e.D)(fail)) return parseErrorNaN
    buffer(i) match {
      case '+' | '-' =>
        if (i < iN-1) buffer(i+1) match {
          case '0' => if (i < iN-2 && ((buffer(i+2)|0x20) == 'x')) xD(fail) else D(fail)
          case _ => D(fail)
        }
        else D(fail)
      case '0' => if (i < iN-1 && ((buffer(i+1)|0x20) == 'x')) xD(fail) else D(fail)
      case _ => D(fail)
    }
  }
  private[this] def unencodedString(o: Int, m: Int) = if (m <= 0) "" else {
    var cs = new Array[Char](m)
    var k = 0
    while (k < m) { cs(k) = (buffer(o+k) & 0xFF).toChar; k += 1 }
    new String(cs)  
  }
  def tok(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.tok)(fail)) return null
    val j = delim.not(buffer, i, iN)
    val ans = unencodedString(i, j-i)
    ready = 0
    t += 1
    i = j
    ans
  }
  final def tokLimit(n: Int)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.tok)(fail)) return null
    val j = math.min(delim.not(buffer, i, iN), i + math.max(0,n))
    val ans = unencodedString(i, j-i)
    ready = 0
    t += 1
    i = j
    ans
  }
  final def tokUntil(p: Int => Boolean)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.tok)(fail)) return null
    val j = delim.not(buffer, i, iN)
    var k = i
    while (k < j && !p(buffer(k) & 0xFF)) k += 1
    val ans = unencodedString(i, k-i)
    ready = 0
    t += 1
    i = k
    ans
  }
  def quoted(implicit fail: GrokHop[this.type]): String = quotedBy('"', '"', '\\')(fail)
  private def quotedByWithEscapes(iStart: Int, iEnd: Int, left: Byte, right: Byte, esc: Byte, escaper: GrokEscape)(implicit fail: GrokHop[this.type]): String = {
    val buf = new Array[Char](iEnd - iStart)
    var j = 0
    var k = iStart
    while (k < iEnd) {
      val c = buffer(k)
      k += 1
      if (c != esc) { buf(j) = (c & 0xFF).toChar; j += 1 }
      else {
        val c = buffer(k)
        k += 1
        var x = escaper.replace(c)
        while (x >= 65536) {
          var n = (x & 0xFF)
          if (k+n >= iEnd) { err(fail, e.wrong, e.quote); return null }
          var l = 0L
          if ((x >>> 28) == 4) while (n > 0) { l = (l << 8) | (buffer(k) & 0xFF); k += 1; n -= 1 }
          else while (n > 0) { 
            l = (l << 4) | ( (buffer(k) | 0x20) - '0' match {
              case x if x >= 0 && x < 10 => x
              case x if x >= 49 && x < 55 => x - 39
              case x => err(fail, e.range, e.quote); return null
            })
            k += 1
            n -= 1
          }
          x = escaper.extended(c, l)
        }
        buf(j) = x.toChar
        j += 1
      }
    }
    val ans = new String(buf, 0, j)
    if (!wrapup(e.quote)(fail)) return null
    ans
  }
  private def quotedByDegenerately(q: Byte)(implicit fail: GrokHop[this.type]): String = {
    val iStart = i
    var doublets = 0
    while (i < iN) {
      val c = buffer(i)
      if (c == q) {
        if (doublets == 2) doublets = 1
        else if (i+1 < iN && buffer(i+1) == q) doublets = 2
        else {
          val ans = 
            if (doublets > 0) {
              val buf = new Array[Char](i - iStart)
              var k = 0
              var j = iStart
              var skip = false
              while (j < i) {
                val c = buffer(j)
                if (!skip) {
                  buf(k) = (c & 0xFF).toChar
                  k += 1
                  if (c == q) skip = true
                }
                else skip = false
                j += 1
              }
              new String(buf, 0, k)
            }
            else unencodedString(iStart, i - iStart)
          i += 1
          if (!wrapup(e.quote)(fail)) return null
          return ans
        }
      }
      i += 1
    }
    err(fail, e.end, e.quote)
    null
  }
  def quotedBy(left: Char, right: Char, esc: Char, escaper: GrokEscape = GrokEscape.standard)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(2, e.quote)(fail)) return null
    val c = buffer(i)
    val bleft = left.toByte
    val bright = right.toByte
    val besc = esc.toByte
    if (c != bleft) { err(fail, e.wrong, e.quote); return null }
    i += 1
    if (bleft == bright && bleft == besc) return quotedByDegenerately(bleft)(fail)
    val iStart = i
    var depth = 1
    var escies = 0
    var esced = false
    var hi = false
    while (i < iN && depth > 0) {
      val c = buffer(i)
      if (esced) esced = false
      else {
        if (c == bright) depth -= 1
        else if (c == bleft) depth += 1
        else if (c == besc) { escies += 1; esced = true }
        else if (c > 0x7F) hi = true
      }
      i += 1
    }
    if (depth != 0) { err(fail, e.wrong, e.quote); return null }
    if (escies > 0) return quotedByWithEscapes(iStart, i-1, bleft, bright, besc, escaper)(fail)
    val ans = if (hi) unencodedString(iStart, i-1-iStart) else new String(buffer, iStart, i-1-iStart)
    if (!wrapup(e.quote)(fail)) return null
    ans
  }
  def qtok(implicit fail: GrokHop[this.type]): String = qtokBy('"', '"', '\\')(fail)
  def qtokBy(left: Char, right: Char, esc: Char, escaper: GrokEscape = GrokEscape.standard)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.quote)(fail)) return null
    if (i >= iN) { error = 0; ready = 0; return "" }
    val j = delim.not(buffer, i, i+1)
    if (j == i) { error = 0; ready = 0; return "" }
    val c = buffer(i)
    if (c != left) tok(fail) else quotedBy(left, right, esc, escaper)(fail)
  }
  def base64(implicit fail: GrokHop[this.type]): Array[Byte] = {
    if (!prepare(0, e.tok)(fail)) return null
    val a = delim.not(buffer, i, iN)
    val buf = new Array[Byte](((a - i).toLong*3/4).toInt)
    val n = kse.eio.base64.decodeFromBase64(buffer, i, a, buf, 0, kse.eio.base64.Url64.decoder)
    if (n < 0) { i = i - (n+1); err(fail, e.wrong, e.b64); return null }
    i = a
    t += 1
    ready = 0
    if (n < buf.length) java.util.Arrays.copyOf(buf, n) else buf
  }
  def base64in(target: Array[Byte], start: Int)(implicit fail: GrokHop[this.type]): Int = {
    if (!prepare(0, e.tok)(fail)) return -1
    val a = delim.not(buffer, i, iN)
    val n = kse.eio.base64.decodeFromBase64(buffer, i, a, target, start, kse.eio.base64.Url64.decoder)
    if (n < 0) { i = i - (n+1); err(fail, e.wrong, e.b64); return -1 }
    i = a
    t += 1
    ready = 0
    n - start
  }
  def exact(c: Char)(implicit fail: GrokHop[this.type]): this.type = {
    if (!prepare(1, e.exact)(fail)) return null
    if ((buffer(i) & 0xFF) != c) { err(fail, e.wrong, e.exact); return this }
    i += 1
    if (!wrapup(e.exact)(fail)) return null
    this
  }
  def exact(s: String)(implicit fail: GrokHop[this.type]): this.type = {
    if (!prepare(0, e.exact)(fail)) return null
    var k = 0
    while (i < iN && k < s.length && (buffer(i) & 0xFF) == s.charAt(k)) { i += 1; k += 1 }
    if (k < s.length) { err(fail, e.wrong, e.exact); return this }
    if (!wrapup(e.exact)(fail)) return null
    this
  }
  def exactNoCase(s: String)(implicit fail: GrokHop[this.type]): this.type =  {
    if (!prepare(0, e.exact)(fail)) return null
    var k = 0
    while (i < iN && k < s.length && 
      ((buffer(i) & 0xFF) match { case x if (x|0x20) >= 'a' && (x|0x20) <= 'z' => (x|0x20) == (s.charAt(k)|0x20); case x => x == s.charAt(k) })
    ) { 
      i += 1; k += 1
    }
    if (k < s.length) { err(fail, e.wrong, e.exact); return this }
    if (!wrapup(e.exact)(fail)) return null
    this    
  }
  def oneOf(s: String*)(implicit fail: GrokHop[this.type]): String = {
    if (!prepare(0, e.exact)(fail)) return null
    val a = delim.not(buffer, i, iN)
    ready = 0
    var n = 0
    while (n < s.length) {
      if (!reqSep || a - i == s(n).length) {
        var j = 0
        var k = i
        val sn = s(n)
        while (k < a && j < sn.length && sn.charAt(j) == (buffer(k) & 0xFF)) { k += 1; j += 1 }
        if (j == sn.length) { i = k; t += 1; return sn }
      }
      n += 1
    }
    if (reqSep) i = a
    err(fail, e.wrong, e.oneOf)
    null
  }
  def oneOfNoCase(s: String*)(implicit fail: GrokHop[this.type]): String = {
    import GrokCharacter._
    if (!prepare(0, e.exact)(fail)) return null
    val a = delim.not(buffer, i, iN)
    ready = 0
    var n = 0
    while (n < s.length) {
      if (!reqSep || a - i == s(n).length) {
        var j = 0
        var k = i
        val sn = s(n)
        while (k < a && j < sn.length && {
          val c = buffer(k) & 0xFF
          val cc = sn.charAt(j)
          (c == cc) || {
            ???
            // if (j > 0 && Character.isLowSurrogate(c)) Character.toUpperCase(sn.codePointAt(j-1)) == Character.toUpperCase(buffer.codePointAt(k-1))
            // else elevateCase(c) == elevateCase(cc)
          }
        }) { k += 1; j += 1 }
        if (j == sn.length) { i = k; t += 1; return sn }
      }
      n += 1
    }
    i = a
    err(fail, e.wrong, e.oneOf)
    null
  }
  def bytes(n: Int)(implicit fail: GrokHop[this.type]): Array[Byte] = {
    if (!prepare(n, e.bin)(fail)) return null
    val ans = {
      val buf = new Array[Byte](n)
      var j = 0
      while (j < n) {
        buf(j) = buffer(i)
        i += 1
        j += 1
      }
      buf
    }
    if (!wrapup(e.bin)(fail)) return null
    ans
  }
  def bytesIn(n: Int, target: Array[Byte], start: Int)(implicit fail: GrokHop[this.type]): this.type = {
    if (!prepare(n, e.bin)(fail)) return null
    if (start < 0 || target.length - start < n) { err(fail, e.range, e.bin); return null }
    var j = start
    val end = start + n
    while (j < end) {
      target(j) = buffer(i)
      i += 1
      j += 1
    }
    if (!wrapup(e.bin)(fail)) return null
    this
  }
  
  private def localPosition: Int = {
    if (ready == 0) {
      val j = delim(buffer, i, iN, nSep)
      if (j < 0) { iN = i; return iN }
      i = j
      ready = 1
    }
    i
  }
  
  def position = localPosition.toLong
  
  def hasToken = localPosition < iN || ready > 0
  
  def hasContent = localPosition < iN
  
  def trim: Int = {
    if (ready != 2 && i < iN) {
      ready = 2
      val j = delim(buffer, i, iN, Int.MaxValue)
      val iOld = i
      i = if (j < 0) { iN = -1-j; iN } else j
      i - iOld
    }
    else 0
  }
  def trimmed: this.type = { trim; this }
  

  def trySkip: Boolean = {
    if (ready == 0) {
      val j = delim(buffer, i, iN, nSep)
      if (j < 0) { iN = i; return false }
      i = j
    }
    i = delim.not(buffer, i, iN)
    ready = 0
    t += 1
    true
  }
  
  def trySkip(n: Int): Int = {
    var k = 0
    while (k < n && trySkip) k += 1
    k
  }
      
  
  def oZ: Option[Boolean] = {
    val iStart = i
    val tStart = t
    val b = aZ(null)
    if (error != 0) {
      i = iStart
      t = tStart
      None
    }
    else Some(b)
  }
  
  def oC: Option[Char] = {
    val iStart = i
    val tStart = t
    val c = C(null)
    if (error != 0) {
      i = iStart
      t = tStart
      None
    }
    else Some(c)
  }
  
  def oI: Option[Int] = {
    val iStart = i
    val tStart = t
    val n = aI(null)
    if (error != 0) {
      i = iStart
      t = tStart
      None
    }
    else Some(n)
  }
  
  def oL: Option[Long] = {
    val iStart = i
    val tStart = t
    val l = aL(null)
    if (error != 0) {
      i = iStart
      t = tStart
      None
    }
    else Some(l)
  }
  
  def oD: Option[Double] = {
    val iStart = i
    val tStart = t
    val d = D(null)
    if (error != 0) {
      i = iStart
      t = tStart
      None
    }
    else Some(d)
  }
  
  def oTok: Option[String] = {
    val iStart = i
    val tStart = t
    val tk = tok(null)
    if (error != 0) {
      i = iStart
      t = tStart
      None
    }
    else Some(tk)
  }
  
  final def indexTok: Long = {
    val l = peekIndices
    if (l != -1) {
      val x = new LongAsBox(l)
      i = x.i1
      ready = 0
    }
    l
  }
  
  def oQuotedBy(left: Char, right: Char, esc: Char, escaper: GrokEscape = GrokEscape.standard): Option[String] = {
    val iStart = i
    val tStart = t
    val qt = quotedBy(left, right, esc, escaper)(null)
    if (error != 0) {
      i = iStart
      t = tStart
      None
    }
    else Some(qt)
  }
  
  def tryExact(c: Char): Boolean = {
    val iStart = i
    val tStart = t
    val u = exact(c)(null)
    if (error != 0) {
      i = iStart
      t = tStart
      false
    }
    else true
  }
  
  def tryExact(s: String): Boolean = {
    val iStart = i
    val tStart = t
    val u = exact(s)(null)
    if (error != 0) {
      i = iStart
      t = tStart
      false
    }
    else true
  }
  
  def peek: Int = {
    if (ready == 0) {
      val j = delim(buffer, i, iN, nSep)
      if (j < 0) { iN = i; return -1 }
      ready = 1
      i = j
    }
    if (i >= iN) -1 else buffer(i) & 0xFF
  }
    
  def peekAt(distance: Int): Int = {
    if (ready == 0) {
      val j = delim(buffer, i, iN, nSep)
      if (j >= 0) {
        ready = 1
        i = j
      }
    }
    val index = i + distance.toLong
    if (index < i0 || index >= iN) -1 else buffer(index.toInt) & 0xFF
  }

  final def peekIndices: Long = {
    if (ready == 0) {
      val j = delim(buffer, i, iN, nSep)
      if (j < 0) { iN = i; return -1L }
      ready = 1
      i = j
    }
    val j = delim.not(buffer, i, iN)
    (i packII j).L
  }

  def peekTok: String = {
    val l = peekIndices
    if (l == -1) null
    else {
      val x = new LongAsBox(l)
      unencodedString(x.i0, x.i1 - x.i0)
    }
  }
  
  def peekBytesIn(n: Int, target: Array[Byte], start: Int): Int = {
    if (ready == 0) {
      val j = delim(buffer, i, iN, nSep)
      if (j < 0) { iN = i; return -1 }
      ready = 1
      i = j
    }
    var k = start
    val kN = start + n
    var j = i
    while (j < iN && k < kN) {
      target(k) = buffer(j)
      k += 1
      j += 1
    }
    k - start
  }
  
  
  def context[A](description: => String)(parse: => A)(implicit fail: GrokHop[this.type]): A = {
    val tOld = t
    val iOld = i
    try { parse }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(sub.whyError, e.sub.toByte, tOld, iOld, description, sub :: Nil)(buffer) ) }
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
    val bufferOld = buffer
    try {
      val ans = parse
      if (buffer eq bufferOld) {
        tToBe = t
        iToBe = i
        readyToBe = ready
      }
      Yes(ans)
    }
    catch { case t if fail is t => No( GrokError(e.wrong.toByte, e.sub.toByte, tToBe, iToBe, null, (fail as t value) :: Nil)(bufferOld) ) }
    finally {
      buffer = bufferOld
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
    val bufferOld = buffer
    try { parse }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(e.delim.toByte, e.alt.toByte, tOld, iOld, null, sub :: Nil)(bufferOld) ) }
    finally {
      buffer = bufferOld
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
    try {
      val ans = Array.newBuilder[A]
      var more = hasToken
      while (more) {
        val iOld = i
        ans += parse
        reqSep = reqSepOld
        nSep = nSepOld
        delim = delimOld
        more = hasToken
        if (more && i == iOld) {
          if (i < iN) { skip; more = hasToken }
          else { ready = 0; more = false }
        }
      }
      ans.result()
    }
    finally {
      reqSep = reqSepOld
      nSep = nSepOld
      delim = delimOld
    }
  }
  
  def filterMap[A,B](parse: => A)(p: A => Boolean)(f: A => B)(implicit fail: GrokHop[this.type], tag: ClassTag[B]): Array[B] = {
    val bs = Array.newBuilder[B]
    var iOld = i
    var tOld = t
    var readyOld = ready
    val i0Old = i0
    val iNOld = iN
    val delimOld = delim
    val nSepOld = nSep
    val reqSepOld = reqSep
    val bufferOld = buffer
    try {
      var pos = position - 1
      while (hasToken && pos != position) {
        iOld = i
        tOld = t
        readyOld = ready
        val a = parse
        reqSep = reqSepOld
        nSep = nSepOld
        delim = delimOld
        iN = iNOld
        i0 = i0Old
        if (p(a)) {
          iOld = i
          tOld = t
          bs += f(a)
          if (buffer ne bufferOld) {
            i = iOld
            t = tOld
            ready = readyOld
            buffer = bufferOld
          }
          reqSep = reqSepOld
          nSep = nSepOld
          delim = delimOld
          iN = iNOld
          i0 = i0Old
        }
      }
    }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(e.delim.toByte, e.alt.toByte, tOld, iOld, null, sub :: Nil)(bufferOld) ) }
    finally {
      if (buffer ne bufferOld) {
        i = iOld
        t = tOld
        ready = readyOld
        buffer = bufferOld
      }
      reqSep = reqSepOld
      nSep = nSepOld
      delim = delimOld
      iN = iNOld
      i0 = i0Old
    }
    bs.result()
  }
  

  def grokEach[A: ClassTag](delimiter: Delimiter, preamble: GrokHop[this.type] => Boolean = _ => true)(f: GrokHop[this.type] => A): Ok[(Array[A], Array[GrokError]), Array[A]] = {
    implicit val fail = new GrokHopImpl[this.type]
    val name = delim match {
      case _: WhiteDelim => "word "
      case _: LineDelim => "line "
      case _ => "entry "
    }
    var tToBe = t
    var iToBe = i
    val delimOld = delim
    val i0Old = i0
    val iNOld = iN
    val nSepOld = nSep
    val reqSepOld = reqSep
    var successBuffer = Array.newBuilder[A]
    lazy val failureBuffer = Array.newBuilder[GrokError]
    var failures = false
    var index = 0
    val delimNew = delimiter terminatedBy delim
    while (hasToken) {
      iToBe = i
      index += 1
      try {
        delim = delimNew
        t = 0
        ready = 1
        if (preamble(fail)) successBuffer += f(fail)
      }
      catch { case x if fail is x =>
        failures = true
        failureBuffer += GrokError(e.wrong.toByte, e.sub.toByte, tToBe, iToBe, name+index, (fail as x value) :: Nil)(buffer)
        i = iToBe
      }
      finally {
        tToBe += 1
        reqSep = reqSepOld
        nSep = nSepOld
        iN = iNOld
        i0 = i0Old
        delim = delimOld
        ready = 1
        trySkip
        iToBe = i
      }
    }
    t = tToBe
    if (!failures) Yes(successBuffer.result()) else No((successBuffer.result(), failureBuffer.result()))
  }
}
