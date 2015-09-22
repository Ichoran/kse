// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015 Rex Kerr, UCSF, and Calico Labs.

package kse.eio

import language.postfixOps

import scala.annotation.tailrec
import scala.reflect.ClassTag
import kse.flow._
import kse.coll.packed._

final class GrokBinary(private[this] var buffer: Array[Byte], initialStart: Int, initialEnd: Int)
extends Grok {
  import kse.eio.{GrokErrorCodes => e}
  import GrokBinary._
  
  i0 = math.max(0, math.min(initialStart, buffer.length))
  i = i0
  iN = math.min(buffer.length, math.max(initialEnd, i0))
  
  reqSep = false  // Reused, in a hideous hack, to mean whether byte order should be inverted or not
  // ready not used
  // nSep not used
  // delim not used
  
  private lazy val g = new GrokBuffer(buffer, i0, iN, Delimiter.zero, 1, false)
  
  def switchEndian(yes: Boolean): this.type = { reqSep = yes; this }
  def otherEndian: this.type = { reqSep = !reqSep; this }
  def useBigEndian: this.type = switchEndian(java.nio.ByteOrder.BIG_ENDIAN != java.nio.ByteOrder.nativeOrder)
  def useLittleEndian: this.type = switchEndian(java.nio.ByteOrder.LITTLE_ENDIAN != java.nio.ByteOrder.nativeOrder)
  
  def input(newInput: Array[Byte], start: Int = 0, end: Int = Int.MaxValue): this.type = {
    buffer = newInput
    i0 = math.max(0, math.min(start, buffer.length))
    iN = math.min(buffer.length, math.max(end, i0))
    i = i0
    this
  }
  
  // Important to keep everything after this point synced with GrokString.  No good way to do this right now, alas.
  private def err(fail: GrokHop[this.type], what: Int, who: Int) {
    error = what.toByte
    if (fail != null) { fail(GrokError(what.toByte, who.toByte, 0, i)(buffer)) }
  }
  
  private final def binaryNumber(dig: Int, id: Int)(fail: GrokHop[this.type]): Long = {
    if (dig < 0 || i + dig.toLong >= iN) { err(fail, e.end, id.toByte); return 0 }
    error = 0
    ???
  }

  def customError = GrokError(e.wrong.toByte, e.custom.toByte, 0, i)(buffer)

  def customError(message: String) = GrokError(e.wrong.toByte, e.custom.toByte, 0, i, message)(buffer)
  
  def skip(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    if (i >= iN) { err(fail, e.end, e.B); return this }
    i += 1
    this
  }
  def skip(n: Int)(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    if (i + n.toLong >= iN) { err(fail, e.end, e.B); return this }
    i += n
    this
  }
  def Z(implicit fail: GrokHop[this.type]): Boolean = {
    if (i >= iN) { err(fail, e.end, e.Z); return false }
    val ans = buffer(i) match {
      case 0 => false
      case 1 => true
      case _ => err(fail, e.range, e.Z); return false
    }
    i += 1
    error = 0
    ans
  }
  def aZ(implicit fail: GrokHop[this.type]): Boolean = {
    if (i >= iN) { err(fail, e.end, e.Z); return false }
    val ans = buffer(i) != 0
    i += 1
    error = 0
    ans
  }
  def B(implicit fail: GrokHop[this.type]): Byte = {
    if (i >= iN) { err(fail, e.end, e.B); 0 }
    val ans = buffer(i)
    i += 1
    error = 0
    ans
  }
  def uB(implicit fail: GrokHop[this.type]): Byte = B
  def S(implicit fail: GrokHop[this.type]): Short = binaryNumber(2, e.S)(fail).toShort
  def uS(implicit fail: GrokHop[this.type]): Short = S
  def C(implicit fail: GrokHop[this.type]): Char = (B & 0xFF).toChar
  def I(implicit fail: GrokHop[this.type]): Int = binaryNumber(4, e.I)(fail).toInt
  def uI(implicit fail: GrokHop[this.type]): Int = I
  def xI(implicit fail: GrokHop[this.type]): Int = I
  def aI(implicit fail: GrokHop[this.type]): Int = I
  def L(implicit fail: GrokHop[this.type]): Long = binaryNumber(8, e.L)(fail)
  def uL(implicit fail: GrokHop[this.type]): Long = L
  def xL(implicit fail: GrokHop[this.type]): Long = L
  def aL(implicit fail: GrokHop[this.type]): Long = L
  def F(implicit fail: GrokHop[this.type]): Float = java.lang.Float.intBitsToFloat(binaryNumber(4, e.F)(fail).toInt)
  def xF(implicit fail: GrokHop[this.type]): Float = F
  def D(implicit fail: GrokHop[this.type]): Double = java.lang.Double.longBitsToDouble(binaryNumber(8, e.D)(fail))
  def xD(implicit fail: GrokHop[this.type]): Double = D
  def aD(implicit fail: GrokHop[this.type]): Double = D
  def tok(implicit fail: GrokHop[this.type]): String = {
    val j = Delimiter.zero.not(buffer, i, iN)
    if (j < 0) { err(fail, e.end, e.tok); return null }
    val ans = new String(buffer, i, j-i)
    error = 0
    i = math.min(iN, j+1L).toInt
    ans
  }
  def tokLimit(n: Int)(implicit fail: GrokHop[this.type]): String = {
    val j = Delimiter.zero.not(buffer, i, iN)
    if (j < 0) { err(fail, e.end, e.tok); return null }
    val m = math.min(j-i, math.max(0,n))
    val ans = new String(buffer, i, m)
    error = 0
    i = math.min(iN, i+m.toLong).toInt
    ans
  }
  def tokUntil(p: Int => Boolean)(implicit fail: GrokHop[this.type]): String = {
    val j = Delimiter.zero.not(buffer, i, iN)
    if (j < 0) { err(fail, e.end, e.tok); return null }
    var k = i
    while (k < j && !p(buffer(k) & 0xFF)) k += 1
    val ans = new String(buffer, i, k-i)
    error = 0
    i = math.min(iN, k).toInt
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
      if (c != esc) { buf(j) = c.toChar; j += 1 }
      else {
        val c = buffer(k)
        k += 1
        var x = escaper.replace(c)
        while (x >= 65536) {
          var n = (x & 0xFF)
          if (k+n >= iEnd) { err(fail, e.wrong, e.quote); return null }
          var l = 0L
          if ((x >>> 28) == 4) while (n > 0) { l = (l << 8) | buffer(k); k += 1; n -= 1 }
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
    new String(buf, 0, j)
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
                  buf(k) = c.toChar
                  k += 1
                  if (c == q) skip = true
                }
                else skip = false
                j += 1
              }
              new String(buf, 0, k)
            }
            else new String(buffer, iStart, i - iStart)
          i += 1
          return ans
        }
      }
      i += 1
    }
    err(fail, e.end, e.quote)
    null
  }
  def quotedBy(left: Char, right: Char, esc: Char, escaper: GrokEscape = GrokEscape.standard)(implicit fail: GrokHop[this.type]): String = {
    error = 0
    if (i >= iN-2) { err(fail, e.end, e.quote); return null }
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
    val ans = if (hi) new String(buffer, iStart, i-1-iStart) else new String(buffer, iStart, i-1-iStart, "ASCII")
    ans
  }
  def qtok(implicit fail: GrokHop[this.type]): String = qtokBy('"', '"', '\\')(fail)
  def qtokBy(left: Char, right: Char, esc: Char, escaper: GrokEscape = GrokEscape.standard)(implicit fail: GrokHop[this.type]): String = {
    if (i >= iN) { err(fail, e.end, e.tok); return null }
    val c = buffer(i)
    if (c != left) tok(fail)
    else {
      val ans = quotedBy(left, right, esc, escaper)(fail)
      if (i < iN && buffer(i) == 0) i += 1
      ans
    }
  }
  def base64(implicit fail: GrokHop[this.type]): Array[Byte] = {
    val a = Delimiter.zero.not(buffer, i, iN)
    if (a < 0) { err(fail, e.end, e.b64); return null }
    val buf = new Array[Byte](((a - i).toLong*3/4).toInt)
    val n = kse.eio.base64.decodeFromBase64(buffer, i, a, buf, 0, kse.eio.base64.Url64.decoder)
    if (n < 0) { i = i - (n+1); err(fail, e.wrong, e.b64); return null }
    i = a
    error = 0
    if (n < buffer.length) java.util.Arrays.copyOf(buffer, n) else buffer
  }
  def base64in(target: Array[Byte], start: Int)(implicit fail: GrokHop[this.type]): Int = {
    val a = Delimiter.zero.not(buffer, i, iN)
    if (a < 0) { err(fail, e.end, e.b64); return -1 }
    val n = kse.eio.base64.decodeFromBase64(buffer, i, a, target, start, kse.eio.base64.Url64.decoder)
    if (n < 0) { i = i - (n+1); err(fail, e.wrong, e.b64); return -1 }
    i = a
    error = 0
    n
  }
  def exact(c: Char)(implicit fail: GrokHop[this.type]): this.type = { if ((B & 0xFF) != c) { err(fail, e.wrong, e.exact) }; this }
  def exact(s: String)(implicit fail: GrokHop[this.type]): this.type = ???
  def exactNoCase(s: String)(implicit fail: GrokHop[this.type]): this.type = ???
  def oneOf(s: String*)(implicit fail: GrokHop[this.type]): String = ???
  def oneOfNoCase(s: String*)(implicit fail: GrokHop[this.type]): String = ???
  def bytes(n: Int)(implicit fail: GrokHop[this.type]): Array[Byte] = {
    error = 0
    if (n <= 0) return new Array[Byte](0)
    if (i >= iN-n) { err(fail, e.end, e.bin); return null }
    val buf = new Array[Byte](n)
    if (n < 16) {
      var j = 0
      while (j < n) {
        buf(j) = buffer(i)
        i += 1
        j += 1
      }
    }
    else {
      java.lang.System.arraycopy(buffer, i, buf, 0, n)
      i += n
    }
    buf
  }
  def bytesIn(n: Int, target: Array[Byte], start: Int)(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    if (n <= 0) return this
    if (i >= iN-n) { err(fail, e.end, e.bin); return null }
    if (n < 16) {
      var j = 0
      while (j < n) {
        target(start + j) = buffer(i)
        i += 1
        j += 1
      }
    }
    else {
      java.lang.System.arraycopy(buffer, i, target, start, n)
      i += n
    }
    this
  }
  
  def position = i.toLong
  
  def hasToken = i < iN // TODO -- forward to g in some sort of sane way
  
  def hasContent = i < iN
  
  def trim: Int = 0
  def trimmed: this.type = this
  

  def trySkip: Boolean =  if (i < iN) { i += 1; true } else false
  
  def trySkip(n: Int): Int = {
    val j = math.min(iN, math.max(i + n.toLong, i))
    val ans = (j-i).toInt
    i = j.toInt
    ans
  }
      
  
  def oZ: Option[Boolean] = if (i < iN) { val b = buffer(i); i += 1; Some(b != 0) } else None
  
  def oC: Option[Char] = if (i < iN) { val b = buffer(i); i += 1; Some((b & 0xFF).toChar) } else None
  
  def oI: Option[Int] = if (i < iN-4) Some(binaryNumber(4, e.I)(null).toInt) else None
  
  def oL: Option[Long] = if (i < iN-8) Some(binaryNumber(8, e.L)(null)) else None
  
  def oD: Option[Double] = if (i < iN-8) Some(java.lang.Double.longBitsToDouble(binaryNumber(8, e.L)(null))) else None
  
  def oTok: Option[String] = {
    val iStart = i
    val tk = tok(null)
    if (error != 0) {
      i = iStart
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
    val qt = quotedBy(left, right, esc, escaper)(null)
    if (error != 0) {
      i = iStart
      None
    }
    else Some(qt)
  }
  
  def tryExact(c: Char): Boolean = if (i < iN) { val b = buffer(i); i += 1; (b&0xFF) == c } else false
  
  def tryExact(s: String): Boolean = {
    val iStart = i
    val u = exact(s)(null)
    if (error != 0) {
      i = iStart
      false
    }
    else true
  }
  
  def peek: Int = if (i < iN) buffer(i) & 0xFF else -1
    
  def peekAt(distance: Int): Int = {
    val index = i + distance.toLong
    if (index < i0 || index >= iN) -1 else buffer(index.toInt) & 0xFF
  }

  final def peekIndices: Long = {
    val j = Delimiter.zero.not(buffer, i, iN)
    if (j < 0) -1L else (i packII j).L
  }

  def peekTok: String = {
    val l = peekIndices
    if (l == -1) null
    else {
      val x = new LongAsBox(l)
      new String(buffer, x.i0, x.i1 - x.i0)
    }
  }
  
  def peekBytesIn(n: Int, target: Array[Byte], start: Int): Int = {
    var k = start
    val kN = math.min(start + n.toLong, Int.MaxValue)
    val m = math.max(0, math.min(iN - i, kN - k)).toInt
    if (m < 16) {
      var j = i
      while (j < iN && k < kN) {
        target(k) = buffer(j)
        k += 1
        j += 1
      }
    }
    else java.lang.System.arraycopy(buffer, i, target, start, m)
    m
  }
  
  def text[A](parse: GrokBuffer => Ok[GrokError,A])(implicit fail: GrokHop[this.type]): A = {
    g.adoptState(buffer, i0, iN, i, Delimiter.zero, 1, true)
    (try { parse(g) } catch { case t if fail is t => No(fail as t value) }) match {
      case Yes(a) => if ((g.bufferIsExactly(buffer)) && g.position >= i) { error = 0; i = g.position.toInt }; a
      case No(ge) => error = ge.whyError; fail(GrokError(e.wrong, e.sub, 0, i, null, ge :: Nil)(buffer)); null.asInstanceOf[A]
    }
  }
  
  def context[A](description: => String)(parse: => A)(implicit fail: GrokHop[this.type]): A = {
    val iOld = i
    try { parse }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(sub.whyError, e.sub.toByte, 0, iOld, description, sub :: Nil)(buffer) ) }
  }
  
  def attempt[A](parse: => A)(implicit fail: GrokHop[this.type]): Ok[GrokError, A] = {
    var iToBe = i
    val i0Old = i0
    val iNOld = iN
    val reqSepOld = reqSep
    val bufferOld = buffer
    try {
      val ans = parse
      if (buffer eq bufferOld) {
        iToBe = i
      }
      Yes(ans)
    }
    catch { case t if fail is t => No( GrokError(e.wrong.toByte, e.sub.toByte, 0, iToBe, null, (fail as t value) :: Nil)(bufferOld) ) }
    finally {
      buffer = bufferOld
      reqSep = reqSepOld
      iN = iNOld
      i0 = i0Old
      i = iToBe
    }
  }
  
  def tangent[A](parse: => A)(implicit fail: GrokHop[this.type]): A = {
    val iOld = i
    val i0Old = i0
    val iNOld = iN
    val nSepOld = nSep
    val reqSepOld = reqSep
    val bufferOld = buffer
    try { parse }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(e.delim.toByte, e.alt.toByte, 0, iOld, null, sub :: Nil)(bufferOld) ) }
    finally {
      buffer = bufferOld
      reqSep = reqSepOld
      iN = iNOld
      i0 = i0Old
      i = iOld
    }
  }
  
  def each[A](parse: => A)(implicit fail: GrokHop[this.type], tag: ClassTag[A]): Array[A] = {
    val reqSepOld = reqSep
    try {
      val ans = Array.newBuilder[A]
      while (hasToken) {
        val iA = i
        ans += parse
        if (i == iA && i < iN) i += 1
        reqSep = reqSepOld
      }
      ans.result()
    }
    finally {
      reqSep = reqSepOld
    }
  }
  
  def filterMap[A,B](parse: => A)(p: A => Boolean)(f: A => B)(implicit fail: GrokHop[this.type], tag: ClassTag[B]): Array[B] = {
    val bs = Array.newBuilder[B]
    var iOld = i
    val i0Old = i0
    val iNOld = iN
    val reqSepOld = reqSep
    val bufferOld = buffer
    try {
      var pos = position - 1
      while (hasToken && pos != position) {
        iOld = i
        val a = parse
        reqSep = reqSepOld
        iN = iNOld
        i0 = i0Old
        if (p(a)) {
          iOld = i
          bs += f(a)
          if (buffer ne bufferOld) {
            i = iOld
            buffer = bufferOld
          }
          reqSep = reqSepOld
          iN = iNOld
          i0 = i0Old
        }
      }
    }
    catch { case t if fail is t => val sub = fail as t value; fail( GrokError(e.delim.toByte, e.alt.toByte, 0, iOld, null, sub :: Nil)(bufferOld) ) }
    finally {
      if (buffer ne bufferOld) {
        i = iOld
        buffer = bufferOld
      }
      reqSep = reqSepOld
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
    var tToBe = 0
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
    if (!failures) Yes(successBuffer.result()) else No((successBuffer.result(), failureBuffer.result()))
  }
}
object GrokBinary {
  private[eio] final val unsafe = classOf[sun.misc.Unsafe].getDeclaredConstructor() match {
    case c => c.setAccessible(true); c.newInstance().asInstanceOf[sun.misc.Unsafe]
  }
  private[eio] final val OFS = unsafe.arrayBaseOffset(classOf[Array[Byte]])
}

