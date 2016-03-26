// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import java.nio._

// WARNING!!! WARNING!!! WARNING!!!
// This file is an unholy amalgam of code from JsonByteBufferParser and JsonStringParser
// with some of its own custom logic as well.  And it uses sun.misc.Unsafe.
// Venture forth with the utmost caution.

trait RecyclingBuffer {
  def start: Int
  def start_=(i: Int): Unit
  def end: Int
  def end_=(i: Int): Unit
  def buffer: Array[Byte]
  def buffer_=(ab: Array[Byte]): Unit
  def offset: Long
  def offset_=(l: Long): Unit
  def source: RecyclingBuffer => Boolean
  def source_=(src: RecyclingBuffer => Boolean): Unit
  def exhausted: Boolean
  def exhausted_=(b: Boolean): Unit
  def pack(): this.type
  def expand(): this.type
  def recycle(): this.type = {
    if (!exhausted) exhausted = source(this)
    this
  }
  def available() = end - start
}

final class JsonRecyclingParser extends RecyclingBuffer {
  import JsonRecyclingParser.unsafe.{getByte => gB, getInt => gI, putChar => pC}
  import JsonRecyclingParser.{oBytes, oChars, nullInInt, trueInInt, alseInInt}
  import JsonGenericParser._

  private[this] var i0: Int = 0
  private[this] var iN: Int = 0
  private[this] var a: Array[Byte] = new Array[Byte](48)
  private[this] var o: Long = 0L
  private[this] var src: RecyclingBuffer => Boolean = (_ => false)
  private[this] var done: Boolean = true
  private[this] var cache: Jast = null
  private[this] var ca: Array[Char] = new Array[Char](24)
  private[this] var strictNumbers = true


  def start = i0
  def start_=(i: Int) { i0 = if (i < 0) 0 else if (i > iN) iN else i }
  def end = iN
  def end_=(i: Int) {
    iN = if (i < 0) 0 else if (i > buffer.length) buffer.length else i
    if (i0 > iN) i0 = iN
  }
  def offset = o
  def offset_=(l: Long) { if (l < 0) o = 0 else o = l }
  def source = src
  def source_=(s: RecyclingBuffer => Boolean) { src = s }
  def exhausted = done
  def exhausted_=(b: Boolean) { done = b }
  def buffer = a
  def buffer_=(b: Array[Byte]) { a = b; if (iN > b.length) iN = b.length }
  def pack(): this.type = {
    if (i0 > 0) {
      o += i0
      System.arraycopy(a, 0, a, i0, iN-i0)
      iN -= i0
      i0 = 0
    }
    this
  }
  def expand(): this.type = {
    val b = new Array[Byte]((a.length | (a.length << (if (a.length < 65536) 2 else 1))) & 0x7FFFFFF0)
    JsonRecyclingParser.unsafe.copyMemory(a, i0.toLong, b, 0L, (iN-i0).toLong)
    a = b
    iN -= i0
    offset += i0
    i0 = 0
    this
  }

  private[this] final def atLeast(n: Int) = 
    if ((iN - i0) >= n) true
    else {
      while (!done && (iN - i0) < n) recycle()
      (iN - i0) >= n
    }

  private[this] final def moreChars(n: Int) = {
    var l = ca.length
    val m = if (n <= 0) l+1 else n
    while (l > 0 && l < m) {
      if (l < 65536) l = l | (l << 2)
      else if (l < 4194304) l = l | (l << 1)
      else l = (l + ((l >>> 1) & 0x7FFFFFF8))
    }
    if (l < 0) l = 0x7FFFFFF8
    if (l > ca.length) ca = java.util.Arrays.copyOf(ca, l)
  }


  /** Relaxed parsing of numbers.  Parse everything to Double. */
  def relaxed: this.type = { strictNumbers = false; this }

  /** Strict parsing of numbers.  Parse everything to its exact form (the default). */
  def strict: this.type = { strictNumbers = true; this }

  /** Set whether parsing of numbers is strict (default) or relaxed */
  def relaxedNumbers(relax: Boolean): this.type = { strictNumbers = !relax; this }

  def refresh(source: RecyclingBuffer => Boolean): this.type = {
    i0 = 0
    iN = 0
    o = 0
    exhausted = false
    src = source
    this
  }

  def parse(source: RecyclingBuffer => Boolean): Jast = refresh(source).parseVal()

  private[jsonal] def parseVal(): Jast = {
    var c: Int = 0
    do {
      if (!atLeast(1)) return JastError("end of input, no value found", o+i0)
      c = gB(a, (oBytes + i0).toLong) - 8
      while (i0+1 < iN && { if ((c & 0xFFFFFFE0) == 0 && ((1 << c) & 0x1000026) != 0) true else return parseValStartingWith(c+8) }) {
        i0 += 1
        c = gB(a, (oBytes + i0).toLong) - 8
      }
    } while (i0+1 < iN || !done);
    JastError("end of input, no value found", o+i0)
  }

  private[jsonal] def parseValStartingWith(c: Int): Jast = {
    if (c == '"') parseStr()
    else if (c == '[') parseArr()
    else if (c == '{') parseObj()
    else if (c == '-' || (c >= '0' && c <= '9')) {
      parseNum(c)
      val ans = cache
      cache = null
      ans
    }
    else if (c == 'n') parseNull()
    else if (c == 't') parseTrue()
    else if (c == 'f') parseFalse()
    else JastError("invalid character: '" + c + "'", o+i0)
  }

  private[jsonal] def parseNull(): Jast = {
    if (!atLeast(3)) JastError("end of input, 'null' expected", o+i0)
    else if (gI(a, oBytes + i0.toLong) == nullInInt) { i0 += 4; Json.Null }
    else JastError("'null' expected but not found", o+i0)
  }

  private[jsonal] def parseTrue(): Jast = {
    if (!atLeast(3)) JastError("end of input, 'true' expected", o+i0)
    else if (gI(a, oBytes + i0.toLong) == trueInInt) { i0 += 4; Json.Bool.True }
    else JastError("'true' expected but not found " + gI(a, oBytes+i0-1L).toHexString, o+i0)    
  }

  private[jsonal] def parseFalse(): Jast = {
    if (!atLeast(4)) JastError("end of input, 'false' expected", o+i0)
    else if (gI(a, oBytes + i0 + 1L) == alseInInt) { i0 += 5; Json.Bool.False }
    else JastError("'false' expected but not found", o+i0) 
  }

  private[jsonal] def parseBool(): Jast = {
    if (!atLeast(4)) JastError("end of input (not room for boolean constant)", o+i0)
    else if (gI(a, oBytes+i0.toLong) == trueInInt) { i0 += 4; Json.Bool.True }
    else if (available >= 5 && gB(a, oBytes+i0.toLong) == 'f' && gI(a, oBytes+i0.toLong+1) == alseInInt) { i0 += 5; Json.Bool.False}
    else JastError("'true' or 'false' expected but not found.", o+i0)
  }

  private def hexifyByte(c: Byte): Int = {
    val x = (c - '0') & 0xFF
    if (x < 10) x
    else {
      val y = x | 0x20
      if (y >= 49 && y <= 54) y - 39
      else -1
    }
  }

  private[jsonal] def parseStr(): Jast = {
    if (!atLeast(2)) JastError("end of string without closing quotes", o+i0)
    var i: Long = i0 + oBytes + 1
    var j: Long = oChars
    var M: Long = iN + oBytes
    var L = oChars + 2L * ca.length
    var c: Int = gB(a, i)
    while (c != '"') {
      if (M - i < 32 && !done) {
        val ix = i0
        recycle()
        if (i0 != ix) i += (i0-ix)
        M = iN + oBytes
      }
      if (((j - oChars) >> 1) + 40 > ca.length) {
        if (((j - oChars) >> 1) + 40 > 0x7FFFFFF8) return JastError("String too long",o+i0)
        moreChars(((j - oChars) >> 1).toInt + 40)
        L = oChars + 2L * ca.length
      }
      val I = math.min(M, i + (ca.length - ((j - oChars) >> 1)))  // Surely okay this far
      if (I - i < 2) return JastError("end of string without closing quotes", o+i0)
      while (c != '\\' && c != '"' && c >= 0 && i+1 < I) {
        pC(ca, j, c.toChar)
        j += 2
        i += 1
        c = gB(a, i)
      }
      while ((c == '\\' || c < 0) && L-j > 4 && (M-i > 7 || done)) {
        if (I - i < 3) return JastError("end of string without closing quotes", o+i0)
        i += 1
        val c2 = gB(a, i)
        if (c == '\\') {
          pC(ca, j,
            gB(a, i) match {
              case 'n' => '\n'
              case 't' => '\t'
              case '\\'=> '\\'
              case '"' => '"'
              case 'u' =>
                if (M-i < 5) return JastError("end of string in unicode escape", o+i0)
                val h = (hexifyByte(gB(a, {i+=1; i})) << 12) |
                        (hexifyByte(gB(a, {i+=1; i})) << 8) |
                        (hexifyByte(gB(a, {i+=1; i})) << 4) |
                        hexifyByte(gB(a, {i+=1; i}))
                if (h < 0) return JastError("bad character in unicode escape", o+i0)
                h.toChar
              case 'r' => '\r'
              case 'b' => '\b'
            }
          )
          j += 2
        }
        else if ((c & 0xE0) == 0xC0) {
          if ((c2 & 0xC0) != 0x80) return JastError("Improper UTF-8 encoding", o+i-oBytes)
          c = ((c&0x1F) << 6) | (c2&0x3F)
          if (c < 0x80) return JastError("Overlong UTF-8 encoding", o+i-1-oBytes)
          pC(ca, j, c.toChar)
          j += 2
        }
        else if ((c & 0xF0) == 0xE0) {
          if (M-i < 2) return JastError("string ends in the middle of UTF-8 multi-byte character", o+i0)
          i += 1
          val c3 = gB(a, i)
          if ((c2 & 0xC0) + (c3 & 0xC0) != 0x100) return JastError("Improper UTF-8 encoding", o+i-1-oBytes)
          c = ((c&0xF) << 12) | ((c2&0x3F) << 6) | (c3&0x3F)
          if (c < 0x800) return JastError("Overlong UTF-8 encoding", o+i-2-oBytes)
          pC(ca, j, c.toChar)
          j += 2
        }
        else if ((c & 0xF8) == 0xF0) {
          if (M-i < 3) return JastError("string ends in the middle of UTF-8 multi-byte character", o+i0)
          val c3 = gB(a, {i+=1; i})
          val c4 = gB(a, {i+=1; i})
          if ((c2 & 0xC0) + (c3 & 0xC0) + (c4 & 0xC0) != 0x180) return JastError("Improper UTF-8 encoding", o+i-2-oBytes)
          c = ((c & 0x7) << 18) | ((c2&0x3F) << 12) | ((c3&0x3F) << 6) | (c4&0x3F)
          if (c < 0x10000 || c > 0x10FFFF) return JastError("Overlong or out of bounds UTF-8 encoding", o+i-3-oBytes)
          pC(ca, j, ((c >> 10) + 0xD800).toChar)
          j += 2
          pC(ca, j, ((c & 0x3FF) + 0xDC00).toChar)
          j += 2
        }
        else return JastError("Improper UTF-8 encoding", o+i-1-oBytes)
        i += 1
        c = gB(a, i)
      }
    }
    i0 = (i+1-oBytes).toInt
    Json.Str(new String(ca, 0, ((j - oChars) >> 1).toInt))
  }

  private[jsonal] def parseNum(initial: Int, toCache: Boolean = true): Double = {
    cache = null
    atLeast(44)   // Can be false, we just want to make sure we have plenty of bytes to handle the fast case
    var i = (i0 + oBytes).toLong
    var M = (iN + oBytes).toLong
    var dadp = 0  // How many of our digits are after the decimal point?
    var dbdp = 0  // How many are before the decimal point?
    var digits = 0L
    var c = if (initial != '-') initial else {
      if (M-i > 1) { i += 1; gB(a, i) }
      else { cache = JastError("unfinished number", o+i0); return Double.NaN }
    }
    if (c > '0' && c <= '9') {
      digits = c - '0'
      dbdp = 1
      while (dbdp < 20 && M - i > 1 && { i += 1; c = gB(a, i); c >= '0' && c <= '9'}) { dbdp += 1; digits = digits*10 + (c - '0') }
      if (dbdp > 19) return parseNumFromString(i, 0, toCache)
    }
    else if (c == '0') {
      if (M - i > 1 && { i += 1; c = gB(a, i); c >= '0' && c <= '9'}) {
        cache = JastError("multi-digit number cannot start with 0", o+i0)
        return Double.NaN
      }
    }
    else { cache = JastError("number should start with a numeric digit", o+i0); return Double.NaN }
    if (c != '.' && (c|0x20) != 'e') {
      // Number is all done.  Might be a Long.  Save it if so!
      if (digits >= 0 || (initial == '-' && digits == Long.MinValue)) {
        // Yes, it's a Long!  Save it.
        i0 = (i - oBytes).toInt
        if (initial == '-') digits = -digits   // No-op for Long.MinValue, so we're okay
        val dbl = digits.toDouble
        if (toCache) cache = new Json.Num(java.lang.Double.longBitsToDouble(digits), null)
        else if (strictNumbers && dbl.toLong != digits) cache = wouldNotFitInDouble
        return dbl
      }
      else return parseNumFromString(i, 3, toCache)
    }
    // Number is not done.  Keep parsing it.
    if (c == '.') {
      val D = math.max(20-dbdp, 0)
      while (dadp < D && M - i > 1 && { i += 1; c = gB(a, i); c >= '0' && c <= '9' }) { dadp += 1; digits = digits*10 + (c - '0') }
      if (dadp == 0) { cache = JastError("need digits after . in number", o+i-oBytes); return Double.NaN }
      if (dadp >= D) return parseNumFromString(i, 1, toCache)
    }
    val ex =
      if ((c | 0x20) != 'e') 0
      else {
        if (M - i <= 1) { cache = JastError("need digits after e in number", o + i0); return Double.NaN }
        i += 1
        c = gB(a, i)
        val negex = c match {
          case '-' =>
            if (M - i <= 1) { 
              cache = JastError("need digits after - in number exponent", o + i - oBytes)
              return Double.NaN
            }
            i += 1
            c = gB(a, i)
            true
          case '+' =>
            if (M - i <= 1) { 
              cache = JastError("need digits after + in number exponent", o + i - oBytes)
              return Double.NaN
            }
            i += 1
            c = gB(a, i)
            false
          case _ => false
        }
        var x = (c - '0')
        if (x < 0 || x >= 10) { 
          cache = JastError("exponent in number must be numeric digits", o + i - oBytes)
          return Double.NaN
        }
        while (M - i > 1 && x < 999 && { i += 1; c = gB(a, i); c >= '0' && c <= '9' }) x = x*10 + (c - '0')
        if (x >= 999) return parseNumFromString(i, 2, toCache)
        if (negex) -x else x
      }
    val shift = ex - dadp
    if (
      dadp + dadp < 19 &&
      java.lang.Long.numberOfLeadingZeros(digits) + java.lang.Long.numberOfTrailingZeros(digits) >= 11 &&
      shift >= -22 &&
      shift <= 22
    ) {
      // We can store the digits in a Double and IEEE demands that * and / are exact (and 1e22 is exact)
      // Thus, we can get the exact result with a single multiplication or division!
      val dbl =
        if (shift == 0) digits.toDouble
        else if (shift > 0) digits * smallPowersOfTen(shift)
        else digits / smallPowersOfTen(-shift)
      val sdbl = if (initial == '-') -dbl else dbl
      if (toCache) cache = new Json.Num(sdbl, "")
      i0 = (i+(if (c >= '0' && c <= '9') 1 else 0)-oBytes).toInt
      sdbl
    }
    else parseNumFromString(i, 3, toCache)
  }

  private[jsonal] def parseNumFromString(ix: Long, state: Int, toCache: Boolean): Double = {
    var i = ix
    var M = (iN + oBytes).toLong
    var st = state
    var c = gB(a, i)
    while (st < 1) {
      while (M - i > 1 && { i += 1; c = gB(a,i); c >= '0' && c <= '9' }) {}
      if ((c >= '0' && c <= '9' || (c == '.') || ((c | 0x20) == 'e')) && M - i <= 3 && !done) {
        val iz = i0
        recycle()
        i += i0-iz
        M = (iN + oBytes).toLong
      }
      else if (c == '.') {
        i += 1
        c = gB(a, i)
        if (c < '0' || c > '9') {
          cache = JastError("No digit after decimal point", o+i-oBytes)
          return Double.NaN
        }
        st = 1
      }
      else if ((c | 0x20) == 'e') {
        i += 1
        c = gB(a, i)
        if (c == '-' || c == '+') {
          i += 1
          c = gB(a, i)
        }
        if (c < '0' || c > '9') {
          cache = JastError("No digit after exponent", o+i-oBytes)
          return Double.NaN
        }
        st = 2
      }
      else st = 3
    }
    while (st < 2) {
      while (M - i > 1 && { i += 1; c = gB(a,i); c >= '0' && c <= '9'}) {}
      if ((c >= '0' && c <= '9' || ((c | 0x20) == 'e')) && M - i <= 3 && !done) {
        val iz = i0
        recycle()
        i += i0-iz
        M = (iN + oBytes).toLong
      }
      else if ((c | 0x20) == 'e') {
        i += 1
        c = gB(a, i)
        if (c == '-' || c == '+') {
          i += 1
          c = gB(a, i)
        }
        if (c < '0' || c > '9') {
          cache = JastError("No digit after exponent", o+i-oBytes)
          return Double.NaN
        }
      }
      else st = 3
    }
    while (st < 3) {
      while (M - i > 1 && { i += 1; c = gB(a,i); c >= '0' && c <= '9'}) {}
      if (c >= '0' && c <= '9' && !done) {
        val iz = i0
        recycle()
        i += i0 - iz
        M = (iN + oBytes).toLong
      }
      else st = 3
    }
    val iM = (i - oBytes).toInt + (if (c >= '0' && c <= '9') 1 else 0)
    val str = new String(a, i0, iM - i0)
    i0 = iM
    val dbl = str.toDouble
    if (toCache)
      cache = 
        if (strictNumbers) new Json.Num(dbl, str)
        else if (java.lang.Double.isNaN(dbl) || java.lang.Double.isInfinite(dbl)) Json.Null
        else new Json.Num(dbl, "")
    else if (strictNumbers && !Json.Num.numericStringEquals(str, dbl.toString))
      cache = wouldNotFitInDouble
    dbl
  }

  private[jsonal] def parseJastNum(initial: Int): Jast = {
    parseNum(initial, true)
    cache
  }


  private[jsonal] def parseArrD(initial: Int): Jast = ???

  private[jsonal] def parseArr(): Jast = ???

  private[jsonal] def parseObj(): Jast = ???


}
object JsonRecyclingParser {
  import JsonGenericParser._

  private[jsonal] final val unsafe = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe") match {
    case uf => uf setAccessible true; uf.get(null).asInstanceOf[sun.misc.Unsafe]
  }
  private[jsonal] final val oBytes = unsafe.arrayBaseOffset(classOf[Array[Byte]])
  private[jsonal] final val oChars = unsafe.arrayBaseOffset(classOf[Array[Char]])
  private[jsonal] final val nullInInt =
    if (ByteOrder.nativeOrder == ByteOrder.BIG_ENDIAN) 'l' | ('l' << 8) | ('u' << 16) | ('n' << 24)
    else                                               'n' | ('u' << 8) | ('l' << 16) | ('l' << 24)
  private[jsonal] final val trueInInt =
    if (ByteOrder.nativeOrder == ByteOrder.BIG_ENDIAN) 'e' | ('u' << 8) | ('r' << 16) | ('t' << 24)
    else                                               't' | ('r' << 8) | ('u' << 16) | ('e' << 24)
  private[jsonal] final val alseInInt =
    if (ByteOrder.nativeOrder == ByteOrder.BIG_ENDIAN) 'e' | ('s' << 8) | ('l' << 16) | ('a' << 24)
    else                                               'a' | ('l' << 8) | ('s' << 16) | ('e' << 24)


  def Json(input: RecyclingBuffer => Boolean, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json] =
    (new JsonRecyclingParser).relaxedNumbers(relaxed).refresh(input).parseVal() match {
      case js: kse.jsonal.Json => Right(js)
      case je: JastError => Left(je)
    }
  def Null(input: RecyclingBuffer => Boolean, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Null] = {
    val jrp = (new JsonRecyclingParser).refresh(input).recycle()
    if (jrp.available < 4) return Left(JastError("Expected JSON null but not enough input", jrp.start))
    if (jrp.buffer(jrp.start) != 'n') Left(JastError("Expected JSON null but did not find 'n'", jrp.start))
    jrp.start += 1
    jrp.parseNull() match {
      case j: kse.jsonal.Json.Null => Right(j)
      case e: JastError => Left(e)
      case _ => Left(JastError("Internal error: parse did not produce JSON null or an error?"))
    }
  }
  def Bool(input: RecyclingBuffer => Boolean, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Bool] = {
    (new JsonRecyclingParser).refresh(input).recycle().parseBool() match {
      case jb: kse.jsonal.Json.Bool =>
        if (jb.value) myRightTrue else myRightFalse
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON boolean or an error?"))
    }
  }
  def Str(input: RecyclingBuffer => Boolean, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Str] = {
    val jrp = (new JsonRecyclingParser).refresh(input).recycle()
    if (jrp.available < 2) return Left(JastError("Expected JSON string but not enough input", jrp.start))
    if (jrp.buffer(jrp.start) != 'n') Left(JastError("Expected JSON string but did not find '\"'", jrp.start))
    jrp.start += 1
    jrp.parseStr() match {
      case js: kse.jsonal.Json.Str => Right(js)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON string or an error?"))
    }
  }
  def Num(input: RecyclingBuffer => Boolean, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Num] = {
    val jrp = (new JsonRecyclingParser).refresh(input).recycle()
    if (jrp.available < 1) return Left(JastError("Expected JSON number but not enough input", jrp.start))
    if (jrp.buffer(jrp.start) match { case c if (c >= '0' && c <= '9') || c == '.' => true; case _ => false})
      Left(JastError("Expected JSON number but did not find digit or '-'", jrp.start))
    jrp.start += 1
    jrp.parseJastNum() match {
      case jn: kse.jsonal.Json.Num => Right(jn)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON string or an error?"))
    }
  }
  def Arr(input: RecyclingBuffer => Boolean, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Arr] = {
    ???
    /*
    if (!input.hasRemaining) return Left(JastError("Expected JSON array but at end of input"))
    val c = input.get
    if (c != '[') {
      input.position(input.position-1)
      return Left(JastError("Expected JSON array but found character "+c, input.position))
    }
    val jcbp = (new JsonByteBufferParser).relaxedNumbers(relaxed)
    jcbp.parseArr(input) match {
      case ja: kse.jsonal.Json.Arr => Right(ja)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON array or an error?"))
    }
    */
  }
  def Obj(input: RecyclingBuffer => Boolean, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Obj] = {
    ???
    /*
    if (!input.hasRemaining) return Left(JastError("Expected JSON object but at end of input"))
    val c = input.get
    if (c != '"') {
      input.position(input.position-1)
      return Left(JastError("Expected JSON object but found character "+c, input.position))
    }
    val jcbp = (new JsonByteBufferParser).relaxedNumbers(relaxed)
    jcbp.parseObj(input) match {
      case jo: kse.jsonal.Json.Obj => Right(jo)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON object or an error?"))
    }
    */
  }

  def recycleString(s: String): RecyclingBuffer => Boolean = new Function1[RecyclingBuffer, Boolean]{
    def apply(rb: RecyclingBuffer): Boolean = {
      if (!rb.exhausted) {
        rb.buffer = s.getBytes("UTF-8")
        rb.start = 0
        rb.end = rb.buffer.length
        rb.offset = 0L
        rb.exhausted = true
        rb.source = this
        true
      }
      else false
    }
  }
}
