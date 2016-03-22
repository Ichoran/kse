// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import java.nio._


// WARNING - this code is almost entirely COPIED from JsonByteBufferParser,
// which is itself copied from JsonStringParser.
// YOU MUST MAINTAIN THIS BY HAND.  AAAAAAAHHHHHHH!!!!!
// This is done both to maximize performance and because there
// are a lot of fiddly little details that need to be altered.
class JsonByteBufferParser {
  import JsonByteBufferParser.smallPowersOfTen
  import JsonByteBufferParser.StringsFromByteBufferSlices

  private[this] var strictNumbers = true
  private[this] var cache: Jast = null

  /** Relaxed parsing of numbers.  Parse everything to Double. */
  def relaxed: this.type = { strictNumbers = false; this }

  /** Strict parsing of numbers.  Parse everything to its exact form (the default). */
  def strict: this.type = { strictNumbers = true; this }

  /** Set whether parsing of numbers is strict (default) or relaxed */
  def relaxedNumbers(relax: Boolean): this.type = { strictNumbers = !relax; this }

  def parse(input: ByteBuffer): Jast = parseVal(input)

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it gets a ByteBuffer pointing past that character
  //    Note that (c & 0xFFFFFFE0) == 0 && ((1 << c) & 0x1000026) != 0
  //      is a magic incantation to test whether (c+8) is whitespace (c: Int)
  /////////////

  private[jsonal] def parseVal(input: ByteBuffer): Jast = {
    var c: Int = 0
    while (
      { if (input.hasRemaining) true else return JastError("end of input, no value found", input.position) } && 
      { c = input.get - 8; (c & 0xFFFFFFE0) == 0 && ((1 << c) & 0x1000026) != 0}
    ) {}
    parseValStartingWith(input, c+8)
  }

  private[jsonal] def parseValStartingWith(input: ByteBuffer, c: Int): Jast = {
    if (c == '"') parseStr(input)
    else if (c == '[') parseArr(input)
    else if (c == '{') parseObj(input)
    else if (c == '-' || (c >= '0' && c <= '9')) {
      parseNum(input, c)
      val ans = cache
      cache = null
      ans
    }
    else if (c == 'n') parseNull(input)
    else if (c == 't') parseTrue(input)
    else if (c == 'f') parseFalse(input)
    else {
      input.position(input.position-1)
      JastError("invalid character: '" + c + "'", input.position)
    }
  }

  private[jsonal] def parseNull(input: ByteBuffer): Jast = {
    val zero = input.position-1
    if (input.remaining > 2 && input.get == 'u' && input.get == 'l' && input.get == 'l') Json.Null
    else {
      input.position(zero)
      JastError("Expected 'null'", zero)
    }
  }

  private[jsonal] def parseTrue(input: ByteBuffer): Jast = {
    val zero = input.position-1
    if (input.remaining > 2 && input.get == 'r' && input.get == 'u' && input.get == 'e') Json.Bool.True
    else {
      input.position(zero)
      JastError("Expected 'true'", zero)
    }
  }

  private[jsonal] def parseFalse(input: ByteBuffer): Jast = {
    val zero = input.position-1
    if (input.remaining > 3 && input.get == 'a' && input.get == 'l' && input.get == 's' && input.get == 'e') Json.Bool.False
    else {
      input.position(zero)
      JastError("Expected 'false'", zero)
    }
  }

  private[jsonal] def parseBool(input: ByteBuffer): Jast =
    if (input.hasRemaining) input.get match {
      case 't' => parseTrue(input)
      case 'f' => parseFalse(input)
      case c => input.position(input.position-1); JastError("Expected boolean but found character "+c, input.position)
    }
    else JastError("Expected boolean but found end of input", input.position)

  private[jsonal] def parseStr(input: ByteBuffer): Jast = {
    val first = input.position
    val c = scanSimpleStr(input)
    if (c == '"') new Json.Str(input.subStr(first, input.position-1))
    else if (c == -129) JastError("No closing quotes on string", first-1)
    else parseComplexStr(input, first, c)
  }
  
  private def scanSimpleStr(input: ByteBuffer): Int = {
    var c: Int = -1
    while ({if (!input.hasRemaining) return -129 else true} && { c = input.get; c != '"' && c != '\\' && c >= 0}) {}
    c
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

  private def parseComplexStr(input: ByteBuffer, first: Int, initial: Int): Jast = {
    val sb = new java.lang.StringBuilder
    var c = initial
    var p = input.position
    if (p-first > 1) input.subSB(sb, first, p-1)

    while (input.hasRemaining) {
      val c2 = input.get
      if (c == '\\') {
        sb append ((c2 match {
          case 'n' => '\n'
          case 'r' => '\r'
          case 't' => '\t'
          case 'u' =>
            if (input.remaining < 4) return JastError("string ends mid-unicode-escape", first-1)
            val h = (hexifyByte(input.get) << 12) | 
                    (hexifyByte(input.get) << 8) | 
                    (hexifyByte(input.get) << 4) | 
                    hexifyByte(input.get)
            if (h < 0) return JastError("non-hex value in unicode escape", input.position-4)
            h.toChar
          case 'f' => '\f'
          case 'b' => '\b'
          case x => 
            if (x == '"' || x == '/' || x == '\\') x.toChar
            else return JastError("invalid quoted character '" + x + "'", input.position-1)
        }): Char)
      }
      else if ((c & 0xE0) == 0xC0) {
        if ((c2 & 0xC0) != 0x80) return JastError("Improper UTF-8 encoding", input.position-1)
        c = ((c&0x1F) << 6) | (c2&0x3F)
        if (c < 0x80) return JastError("Overlong UTF-8 encoding", input.position-2)
        sb append c.toChar
      }
      else if ((c & 0xF0) == 0xE0) {
        if (!input.hasRemaining) return JastError("string ends in the middle of UTF-8 multi-byte character", first-1)
        val c3 = input.get
        if ((c2 & 0xC0) + (c3 & 0xC0) != 0x100) return JastError("Improper UTF-8 encoding", input.position-2)
        c = ((c&0xF) << 12) | ((c2&0x3F) << 6) | (c3&0x3F)
        if (c < 0x800) return JastError("Overlong UTF-8 encoding", input.position-3)
        sb append c.toChar
      }
      else if ((c & 0xF8) == 0xF0) {
        if (input.remaining < 2) return JastError("string ends in the middle of UTF-8 multi-byte character", first-1)
        val c3 = input.get
        val c4 = input.get
        if ((c2 & 0xC0) + (c3 & 0xC0) + (c4 & 0xC0) != 0x180) return JastError("Improper UTF-8 encoding", input.position-3)
        c = ((c & 0x7) << 18) | ((c2&0x3F) << 12) | ((c3&0x3F) << 6) | (c4&0x3F)
        if (c < 0x10000 || c > 0x10FFFF) return JastError("Overlong or out of bounds UTF-8 encoding", input.position-4)
        sb appendCodePoint c
      }
      else return JastError("Improper UTF-8 encoding", input.position-2)
      if (!input.hasRemaining) return JastError("No closing quotes on string", first-1)
      p = input.position
      c = input.get
      while (
        ( if (c == '"') { 
            if (input.position - p > 1) input.subSB(sb, p, input.position-1)
            return new Json.Str(sb.toString)
          } else true
        ) &&
        c != '\\' &&
        (if (input.hasRemaining) true else return JastError("No closing quotes on string", first-1))
      ) { c = input.get }
      if (input.position - p > 1) input.subSB(sb, p, input.position - 1)
    }
    JastError("No closing quotes on string", first-1)
  }


  private[jsonal] def parseNum(input: ByteBuffer, initial: Int, toCache: Boolean = true): Double = {
    val zero = input.position-1
    cache = null
    var dadp = 0  // How many of our digits are after the decimal point?
    var dbdp = 0  // How many are before the decimal point?
    var digits = 0L
    var c = if (initial != '-') initial else {
      if (input.hasRemaining) input.get
      else { cache = JastError("unfinished number", zero); input.position(zero); return Double.NaN }
    }
    if (c > '0' && c <= '9') {
      digits = c - '0'
      dbdp = 1
      while (dbdp < 19 && input.hasRemaining && { c = input.get; c >= '0' && c <= '9'}) { dbdp += 1; digits = digits*10 + (c - '0') }
      if (dbdp >= 19) while (input.hasRemaining && { c = input.get; c >= '0' && c <= '9' }) dbdp += 1
    }
    else if (c == '0') {
      if (input.hasRemaining && {c = input.get; c >= '0' && c <= '9'}) {
        cache = JastError("multi-digit number cannot start with 0", input.position-1)
        input.position(zero)
        return Double.NaN
      }
    }
    else { cache = JastError("number should start with a numeric digit", input.position-1); input.position(zero); return Double.NaN }
    if (c != '.' && (c|0x20) != 'e') {
      // Number is all done.  Might be a Long.  Save it if so!
      if (c < '0' || c > '9') input.position(input.position-1)  // Overshot, so back up
      if (dbdp < 20 && (digits >= 0) || (initial == '-' && digits == Long.MinValue)) {
        // Yes, it's a Long!  Save it.
        if (initial == '-') digits = -digits   // No-op for Long.MinValue, so we're okay
        val dbl = digits.toDouble
        if (toCache) cache = new Json.Num(java.lang.Double.longBitsToDouble(digits), null)
        else if (strictNumbers && dbl.toLong != digits) cache = JsonByteBufferParser.wouldNotFitInDouble
        return dbl
      }
      else {
        val text = input.subStr(zero, input.position)
        val dbl = text.toDouble
        if (toCache) cache = new Json.Num(dbl, text)
        else if (strictNumbers && !Json.Num.numericStringEquals(dbl.toString, text))
          cache = JsonByteBufferParser.wouldNotFitInDouble
        return dbl
      }
    }
    // Number is not done.  Keep parsing it.
    if (c == '.') {
      val M = math.max(19-dbdp, 0)
      while (dadp < M && input.hasRemaining && { c = input.get; c >= '0' && c <= '9' }) { dadp += 1; digits = digits*10 + (c - '0') }
      if (dadp >= M) while (input.hasRemaining && { c = input.get; c >= '0' && c <= '9' }) dadp += 1
      if (dadp == 0) { cache = JastError("need digits after . in number", input.position-1); input.position(zero); return Double.NaN }
    }
    val ex =
      if (!input.hasRemaining || (c | 0x20) != 'e') 0
      else {
        if (!input.hasRemaining) { cache = JastError("need digits after e in number", zero); input.position(zero); return Double.NaN }
        c = input.get
        val negex = c match {
          case '-' =>
            if (!input.hasRemaining) { 
              cache = JastError("need digits after - in number exponent", zero)
              input.position(zero)
              return Double.NaN
            }
            c = input.get
            true
          case '+' =>
            if (!input.hasRemaining) { 
              cache = JastError("need digits after + in number exponent", zero)
              input.position(zero)
              return Double.NaN
            }
            c = input.get
            false
          case _ => false
        }
        var x = (c - '0')
        if (x < 0 || x >= 10) { 
          cache = JastError("exponent in number must be numeric digits", input.position-1)
          input.position(zero)
          return Double.NaN
        }
        while (input.hasRemaining && x < 999 && { c = input.get; c >= '0' && c <= '9' }) x = x*10 + (c - '0')
        if (x >= 999) {
          while (input.hasRemaining && { c = input.get; c >= '0' && c <= '9' }) {}
          if (c < '0' || c > '9') input.position(input.position-1)
          val str = input.subStr(zero, input.position)
          val dbl = str.toDouble
          if (toCache)
            cache = 
              if (strictNumbers) new Json.Num(dbl, str)
              else if (java.lang.Double.isNaN(dbl) || java.lang.Double.isInfinite(dbl)) Json.Null
              else new Json.Num(dbl, "")
          else if (strictNumbers && !Json.Num.numericStringEquals(str, dbl.toString))
            cache = JsonByteBufferParser.wouldNotFitInDouble
          return dbl
        }
        if (negex) -x else x
      }
    val shift = ex - dadp
    if (c < '0' || c > '9') input.position(input.position-1)
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
      sdbl
    }
    else {
      val str = input.subStr(zero, input.position)
      val dbl = str.toDouble
      if (toCache)
        cache =
          if (strictNumbers) new Json.Num(dbl, str)
          else if (java.lang.Double.isNaN(dbl) || java.lang.Double.isInfinite(dbl)) Json.Null
          else new Json.Num(dbl, "")
      else if (strictNumbers && !Json.Num.numericStringEquals(str, dbl.toString))
        cache = JsonByteBufferParser.wouldNotFitInDouble
      dbl
    }
  }

  private[jsonal] def parseJastNum(input: ByteBuffer, initial: Int): Jast = {
    parseNum(input, initial, true)
    cache
  }


  private def parseArrD(input: ByteBuffer, initial: Int): Boolean = {
    var c = initial
    var buffer = new Array[Double](6)
    var n = 0
    while (c != ']') {
      if (((c < '0' && (c != '-')) || c > '9') && (strictNumbers || c != 'n')) return false
      cache = null
      val ans = 
        if (c != 'n') parseNum(input, c)
        else if (input.remaining < 3) return false
        else if (input.get != 'u' || input.get != 'l' || input.get != 'l') return false
        else Double.NaN
      if (strictNumbers && (cache eq JsonStringParser.wouldNotFitInDouble)) return false
      if (ans.isNaN && (cache ne null) && cache.isInstanceOf[JastError]) return false
      if (n >= buffer.length) buffer = java.util.Arrays.copyOf(buffer, 0x7FFFFFFE & ((buffer.length << 1) | 0x2))
      buffer(n) = ans      
      n += 1
      while (
        { if (input.hasRemaining) true else return false } && 
        { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
      ) {}
      if (c == ',') {
        while (
          { if (input.hasRemaining) true else return false } && 
          { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
        ) {}
        if (c == ']') return false
      }
    }
    cache = new Json.Arr.Dbl(if (buffer.length != n) java.util.Arrays.copyOf(buffer, n) else buffer)
    true
  }

  private[jsonal] def parseArr(input: ByteBuffer): Jast = {
    val zero = input.position-1
    var c: Int = '['
    while (
      { if (input.hasRemaining) true else return JastError("end of input with unclosed array", zero) } && 
      { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
    ) {}
    if (c == ']') return Json.Arr.All.empty
    val start = input.position
    if (c == '-' || (c >= '0' && c <='9')) {
      if (parseArrD(input, c)) {
        val ans = cache
        cache = null
        return ans
      }
      else {
        cache = null
        input.position(start)
      }
    }
    val contents = Json.Arr.All.builder
    var n = 0
    while (c != ']') {
      n += 1
      parseValStartingWith(input, c) match {
        case js: Json => contents ~ js
        case je: JastError => 
          val p = input.position - 1
          input.position(zero)
          return JastError("error in array element "+n, p, je)
      }
      while (
        { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input with unclosed array", zero) } } && 
        { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
      ) {}
      if (c == ',') {
        while (
          { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input with unclosed array", zero) } } && 
          { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
        ) {}
      }
      else if (c != ']') {
        val oopsi = input.position-1
        input.position(zero);
        return JastError("unexpected character '" + c.toChar + "' in array after index "+n, oopsi)
      }
    }
    contents ~ Json.Arr.All
  }


  private[jsonal] def parseObj(input: ByteBuffer): Jast = {
    val zero = input.position - 1
    var c: Int = '{'
    while (
      { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input with unclosed object", zero) } } && 
      { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
    ) {}
    if (c == '}') return Json.Obj.empty
    var kvs = new Array[AnyRef](6)
    var n = 0
    while (c != '}') {
      if (n >= kvs.length-1) kvs = java.util.Arrays.copyOf(kvs, 0x7FFFFFFE & ((kvs.length << 1) | 0x2))
      val p = input.position - 1
      if (c != '"') {
        input.position(zero)
        return JastError("object keys must be strings", p)
      }
      parseStr(input) match {
        case js: Json.Str => kvs(n) = js.text
        case je: JastError =>
          input.position(zero)
          return JastError("error reading key "+(n/2+1)+" in object", p, je)
        case _ =>
          input.position(zero)
          return JastError("object keys must be strings", p)
      }
      n += 1
      while (
        { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input after object key", zero) } } && 
        { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
      ) {}
      if (c != ':') { 
        val p = input.position - 1
        input.position(zero)
        return JastError("object key not followed with ':'", p)
      }
      while (
        { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input after object key", zero) } } && 
        { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
      ) {}
      parseValStartingWith(input, c) match {
        case js: Json => kvs(n) = js
        case je: JastError =>
          val p = input.position-1
          input.position(zero)
          return JastError("error reading value "+(n/2+1)+" (key " + kvs(n-1) + ") in object", p, je)
      }
      while (
        input.hasRemaining && 
        { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
      ) {}
      if (c != '}' && c != ',') {
        val p = input.position-1
        input.position(zero)
        return JastError("unexpected character '" + c.toChar + "' in object after entry " + (n/2+1) + "(key " + kvs(n-1) + ")", p)
      }
      if (c == ',') while (
        input.hasRemaining && 
        { c = input.get; val x = c-8; (x & 0xFFFFFFE0) == 0 && ((1 << x) & 0x1000026) != 0 }
      ) {}
      n += 1
    }
    Json.Obj.fromFlatArray(if (kvs.length == n) kvs else java.util.Arrays.copyOf(kvs, n))
  }
}


object JsonByteBufferParser{
  private[jsonal] val smallPowersOfTen = Array.tabulate(23)(i => s"1e$i".toDouble)

  private[jsonal] implicit class StringsFromByteBufferSlices(private val underlying: ByteBuffer) extends AnyVal {
    def subStr(start: Int, end: Int): String =
      if (underlying.hasArray) new String(underlying.array, start + underlying.arrayOffset, end - start, "UTF-8")
      else {
        val p = underlying.position
        underlying.position(0)
        val a = new Array[Byte](end-start)
        underlying.get(a)
        underlying.position(p)
        new String(a, "UTF-8")
      }
    def subSB(sb: java.lang.StringBuilder, start: Int, end: Int) {
      val ca = new Array[Char](end-start)
      if (underlying.hasArray) {
        val a = underlying.array
        var i = start + underlying.arrayOffset
        var j = 0
        while (j < ca.length) { ca(j) = a(i).toChar; i += 1; j += 1 }
      }
      else {
        val p = underlying.position
        underlying.position(start)
        var j = 0
        while (j < ca.length) { ca(j) = underlying.get.toChar; j += 1 }
        underlying.position(p)
      }
      sb append ca
    }
  }

  private val myRightNull: Either[JastError, kse.jsonal.Json.Null] = Right(kse.jsonal.Json.Null)
  private val myRightTrue: Either[JastError, kse.jsonal.Json.Bool] = Right(kse.jsonal.Json.Bool.True)
  private val myRightFalse: Either[JastError, kse.jsonal.Json.Bool] = Right(kse.jsonal.Json.Bool.False)

  private[jsonal] val wouldNotFitInDouble: JastError = JastError("Text number would not fit in a Double")

  def Json(input: ByteBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json] =
    (new JsonByteBufferParser).relaxedNumbers(relaxed).parseVal(input) match {
      case js: kse.jsonal.Json => Right(js)
      case je: JastError => Left(je)
    }
  def Null(input: ByteBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Null] = {
    if (input.remaining < 4) return Left(JastError("Expected JSON null but not enough input", input.position))
    val zero = input.position
    if (input.get != 'n' || input.get != 'u' || input.get != 'l' || input.get != 'l') {
      input.position(zero)
      Left(JastError("Expected JSON null but did not find literal text 'null'", zero))
    }
    else Right(kse.jsonal.Json.Null)
  }
  def Bool(input: ByteBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Bool] = {
    (new JsonByteBufferParser).relaxedNumbers(relaxed).parseBool(input) match {
      case jb: kse.jsonal.Json.Bool =>
        if (jb.value) myRightTrue else myRightFalse
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON boolean or an error?"))
    }
  }
  def Str(input: ByteBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Str] = {
    if (input.remaining < 2) return Left(JastError("Expected JSON string but at end of input"))
    if (input.get != '"') {
      input.position(input.position-1)
      return Left(JastError("Expected JSON string but did not find '\"'", input.position))
    }
    val jcbp = (new JsonByteBufferParser).relaxedNumbers(relaxed)
    jcbp.parseStr(input) match {
      case js: kse.jsonal.Json.Str => Right(js)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON string or an error?"))
    }
  }
  def Num(input: ByteBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Num] = {
    if (!input.hasRemaining) return Left(JastError("Expected JSON number but at end of input"))
    val c = input.get
    if (c != '-' && (c < '0' || c > '9')) {
      input.position(input.position-1)
      return Left(JastError("Expected JSON number but found character "+c, input.position))
    }
    val jcbp = (new JsonByteBufferParser).relaxedNumbers(relaxed)
    jcbp.parseJastNum(input, c) match {
      case jn: kse.jsonal.Json.Num => Right(jn)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON number or an error?"))
    }
  }
  def Arr(input: ByteBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Arr] = {
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
  }
  def Obj(input: ByteBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Obj] = {
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
  }
}

