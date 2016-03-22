// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import java.nio._


// WARNING - this code is largely COPIED from JsonStringParser.
// YOU MUST MAINTAIN THIS BY HAND.  IT IS THE CRAZY, I KNOW.
// This is done both to maximize performance and because there
// are a lot of fiddly little details that need to be altered.
class JsonCharBufferParser {
  import JsonCharBufferParser.smallPowersOfTen
  import JsonCharBufferParser.StringsFromCharBufferSlices

  private[this] var strictNumbers = true
  private[this] var cache: Jast = null

  /** Relaxed parsing of numbers.  Parse everything to Double. */
  def relaxed: this.type = { strictNumbers = false; this }

  /** Strict parsing of numbers.  Parse everything to its exact form (the default). */
  def strict: this.type = { strictNumbers = true; this }

  /** Set whether parsing of numbers is strict (default) or relaxed */
  def relaxedNumbers(relax: Boolean): this.type = { strictNumbers = !relax; this }

  def parse(input: CharBuffer): Jast = parseVal(input)

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it gets a CharBuffer pointing past that character 
  /////////////

  private[jsonal] def parseVal(input: CharBuffer): Jast = {
    var c: Char = 0
    while (
      { if (input.hasRemaining) true else return JastError("end of input, no value found", input.position) } && 
      { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) {}
    parseValStartingWith(input, c)
  }

  private[jsonal] def parseValStartingWith(input: CharBuffer, c: Char): Jast = {
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

  private[jsonal] def parseNull(input: CharBuffer): Jast = {
    val zero = input.position-1
    if (input.remaining > 2 && input.get == 'u' && input.get == 'l' && input.get == 'l') Json.Null
    else {
      input.position(zero)
      JastError("Expected 'null'", zero)
    }
  }

  private[jsonal] def parseTrue(input: CharBuffer): Jast = {
    val zero = input.position-1
    if (input.remaining > 2 && input.get == 'r' && input.get == 'u' && input.get == 'e') Json.Bool.True
    else {
      input.position(zero)
      JastError("Expected 'true'", zero)
    }
  }

  private[jsonal] def parseFalse(input: CharBuffer): Jast = {
    val zero = input.position-1
    if (input.remaining > 3 && input.get == 'a' && input.get == 'l' && input.get == 's' && input.get == 'e') Json.Bool.False
    else {
      input.position(zero)
      JastError("Expected 'false'", zero)
    }
  }

  private[jsonal] def parseBool(input: CharBuffer): Jast =
    if (input.hasRemaining) input.get match {
      case 't' => parseTrue(input)
      case 'f' => parseFalse(input)
      case c => input.position(input.position-1); JastError("Expected boolean but found character "+c, input.position)
    }
    else JastError("Expected boolean but found end of input", input.position)

  private[jsonal] def parseStr(input: CharBuffer): Jast = {
    val first = input.position
    val c = scanSimpleStr(input)
    if (c == '"') new Json.Str(input.subStr(first, input.position-1))
    else if (c < 0) JastError("No closing quotes on string", first-1)
    else parseComplexStr(input, first)
  }
  
  private def scanSimpleStr(input: CharBuffer): Int = {
    var c: Int = -1
    while ({if (!input.hasRemaining) return -1 else true} && { c = input.get; c != '"' && c != '\\' }) {}
    c
  }

  private def hexifyChar(c: Char): Int = hexifyLowerChar(c | 0x20)

  private def hexifyLowerChar(c: Int): Int =
    if (c >= '0' && c <= '9') c - '0' else if (c >= 'a' && c <= 'f') c - 87 else -1

  private def parseComplexStr(input: CharBuffer, first: Int): Jast = {
    val sb = new java.lang.StringBuilder
    var c: Char = '\\'
    var p = input.position
    if (p-first > 1) input.subSB(sb, first, p-1)

    while(input.hasRemaining) {
      sb append (input.get match {
        case 'n' => '\n'
        case 'r' => '\r'
        case 't' => '\t'
        case 'u' =>
          if (input.remaining < 4) return JastError("string ends mid-unicode-escape", input.position)
          val h = (hexifyChar(input.get) << 12) | 
                  (hexifyChar(input.get) << 8) | 
                  (hexifyChar(input.get) << 4) | 
                  hexifyChar(input.get)
          if (h < 0) return JastError("non-hex value in unicode escape", input.position-4)
          h.toChar
        case 'f' => '\f'
        case 'b' => '\b'
        case x => 
          if (x == '"' || x == '/' || x == '\\') x
          else return JastError("invalid quoted character '" + x + "'", input.position-1)
      })
      if (!input.hasRemaining) return JastError("No closing quotes on string", first-1)
      p = input.position
      if (p < 0) throw new Exception(f"WTF?! $p $input")
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


  private[jsonal] def parseNum(input: CharBuffer, initial: Char, toCache: Boolean = true): Double = {
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
        else if (strictNumbers && dbl.toLong != digits) cache = JsonStringParser.wouldNotFitInDouble
        return dbl
      }
      else {
        val text = input.subStr(zero, input.position)
        val dbl = text.toDouble
        if (toCache) cache = new Json.Num(dbl, text)
        else if (strictNumbers && !Json.Num.numericStringEquals(dbl.toString, text))
          cache = JsonStringParser.wouldNotFitInDouble
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
            cache = JsonStringParser.wouldNotFitInDouble
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
        cache = JsonStringParser.wouldNotFitInDouble
      dbl
    }
  }

  private[jsonal] def parseJastNum(input: CharBuffer, initial: Char): Jast = {
    parseNum(input, initial, true)
    cache
  }


  private def parseArrD(input: CharBuffer, initial: Char): Boolean = {
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
        { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) {}
      if (c == ',') {
        while (
          { if (input.hasRemaining) true else return false } && 
          { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
        ) {}
        if (c == ']') return false
      }
    }
    cache = new Json.Arr.Dbl(if (buffer.length != n) java.util.Arrays.copyOf(buffer, n) else buffer)
    true
  }

  private[jsonal] def parseArr(input: CharBuffer): Jast = {
    val zero = input.position-1
    var c = '['
    while (
      { if (input.hasRemaining) true else return JastError("end of input with unclosed array", zero) } && 
      { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
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
        { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) {}
      if (c == ',') {
        while (
          { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input with unclosed array", zero) } } && 
          { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
        ) {}
      }
      else if (c != ']') {
        val oopsi = input.position-1
        input.position(zero);
        return JastError("unexpected character '" + c + "' in array after index "+n, oopsi)
      }
    }
    contents ~ Json.Arr.All
  }


  private[jsonal] def parseObj(input: CharBuffer): Jast = {
    val zero = input.position - 1
    var c = '{'
    while (
      { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input with unclosed object", zero) } } && 
      { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
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
        { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) {}
      if (c != ':') { 
        val p = input.position - 1
        input.position(zero)
        return JastError("object key not followed with ':'", p)
      }
      while (
        { if (input.hasRemaining) true else { input.position(zero); return JastError("end of input after object key", zero) } } && 
        { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
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
        { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) {}
      if (c != '}' && c != ',') {
        val p = input.position-1
        input.position(zero)
        return JastError("unexpected character '" + c + "' in object after entry " + (n/2+1) + "(key " + kvs(n-1) + ")", p)
      }
      if (c == ',') while (
        input.hasRemaining && 
        { c = input.get; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) {}
      n += 1
    }
    Json.Obj.fromFlatArray(if (kvs.length == n) kvs else java.util.Arrays.copyOf(kvs, n))
  }
}


object JsonCharBufferParser{
  private[jsonal] val smallPowersOfTen = Array.tabulate(23)(i => s"1e$i".toDouble)

  private[jsonal] implicit class StringsFromCharBufferSlices(private val underlying: CharBuffer) extends AnyVal {
    def subStr(start: Int, end: Int): String =
      if (underlying.hasArray) new String(underlying.array, start + underlying.arrayOffset, end - start)
      else {
        val p = underlying.position
        underlying.position(0)
        val ans = underlying.subSequence(start, end).toString
        underlying.position(p)
        ans
      }
    def subSB(sb: java.lang.StringBuilder, start: Int, end: Int) {
      if (underlying.hasArray) sb.append(underlying.array, start + underlying.arrayOffset, end - start)
      else {
        val p = underlying.position
        underlying.position(start)
        var n = end-start
        while (n > 0) { sb append underlying.get; n -= 1 }
        underlying.position(p)
      }
    }
  }

  private val myRightNull: Either[JastError, kse.jsonal.Json.Null] = Right(kse.jsonal.Json.Null)
  private val myRightTrue: Either[JastError, kse.jsonal.Json.Bool] = Right(kse.jsonal.Json.Bool.True)
  private val myRightFalse: Either[JastError, kse.jsonal.Json.Bool] = Right(kse.jsonal.Json.Bool.False)

  private[jsonal] val wouldNotFitInDouble: JastError = JastError("Text number would not fit in a Double")

  def Json(input: CharBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json] =
    (new JsonCharBufferParser).relaxedNumbers(relaxed).parseVal(input) match {
      case js: kse.jsonal.Json => Right(js)
      case je: JastError => Left(je)
    }
  def Null(input: CharBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Null] = {
    if (input.remaining < 4) return Left(JastError("Expected JSON null but not enough input", input.position))
    val zero = input.position
    if (input.get != 'n' || input.get != 'u' || input.get != 'l' || input.get != 'l') {
      input.position(zero)
      Left(JastError("Expected JSON null but did not find literal text 'null'", zero))
    }
    else Right(kse.jsonal.Json.Null)
  }
  def Bool(input: CharBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Bool] = {
    (new JsonCharBufferParser).relaxedNumbers(relaxed).parseBool(input) match {
      case jb: kse.jsonal.Json.Bool =>
        if (jb.value) myRightTrue else myRightFalse
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON boolean or an error?"))
    }
  }
  def Str(input: CharBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Str] = {
    if (input.remaining < 2) return Left(JastError("Expected JSON string but at end of input"))
    if (input.get != '"') {
      input.position(input.position-1)
      return Left(JastError("Expected JSON string but did not find '\"'", input.position))
    }
    val jcbp = (new JsonCharBufferParser).relaxedNumbers(relaxed)
    jcbp.parseStr(input) match {
      case js: kse.jsonal.Json.Str => Right(js)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON string or an error?"))
    }
  }
  def Num(input: CharBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Num] = {
    if (!input.hasRemaining) return Left(JastError("Expected JSON number but at end of input"))
    val c = input.get
    if (c != '-' && (c < '0' || c > '9')) {
      input.position(input.position-1)
      return Left(JastError("Expected JSON number but found character "+c, input.position))
    }
    val jcbp = (new JsonCharBufferParser).relaxedNumbers(relaxed)
    jcbp.parseJastNum(input, c) match {
      case jn: kse.jsonal.Json.Num => Right(jn)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON number or an error?"))
    }
  }
  def Arr(input: CharBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Arr] = {
    if (!input.hasRemaining) return Left(JastError("Expected JSON array but at end of input"))
    val c = input.get
    if (c != '[') {
      input.position(input.position-1)
      return Left(JastError("Expected JSON array but found character "+c, input.position))
    }
    val jcbp = (new JsonCharBufferParser).relaxedNumbers(relaxed)
    jcbp.parseArr(input) match {
      case ja: kse.jsonal.Json.Arr => Right(ja)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON array or an error?"))
    }
  }
  def Obj(input: CharBuffer, relaxed: Boolean = false): Either[JastError, kse.jsonal.Json.Obj] = {
    if (!input.hasRemaining) return Left(JastError("Expected JSON object but at end of input"))
    val c = input.get
    if (c != '"') {
      input.position(input.position-1)
      return Left(JastError("Expected JSON object but found character "+c, input.position))
    }
    val jcbp = (new JsonCharBufferParser).relaxedNumbers(relaxed)
    jcbp.parseObj(input) match {
      case jo: kse.jsonal.Json.Obj => Right(jo)
      case je: JastError => Left(je)
      case _ => Left(JastError("Internal error: parse did not produce JSON object or an error?"))
    }
  }
}
