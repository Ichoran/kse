// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import kse.flow._

final class JsonStringParser {
  import JsonGenericParser._

  private[this] var strictNumbers = true
  private[this] var idx = 0
  private[this] var cache: Jast = null

  /** Relaxed parsing of numbers.  Parse everything to Double. */
  def relaxed: this.type = { strictNumbers = false; this }

  /** Strict parsing of numbers.  Parse everything to its exact form (the default). */
  def strict: this.type = { strictNumbers = true; this }

  /** Set whether parsing of numbers is strict (default) or relaxed */
  def relaxedNumbers(relax: Boolean): this.type = { strictNumbers = !relax; this }

  def parse(input: String): Jast = parseVal(input, 0, input.length)

  /////////////
  // Important invariants within methods:
  //    Upon error return, idx is unchanged
  //    Upon valid return, idx points after the last parsed character
  //    c holds the current character
  //    i points to the next character after c
  //    If a unique character is already parsed, the method that parses the rest of it gets an index past that character 
  /////////////

  private[jsonal] def parseVal(input: String, index: Int, end: Int): Jast = {
    var i = index
    var c: Char = 0
    while (
      { if (i < end) true else return JastError("end of input, no value found", index) } && 
      { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    i += 1
    if (c == '"') parseStr(input, i, end)
    else if (c == '[') parseArr(input, i, end)
    else if (c == '{') parseObj(input, i, end)
    else if (c == '-') {
      parseNum(input, i-1, end, true)
      val ans = cache
      cache = null
      ans
    }
    else if (c >= '0' && c <= '9') {
      parseNum(input, i-1, end, false)
      val ans = cache
      cache = null
      ans
    }
    else if (c == 'n') parseNull(input, i, end)
    else if (c == 't') parseTrue(input, i, end)
    else if (c == 'f') parseFalse(input, i, end)
    else JastError("invalid character: '" + c + "'", i-1)
  }

  private[jsonal] def parseNull(input: String, index: Int, end: Int): Jast = {
    if (index < end-2 && input.charAt(index) == 'u' && input.charAt(index+1) == 'l' && input.charAt(index+2) == 'l') {
      idx = index+3
      Json.Null
    }
    else JastError("Expected 'null' but found "+input.substring(index-1, math.min(end, index+3)), index-1)
  }

  private[jsonal] def parseTrue(input: String, index: Int, end: Int): Jast = {
    if (index < end-2 && input.charAt(index) == 'r' && input.charAt(index+1) == 'u' && input.charAt(index+2) == 'e') {
      idx = index+3
      Json.Bool.True
    }
    else JastError("Expected 'true' but found "+input.substring(index-1, math.min(end, index+3)), index-1)
  }

  private[jsonal] def parseFalse(input: String, index: Int, end: Int): Jast = {
    if (index < end-3 && input.charAt(index) == 'a' && input.charAt(index+1) == 'l' && input.charAt(index+2) == 's' && input.charAt(index+3) == 'e') {
      idx = index+4
      Json.Bool.False
    }
    else JastError("Expected 'false' but found "+input.substring(index-1, math.min(end, index+4)), index-1)
  }

  private[jsonal] def parseBool(input: String, index: Int, end: Int): Jast =
    if (index < end) input.charAt(index) match {
      case 't' => parseTrue(input, index+1, end)
      case 'f' => parseFalse(input, index+1, end)
      case c => JastError("Expected boolean but found character "+c, index)
    }
    else JastError("Expected boolean but found end of input", index)

  private[jsonal] def parseStr(input: String, index: Int, end: Int): Jast = {
    val i = parseSimpleStr(input, index, end)
    if (i >= 0) { idx = i; new Json.Str(input.substring(index, i-1)) }
    else parseComplexStr(input, index, end, -i-1)
  }
  
  private def parseSimpleStr(input: String, index: Int, end: Int): Int = {
    var i = index
    var c: Char = 0
    while (i < end && { c = input.charAt(i); c != '"' && c != '\\' }) i += 1
    if (c == '"') i+1 else -i-1
  }

  private def hexifyChar(c: Char): Int = {
    val x = (c - '0') & 0xFFFF
    if (x < 10) x
    else {
      val y = x | 0x20
      if (y >= 49 && y <= 54) y - 39
      else -1
    }
  }

  private def parseComplexStr(input: String, index: Int, end: Int, cleanUntil: Int): Jast = {
    val N = end
    var n = (0xFFFFFFFF >>> math.min(28, java.lang.Integer.numberOfLeadingZeros(cleanUntil - index))) & 0x7FFFFFFE
    var j = 0
    var buffer = new Array[Char](n)
    var i0 = index
    var iN = cleanUntil
    var c: Char = '\\'
    do {
      if (i0 < iN) {
        var resize = false
        while (iN - i0 > n - j) {
          n = (n << 1) | 0x2
          resize = true
          if (n < 0) return JastError("string too long", index)
        }
        if (resize) buffer = java.util.Arrays.copyOf(buffer, n)
        input.getChars(i0, iN, buffer, j)
        j += (iN - i0)
        i0 = iN
      }
      if (c == '"') {
        idx = i0+1
        return new Json.Str(new String(buffer, 0, j))
      }
      while (c == '\\') {
        i0 += 1
        if (i0 < N) {
          val q = input.charAt(i0) match {
            case 'n' => '\n'
            case 'r' => '\r'
            case 't' => '\t'
            case 'u' =>
              if (i0 >= N - 4) return JastError("string ends mid-unicode-escape", i0)
              val h = (hexifyChar(input.charAt(i0+1)) << 12) | 
                      (hexifyChar(input.charAt(i0+2)) << 8) | 
                      (hexifyChar(input.charAt(i0+3)) << 4) | 
                      hexifyChar(input.charAt(i0+4))
              if (h < 0) return JastError("non-hex value in unicode escape", i0+1)
              i0 += 4
              h.toChar
            case 'f' => '\f'
            case 'b' => '\b'
            case x => 
              if (x == '"' || x == '/' || x == '\\') x
              else return JastError("invalid quoted character '" + x + "'", i0)
          }
          i0 += 1
          if (i0 < N) c = input.charAt(i0)
          if (j >= n) {
            n = (n << 1) | 0x2
            if (n < 0) return JastError("string too long", index)
            buffer = java.util.Arrays.copyOf(buffer, n)
          }
          buffer(j) = q
          j += 1
        }
        else return JastError("string ends mid-escape", i0)
        if (i0 < N) c = input.charAt(i0)
        else return JastError("no closing quote on string", index)
      }
      if (c == '"') {
        idx = i0+1
        return new Json.Str(new String(buffer, 0, j))
      }
      if (i0 < N) {
        iN = parseSimpleStr(input, i0+1, end)
        if (iN < 0) iN = -iN-1
        else iN -= 1
        if (iN < N) c = input.charAt(iN)
      }
      else i0 = N
    } while (i0 < N)
    JastError("no closing quote on string", index)
  }

  private[jsonal] def parseNum(input: String, index: Int, end: Int, negative: Boolean, toCache: Boolean = true): Double = {
    cache = null
    var i = if (negative) index+1 else index
    if (i >= end) { cache = JastError("unfinished number", index); return Double.NaN }
    var dadp = 0  // How many of our digits are after the decimal point?
    var dbdp = 0  // How many are before the decimal point?
    var digits = 0L
    var c = input.charAt(i)
    if (c > '0' && c <= '9') {
      dbdp = i  // Hack to count the number of digits
      digits = c - '0'
      val M = math.min(i+19, end)  // A Long could have up to 19 digits and still fit (-9 223 372 036 854 775 808)
      i += 1
      while (i < M && { c = input.charAt(i); c >= '0' && c <= '9' }) { i += 1; digits = digits*10 + (c - '0') }
      if (i == M) while (i < end && { c = input.charAt(i); c >= '0' && c <= '9'}) i += 1
      dbdp = i - dbdp   // End of hack, correct number of digits now
    }
    else if (c == '0') {
      i += 1
      if (i < end && { c = input.charAt(i); c >= '0' && c <= '9'}) {
        cache = JastError("multi-digit number cannot start with 0", i-1)
        return Double.NaN
      }
    }
    else { cache = JastError("number should start with a numeric digit", i); return Double.NaN }
    if (c != '.' && (c|0x20) != 'e') {
      // Number is all done.  Might be a Long.  Save it if so!
      idx = i
      if (dbdp < 20 && (digits >= 0) || (negative && digits==Long.MinValue)) {
        // Yes, it's a Long!  Save it.
        if (negative) digits = -digits   // No-op for Long.MinValue, so we're okay
        val dbl = digits.toDouble
        if (toCache) cache = new Json.Num(java.lang.Double.longBitsToDouble(digits), null)
        else if (strictNumbers && dbl.toLong != digits) cache = wouldNotFitInDouble
        return dbl
      }
      else {
        val text = input.substring(index, i)
        val dbl = text.toDouble
        if (toCache) cache = new Json.Num(dbl, text)
        else if (strictNumbers && !Json.Num.numericStringEquals(dbl.toString, text))
          cache = wouldNotFitInDouble
        return dbl
      }
    }
    // Number is not done.  Keep parsing it.
    if (c == '.') {
      i += 1
      val dp = i
      val M = math.min(if (dbdp > 19) i else i + (19 - dbdp), end)
      while (i < M && { c = input.charAt(i); c >= '0' && c <= '9' }) { i += 1; digits = digits*10 + (c - '0') }
      if (i >= M) while (i < end && { c = input.charAt(i); c >= '0' && c <= '9' }) i += 1
      dadp = i - dp
      if (dadp == 0) { cache = JastError("need digits after . in number", i); return Double.NaN }
    }
    val ex =
      if ((c | 0x20) != 'e') 0
      else {
        i += 1
        if (i >= end) { cache = JastError("need digits after e in number", i); return Double.NaN }
        c = input.charAt(i)
        val negex = c match {
          case '-' =>
            i += 1
            if (i >= end) { cache = JastError("need digits after - in number exponent", i); return Double.NaN }
            c = input.charAt(i)
            true
          case '+' =>
            i += 1
            if (i >= end) { cache = JastError("need digits after + in number exponent", i); return Double.NaN }
            c = input.charAt(i)
            false
          case _ => false
        }
        var x = (c - '0')
        if (x < 0 || x >= 10) { cache = JastError("exponent in number must be numeric digits", i); return Double.NaN }
        i += 1
        while (i < end && x < 999 && { c = input.charAt(i); c >= '0' && c <= '9' }) { x = x*10 + (c - '0'); i += 1 }
        if (x >= 999) {
          while (i < end && { c = input.charAt(i); c >= '0' && c <= '9' }) i += 1
          val str = input.substring(index, i)
          val dbl = str.toDouble
          if (toCache)
            cache = 
              if (strictNumbers) new Json.Num(dbl, str)
              else if (java.lang.Double.isNaN(dbl) || java.lang.Double.isInfinite(dbl)) Json.Null
              else new Json.Num(dbl, "")
          else if (strictNumbers && !Json.Num.numericStringEquals(str, dbl.toString))
            cache = wouldNotFitInDouble
          idx = i
          return dbl
        }
        if (negex) -x else x
      }
    val shift = ex - dadp
    idx = i
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
      val sdbl = if (negative) -dbl else dbl
      if (toCache) cache = new Json.Num(sdbl, "")
      sdbl
    }
    else {
      val str = input.substring(index, i)
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
  }

  private[jsonal] def parseJastNum(input: String, index: Int, end: Int, negative: Boolean): Jast = {
    parseNum(input, index, end, negative, true)
    cache
  }

  private def parseArrD(input: String, index: Int, end: Int, c0: Char): Boolean = {
    val N = end
    var i = index
    var c = c0
    var buffer = new Array[Double](6)
    var n = 0
    while (c != ']') {
      cache = null
      val ans = 
        if (c == '-') parseNum(input, i, end, true, false)
        else if (c >= '0' && c <= '9') parseNum(input, i, end, false, false)
        else if (c == 'n') {
          if (i < end-2 && input.charAt(i+1)=='u' && input.charAt(i+2)=='l' && input.charAt(i+3)=='l') {
            idx = i+4
            Double.NaN
          }
          else return false
        }
        else return false
      i = idx
      if (strictNumbers && (cache eq wouldNotFitInDouble)) return false
      if (ans.isNaN && (cache ne null) && cache.isInstanceOf[JastError]) return false
      if (n >= buffer.length) buffer = java.util.Arrays.copyOf(buffer, 0x7FFFFFFE & ((buffer.length << 1) | 0x2))
      buffer(n) = ans
      n += 1
      while (
        { if (i < N) true else return false } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c == ',') {
        i += 1
        while (
          { if (i < N) true else return false } && 
          { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
        ) i += 1
        if (c == ']') return false
      }
    }
    idx = i+1
    cache = new Json.Arr.Dbl(if (buffer.length != n) java.util.Arrays.copyOf(buffer, n) else buffer)
    true
  }

  private[jsonal] def parseArr(input: String, index: Int, end: Int): Jast = {
    val N = end
    var i = index
    var c = (0: Char)
    while (
      { if (i < N) true else return JastError("end of input with unclosed array", index) } && 
      { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    if (c == ']') { idx = i+1; return Json.Arr.All.empty }
    val idx0 = idx
    if (parseArrD(input, i, end, c)) {
      val ans = cache
      cache = null
      return ans
    }
    else {
      cache = null
      idx = idx0
    }
    val contents = Json.Arr.All.builder
    var n = 0
    while (c != ']') {
      n += 1
      parseVal(input, i, end) match {
        case js: Json => contents ~ js
        case je: JastError => return JastError("error in array element "+n, i, je)
      }
      i = idx
      while (
        { if (i < N) true else return JastError("end of input with unclosed array", index) } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != ']' && c != ',') return JastError("unexpected character '" + c + "' in array after index "+n, i)
      i += 1
    }
    idx = i
    contents ~ Json.Arr.All
  }

  private[jsonal] def parseObj(input: String, index: Int, end: Int): Jast = {
    val N = end
    var i = index
    var c = (0: Char)
    while (
      { if (i < N) true else return JastError("end of input with unclosed object", index) } && 
      { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    if (c == '}') { idx = i+1; return Json.Obj.empty }
    var kvs = new Array[AnyRef](6)
    var n = 0
    while (c != '}') {
      if (n >= kvs.length-1) kvs = java.util.Arrays.copyOf(kvs, 0x7FFFFFFE & ((kvs.length << 1) | 0x2))
      parseVal(input, i, end) match {
        case js: Json.Str => kvs(n) = js.text
        case je: JastError => return JastError("error reading key "+(n/2+1)+" in object", i, je)
        case _ => return JastError("object keys must be strings", i)
      }
      n += 1
      i = idx
      while (
        { if (i < N) true else return JastError("end of input after object key -- no value", index) } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != ':') return JastError(f"object's key ${kvs(n-1)} not followed with ':'", i)
      i += 1
      while (
        { if (i < N) true else return JastError("end of input after object key -- no value", index) } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      parseVal(input, i, end) match {
        case js: Json => kvs(n) = js
        case je: JastError => return JastError("error reading value "+(n/2+1)+" (key " + kvs(n-1) + ") in object", i, je)
      }
      i = idx
      while (
        { if (i < N) true else return JastError("end of input after object key -- no value", index) } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != '}' && c != ',') return JastError("unexpected character '" + c + "' in object after entry " + (n/2+1) + "(key " + kvs(n-1) + ")", i)
      i += 1
      n += 1
    }
    idx = i
    Json.Obj.fromFlatArray(if (kvs.length == n) kvs else java.util.Arrays.copyOf(kvs, n))
  }

  private[jsonal] def setEndpoint(ep: FromJson.Endpoint) { ep.index = idx }
}

object JsonStringParser{
  import JsonGenericParser._

  def Json(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint, relaxed: Boolean = false): Jast.To[kse.jsonal.Json] =
    (new JsonStringParser).relaxedNumbers(relaxed).parseVal(input, math.max(0, i0), math.min(iN, input.length)) match {
      case js: kse.jsonal.Json => Yes(js)
      case je: JastError => No(je)
      case _ => No(JastError("Internal error: parse did not produce JSON or an error?"))
    }
  def Null(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Jast.To[kse.jsonal.Json.Null] = {
    val i = math.max(0, i0)
    val iM = math.min(iN, input.length)
    if (i >= iM) return No(JastError("Expected JSON null but at end of input"))
    if (input.charAt(i) != 'n') return No(JastError("Expected JSON null but found character "+input.charAt(i).toString, i))
    val jsp = new JsonStringParser
    jsp.parseNull(input, i+1, iM) match {
      case jn: kse.jsonal.Json.Null => if (ep ne null) jsp.setEndpoint(ep); myRightNull
      case je: JastError => No(je)
      case _ => No(JastError("Internal error: parse did not produce JSON null or an error?"))
    }
  }
  def Bool(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Jast.To[kse.jsonal.Json.Bool] = {
    val jsp = new JsonStringParser
    jsp.parseBool(input, math.max(0, i0), math.min(iN, input.length)) match {
      case jb: kse.jsonal.Json.Bool =>
        if (ep ne null) jsp.setEndpoint(ep)
        if (jb.value) myRightTrue else myRightFalse
      case je: JastError => No(je)
      case _ => No(JastError("Internal error: parse did not produce JSON boolean or an error?"))
    }
  }
  def Str(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Jast.To[kse.jsonal.Json.Str] = {
    val i = math.max(0, i0)
    val iM = math.min(iN, input.length)
    if (i >= iM) return No(JastError("Expected JSON string but at end of input"))
    if (input.charAt(i) != '"') return No(JastError("Expected JSON string but found character "+input.charAt(i).toString, i))
    val jsp = new JsonStringParser
    jsp.parseStr(input, i+1, iM) match {
      case js: kse.jsonal.Json.Str => if (ep ne null) jsp.setEndpoint(ep); Yes(js)
      case je: JastError => No(je)
      case _ => No(JastError("Internal error: parse did not produce JSON string or an error?"))
    }
  }
  def Num(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint, relaxed: Boolean = false): Jast.To[kse.jsonal.Json.Num] = {
    val i = math.max(0, i0)
    val iM = math.min(iN, input.length)
    if (i >= iM) return No(JastError("Expected JSON number but at end of input"))
    val c = input.charAt(i)
    if (c != '-' && (c < '0' || c > '9')) return No(JastError("Expected JSON number but found character "+input.charAt(i).toString, i))
    val jsp = (new JsonStringParser).relaxedNumbers(relaxed)
    jsp.parseJastNum(input, i, iM, c == '-') match {
      case jn: kse.jsonal.Json.Num => if (ep ne null) jsp.setEndpoint(ep); Yes(jn)
      case je: JastError => No(je)
      case _ => No(JastError("Internal error: parse did not produce JSON number or an error?"))
    }
  }
  def Arr(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint, relaxed: Boolean = false): Jast.To[kse.jsonal.Json.Arr] = {
    val i = math.max(0, i0)
    val iM = math.min(iN, input.length)
    if (i >= iM) return No(JastError("Expected JSON array but at end of input"))
    if (input.charAt(i) != '[') return No(JastError("Expected JSON array but found character "+input.charAt(i).toString, i))
    val jsp = (new JsonStringParser).relaxedNumbers(relaxed)
    jsp.parseArr(input, i+1, iM) match {
      case ja: kse.jsonal.Json.Arr => if (ep ne null) jsp.setEndpoint(ep); Yes(ja)
      case je: JastError => No(je)
      case _ => No(JastError("Internal error: parse did not produce JSON array or an error?"))
    }
  }
  def Obj(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint, relaxed: Boolean = false): Jast.To[kse.jsonal.Json.Obj] = {
    val i = math.max(0, i0)
    val iM = math.min(iN, input.length)
    if (i >= iM) return No(JastError("Expected JSON object but at end of input"))
    if (input.charAt(i) != '{') return No(JastError("Expected JSON object but found character "+input.charAt(i).toString, i))
    val jsp = (new JsonStringParser).relaxedNumbers(relaxed)
    jsp.parseObj(input, i+1, math.min(iN, input.length)) match {
      case jo: kse.jsonal.Json.Obj => if (ep ne null) jsp.setEndpoint(ep); Yes(jo)
      case je: JastError => No(je)
      case _ => No(JastError("Internal error: parse did not produce JSON object or an error?"))
    }
  }
}
