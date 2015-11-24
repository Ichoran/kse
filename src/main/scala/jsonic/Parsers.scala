// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

package kse.jsonic.parsers

import kse.jsonic.ast._

trait JsonParser[A] { def parse(input: A): JsResult }
object JsonParser {
  private[parsers] val smallPowersOfTen = Array.tabulate(30)(i => s"1e$i".toDouble)

  def parse(s: String): JsResult = StringParser parse s
  def parse(b: Array[Byte]): JsResult = BytesParser parse b

  def parseWithDoubleArrays(s: String): JsResult = StringParser parseWithDoubleArrays s
  def parseWithDoubleArrays(b: Array[Byte]): JsResult = BytesParser parseWithDoubleArrays b
}

class StringParser extends JsonParser[String] {
  import JsonParser.smallPowersOfTen

  private[this] var idx = 0
  private[this] var cache: JsResult = null
  private[this] var myParseDoubleArrays = false

  def parseDoubleArrays(value: Boolean): this.type = { myParseDoubleArrays = value; this }

  def parse(input: String): JsResult = parseVal(input, 0)

  /////////////
  // Important invariants within methods:
  //    Upon error return, idx is unchanged
  //    Upon valid return, idx points after the last parsed character
  //    c holds the current character
  //    i points to the next character after c
  //    If a unique character is already parsed, the method that parses the rest of it gets an index past that character 
  /////////////

  private def parseVal(input: String, index: Int): JsResult = {
    var i = index
    var c: Char = 0
    while (
      { if (i < input.length) true else return JsError("end of input, no value found", index, i, None) } && 
      { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    i += 1
    if (c == '"') parseStr(input, i)
    else if (c == '[') parseArr(input, i)
    else if (c == '{') parseObj(input, i)
    else if (c == '-') {
      parseNum(input, i-1, true)
      val ans = cache
      cache = null
      ans
    }
    else if (c >= '0' && c <= '9') {
      parseNum(input, i-1, false)
      val ans = cache
      cache = null
      ans
    }
    else if (c == 'n') parseNull(input, i)
    else if (c == 't') parseTrue(input, i)
    else if (c == 'f') parseFalse(input, i)
    else JsError("invalid character: '" + c + "'", index, i, None)
  }

  private def parseNull(input: String, index: Int): JsResult = {
    if (index+3 <= input.length && input.charAt(index) == 'u' && input.charAt(index+1) == 'l' && input.charAt(index+2) == 'l') { idx = index+3; JsNull }
    else JsError("Expected 'null' but found '"+input.substring(index-1, index+3), index-1, index+3, None)
  }

  private def parseTrue(input: String, index: Int): JsResult = {
    if (index+3 <= input.length && input.charAt(index) == 'r' && input.charAt(index+1) == 'u' && input.charAt(index+2) == 'e') { idx = index+3; JsTrue }
    else JsError("Expected 'true' but found "+input.substring(index-1, index+3), index-1, index+3, None)
  }

  private def parseFalse(input: String, index: Int): JsResult = {
    if (index+4 <= input.length && input.charAt(index) == 'a' && input.charAt(index+1) == 'l' && input.charAt(index+2) == 's' && input.charAt(index+3) == 'e') {
      idx = index+4
      JsTrue
    }
    else JsError("Expected 'false' but found "+input.substring(index-1, index+4), index-1, index+3, None)
  }

  private def parseStr(input: String, index: Int): JsResult = {
    val i = parseSimpleStr(input, index)
    if (i >= 0) { idx = i; JsStr(input.substring(index, i-1)) }
    else parseComplexStr(input, index, -i-1)
  }
  
  private def parseSimpleStr(input: String, index: Int): Int = {
    var i = index
    var c: Char = 0
    while (i < input.length && { c = input.charAt(i); c != '"' && c != '\\' }) i += 1
    if (c == '"') i+1 else -i-1
  }

  private def hexifyChar(c: Char): Int = hexifyLowerChar(c | 0x20)

  private def hexifyLowerChar(c: Int): Int =
    if (c >= '0' && c <= '9') c - '0' else if (c >= 'a' && c <= 'f') c - 87 else -1

  private def parseComplexStr(input: String, index: Int, cleanUntil: Int): JsResult = {
    val N = input.length
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
          if (n < 0) return JsError("string too long", index, N, None)
        }
        if (resize) buffer = java.util.Arrays.copyOf(buffer, n)
        input.getChars(i0, iN, buffer, j)
        j += (iN - i0)
        i0 = iN
      }
      if (c == '"') {
        idx = i0+1
        return JsStr(new String(buffer, 0, j))
      }
      while (c == '\\') {
        i0 += 1
        if (i0 < N) {
          val q = input.charAt(i0) match {
            case 'n' => '\n'
            case 'r' => '\r'
            case 't' => '\t'
            case 'u' =>
              if (i0 >= N - 4) return JsError("string ends mid-unicode-escape", index, N, None)
              val h = (hexifyChar(input.charAt(i0+1)) << 12) | 
                      (hexifyChar(input.charAt(i0+2)) << 8) | 
                      (hexifyChar(input.charAt(i0+3)) << 4) | 
                      hexifyChar(input.charAt(i0+4))
              if (h < 0) return JsError("non-hex value in unicode escape", index, i0+1, None)
              i0 += 4
              h.toChar
            case 'f' => '\f'
            case 'b' => '\b'
            case x => 
              if (x == '"' || x == '/' || x == '\\') x
              else return JsError("invalid quoted character '" + x + "'", index, i0, None)
          }
          i0 += 1
          if (i0 < N) c = input.charAt(i0)
          if (j >= n) {
            n = (n << 1) | 0x2
            if (n < 0) return JsError("string too long", index, N, None)
            buffer = java.util.Arrays.copyOf(buffer, n)
          }
          buffer(j) = q
          j += 1
        }
        else return JsError("string ends mid-escape", index, i0, None)
        if (i0 < N) c = input.charAt(i0)
        else return JsError("no closing quote on string", index, i0, None)
      }
      if (c == '"') {
        idx = i0+1
        return JsStr(new String(buffer, 0, j))
      }
      if (i0 < N) {
        iN = parseSimpleStr(input, i0+1)
        if (iN < 0) iN = -iN-1
        else iN -= 1
        if (iN < N) c = input.charAt(iN)
      }
      else i0 = N
    } while (i0 < N)
    JsError("no closing quote on string", index, iN, None)
  }

  private def parseNum(input: String, index: Int, negative: Boolean, toCache: Boolean = true): Double = {
    var i = if (negative) index+1 else index
    if (i >= input.length) { cache = JsError("unfinished number", index, i, None); return Double.NaN }
    var dadp = 0  // How many of our digits are after the decimal point?
    var dbdp = 0  // How many are before the decimal point?
    var digits = 0L
    var c = input.charAt(i)
    val N = input.length
    if (c > '0' && c <= '9') {
      digits = c - '0'
      val M = math.min(i+15, N)
      i += 1
      while (i < M && { c = input.charAt(i); c >= '0' && c <= '9' }) { i += 1; digits = digits*10 + (c - '0') }
      if (i == M) while (i < N && { c = input.charAt(i); c >= '0' && c <= '9'}) i += 1
      dbdp = i - index
    }
    else if (c == '0') {
      i += 1
      if (i < N && { c = input.charAt(i); c >= '0' && c <= '9'}) { cache = JsError("multi-digit number cannot start with 0", index, i, None); return Double.NaN }
    }
    else { cache = JsError("number should start with a numeric digit", index, i, None); return Double.NaN }
    if (c == '.') {
      val dp = i
      i += 1
      val M = math.min(if (dbdp > 15) i else i + (15 - dbdp), N)
      while (i < M && { c = input.charAt(i); c >= '0' && c <= '9' }) { i += 1; digits = digits*10 + (c - '0') }
      if (i >= M) while (i < N && { c = input.charAt(i); c >= '0' && c <= '9' }) i += 1
      dadp = (i - dp) - 1
      if (dadp == 0) { cache = JsError("need digits after . in number", index, i, None); return Double.NaN }
    }
    val ex =
      if (i >= N || (c | 0x20) != 'e') 0
      else {
        i += 1
        if (i >= N) { cache = JsError("need digits after e in number", index, i, None); return Double.NaN }
        c = input.charAt(i)
        val negex = c match {
          case '-' =>
            i += 1
            if (i >= N) { cache = JsError("need digits after - in number exponent", index, i, None); return Double.NaN }
            c = input.charAt(i)
            true
          case '+' =>
            i += 1
            if (i >= N) { cache = JsError("need digits after + in number exponent", index, i, None); return Double.NaN }
            c = input.charAt(i)
            false
          case _ => false
        }
        var x = (c - '0')
        if (x < 0 || x >= 10) { cache = JsError("exponent in number must be numeric digits", index, i, None); return Double.NaN }
        i += 1
        while (i < N && x < 99 && { c = input.charAt(i); c >= '0' && c <= '9' }) { x = x*10 + (c - '0'); i += 1 }
        if (x > 99) {
          while (i < N && { c = input.charAt(i); c >= '0' && c <= '9' }) i += 1
          val str = input.substring(index, i)
          val dbl = str.toDouble
          if (toCache) cache = JsNum(dbl, str)
          idx = i
          return dbl
        }
        if (negex) -x else x
      }
    idx = i
    if (dadp + dbdp <= 15 && dbdp + ex <= 17 && dadp - ex <= 17) {
      val shift = ex - dadp
      val dbl =
        if (shift == 0) digits.toDouble
        else if (shift > 0) digits * smallPowersOfTen(shift)
        else digits / smallPowersOfTen(-shift)
      val sdbl = if (negative) -dbl else dbl
      if (toCache) cache = JsNum(sdbl, input.substring(index, i))
      sdbl
    }
    else {
      val str = input.substring(index, i)
      val dbl = str.toDouble
      if (toCache) cache = JsNum(dbl, str)
      dbl
    }
  }

  private def parseArrD(input: String, index: Int, c0: Char): Boolean = {
    val N = input.length
    var i = index
    var c = c0
    var buffer = new Array[Double](6)
    var n = 0
    while (c != ']') {
      val ans = 
        if (c == '-') parseNum(input, i, true, false)
        else if (c >= '0' && c <= '9') parseNum(input, i, false, false)
        else return false
      i = idx
      if (ans.isNaN && (cache ne null) && cache.isInstanceOf[JsError]) return false
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
    cache = JsArrD(if (buffer.length != n) java.util.Arrays.copyOf(buffer, n) else buffer)
    true
  }

  private def parseArr(input: String, index: Int): JsResult = {
    val N = input.length
    var i = index
    var c = (0: Char)
    while (
      { if (i < N) true else return JsError("end of input with unclosed array", index, i, None) } && 
      { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    if (c == ']') { idx = i+1; return JsArr.empty }
    if (myParseDoubleArrays) {
      val idx0 = idx
      if (parseArrD(input, i, c)) {
        val ans = cache
        cache = null
        return ans
      }
      else {
        cache = null
        idx = idx0
      }
    }
    val contents = Array.newBuilder[JsVal]
    var n = 0
    while (c != ']') {
      n += 1
      parseVal(input, i) match {
        case jv: JsVal => contents += jv
        case je: JsError => return JsError("error in array element "+n, index, i, Some(je))
      }
      i = idx
      while (
        { if (i < N) true else return JsError("end of input with unclosed array", index, i, None) } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != ']' && c != ',') return JsError("unexpected character '" + c + "' in array after index "+n, index, i, None)
      i += 1
    }
    idx = i
    JsArrV(contents.result())
  }

  private def parseObj(input: String, index: Int): JsResult = {
    val N = input.length
    var i = index
    var c = (0: Char)
    while (
      { if (i < N) true else return JsError("end of input with unclosed object", index, i, None) } && 
      { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    if (c == '}') { idx = i; return JsObj.empty }
    var keys = new Array[String](6)
    var values = new Array[JsVal](6)
    var n = 0
    while (c != '}') {
      if (n >= keys.length) {
        keys = java.util.Arrays.copyOf(keys, 0x7FFFFFFE & ((keys.length << 1) | 0x2))
        values = java.util.Arrays.copyOf(values, keys.length)
      }
      parseVal(input, i) match {
        case js: JsStr => keys(n) = js.value
        case je: JsError => return JsError("error reading key "+(n+1)+" in object", index, i, Some(je))
        case _ => return JsError("object keys must be strings", index, i, None)
      }
      i = idx
      while (
        { if (i < N) true else return JsError("end of input after object key but no value", index, i, None) } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != ':') return JsError("object key not followed with ':'", index, i, None)
      i += 1
      parseVal(input, i) match {
        case jv: JsVal => values(n) = jv
        case je: JsError => return JsError("error reading value "+(n+1)+" (key " + keys(n) + ") in object", index, i, Some(je))
      }
      i = idx
      while (
        { if (i < N) true else return JsError("end of input after object key but no value", index, i, None) } && 
        { c = input.charAt(i); c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != '}' && c != ',') return JsError("unexpected character '" + c + "' in object after entry " + (n+1) + "(key " + keys(n) + ")", index, i, None)
      i += 1
      n += 1
    }
    idx = i
    val m = collection.mutable.AnyRefMap.empty[String, JsVal]
    var k = 0
    while (k < n) { m += (keys(k), values(k)); k +=1 }
    if (n == keys.length) JsObj(keys, values, m)
    else JsObj(java.util.Arrays.copyOf(keys, n), java.util.Arrays.copyOf(values, n), m)
  }
}
object StringParser{
  def parse(s: String): JsResult = (new StringParser).parse(s)
  def parseWithDoubleArrays(s: String): JsResult = (new StringParser).parseDoubleArrays(true).parse(s)
}

class BytesParser extends JsonParser[Array[Byte]] {
  import JsonParser.smallPowersOfTen

  private[this] var idx = 0
  private[this] var cache: JsResult = null
  private[this] var myParseDoubleArrays = false

  def parseDoubleArrays(value: Boolean): this.type = { myParseDoubleArrays = value; this }

  def parse(input: Array[Byte]): JsResult = parseVal(input, 0, input.length)

  def parse(input: Array[Byte], start: Int, end: Int) = {
    val i0 = math.max(0, start)
    val iN = math.min(input.length, math.max(i0, end))
    parseVal(input, i0, iN)
  }

  /////////////
  // Important invariants within methods:
  //    Upon error return, idx is unchanged
  //    Upon valid return, idx points after the last parsed character
  //    c holds the current character
  //    i points to the next character after c
  //    If a unique character is already parsed, the method that parses the rest of it gets an index past that character 
  /////////////

  private def parseVal(input: Array[Byte], index: Int, limit: Int): JsResult = {
    var i = index
    var c = 0
    while (
      { if (i < limit) true else return JsError("end of input, no value found", index, i, None) } && 
      { c = input(i) & 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    i += 1
    if (c == '"') parseStr(input, i, limit)
    else if (c == '[') parseArr(input, i, limit)
    else if (c == '{') parseObj(input, i, limit)
    else if (c == '-') {
      parseNum(input, i-1, limit, true)
      val ans = cache
      cache = null
      ans
    }
    else if (c >= '0' && c <= '9') {
      parseNum(input, i-1, limit, false)
      val ans = cache
      cache = null
      ans
    }
    else if (c == 'n') parseNull(input, i, limit)
    else if (c == 't') parseTrue(input, i, limit)
    else if (c == 'f') parseFalse(input, i, limit)
    else JsError("invalid character: '" + c + "'", index, i, None)
  }

  private def parseNull(input: Array[Byte], index: Int, limit: Int): JsResult = {
    if (index+3 <= limit && input(index) == 'u' && input(index+1) == 'l' && input(index+2) == 'l') { idx = index+3; JsNull }
    else JsError("Expected 'null' but found '"+(new String (input, index-1, 4)), index-1, index+3, None)
  }

  private def parseTrue(input: Array[Byte], index: Int, limit: Int): JsResult = {
    if (index+3 <= limit && input(index) == 'r' && input(index+1) == 'u' && input(index+2) == 'e') { idx = index+3; JsTrue }
    else JsError("Expected 'true' but found "+(new String (input, index-1, 4)), index-1, index+3, None)
  }

  private def parseFalse(input: Array[Byte], index: Int, limit: Int): JsResult = {
    if (index+4 <= limit && input(index) == 'a' && input(index+1) == 'l' && input(index+2) == 's' && input(index+3) == 'e') {
      idx = index+4
      JsTrue
    }
    else JsError("Expected 'false' but found "+(new String (input, index-1, 5)), index-1, index+3, None)
  }

  private def parseStr(input: Array[Byte], index: Int, limit: Int): JsResult = {
    val i = parseSimpleStr(input, index, limit)
    if (i >= 0) { idx = i; JsStr(new String(input, index, i-index-1)) }
    else parseComplexStr(input, index, limit, -i-1)
  }
  
  private def parseSimpleStr(input: Array[Byte], index: Int, limit: Int): Int = {
    var i = index
    var c: Byte = 0
    while (i < limit && { c = input(i); c >= 0 && c != '"' && c != '\\' }) i += 1
    if (c == '"') i+1 else -i-1
  }

  private def hexifyChar(c: Int): Int = hexifyLowerChar(c | 0x20)

  private def hexifyLowerChar(c: Int): Int =
    if (c >= '0' && c <= '9') c - '0' else if (c >= 'a' && c <= 'f') c - 87 else -1

  // Lower 24 bits are for code point, upper 8 are for number of characters consumed
  private def utf8CodePoint(input: Array[Byte], index: Int, limit: Int, c: Int, cc: Int): Int = {
    if ((c & 0xC0) == 0x80) 0xFFFD
    if ((c & 0xE0) == 0xC0) (((c & 0x1F) << 6) | (cc & 0x3F)).toChar match {
      case x if x < 0x40 => if (x != 0) 0xFFFD else x | 0x1000000
      case x => x | 0x1000000
    }
    else if ((c & 0xF0) == 0xE0) {
      if (index + 1 < limit) {
        val ccc = input(index+1)
        (((c & 0xF) << 12) | ((cc & 0x3F) << 6) | (ccc & 0x3F)).toChar match {
          case x if x < 0x800 => 0xFFFD
          case x => x | 0x2000000
        }
      }
      else -1
    }
    else if ((c & 0xF8) == 0xF0) {
      if (index+2 < limit) {
        val ccc = input(index+1)
        val cccc = input(index+2)
        val cp = ((c & 0xF) << 18) | ((cc & 0x3F) << 12) | ((ccc & 0x3F) << 6) | (cccc & 0x3F)
        if (cp < 0x10000 | cp > 0x10FFFF) 0xFFFD
        else cp | 0x3000000
      }
      else -1
    }
    else 0xFFFD
  }

  private def parseComplexStr(input: Array[Byte], index: Int, limit: Int, cleanUntil: Int): JsResult = {
    var n = (0xFFFFFFFF >>> math.min(28, java.lang.Integer.numberOfLeadingZeros(cleanUntil - index))) & 0x7FFFFFFE
    var j = 0
    var buffer = new Array[Char](n)
    var i0 = index
    var iN = cleanUntil
    var c: Int = '\\'
    do {
      if (i0 < iN) {
        var resize = false
        while (iN - i0 > n - j) {
          n = (n << 1) | 0x2
          resize = true
          if (n < 0) return JsError("string too long", index, limit, None)
        }
        if (resize) buffer = java.util.Arrays.copyOf(buffer, n)
        while (i0 < iN) { buffer(j) = (input(i0) & 0xFF).toChar; j +=1; i0 += 1 }
      }
      if (c == '"') {
        idx = i0+1
        return JsStr(new String(buffer, 0, j))
      }
      while (c == '\\' || (c & 0x80) != 0) {
        i0 += 1
        if (i0 < limit) {
          val cc = input(i0)
          val q: Char = (
            if (c == '\\') cc match {
              case 'n' => '\n'
              case 'r' => '\r'
              case 't' => '\t'
              case 'u' =>
                if (i0 >= limit - 4) return JsError("string ends mid-unicode-escape", index, limit, None)
                val h = (hexifyChar(input(i0+1)) << 12) | 
                        (hexifyChar(input(i0+2)) << 8) | 
                        (hexifyChar(input(i0+3)) << 4) | 
                        hexifyChar(input(i0+4))
                if (h < 0) return JsError("non-hex value in unicode escape", index, i0+1, None)
                i0 += 4
                h.toChar
              case 'f' => '\f'
              case 'b' => '\b'
              case x => 
                if (x == '"' || x == '/' || x == '\\') x.toChar
                else return JsError("invalid quoted character '" + x + "'", index, i0, None)
            }
            else utf8CodePoint(input, i0, limit, c, cc) match {
              case x if x >= 0 && (x & 0xFFFFFF) <= 0x10000 =>
                i0 += (x >>> 24) - 1
                (x & 0xFFFF).toChar
              case x if x < 0 => return JsError("string ends in the middle of a UTF8 code point", index, limit, None)
              case x => 
                i0 += (x >>> 24) - 1
                val cp = x & 0xFFFFFF
                val lo = (((cp - 0x10000) & 0x3FF) + 0xDC00).toChar
                val hi = (((cp - 0x10000) >> 10) + 0xD800).toChar
                buffer(j) = hi
                j += 1
                lo
            }
          ) // end of val q
          i0 += 1
          if (i0 < limit) c = input(i0)
          if (j >= n) {
            n = (n << 1) | 0x2
            if (n < 0) return JsError("string too long", index, limit, None)
            buffer = java.util.Arrays.copyOf(buffer, n)
          }
          buffer(j) = q
          j += 1
        }
        else return JsError("string ends mid-escape", index, i0, None)
        if (i0 < limit) c = input(i0)
        else return JsError("no closing quote on string", index, i0, None)
      }
      if (c == '"') {
        idx = i0+1
        return JsStr(new String(buffer, 0, j))
      }
      if (i0 < limit) {
        iN = parseSimpleStr(input, i0+1, limit)
        if (iN < 0) iN = -iN-1
        else iN -= 1
        if (iN < limit) c = input(iN)
      }
      else i0 = limit
    } while (i0 < limit)
    JsError("no closing quote on string", index, iN, None)
  }

  private def parseNum(input: Array[Byte], index: Int, limit: Int, negative: Boolean, toCache: Boolean = true): Double = {
    var i = if (negative) index+1 else index
    if (i >= limit) { cache = JsError("unfinished number", index, i, None); return Double.NaN }
    var dadp = 0  // How many of our digits are after the decimal point?
    var dbdp = 0  // How many are before the decimal point?
    var digits = 0L
    var c = input(i)
    if (c > '0' && c <= '9') {
      digits = c - '0'
      val M = math.min(i+15, limit)
      i += 1
      while (i < M && { c = input(i); c >= '0' && c <= '9' }) { i += 1; digits = digits*10 + (c - '0') }
      if (i == M) while (i < limit && { c = input(i); c >= '0' && c <= '9'}) i += 1
      dbdp = i - index
    }
    else if (c == '0') {
      i += 1
      if (i < limit && { c = input(i); c >= '0' && c <= '9'}) { cache = JsError("multi-digit number cannot start with 0", index, i, None); return Double.NaN }
    }
    else { cache = JsError("number should start with a numeric digit", index, i, None); return Double.NaN }
    if (c == '.') {
      val dp = i
      i += 1
      val M = math.min(if (dbdp > 15) i else i + (15 - dbdp), limit)
      while (i < M && { c = input(i); c >= '0' && c <= '9' }) { i += 1; digits = digits*10 + (c - '0') }
      if (i >= M) while (i < limit && { c = input(i); c >= '0' && c <= '9' }) i += 1
      dadp = (i - dp) - 1
      if (dadp == 0) { cache = JsError("need digits after . in number", index, i, None); return Double.NaN }
    }
    val ex =
      if (i >= limit || (c | 0x20) != 'e') 0
      else {
        i += 1
        if (i >= limit) { cache = JsError("need digits after e in number", index, i, None); return Double.NaN }
        c = input(i)
        val negex = c match {
          case '-' =>
            i += 1
            if (i >= limit) { cache = JsError("need digits after - in number exponent", index, i, None); return Double.NaN }
            c = input(i)
            true
          case '+' =>
            i += 1
            if (i >= limit) { cache = JsError("need digits after + in number exponent", index, i, None); return Double.NaN }
            c = input(i)
            false
          case _ => false
        }
        var x = (c - '0')
        if (x < 0 || x >= 10) { cache = JsError("exponent in number must be numeric digits", index, i, None); return Double.NaN }
        i += 1
        while (i < limit && x < 99 && { c = input(i); c >= '0' && c <= '9' }) { x = x*10 + (c - '0'); i += 1 }
        if (x > 99) {
          while (i < limit && { c = input(i); c >= '0' && c <= '9' }) i += 1
          val str = new String(input, index, i - index)
          val dbl = str.toDouble
          if (toCache) cache = JsNum(dbl, str)
          idx = i
          return dbl
        }
        if (negex) -x else x
      }
    idx = i
    if (dadp + dbdp <= 15 && dbdp + ex <= 17 && dadp - ex <= 17) {
      val shift = ex - dadp
      val dbl =
        if (shift == 0) digits.toDouble
        else if (shift > 0) digits * smallPowersOfTen(shift)
        else digits / smallPowersOfTen(-shift)
      val sdbl = if (negative) -dbl else dbl
      if (toCache) cache = JsNum(sdbl, new String(input, index, i-index))
      sdbl
    }
    else {
      val str = new String(input, index, i-index)
      val dbl = str.toDouble
      if (toCache) cache = JsNum(dbl, str)
      dbl
    }
  }

  private def parseArrD(input: Array[Byte], index: Int, limit: Int, c0: Int): Boolean = {
    var i = index
    var c = c0
    var buffer = new Array[Double](6)
    var n = 0
    while (c != ']') {
      val ans = 
        if (c == '-') parseNum(input, i, limit, true, false)
        else if (c >= '0' && c <= '9') parseNum(input, i, limit, false, false)
        else return false
      i = idx
      if (ans.isNaN && (cache ne null) && cache.isInstanceOf[JsError]) return false
      if (n >= buffer.length) buffer = java.util.Arrays.copyOf(buffer, 0x7FFFFFFE & ((buffer.length << 1) | 0x2))
      buffer(n) = ans
      n += 1
      while (
        { if (i < limit) true else return false } && 
        { c = input(i) & 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c == ',') {
        i += 1
        while (
          { if (i < limit) true else return false } && 
          { c = input(i)& 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
        ) i += 1
        if (c == ']') return false
      }
    }
    idx = i+1
    cache = JsArrD(if (buffer.length != n) java.util.Arrays.copyOf(buffer, n) else buffer)
    true
  }

  private def parseArr(input: Array[Byte], index: Int, limit: Int): JsResult = {
    var i = index
    var c = 0
    while (
      { if (i < limit) true else return JsError("end of input with unclosed array", index, i, None) } && 
      { c = input(i) & 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    if (c == ']') { idx = i+1; return JsArr.empty }
    if (myParseDoubleArrays) {
      val idx0 = idx
      if (parseArrD(input, i, limit, c)) {
        val ans = cache
        cache = null
        return ans
      }
      else {
        cache = null
        idx = idx0
      }
    }
    val contents = Array.newBuilder[JsVal]
    var n = 0
    while (c != ']') {
      n += 1
      parseVal(input, i, limit) match {
        case jv: JsVal => contents += jv
        case je: JsError => return JsError("error in array element "+n, index, i, Some(je))
      }
      i = idx
      while (
        { if (i < limit) true else return JsError("end of input with unclosed array", index, i, None) } && 
        { c = input(i) & 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != ']' && c != ',') return JsError("unexpected character '" + c + "' in array after index "+n, index, i, None)
      i += 1
    }
    idx = i
    JsArrV(contents.result())
  }

  private def parseObj(input: Array[Byte], index: Int, limit: Int): JsResult = {
    val N = input.length
    var i = index
    var c = 0
    while (
      { if (i < N) true else return JsError("end of input with unclosed object", index, i, None) } && 
      { c = input(i) & 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
    ) i += 1
    if (c == '}') { idx = i; return JsObj.empty }
    var keys = new Array[String](6)
    var values = new Array[JsVal](6)
    var n = 0
    while (c != '}') {
      if (n >= keys.length) {
        val L = 0x7FFFFFFE & ((keys.length << 1) | 0x2)
        if (L < 0) return JsError("object has too many entries", index, i, None)
        keys = java.util.Arrays.copyOf(keys, L)
        values = java.util.Arrays.copyOf(values, L)
      }
      parseVal(input, i, limit) match {
        case js: JsStr => keys(n) = js.value
        case je: JsError => return JsError("error reading key "+(n+1)+" in object", index, i, Some(je))
        case _ => return JsError("object keys must be strings", index, i, None)
      }
      i = idx
      while (
        { if (i < limit) true else return JsError("end of input after object key but no value", index, i, None) } && 
        { c = input(i) & 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != ':') return JsError("object key not followed with ':'", index, i, None)
      i += 1
      parseVal(input, i, limit) match {
        case jv: JsVal => values(n) = jv
        case je: JsError => return JsError("error reading value "+(n+1)+" (key " + keys(n) + ") in object", index, i, Some(je))
      }
      i = idx
      while (
        { if (i < limit) true else return JsError("end of input after object key but no value", index, i, None) } && 
        { c = input(i) & 0xFF; c < 0x21 && (c == ' ' || c == '\n' || c == '\r' || c == '\t')}
      ) i += 1
      if (c != '}' && c != ',') return JsError("unexpected character '" + c + "' in object after entry " + (n+1) + "(key " + keys(n) + ")", index, i, None)
      i += 1
      n += 1
    }
    idx = i
    val m = collection.mutable.AnyRefMap.empty[String, JsVal]
    var k = 0
    while (k < n) { m += (keys(k), values(k)); k +=1 }
    if (n == keys.length) JsObj(keys, values, m)
    else JsObj(java.util.Arrays.copyOf(keys, n), java.util.Arrays.copyOf(values, n), m)
  }
}
object BytesParser {
  def parse(b: Array[Byte]): JsResult = (new BytesParser).parse(b)
  def parse(b: Array[Byte], start: Int, end: Int): JsResult = (new BytesParser).parse(b, start, end)
  def parseWithDoubleArrays(b: Array[Byte]): JsResult = (new BytesParser).parseDoubleArrays(true).parse(b)
  def parseWithDoubleArrays(b: Array[Byte], start: Int, end: Int): JsResult = (new BytesParser).parseDoubleArrays(true).parse(b, start, end)
}

trait JsonWisdom[A] {
  def in(i: Int, p: JsonParser[A]): Unit
  def in(key: String, p: JsonParser[A]): Unit
  def out(i: Int, p: JsonParser[A]): Unit
  def out(key: String, p: JsonParser[A]): Unit
  def edit(jr: JsResult, p: JsonParser[A]): JsResult
  def fail(je: JsError, p: JsonParser[A]): JsError
}

trait JsonChunker[@specialized(Byte, Char, Int) A] {
  def goto(delta: Long): Long
  def available: Long
  def read(buffer: Array[A], start: Int, end: Int): Int
}

class ByteChunkParser(val wisdom: JsonWisdom[JsonChunker[Byte]]) extends JsonParser[JsonChunker[Byte]] {
  def parse(chunker: JsonChunker[Byte]): JsResult = ???
}

class CharChunkParser(val wisdom: JsonWisdom[JsonChunker[Char]]) extends JsonParser[JsonChunker[Char]] {
  def parse(chunker: JsonChunker[Char]): JsResult = ???
}
