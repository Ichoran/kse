// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

/** The Jsonal AST is an opinionated Scala representation of a JSON file.
  * 
  * It is minimal: data structures are as close as possible to the underlying JSON specification.
  *
  * It is compact: composite data structures are all stored in arrays.
  *
  * It is powerful: objects have a custom map implementation that enables rapid lookup in the underlying array.
  *
  * It loves numbers: arrays of numbers end up as arrays of numbers, not a clunky boxed mess!
  *
  * It is easy to build: just say the name of what you want to build, e.g. `Js.Obj`,
  * add in the pieces with `~`, and end with the same name (`Js.Obj`).  Or, use `~~` to add
  * whole collections.  Want to build from your own types?  No problem--just extend `AsJson` or
  * provide the `Jsonable` typeclass.
  *
  * It cleans up its own messes: the error data type is part of the AST.
  *
  * You can take it out in public: it will serialize itself to String or Byte representations, and it
  * comes with default parsers to deserialize itself.
  *
  * It is NOT meant for repeated modification of a JSON AST; for that you want a lot of structural
  * sharing and/or exposed mutable data structures, neither of which this AST provides.
  */

package kse.jsonal

import java.nio._

trait AsJson {
  def json: Js
  def jsonString(sb: java.lang.StringBuilder) { json.jsonString(sb) }
  def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) { json.jsonBytes(bb, refresh) }
  def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) { json.jsonChars(cb, refresh) }
}

trait Jsonize[A] {
  def jsonize(a: A): Js
  def jsonizeString(a: A, sb: java.lang.StringBuilder) { jsonize(a).jsonString(sb) }
  def jsonizeBytes(a: A, bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) { jsonize(a).jsonBytes(bb, refresh) }
  def jsonizeChars(a: A, cb: CharBuffer, refresh: CharBuffer => CharBuffer) { jsonize(a).jsonChars(cb, refresh) }
}

trait FromJson[A] {
  def parse(input: Js): Either[JastError, A]

  def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Either[JastError, A] = Js.parse(input, i0, iN, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }
  def parse(input: String): Either[JastError, A] = parse(input, 0, input.length, new FromJson.Endpoint(0))

  def parse(input: ByteBuffer): Either[JastError, A] = Js.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  def parse(input: CharBuffer): Either[JastError, A] = Js.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Either[JastError, A] = Js.parse(input, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }
}
object FromJson {
  case class Endpoint(var index: Long) {}
}

sealed trait Jast {}

final case class JastError(msg: String, where: Long = -1L, because: Jast = Js.Null) extends Jast {}

sealed trait Js extends Jast with AsJson {
  def double = Double.NaN

  def json: Js = this
  override def toString = { val sb = new java.lang.StringBuilder; jsonString(sb); sb.toString }
}
object Js extends FromJson[Js] {
  private[jsonal] def loadByteBuffer(bytes: Array[Byte], bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) {
    var b = bb
    var i = 0
    while(true) {
      val n = math.max(bytes.length - i, bb.remaining) match { case 0 => bytes.length - i; case x => x }
      b.put(bytes, i, n)
      i += n
      if (i < bytes.length) b = refresh(b) else return
    }
  }
  private[jsonal] def loadCharBuffer(chars: Array[Char], cb: CharBuffer, refresh: CharBuffer => CharBuffer) {
    var c = cb
    var i = 0
    while(true) {
      val n = math.max(chars.length - i, cb.remaining) match { case 0 => chars.length - i; case x => x }
      c.put(chars, i, n)
      i += n
      if (i < chars.length) c = refresh(c) else return
    }
  }

  def parse(input: Js): Either[JastError, Js] = Right(input)
  override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = JsonStringParser.Js(input, i0, iN, ep)
  override def parse(input: ByteBuffer) = JsonByteBufferParser.Js(input)
  override def parse(input: CharBuffer) = JsonCharBufferParser.Js(input)
  override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = JsonInputStreamParser.Js(input, ep)

  sealed abstract class Null extends Js {}
  final object Null extends Null with FromJson[Null] {
    final private[this] val myBytesSayNull = "null".getBytes
    final private[this] val myCharsSayNull = "null".toCharArray
    override def jsonString(sb: java.lang.StringBuilder) { sb append "null" }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) {
      (if (bb.remaining < 4) refresh(bb) else bb) put myBytesSayNull
    }
    override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) {
      (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayNull
    }
    override def toString = "null"
    def parse(input: Js): Either[JastError, Null] = if (this eq input) Right(this) else Left(JastError("expected null"))
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = JsonStringParser.Null(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Null(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Null(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = JsonInputStreamParser.Null(input, ep)
  }

  sealed abstract class Bool extends Js { def value: Boolean }
  object Bool extends FromJson[Bool] { 
    final private val myBytesSayTrue = "true".getBytes
    final private val myBytesSayFalse = "false".getBytes
    final private val myCharsSayTrue = "true".toCharArray
    final private val myCharsSayFalse = "false".toCharArray
    def unapply(js: Js): Option[Boolean] = js match { case b: Bool => Some(b.value); case _ => None }
    case object True extends Bool { 
      def value = true
      override def jsonString(sb: java.lang.StringBuilder) { sb append "true" }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) {
        (if (bb.remaining < 4) refresh(bb) else bb) put myBytesSayTrue
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) {
        (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayTrue
      }
    }
    case object False extends Bool {
      def value = false
      override def jsonString(sb: java.lang.StringBuilder) { sb append "false" }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) {
        (if (bb.remaining < 5) refresh(bb) else bb) put myBytesSayFalse
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) {
        (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayFalse
      }
    }
    override def parse(input: Js): Either[JastError, Bool] = input match {
      case b: Bool => Right(b)
      case _       => Left(JastError("expected Js.Bool"))
    }
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = JsonStringParser.Bool(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Bool(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Bool(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = JsonInputStreamParser.Bool(input, ep)
  }

  final case class Str(text: String) extends Js {
    override def jsonString(sb: java.lang.StringBuilder) { JsonStringParser.encodeString(text, sb) }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) { 
      JsonByteBufferParser.encodeString(text, bb, refresh)
    }
    override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) {
      JsonCharBufferParser.encodeString(text, cb, refresh)
    }    
  }
  object Str extends FromJson[Str] {
    override def parse(input: Js): Either[JastError, Str] = input match {
      case s: Str => Right(s)
      case _      => Left(JastError("expected Js.Str"))
    }
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = JsonStringParser.Str(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Str(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Str(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = JsonInputStreamParser.Str(input, ep)    
  }

  sealed abstract class Num extends Js {
    def isDouble: Boolean
    def isLong: Boolean
    def long: Long
    def longOr(fallback: Long): Long
    def big: BigDecimal
  }
  object Num {
    private[jsonal] val numericFormatTable: Array[String] = ???

    final class Dbl(format: Int, content: Double) extends Num {
      def isDouble = true
      override def double = content
      def isLong = content.toLong == content
      def long = content.toLong
      def longOr(fallback: Long) = { val ans = content.toLong; if (ans == content) ans else fallback }
      def big = BigDecimal(toString)
      override def toString = numericFormatTable(format).format(content)
      override def jsonString(sb: java.lang.StringBuilder) { sb append toString }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) { 
        (if (bb.remaining < 30) refresh(bb) else bb) put toString.getBytes
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) {
        (if (cb.remaining < 30) refresh(cb) else cb) put toString.toCharArray
      }
    }
    final class LongNotDbl(content: Long) extends Num {
      def isDouble = false
      override def double = content.toDouble
      def isLong = true
      def long = content
      def longOr(fallback: Long) = content
      def big: BigDecimal = BigDecimal(content)
      override def toString = content.toString
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) { 
        (if (bb.remaining < 20) refresh(bb) else bb) put toString.getBytes
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) {
        (if (cb.remaining < 20) refresh(cb) else cb) put toString.toCharArray
      }
    }
    final class Big(text: String) extends Num {
      def isDouble = false
      override def double = text.toDouble
      def isLong = false
      def long = if (text startsWith "-") Long.MaxValue else Long.MinValue
      def longOr(fallback: Long) = fallback
      def big = BigDecimal(text)
      override def toString = text
      override def jsonString(sb: java.lang.StringBuilder) { sb append text }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer) { loadByteBuffer(text.getBytes, bb, refresh) }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer) { loadCharBuffer(text.toCharArray, cb, refresh) }
    }
  }

  sealed trait Arr extends Js { def size: Int; def apply(i: Int): Jast }
  object Arr {
    final class Jses(jses: Array[Js]) extends Arr {
      def size = jses.length
      def apply(i: Int) = jses(i)
    }
    final class Dbls(precision: Int, xs: Array[Double]) extends Arr {
      def size = xs.length
      def apply(i: Int) = ???
    }
  }
  final class Obj(kvs: Array[Js]) extends Js {
    private lazy val mapped = { 
      val m = new collection.mutable.AnyRefMap[String, Js]
      var i = 0
      while (i < kvs.length - 1) {
        m += (kvs(i).asInstanceOf[Str].text, kvs(i+1))
        i += 2
      }
      m
    }
    def size: Int = kvs.length >> 1
    def apply(key: String): Jast = mapped.getOrElse(key, JastError("no key: " + key))
  }
}

/*

/** Anything that implements `ToJson` can try to turn itself into a JSON AST.
  * It may fail, in which case it will generate a `JsError`.
  */
trait ToJson {
  def toJson: JsResult
}

trait FromJson[A] {
  def from(source: A): JsResult
}

/** `Jsonal` objects can always turn themselves into a JSON AST.  They also
  * know how to serialize themselves as a JSON string.
  */
trait Jsonal extends ToJson {
  override def toJson: JsVal
  def jsString(sb: java.lang.StringBuilder): Unit
}

/** `Jsonize` encapsulates the functionality of producing a JSON AST from a class. */
trait Jsonize[A] {
  def jsonize(a: A): JsVal
  def serialize(a: A): String = jsonize(a).toString
}

/** The supertype of the Jsonal AST hierarchy.  Any parsing that might fail should return a JsResult. */
sealed trait JsResult extends ToJson { def toJson = this }

/** Represents an error in conversion to a JSON AST, including range positions if available. */
case class JsError(msg: String, index: Long = -1L, because: JsResult = JsNull) extends JsResult {
  override def toString =
    if (index >= 0)
      if (because eq JsNull) f"At $index, error $message"
      else                   f"At $index, error $message\nbecause $because"
    else
      if (because eq JsNull) f"Error $message"
      else f"Error $message\nbecause $because"
}

/** `Js` corresponds to a JSON value--it can hold any valid JSON. */
sealed trait Js extends JsResult with Jsonal {
  override def toString = { val sb = new java.lang.StringBuilder; jsString(sb); sb.result }
}

/** `JsNull` corresponds to a JSON `null`. */
case object JsNull extends JsVal { override def toString = "null" }

/** `JsBool` covers both `JsTrue` and `JsFalse` (the `true` and `false` values in JSON). */
sealed trait JsBool extends JsVal { def value: Boolean }

/** `JsTrue` corresponds to a JSON `true`. */
case object JsTrue extends JsBool { def value = true; override def toString = "true" }

/** `JsFalse` corresponds to a JSON `false`. */
case object JsFalse extends JsBool { def value = false; override def toString = "false" }

/** `JsStr` corresponds to a JSON string. */
final case class JsStr(value: String) extends JsVal { override def toString = JsStr.escaped(value, quotes=true) }
object JsStr {
  private[this] def hx(i: Int) = { val j = i&0xF; if (j < 10) (j + '0').toChar else (j + 55).toChar }
  def empty = new JsStr("")
  def escaped(s: String, quotes: Boolean = false, ascii: Boolean = false): String = {
    var i = 0
    var n = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c < 32) {
        if (c == '\n' || c == '\r' || c == '\t' || c == '\r' || c == '\f' || c == '\b') n += 2
        else n += 6
      }
      else if (c == '"' || c == '\\') n += 2
      else if ({ if (ascii) c >= 127 else java.lang.Character.isSurrogate(c) }) n += 6
      else n += 1
      i += 1
    }
    if (n == i) { if (quotes) "\"" + s + "\"" else s }
    else {
      if (quotes) n += 2
      var buf = new Array[Char](n)
      if (quotes) { buf(0) = '"'; n = 1 } else n = 0
      i = 0
      while (i < s.length) {
        val c = s.charAt(i)
        if (c < 32) {
          if      (c == '\n') { buf(n) = '\\'; buf(n+1) = 'n'; n += 2 }
          else if (c == '\t') { buf(n) = '\\'; buf(n+1) = 't'; n += 2 }
          else if (c == '\r') { buf(n) = '\\'; buf(n+1) = 'r'; n += 2 }
          else if (c == '\f') { buf(n) = '\\'; buf(n+1) = 'f'; n += 2 }
          else if (c == '\b') { buf(n) = '\\'; buf(n+1) = 'b'; n += 2 }
          else {
            buf(n) = '\\'; buf(n+1) = 'u'; buf(n+2) = hx(c >> 12); buf(n+3) = hx(c >> 8); buf(n+4) = hx(c >> 4); buf(n+5) = hx(c); n += 6
          }
        }
        else if (c == '\\') { buf(n) = '\\'; buf(n+1) = '\\'; n += 2 }
        else if (c == '"') { buf(n) = '\\'; buf(n+1) = '"'; n += 2 }
        else if ({ if (ascii) c >= 127 else java.lang.Character.isSurrogate(c) }) {
          buf(n) = '\\'; buf(n+1) = 'u'; buf(n+2) = hx(c >> 12); buf(n+3) = hx(c >> 8); buf(n+4) = hx(c >> 4); buf(n+5) = hx(c); n += 6
        }
        else { buf(n) = c; n += 1 }
        i += 1
      }
      if (quotes) { buf(n) = '"'; n += 1 }
      new String(buf)
    }
  }
}

/** `JsNum` corresponds to a JSON number.  If the number fits nicely into a `Double`, that's where it will be.
  * Otherwise, the literal value will be stored as a `String`.
  */
final case class JsNum(value: Double, literal: String) extends JsVal {
  def hasValue(d: Double) = value == d || (d.isNaN && value.isNaN)
  override def equals(a: Any) = a match {
    case JsNum(v, l) => value == v || (v.isNaN && value.isNaN)
    case d: Double => value == d || (d.isNaN && value.isNaN)
    case _ => false
  } 
  override def toString = literal
}
object JsNum {
  def nan = new JsNum(Double.NaN, "null")
  def approx(value: Double): String = ???
}


sealed trait JsArr extends JsVal { def values: Array[JsVal] }
final case class JsArrV(values: Array[JsVal]) extends JsArr {
  override def equals(a: Any): Boolean = a match {
    case JsArrV(v) => 
      v.length == values.length && 
      { var i = 0; while (i < values.length) { if (v(i) != values(i)) return false; i += 1 }; true }
    case JsArrD(ds) => 
      ds.length == values.length && 
      { 
        var i = 0
        while (i < values.length) {
          values(i) match {
            case jn: JsNum => if (!(jn hasValue ds(i))) return false
            case _ => return false
          }
          i += 1
        }
        true
      }
    case _ => false
  }
  override def toString = {
    val parts = new Array[String](values.length)
    var i = 0
    var n = 0
    while (i < parts.length) { parts(i) = values(i).toString; n += parts(i).length; i += 1 }
    val text = new Array[Char](2 + n + math.max(parts.length-1, 0)*2)
    i = 0
    n = 1
    text(0) = '['
    if (parts.length > 0) {
      parts(0).getChars(0, parts(0).length, text, 1)
      n += parts(0).length
      i = 1
    }
    while (i < parts.length) {
      text(n) = ','
      text(n+1) = ' '
      parts(i).getChars(0, parts(i).length, text, n+2)
      n += parts(i).length + 2
      i += 1
    }
    text(n) = ']'
    new String(text)
  }
}
final case class JsArrD(doubles: Array[Double]) extends JsArr {
  lazy val values = { var i = 0; val v = new Array[JsVal](doubles.length); while (i < doubles.length) { v(i) = JsNum(doubles(i), doubles(i).toString); i += 1 }; v }
  override def equals(a: Any): Boolean = a match {
    case JsArrD(ds) => 
      ds.length == doubles.length && 
      { 
        var i = 0
        while (i < doubles.length) {
          if (doubles(i) != ds(i) && !(doubles(i).isNaN && ds(i).isNaN)) return false
          i += 1
        }
        true
      }
    case jv: JsArrV => jv == this 
    case _ => false
  }
  override def toString = {
    val parts = new Array[String](doubles.length)
    var i = 0
    var n = 0
    while (i < parts.length) {
      val vi = doubles(i)
      parts(i) = if (vi.isNaN || vi.isInfinite) "null" else vi.toString
      n += parts(i).length
      i += 1
    }
    val text = new Array[Char](2 + n + math.max(parts.length-1, 0)*2)
    i = 0
    n = 1
    text(0) = '['
    if (parts.length > 0) {
      parts(0).getChars(0, parts(0).length, text, 1)
      n += parts(0).length
      i = 1
    }
    while (i < parts.length) {
      text(n) = ','
      text(n+1) = ' '
      parts(i).getChars(0, parts(i).length, text, n+2)
      n += parts(i).length + 2
      i += 1
    }
    text(n) = ']'
    new String(text)
  }
}
object JsArr { def empty: JsArr = new JsArrV(new Array[JsVal](0)) }

final case class JsObj(keys: Array[String], values: Array[JsVal], table: collection.Map[String, JsVal]) extends JsVal {
  def apply(s: String): Option[JsVal] = if (table ne null) table get s else {
    var i = 0
    while (i < keys.length) {
      if (s == keys(i)) return Some(values(i))
      i += 1
    }
    None
  }
  def hasDuplicateKeys = ((table eq null) && keys.length > 0) || table.size < keys.length
  override def equals(a: Any): Boolean = a match {
    case JsObj(k, v, t) =>
      if (k.length == 0 && keys.length == 0) true
      else if (k.length != keys.length) false
      else if (hasDuplicateKeys) {
        var i = 0
        while (i < keys.length) {
          if (k(i) != keys(i) || v(i) != values(i)) return false;
          i += 1
        }
        true
      }
      else table == t
    case _ => false
  }
  override def toString = {
    val parts = new Array[String](2 * values.length)
    var i, j = 0
    var n = 0
    while (i < keys.length) { 
      val ks = JsStr.escaped(keys(i))
      val vs = values(i).toString
      n += ks.length + 3 + vs.length
      parts(j) = ks
      j += 1
      parts(j) = vs
      j += 1
      i += 1
    }
    val text = new Array[Char](2 + n + math.max(parts.length-2, 0))
    j = 0
    n = 1
    text(0) = '{'
    if (parts.length > 1) {
      text(1) = '"'
      parts(0).getChars(0, parts(0).length, text, 2)
      n += 1 + parts(0).length
      text(n) = '"'
      text(n+1) = ':'
      parts(1).getChars(0, parts(1).length, text, n+2)
      n += 2 + parts(1).length
      j = 2
    }
    while (j < parts.length) {
      text(n) = ','
      text(n+1) = ' '
      text(n+2) = '"'
      parts(j).getChars(0, parts(j).length, text, n+3)
      n += 3 + parts(j).length
      j += 1
      text(n) = '"'
      text(n+1) = ':'
      parts(j).getChars(0, parts(j).length, text, n+2)
      n += 2 + parts(j).length
      j += 1
    }
    text(n) = '}'
    new String(text)
  }
}
object JsObj { def empty = new JsObj(new Array[String](0), new Array[JsVal](0), Map.empty[String, JsVal]) }

*/

trait JsonGenericParser {
  def Js(a: Any): Either[JastError, kse.jsonal.Js] = ???
  def Js(a: Any, b: Any): Either[JastError, kse.jsonal.Js] = ???
  def Js(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Js] = ???
  def Null(a: Any): Either[JastError, kse.jsonal.Js.Null.type] = ???
  def Null(a: Any, b: Any): Either[JastError, kse.jsonal.Js.Null.type] = ???
  def Null(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Js.Null.type] = ???
  def Bool(a: Any): Either[JastError, kse.jsonal.Js.Bool] = ???
  def Bool(a: Any, b: Any): Either[JastError, kse.jsonal.Js.Bool] = ???
  def Bool(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Js.Bool] = ???
  def Str(a: Any): Either[JastError, kse.jsonal.Js.Str] = ???
  def Str(a: Any, b: Any): Either[JastError, kse.jsonal.Js.Str] = ???
  def Str(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Js.Str] = ???
  def encodeString(a: Any, b: Any) {}
  def encodeString(a: Any, b: Any, c: Any) {}
}

object JsonInputStreamParser extends JsonGenericParser {}

object JsonByteBufferParser extends JsonGenericParser {}

object JsonCharBufferParser extends JsonGenericParser {}

object JsonStringParser extends JsonGenericParser {}
