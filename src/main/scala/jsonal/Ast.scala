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
  * It is easy to build: just say the name of what you want to build, e.g. `Json.Obj`,
  * add in the pieces with `~`, and end with the same name (`Json.Obj`).  Or, use `~~` to add
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
  def json: Json
  def jsonString(sb: java.lang.StringBuilder) { json.jsonString(sb) }
  def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = json.jsonBytes(bb, refresh)
  def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = json.jsonChars(cb, refresh)
}

trait Jsonize[A] {
  def jsonize(a: A): Json
  def jsonizeString(a: A, sb: java.lang.StringBuilder) { jsonize(a).jsonString(sb) }
  def jsonizeBytes(a: A, bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = jsonize(a).jsonBytes(bb, refresh)
  def jsonizeChars(a: A, cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = jsonize(a).jsonChars(cb, refresh)
}

trait FromJson[A] {
  def parse(input: Json): Either[JastError, A]

  def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Either[JastError, A] = Json.parse(input, i0, iN, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }
  def parse(input: String): Either[JastError, A] = parse(input, 0, input.length, new FromJson.Endpoint(0))

  def parse(input: ByteBuffer): Either[JastError, A] = Json.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  def parse(input: CharBuffer): Either[JastError, A] = Json.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Either[JastError, A] = Json.parse(input, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }
}
object FromJson {
  case class Endpoint(var index: Long) {}
}

sealed trait Jast {
  def simple: Boolean   // Is null, boolean, number, or string
  def double: Double
  def bool: Option[Boolean]
  def string: Option[String]
  def apply(i: Int): Jast
  def apply(key: String): Jast
}

final case class JastError(msg: String, where: Long = -1L, because: Jast = Json.Null) extends Jast {
  def simple = false
  def double = Json.not_a_normal_NaN
  def bool = None
  def string = None
  def apply(i: Int) = this
  def apply(key: String) = this
}

trait JsonBuildTerminator[T] {}

sealed trait Json extends Jast with AsJson {
  protected def myName: String
  def double = Json.not_a_normal_NaN
  def bool: Option[Boolean] = None
  def string: Option[String] = None
  def apply(i: Int): Jast = JastError("Indexing into "+myName)
  def apply(key: String): Jast = JastError("Map looking on "+myName)

  def json: Json = this
  override def toString = { val sb = new java.lang.StringBuilder; jsonString(sb); sb.toString }
}
object Json extends FromJson[Json] with JsonBuildTerminator[Json] {
  private[jsonal] val not_a_normal_NaN = java.lang.Double.longBitsToDouble(0x7FF9000000000000L)

  private[jsonal] def loadByteBuffer(bytes: Array[Byte], bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
    var b = bb
    var i = 0
    while(true) {
      val n = math.max(bytes.length - i, bb.remaining) match { case 0 => bytes.length - i; case x => x }
      b.put(bytes, i, n)
      i += n
      if (i < bytes.length) b = refresh(b) else return b
    }
    null
  }
  private[jsonal] def loadByteBuffer(bytes: String, bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
    var b = bb
    var i = 0
    while (i < bytes.length) {
      val j = i
      if (!b.hasRemaining) b = refresh(b)
      while (i < bytes.length && b.hasRemaining) {
        b put bytes.charAt(i).toByte
        i += 1
      }
    }
    b
  }
  private[jsonal] def loadCharBuffer(chars: Array[Char], cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
    var c = cb
    var i = 0
    while(true) {
      val n = math.max(chars.length - i, cb.remaining) match { case 0 => chars.length - i; case x => x }
      c.put(chars, i, n)
      i += n
      if (i < chars.length) c = refresh(c) else return c
    }
    null
  }
  private[jsonal] def loadCharBuffer(chars: String, cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
    var c = cb
    var i = 0
    while (i < chars.length) {
      val j = i
      if (!c.hasRemaining) c = refresh(c)
      while (i < chars.length && c.hasRemaining) {
        c put chars.charAt(i)
        i += 1
      }
    }
    c
  }

  def apply(): Json = Null
  def apply(b: Boolean): Json = if (b) Bool.True else Bool.False
  def apply(s: String): Json = Str(s)
  def apply(d: Double): Json = Num(d)
  def apply(bd: BigDecimal): Json = Num(bd)
  def apply(aj: Array[Json]): Json = Arr.All(aj)
  def apply(xs: Array[Double]): Json = Arr.Dbl(xs)
  def apply(kvs: Map[String, Json]): Json = Obj(kvs)

  def ~(dbl: Double): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~ dbl
  def ~(nul: scala.Null): Arr.All.Build[Json] = (new Arr.All.Build[Json]) ~ Null
  def ~(js: Json): Arr.All.Build[Json] = (new Arr.All.Build[Json]) ~ js
  def ~[A: Jsonize](a: A): Arr.All.Build[Json] = (new Arr.All.Build[Json]) ~ a

  def ~(key: Str, nul: scala.Null) = (new Obj.Build[Json]) ~ (key, nul)
  def ~(key: String, nul: scala.Null) = (new Obj.Build[Json]) ~ (key, nul)
  def ~(key: Str, js: Json) = (new Obj.Build[Json]) ~ (key, js)
  def ~(key: String, js: Json) = (new Obj.Build[Json]) ~ (Str(key), js)
  def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]) = (new Obj.Build[Json]) ~ (key, jser.jsonize(a))
  def ~[A](key: String, a: A)(implicit jser: Jsonize[A]) = (new Obj.Build[Json]) ~ (Str(key), jser.jsonize(a))
  def ~~(coll: collection.TraversableOnce[(Str,Json)]) = (new Obj.Build[Json]) ~~ coll
  def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= String) = (new Obj.Build[Json]) ~~ coll
  def ~~[A: Jsonize](coll: collection.TraversableOnce[(Str,A)]) = (new Obj.Build[Json]) ~~ coll
  def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= String) = (new Obj.Build[Json]) ~~ coll

  def parse(input: Json): Either[JastError, Json] = Right(input)
  override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = JsonStringParser.Js(input, i0, iN, ep)
  override def parse(input: ByteBuffer) = JsonByteBufferParser.Js(input)
  override def parse(input: CharBuffer) = JsonCharBufferParser.Js(input)
  override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = JsonInputStreamParser.Js(input, ep)

  sealed abstract class Null extends Json {}
  final object Null extends Null with FromJson[Null] {
    final private[this] val myBytesSayNull = "null".getBytes
    final private[this] val myCharsSayNull = "null".toCharArray
    protected def myName = "null"
    def simple = true
    override def jsonString(sb: java.lang.StringBuilder) { sb append "null" }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
      (if (bb.remaining < 4) refresh(bb) else bb) put myBytesSayNull
    override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
      (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayNull
    override def toString = "null"
    def parse(input: Json): Either[JastError, Null] =
      if (this eq input) Right(this)
      else Left(JastError("expected null"))
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = 
      JsonStringParser.Null(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Null(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Null(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
      JsonInputStreamParser.Null(input, ep)
  }

  sealed abstract class Bool extends Json { 
    protected def myName = "boolean"
    def simple = true
    override def bool = Some(value)
    def value: Boolean
  }
  object Bool extends FromJson[Bool] { 
    final private val myBytesSayTrue = "true".getBytes
    final private val myBytesSayFalse = "false".getBytes
    final private val myCharsSayTrue = "true".toCharArray
    final private val myCharsSayFalse = "false".toCharArray
    def unapply(js: Json): Option[Boolean] = js match { case b: Bool => Some(b.value); case _ => None }
    case object True extends Bool { 
      def value = true
      override def jsonString(sb: java.lang.StringBuilder) { sb append "true" }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
        (if (bb.remaining < 4) refresh(bb) else bb) put myBytesSayTrue
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
        (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayTrue
    }
    case object False extends Bool {
      def value = false
      override def jsonString(sb: java.lang.StringBuilder) { sb append "false" }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
        (if (bb.remaining < 5) refresh(bb) else bb) put myBytesSayFalse
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
        (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayFalse
    }
    override def parse(input: Json): Either[JastError, Bool] = input match {
      case b: Bool => Right(b)
      case _       => Left(JastError("expected Json.Bool"))
    }
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = 
      JsonStringParser.Bool(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Bool(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Bool(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = 
      JsonInputStreamParser.Bool(input, ep)
  }

  final case class Str(text: String) extends Json {
    protected def myName = "string"
    def simple = true
    override def jsonString(sb: java.lang.StringBuilder) {
      var i = 0
      sb append '"'
      while (i < text.length) {
        val c = text.charAt(i)
        if (c == '"' || c == '\\') sb append '\\' append c
        else if (c >= ' ' && c <= '~') sb append c
        else if (c == '\n') sb append "\\n"
        else if (c == '\t') sb append "\\t"
        else if (c == '\r') sb append "\\r"
        else if (c == '\f') sb append "\\f"
        else if (c == '\b') sb append "\\b"
        else sb append "\\u%04x".format(c.toInt)
        i += 1
      }
      sb append '"'
    }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
      var b = if (bb.hasRemaining) bb else refresh(bb)
      b put '"'.toByte
      var i = 0
      while (i < text.length) {
        val c = text.charAt(i)
        if (c == '"' || c == '\\') { if (b.remaining < 2) b = refresh(b); b put '\\'.toByte put c.toByte }
        else if (c >= ' ' && c <= '~') { if (!b.hasRemaining) b = refresh(b); b put c.toByte }
        else if (c == '\n') { if (b.remaining < 2) b = refresh(b); b put '\\'.toByte put 'n'.toByte }
        else if (c == '\t') { if (b.remaining < 2) b = refresh(b); b put '\\'.toByte put 't'.toByte }
        else if (c == '\r') { if (b.remaining < 2) b = refresh(b); b put '\\'.toByte put 'r'.toByte }
        else if (c == '\f') { if (b.remaining < 2) b = refresh(b); b put '\\'.toByte put 'f'.toByte }
        else if (c == '\b') { if (b.remaining < 2) b = refresh(b); b put '\\'.toByte put 'b'.toByte }
        else { if (b.remaining < 6) b = refresh(b); b put "\\u%04x".format(c.toInt).getBytes }
        i += 1
      }
      if (!b.hasRemaining) b = refresh(b)
      b put '"'.toByte
    }
    override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
      var b = if (cb.hasRemaining) cb else refresh(cb) // Normally we name this c, but call it b to avoid collision with char c below
      b put '"'
      var i = 0
      while (i < text.length) {
        val c = text.charAt(i)
        if (c == '"' || c == '\\') { if (b.remaining < 2) b = refresh(b); b put '\\' put c }
        else if (c >= ' ' && c <= '~') { if (!b.hasRemaining) b = refresh(b); b put c }
        else if (c == '\n') { if (b.remaining < 2) b = refresh(b); b put '\\' put 'n' }
        else if (c == '\t') { if (b.remaining < 2) b = refresh(b); b put '\\' put 't' }
        else if (c == '\r') { if (b.remaining < 2) b = refresh(b); b put '\\' put 'r' }
        else if (c == '\f') { if (b.remaining < 2) b = refresh(b); b put '\\' put 'f' }
        else if (c == '\b') { if (b.remaining < 2) b = refresh(b); b put '\\' put 'b' }
        else { if (b.remaining < 6) b = refresh(b); b put "\\u%04x".format(c.toInt).toCharArray }
        i += 1
      }
      if (!b.hasRemaining) b = refresh(b)
      b put '"'
    }
  }
  object Str extends FromJson[Str] {
    override def parse(input: Json): Either[JastError, Str] = input match {
      case s: Str => Right(s)
      case _      => Left(JastError("expected Json.Str"))
    }
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
      JsonStringParser.Str(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Str(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Str(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
      JsonInputStreamParser.Str(input, ep)    
  }

  // TODO -- remove format parameter.  Need to infer from String what's going on.
  class Num private[jsonal] (format: Int, content: Double, text: String) extends Json {
    protected def myName = "number"
    def simple = true
    def isDouble: Boolean = format >= 0
    override def double = content
    def isLong: Boolean = (format == -1) || (format > 0 && content.toLong == content)
    def long: Long =
      if (format == -1) java.lang.Double.doubleToRawLongBits(content)
      else content.toLong
    def longOr(fallback: Long): Long =
      if (format == -1) java.lang.Double.doubleToRawLongBits(content)
      else if (format >= 0) {
        val l = content.toLong
        if (l == content) l else fallback
      }
      else fallback
    def big: BigDecimal =
      if (text ne null) BigDecimal(text)
      else if (isDouble) BigDecimal.decimal(content)
      else BigDecimal(java.lang.Double.doubleToRawLongBits(content))
    override def toString =
      if (text ne null) text
      else if (format >= 0) Num.formatTable(format).format(content)
      else java.lang.Double.doubleToRawLongBits(content).toString
    override def jsonString(sb: java.lang.StringBuilder) {
      if (text ne null) sb append text
      else if (format >= 0) sb append Num.formatTable(format).format(content)
      else sb append java.lang.Double.doubleToRawLongBits(content)
    }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
      loadByteBuffer(toString.getBytes, bb, refresh)
    override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
      loadCharBuffer(toString.toCharArray, cb, refresh)
  }
  object Num extends FromJson[Num] {
    private[jsonal] val formatTable: Array[String] = (0 until 30).map(i => s"%.${i}f").toArray ++ (0 until 20).map(i => s"%.${i}e")
    private[jsonal] def formatNum(precision: Int, value: Double): String = {
      // Note--returns used to untangle deeply nested conditionals.
      if (precision < 0) return value.toString
      val fmt = formatTable(precision)
      val s = fmt.format(value)
      if (fmt.charAt(fmt.length-1) == 'f') {
        if (s.charAt(s.length-1) != '0') return s
        val i = s.indexOf('.')
        if (i < 0) return s
        var j = s.length - 2
        while (j > i && s.charAt(j) == '0') j -= 1
        if (j == i) j -= 1
        s.substring(0,j+1)
      }
      else {
        val i = s.lastIndexOf('e')
        if (i <= 2 || s.charAt(i-1) != '0') return s
        var j = i-2
        while (j > 0 && s.charAt(j) == '0') j -= 1
        if (s.charAt(j) != '.') j += 1
        s.substring(0, j) + s.substring(i) 
      }
    }

    def apply(d: Double, precision: Int = 16): Num = ???
    def apply(bd: BigDecimal): Num = ???

    override def parse(input: Json): Either[JastError, Num] = input match {
      case n: Num => Right(n)
      case _      => Left(JastError("expected Json.Num"))
    }
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
      JsonStringParser.Num(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Num(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Num(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
      JsonInputStreamParser.Num(input, ep)    
  }

  sealed trait Arr extends Json {
    protected def myName = "array"
    def simple = false
    def size: Int
  }
  object Arr extends FromJson[Arr] with JsonBuildTerminator[Arr] {
    def apply(aj: Array[Json]): Arr = All(aj)
    def apply(xs: Array[Double], precision: Int = 16): Arr = Dbl(xs, precision)

    def ~(done: Json.type): Json = Dbl.empty
    def ~(done: Arr.type): Arr = Dbl.empty
    def ~(dbl: Double): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~ dbl
    def ~(nul: scala.Null): All.Build[Arr] = (new All.Build[Arr]) ~ null
    def ~(js: Json): All.Build[Arr] = (new All.Build[Arr]) ~ js
    def ~[A: Jsonize](a: A): All.Build[Arr] = (new All.Build[Arr]) ~ a

    final class All(alls: Array[Json]) extends Arr {
      def size = alls.length
      override def apply(i: Int) = if (i < 0 || i >= alls.length) JastError("bad index "+i) else alls(i)
      override def jsonString(sb: java.lang.StringBuilder) {
        sb append '['
        if (alls.length > 0) {
          alls(0).jsonString(sb)
          var i = 1
          while (i < alls.length) {
            sb append ", "
            alls(i).jsonString(sb)
            i += 1
          }
        }
        sb append ']'
      }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
        var b = if (bb.hasRemaining) bb else refresh(bb)
        b put '['.toByte
        if (alls.length > 0) {
          b = alls(0).jsonBytes(b, refresh)
          var i = 1
          while (i < alls.length) {
            if (b.remaining < 2) b = refresh(b)
            b put ','.toByte put ' '.toByte
            b = alls(i).jsonBytes(b, refresh)
            i += 1
          }
        }
        if (!b.hasRemaining) b = refresh(b)
        b put ']'.toByte
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
        var c = if (cb.hasRemaining) cb else refresh(cb)
        c put '['
        if (alls.length > 0) {
          c = alls(0).jsonChars(c, refresh)
          var i = 1
          while (i < alls.length) {
            if (c.remaining < 2) c = refresh(c)
            c put ',' put ' '
            c = alls(i).jsonChars(c, refresh)
            i += 1
          }
        }
        if (!c.hasRemaining) c = refresh(c)
        c put ']'
      }
    }
    object All extends JsonBuildTerminator[All] {
      def apply(aj: Array[Json]) = new All(aj)

      def ~(nul: scala.Null) = (new Build[All]) ~ Null
      def ~(js: Json) = (new Build[All]) ~ js
      def ~[A: Jsonize](a: A) = (new Build[All]) ~ a

      class Build[T >: All] {
        private[this] var i = 0
        private[this] var a: Array[Json] = new Array[Json](6)
        def ~(done: JsonBuildTerminator[T]): T =
          new All(if (i==a.length) a else java.util.Arrays.copyOf(a, i))
        def ~(js: Json): this.type = {
          if (i >= a.length) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
          a(i) = js
          i += 1
          this
        }
        def ~(nul: scala.Null): this.type = this ~ Null
        def ~[A](a: A)(implicit jser: Jsonize[A]): this.type = this ~ jser.jsonize(a)
        def ~~(jses: Array[Json]): this.type = {
          // TODO - make this efficient!
          var i = 0
          while (i < jses.length) { this ~ jses(i); i += 1 }
          this
        }
        def ~~[A: Jsonize](as: Array[A]): this.type = {
          // TODO - make this efficient!
          var i = 0
          while (i < as.length) { this ~ as(i); i += 1 }
          this
        }
        def ~~(coll: collection.TraversableOnce[Json]): this.type = {
          coll.foreach(this ~ _)
          this
        }
        def ~~[A: Jsonize](coll: collection.TraversableOnce[A]): this.type = {
          coll.foreach(x => this ~ x)
          this
        }
      }
    }

    final class Dbl(format: Int, val direct: Array[Double]) extends Arr {
      def size = direct.length
      override def apply(i: Int) = if (i < 0 || i >= direct.length) JastError("bad index "+i) else new Num(0, direct(i),"%e")
      override def jsonString(sb: java.lang.StringBuilder) {
        sb append '['
        var i = 0
        while (i < direct.length) {
          if (i > 0) sb append ", "
          val d = direct(i)
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) sb append "null"
          else sb append Num.formatNum(format, d)
          i += 1
        }
        sb append ']'
      }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
        var b = if (bb.hasRemaining) bb else refresh(bb)
        b put '['.toByte
        var i = 0
        while (i < direct.length) {
          if (i > 0) {
            if (b.remaining < 2) b = refresh(b)
            b put '['.toByte put ' '.toByte
          }
          val d = direct(i)
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) b = Null.jsonBytes(b, refresh)
          else b = loadByteBuffer(Num.formatTable(format).format(d).getBytes, b, refresh)
          i += 1
        }
        if (!b.hasRemaining) b = refresh(b)
        b put ']'.toByte
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
        var c = if (cb.hasRemaining) cb else refresh(cb)
        c put '['
        var i = 0
        while (i < direct.length) {
          if (i > 0) {
            if (c.remaining < 2) c = refresh(c)
            c put '[' put ' '
          }
          val d = direct(i)
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) c = Null.jsonChars(c, refresh)
          else c = loadCharBuffer(Num.formatTable(format).format(d).toCharArray, c, refresh)
          i += 1
        }
        if (!c.hasRemaining) c = refresh(c)
        c put ']'
      }
    }
    object Dbl extends JsonBuildTerminator[Dbl] {
      def apply(xs: Array[Double], precision: Int): Dbl = ???
      def apply(xs: Array[Double]): Dbl = apply(xs, 16)
      def apply(xs: Array[Float], precision: Int): Dbl = ???
      def apply(xs: Array[Float]): Dbl = apply(xs, 7)
      def apply(xs: Array[Long]): Dbl = ???
      def apply(xs: Array[Int]): Dbl = ???

      val empty = new Dbl(0, new Array[Double](0))

      def ~(me: Dbl.type) = empty
      def ~(dbl: Double): Build[Dbl] = (new Build[Dbl]) ~ dbl
      def ~(dbl: Double, precision: Int): Build[Dbl] = (new Build[Dbl]) ~ (dbl, precision)

      class Build[T >: Dbl] {
        private[this] var i = 0
        private[this] var a: Array[Double] = new Array[Double](6)
        private[this] var prec: Int = -1
        def ~(dbl: Double): this.type = {
          if (i >= a.length) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
          a(i) = dbl
          i += 1
          this
        }
        def ~(dbl: Double, precision: Int): this.type = {
          prec = math.max(prec, precision)
          this ~ dbl
        }
        def ~(flt: Float): this.type = this ~ (flt.toDouble, 7)
        def ~(done: JsonBuildTerminator[T]): T =
          new Dbl(if (prec < 0) 16 else prec, if (i==a.length) a else java.util.Arrays.copyOf(a, i))
        def ~~(dbls: Array[Double]): this.type = {
          // TODO - make this efficient!
          var i = 0
          while (i < dbls.length) { this ~ dbls(i); i += 1 }
          this
        }
        def ~~(floats: Array[Float]): this.type = {
          // TODO - make this efficient!
          var i = 0
          while (i < floats.length) { this ~ floats(i); i += 1 }
          this
        }
        def ~~(coll: collection.TraversableOnce[Double]): this.type = {
          coll.foreach(this ~ _)
          this
        }
        def ~~[A](coll: collection.TraversableOnce[A])(implicit ev: A =:= Float): this.type = {
          coll.foreach(x => this ~ x.asInstanceOf[Float])
          this
        }
      }
    }

    override def parse(input: Json): Either[JastError, Arr] = input match {
      case n: Arr => Right(n)
      case _      => Left(JastError("expected Json.Arr"))
    }
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
      JsonStringParser.Arr(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Arr(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Arr(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
      JsonInputStreamParser.Arr(input, ep)    
  }

  final class Obj(kvs: Array[Json]) extends Json {
    private lazy val mapped = { 
      val m = new collection.mutable.AnyRefMap[String, Json]
      var i = 0
      while (i < kvs.length - 1) {
        m += (kvs(i).asInstanceOf[Str].text, kvs(i+1))
        i += 2
      }
      m
    }
    protected def myName = "object"
    def simple = false
    def size: Int = kvs.length >> 1
    override def apply(key: String): Jast = mapped.getOrElse(key, JastError("no key: " + key))
    def get(key: String): Option[Json] = mapped.get(key)
    override def jsonString(sb: java.lang.StringBuilder) {
      sb append '{'
      var i = 0
      while (i < kvs.length-1) {
        sb append (if (i > 0) ", " else " ")
        kvs(i).jsonString(sb)
        sb append ':'
        kvs(i+1).jsonString(sb)
        i += 2
      }
      sb append (if (i > 0) " }" else "}")
    }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
      var b = if (bb.remaining >= 2) bb else refresh(bb)
      b put '{'.toByte
      if (kvs.length > 0) b put ' '.toByte
      var i = 0
      while (i < kvs.length-1) {
        if (i > 0) {
          if (b.remaining < 2) b = refresh(b)
          b put ','.toByte put ' '.toByte
        }
        else {
          if (!b.hasRemaining) b = refresh(b)
          b put ' '.toByte
        }
        b = kvs(i).jsonBytes(b, refresh)
        if (!b.hasRemaining) b = refresh(b)
        b put ':'.toByte
        kvs(i+1).jsonBytes(b, refresh)
        i += 2
      }
      if (kvs.length > 0) {
        if (b.remaining < 2) b = refresh(b)
        b put ' '.toByte put '}'.toByte
      }
      else {
        if (!b.hasRemaining) b = refresh(b)
        b put '}'.toByte
      }
    }
    override def jsonChars(bb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
      var b = if (bb.remaining >= 2) bb else refresh(bb)
      b put '{'
      if (kvs.length > 0) b put ' '
      var i = 0
      while (i < kvs.length-1) {
        if (i > 0) {
          if (b.remaining < 2) b = refresh(b)
          b put '[' put ' '
        }
        else {
          if (!b.hasRemaining) b = refresh(b)
          b put ' '
        }
        b = kvs(i).jsonChars(b, refresh)
        if (!b.hasRemaining) b = refresh(b)
        b put ':'
        b = kvs(i+1).jsonChars(b, refresh)
        i += 2
      }
      if (kvs.length > 0) {
        if (b.remaining < 2) b = refresh(b)
        b put ' ' put '}'
      }
      else {
        if (!b.hasRemaining) b = refresh(b)
        b put '}'
      }
    }
  }
  object Obj extends FromJson[Obj] with JsonBuildTerminator[Obj] {
    val empty = new Obj(Array())

    def apply(kvs: collection.Map[String, Json]) = {
      val a = new Array[Json](2*kvs.size)
      var i = 0
      kvs.foreach{ case (k,v) => a(i) = new Str(k); a(i+1) = v; i += 2 }
      new Obj(a)
    }

    def ~(done: Obj.type) = empty
    def ~~(done: Obj.type) = empty
    def ~(key: Str, nul: scala.Null) = (new Build[Obj]) ~ (key, nul)
    def ~(key: String, nul: scala.Null) = (new Build[Obj]) ~ (key, nul)
    def ~(key: Str, js: Json) = (new Build[Obj]) ~ (key, js)
    def ~(key: String, js: Json) = (new Build[Obj]) ~ (Str(key), js)
    def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]) = (new Build[Obj]) ~ (key, jser.jsonize(a))
    def ~[A](key: String, a: A)(implicit jser: Jsonize[A]) = (new Build[Obj]) ~ (Str(key), jser.jsonize(a))
    def ~~(coll: collection.TraversableOnce[(Str,Json)]) = (new Build[Obj]) ~~ coll
    def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= String) = (new Build[Obj]) ~~ coll
    def ~~[A: Jsonize](coll: collection.TraversableOnce[(Str,A)]) = (new Build[Obj]) ~~ coll
    def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= String) = (new Build[Obj]) ~~ coll

    class Build[T >: Obj] {
      private[this] var i = 0
      private[this] var a: Array[Json] = new Array[Json](6)
      def ~(done: JsonBuildTerminator[T]): T =
        new Obj(if (i==a.length) a else java.util.Arrays.copyOf(a, i))
      def ~~(done: JsonBuildTerminator[T]): T = this ~ done
      def ~(key: Str, js: Json): this.type = {
        if (i >= a.length-1) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
        a(i) = key
        a(i+1) = js
        i += 2
        this
      }
      def ~(key: String, js: Json): this.type = this ~ (Str(key), js)
      def ~(key: Str, nul: scala.Null): this.type = this ~ (key, Null)
      def ~(key: String, nul: scala.Null): this.type = this ~ (Str(key), Null)
      def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]): this.type = this ~ (key, jser.jsonize(a))
      def ~[A](key: String, a: A)(implicit jser: Jsonize[A]): this.type = this ~(Str(key), jser.jsonize(a))
      def ~~(coll: collection.TraversableOnce[(Str,Json)]): this.type = {
        coll.foreach{ case (k,v) => this ~ (k,v) }
        this
      }
      def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= String): this.type = {
        coll.foreach{ case (k,v) => this ~ (Str(ev(k)),v) }
        this
      }
      def ~~[A: Jsonize](coll: collection.TraversableOnce[(Str,A)]): this.type = {
        coll.foreach{ case (k,a) => this ~ (k,a) }
        this
      }
      def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= String): this.type = {
        coll.foreach{ case (k,a) => this ~ (Str(ev(k)),a) }
        this
      }
    }

    override def parse(input: Json): Either[JastError, Obj] = input match {
      case n: Obj => Right(n)
      case _      => Left(JastError("expected Json.Obj"))
    }
    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
      JsonStringParser.Obj(input, i0, iN, ep)
    override def parse(input: ByteBuffer) = JsonByteBufferParser.Obj(input)
    override def parse(input: CharBuffer) = JsonCharBufferParser.Obj(input)
    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
      JsonInputStreamParser.Obj(input, ep)    
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
  def Js(a: Any): Either[JastError, kse.jsonal.Json] = ???
  def Js(a: Any, b: Any): Either[JastError, kse.jsonal.Json] = ???
  def Js(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json] = ???
  def Null(a: Any): Either[JastError, kse.jsonal.Json.Null.type] = ???
  def Null(a: Any, b: Any): Either[JastError, kse.jsonal.Json.Null.type] = ???
  def Null(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json.Null.type] = ???
  def Bool(a: Any): Either[JastError, kse.jsonal.Json.Bool] = ???
  def Bool(a: Any, b: Any): Either[JastError, kse.jsonal.Json.Bool] = ???
  def Bool(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json.Bool] = ???
  def Str(a: Any): Either[JastError, kse.jsonal.Json.Str] = ???
  def Str(a: Any, b: Any): Either[JastError, kse.jsonal.Json.Str] = ???
  def Str(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json.Str] = ???
  def Num(a: Any): Either[JastError, kse.jsonal.Json.Num] = ???
  def Num(a: Any, b: Any): Either[JastError, kse.jsonal.Json.Num] = ???
  def Num(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json.Num] = ???
  def Arr(a: Any): Either[JastError, kse.jsonal.Json.Arr] = ???
  def Arr(a: Any, b: Any): Either[JastError, kse.jsonal.Json.Arr] = ???
  def Arr(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json.Arr] = ???
  def Obj(a: Any): Either[JastError, kse.jsonal.Json.Obj] = ???
  def Obj(a: Any, b: Any): Either[JastError, kse.jsonal.Json.Obj] = ???
  def Obj(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json.Obj] = ???
}

object JsonInputStreamParser extends JsonGenericParser {}

object JsonByteBufferParser extends JsonGenericParser {}

object JsonCharBufferParser extends JsonGenericParser {}

object JsonStringParser extends JsonGenericParser {}
