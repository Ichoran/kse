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
  * sharing and/or exposed mutable data structures, neither of which this AST provides.  You can get _part_
  * of the way there by using `Obj` to wrap an immutable map, and unwrapping it (with `asMap`) and
  * rebuilding when you want to change things.
  */

package kse.jsonal

import java.nio._

// TODO--this is buggy.  Fix it.
class PrettyJson(val lines: collection.mutable.ArrayBuilder[String], val lastLine: java.lang.StringBuilder, indent: Int = 2, margin: Int = 70) {
  private[jsonal] val myLine = new java.lang.StringBuilder
  private var lastSpaces = 0
  private var myIndent = math.max(0, indent)
  private var myMargin = math.max(0, margin)
  private var mySpaces = PrettyJson.lotsOfSpaces
  private var myDepth = 0
  def depth = myDepth
  def deeper: this.type = { myDepth += 1; this }
  def shallower: this.type = { myDepth = if (myDepth > 0) myDepth -1 else 0; this }
  def tooDeep = lastSpaces >= myMargin
  def append(s: String): this.type = { lastLine append s; this }
  def pad(): this.type = {
    if (indent-1 > mySpaces.length) mySpaces = Array.fill(indent)(' ')
    lastLine.append(mySpaces, 0, math.max(0, indent-1))
    this
  }
  def advance(depth: Int): this.type = {
    myDepth = depth
    val nsp = math.min(myMargin, depth * myIndent)
    if (lastLine.length > 0) { lines += lastLine.toString; lastLine.setLength(math.min(lastSpaces, nsp)) }
    val missing = lastLine.length - nsp
    if (missing > 0) {
      if (missing > mySpaces.length) mySpaces = Array.fill(mySpaces.length * 2)(' ')
      lastLine.append(mySpaces, 0, missing)
    }
    lastSpaces = lastLine.length
    this
  }
  def advance(): this.type = advance(myDepth)
  def advanceWithCarryover(n: Int, depth: Int): this.type = {
    val iN = math.max(0, math.min(n, lastLine.length))
    lines += lastLine.substring(0, iN)
    val nsp = math.min(myMargin, depth * myIndent)
    if (nsp > iN) {
      var i = lastSpaces
      while (i < iN) { lastLine.setCharAt(i, ' '); i += 1 }
      if (mySpaces.length < nsp-iN) mySpaces = Array.fill(nsp-iN)(' ')
      lastLine.insert(iN, mySpaces, 0, nsp-iN)
    }
    else {
      if (nsp < iN) lastLine.delete(nsp, iN)
      if (lastSpaces < nsp) {
        var i = lastSpaces
        while (i < iN) { lastLine.setCharAt(i, ' '); i += 1 }
      }
    }
    myDepth = math.max(depth, 0)
    lastSpaces = nsp
    this
  }
  def result() = {
    if (lastLine.length > 0) { lines += lastLine.toString ; lastLine.setLength(0) }
    lines.result()
  }
}
object PrettyJson {
  private[jsonal] val lotsOfSpaces: Array[Char] = Array.fill(1022)(' ')
}

trait AsJson {
  def json: Json
  def jsonPretty(pretty: PrettyJson, depth: Int) { json.jsonPretty(pretty, depth) }
  def jsonString(sb: java.lang.StringBuilder) { json.jsonString(sb) }
  def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = json.jsonBytes(bb, refresh)
  def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = json.jsonChars(cb, refresh)
}

trait Jsonize[A] {
  def jsonize(a: A): Json
  def jsonizePretty(a: A, pretty: PrettyJson, depth: Int) { jsonize(a).jsonPretty(pretty, depth) }
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
  def isNull: Boolean
  def double: Double
  def bool: Option[Boolean]
  def string: Option[String]
  def apply(i: Int): Jast
  def apply(key: String): Jast
}

final case class JastError(msg: String, where: Long = -1L, because: Jast = Json.Null) extends Jast {
  def simple = false
  def isNull = false
  def double = Json.not_a_normal_NaN
  def bool = None
  def string = None
  def apply(i: Int) = this
  def apply(key: String) = this
}

trait JsonBuildTerminator[T] {}

sealed trait Json extends Jast with AsJson {
  protected def myName: String
  def isNull = false
  def double = Json.not_a_normal_NaN
  def bool: Option[Boolean] = None
  def string: Option[String] = None
  def apply(i: Int): Jast = JastError("Indexing into "+myName)
  def apply(key: String): Jast = JastError("Map looking on "+myName)

  def json: Json = this
  override def toString = { val sb = new java.lang.StringBuilder; jsonString(sb); sb.toString }
  override def jsonPretty(pretty: PrettyJson, depth: Int) { jsonString(pretty.lastLine) }
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
  def apply(l: Long): Json = Num(l)
  def apply(d: Double): Json = Num(d)
  def apply(bd: BigDecimal): Json = Num(bd)
  def apply(aj: Array[Json]): Json = Arr.All(aj)
  def apply(xs: Array[Double]): Json = Arr.Dbl(xs)
  def apply(kvs: Map[String, Json]): Json = Obj(kvs)
  def apply(keys: Array[String], values: Array[Json]): Jast = Obj(keys, values)

  def ~(dbl: Double): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~ dbl
  def ~~(doubles: Array[Double]): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~~ doubles
  def ~~(doubles: Array[Double], i0: Int, iN: Int): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~~ (doubles, i0, iN)
  def ~~(coll: collection.TraversableOnce[Double]) = (new Arr.Dbl.Build[Json]) ~~ coll
  def ~~(existing: Arr.Dbl) = (new Arr.Dbl.Build[Json]) ~~ existing

  def ~(js: Json) = (new Arr.All.Build[Json]) ~ js
  def ~(nul: scala.Null) = (new Arr.All.Build[Json]) ~ Null
  def ~[A: Jsonize](a: A) = (new Arr.All.Build[Json]) ~ a
  def ~~(jses: Array[Json]) = (new Arr.All.Build[Json]) ~~ jses
  def ~~(jses: Array[Json], i0: Int, iN: Int) = (new Arr.All.Build[Json]) ~~ (jses, i0, iN)
  def ~~[A: Jsonize](as: Array[A]) = (new Arr.All.Build[Json]) ~~ as
  def ~~[A: Jsonize](as: Array[A], i0: Int, iN: Int) = (new Arr.All.Build[Json]) ~~ (as, i0, iN)
  def ~~(coll: collection.TraversableOnce[Json]) = (new Arr.All.Build[Json]) ~~ coll
  def ~~[A: Jsonize](coll: collection.TraversableOnce[A]) = (new Arr.All.Build[Json]) ~~ coll

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
    override def isNull = true
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
      override val bool = Some(true)
      override def jsonString(sb: java.lang.StringBuilder) { sb append "true" }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
        (if (bb.remaining < 4) refresh(bb) else bb) put myBytesSayTrue
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
        (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayTrue
    }
    case object False extends Bool {
      def value = false
      override val bool = Some(false)
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

  class Num private[jsonal] (content: Double, text: String) extends Json {
    protected def myName = "number"
    def simple = true
    def isDouble: Boolean = (text ne null) && !java.lang.Double.isNaN(content) && !java.lang.Double.isInfinite(content)
    override def double = if (text eq null) java.lang.Double.doubleToRawLongBits(content).toDouble else content
    def isLong: Boolean = text eq null
    def long: Long = if (text eq null) java.lang.Double.doubleToRawLongBits(content) else content.toLong
    def longOr(fallback: Long): Long =
      if (text eq null) java.lang.Double.doubleToRawLongBits(content)
      else {
        val l = content.toLong
        if (l == content) l else fallback
      }
    def big: BigDecimal =
      if (text eq null) BigDecimal(long)
      else if (text.isEmpty) BigDecimal(content)
      else BigDecimal(text)
    override def toString =
      if (text eq null) java.lang.Double.doubleToRawLongBits(content).toString
      else if (text.isEmpty) content.toString
      else text
    override def jsonString(sb: java.lang.StringBuilder) { sb append this.toString }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
      loadByteBuffer(toString.getBytes, bb, refresh)
    override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
      loadCharBuffer(toString.toCharArray, cb, refresh)
  }
  object Num extends FromJson[Num] {
    /*
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
    */

    def apply(l: Long): Num = new Num(java.lang.Double.longBitsToDouble(l), null)
    def apply(d: Double): Json = if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) Null else new Num(d, "")
    def apply(bd: BigDecimal): Num = new Num(bd.doubleValue, bd.toString)

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
    def apply(xs: Array[Double]): Arr = Dbl(xs)
    def decimal(xs: Array[Float]): Arr = Dbl decimal xs
    def exact(xs: Array[Float]): Arr = Dbl exact xs

    def ~(me: Arr.type) = Dbl.empty
    def ~(dbl: Double): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~ dbl
    def ~~(me: Arr.type) = Dbl.empty
    def ~~(doubles: Array[Double]): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~~ doubles
    def ~~(doubles: Array[Double], i0: Int, iN: Int): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~~ (doubles, i0, iN)
    def ~~(coll: collection.TraversableOnce[Double]) = (new Dbl.Build[Arr]) ~~ coll
    def ~~(existing: Dbl) = (new Dbl.Build[Arr]) ~~ existing

    def ~(js: Json) = (new All.Build[Arr]) ~ js
    def ~(nul: scala.Null) = (new All.Build[Arr]) ~ Null
    def ~[A: Jsonize](a: A) = (new All.Build[Arr]) ~ a
    def ~~(jses: Array[Json]) = (new All.Build[Arr]) ~~ jses
    def ~~(jses: Array[Json], i0: Int, iN: Int) = (new All.Build[Arr]) ~~ (jses, i0, iN)
    def ~~[A: Jsonize](as: Array[A]) = (new All.Build[Arr]) ~~ as
    def ~~[A: Jsonize](as: Array[A], i0: Int, iN: Int) = (new All.Build[Arr]) ~~ (as, i0, iN)
    def ~~(coll: collection.TraversableOnce[Json]) = (new All.Build[Arr]) ~~ coll
    def ~~[A: Jsonize](coll: collection.TraversableOnce[A]) = (new All.Build[Arr]) ~~ coll

    final class All(values: Array[Json]) extends Arr {
      def size = values.length
      override def apply(i: Int) = if (i < 0 || i >= values.length) JastError("bad index "+i) else values(i)
      override def jsonPretty(pretty: PrettyJson, depth: Int) {
        // TODO--actually make this pretty.
        if (size == 0) pretty append "[]"
        else if (size == 1 && values(0).simple) {
          pretty append "["
          values(0).jsonString(pretty.lastLine)
          pretty append "]"
        }
        else jsonString(pretty.lastLine)
      }
      override def jsonString(sb: java.lang.StringBuilder) {
        sb append '['
        if (values.length > 0) {
          values(0).jsonString(sb)
          var i = 1
          while (i < values.length) {
            sb append ", "
            values(i).jsonString(sb)
            i += 1
          }
        }
        sb append ']'
      }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
        var b = if (bb.hasRemaining) bb else refresh(bb)
        b put '['.toByte
        if (values.length > 0) {
          b = values(0).jsonBytes(b, refresh)
          var i = 1
          while (i < values.length) {
            if (b.remaining < 2) b = refresh(b)
            b put ','.toByte put ' '.toByte
            b = values(i).jsonBytes(b, refresh)
            i += 1
          }
        }
        if (!b.hasRemaining) b = refresh(b)
        b put ']'.toByte
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
        var c = if (cb.hasRemaining) cb else refresh(cb)
        c put '['
        if (values.length > 0) {
          c = values(0).jsonChars(c, refresh)
          var i = 1
          while (i < values.length) {
            if (c.remaining < 2) c = refresh(c)
            c put ',' put ' '
            c = values(i).jsonChars(c, refresh)
            i += 1
          }
        }
        if (!c.hasRemaining) c = refresh(c)
        c put ']'
      }
    }
    object All extends JsonBuildTerminator[All] {
      def apply(aj: Array[Json]) = new All(aj)

      def ~(js: Json) = (new Build[All]) ~ js
      def ~(nul: scala.Null) = (new Build[All]) ~ Null
      def ~[A: Jsonize](a: A) = (new Build[All]) ~ a
      def ~~(jses: Array[Json]) = (new Build[All]) ~~ jses
      def ~~[A: Jsonize](as: Array[A]) = (new Build[All]) ~~ as
      def ~~(coll: collection.TraversableOnce[Json]) = (new Build[All]) ~~ coll
      def ~~[A: Jsonize](coll: collection.TraversableOnce[A]) = (new Build[All]) ~~ coll

      class Build[T >: All] {
        private[this] var i = 0
        private[this] var a: Array[Json] = new Array[Json](6)
        private[this] def ensureAtLeast(n: Int) {
          if (n > a.length && a.length < 0x7FFFFFFE) {
            var m = a.length
            while (m < n && (m&0x40000000) == 0) m = ((m << 1) | m) & 0x7FFFFFFE
            if (m < n) m = 0x7FFFFFFE
            a = java.util.Arrays.copyOf(a, m)
          }
        }
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
        def ~~(done: JsonBuildTerminator[T]): T = this ~ done
        def ~~(jses: Array[Json]): this.type = {
          ensureAtLeast(i + jses.length)
          System.arraycopy(jses, 0, a, i, jses.length)
          i += jses.length
          this
        }
        def ~~(jses: Array[Json], i0: Int, iN: Int): this.type = {
          val j0 = math.max(i0, 0)
          val jN = math.max(j0, math.min(jses.length, iN))
          val n = jN - j0
          if (n > 0) {
            ensureAtLeast(i + n)
            System.arraycopy(jses, j0, a, i, n)
            i += n
          }
          this
        }
        def ~~[A](as: Array[A])(implicit jser: Jsonize[A]): this.type = {
          ensureAtLeast(i + as.length)
          var j = 0
          while (j < as.length) {
            a(i) = jser.jsonize(as(j))
            i += 1
            j += 1
          }
          this
        }
        def ~~[A](as: Array[A], i0: Int, iN: Int)(implicit jser: Jsonize[A]): this.type = {
          val j0 = math.max(i0, 0)
          val jN = math.max(j0, math.min(as.length, iN))
          val n = jN - j0
          if (n > 0) {
            ensureAtLeast(i + as.length)
            var j = j0
            while (j < jN) {
              a(i) = jser.jsonize(as(j))
              i += 1
              j += 1
            }
          }
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

    final class Dbl(val doubles: Array[Double]) extends Arr {
      def size = doubles.length
      override def apply(i: Int) =
        if (i < 0 || i >= doubles.length) JastError("bad index "+i)
        else {
          val di = doubles(i)
          if (java.lang.Double.isNaN(di) || java.lang.Double.isInfinite(di)) Null
          else new Num(doubles(i), "")
        }
      override def jsonPretty(pretty: PrettyJson, depth: Int) {
        // TODO--actually make this pretty.
        if (size == 0) pretty append "[]"
        else if (size == 1) {
          pretty append "["
          Num(doubles(0)).jsonString(pretty.lastLine)
          pretty append "]"
        }
        else jsonString(pretty.lastLine)
      }
      override def jsonString(sb: java.lang.StringBuilder) {
        sb append '['
        var i = 0
        while (i < doubles.length) {
          if (i > 0) sb append ", "
          val d = doubles(i)
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) sb append "null"
          else sb append d.toString
          i += 1
        }
        sb append ']'
      }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
        var b = if (bb.hasRemaining) bb else refresh(bb)
        b put '['.toByte
        var i = 0
        while (i < doubles.length) {
          if (i > 0) {
            if (b.remaining < 2) b = refresh(b)
            b put '['.toByte put ' '.toByte
          }
          val d = doubles(i)
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) b = Null.jsonBytes(b, refresh)
          else b = loadByteBuffer(d.toString.getBytes, b, refresh)
          i += 1
        }
        if (!b.hasRemaining) b = refresh(b)
        b put ']'.toByte
      }
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
        var c = if (cb.hasRemaining) cb else refresh(cb)
        c put '['
        var i = 0
        while (i < doubles.length) {
          if (i > 0) {
            if (c.remaining < 2) c = refresh(c)
            c put '[' put ' '
          }
          val d = doubles(i)
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) c = Null.jsonChars(c, refresh)
          else c = loadCharBuffer(d.toString.toCharArray, c, refresh)
          i += 1
        }
        if (!c.hasRemaining) c = refresh(c)
        c put ']'
      }
    }
    object Dbl extends JsonBuildTerminator[Dbl] {
      def apply(xs: Array[Double]): Dbl = new Dbl(xs)
      def apply(xs: Array[Long]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toDouble; i += 1}
        new Dbl(ys)
      }
      def apply(xs: Array[Int]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toDouble; i += 1}
        new Dbl(ys)
      }
      def decimal(xs: Array[Float]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toString.toDouble; i += 1}
        new Dbl(ys)
      }
      def exact(xs: Array[Float]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toString.toDouble; i += 1}
        new Dbl(ys)
      }

      val empty = new Dbl(new Array[Double](0))

      def ~(me: Dbl.type) = empty
      def ~(dbl: Double): Build[Dbl] = (new Build[Dbl]) ~ dbl
      def ~~(me: Dbl.type) = empty
      def ~~(doubles: Array[Double]): Build[Dbl] = (new Build[Dbl]) ~~ doubles
      def ~~(doubles: Array[Double], i0: Int, iN: Int): Build[Dbl] = (new Build[Dbl]) ~~ (doubles, i0, iN)
      def ~~(coll: collection.TraversableOnce[Double]) = (new Build[Dbl]) ~~ coll
      def ~~(existing: Dbl) = (new Build[Dbl]) ~~ existing.doubles

      class Build[T >: Dbl] {
        private[this] var i = 0
        private[this] var a: Array[Double] = new Array[Double](6)
        private[this] def ensureAtLeast(n: Int) {
          if (n > a.length && a.length < 0x7FFFFFFE) {
            var m = a.length
            while (m < n && (m&0x40000000) == 0) m = ((m << 1) | m) & 0x7FFFFFFE
            if (m < n) m = 0x7FFFFFFE
            a = java.util.Arrays.copyOf(a, m)
          }
        }
        def ~(done: JsonBuildTerminator[T]): T =
          new Dbl(if (i==a.length) a else java.util.Arrays.copyOf(a, i))
        def ~(dbl: Double): this.type = {
          if (i >= a.length) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
          a(i) = dbl
          i += 1
          this
        }
        def ~~(done: JsonBuildTerminator[T]): T = this ~ done
        def ~~(doubles: Array[Double]): this.type = {
          ensureAtLeast(i + doubles.length)
          System.arraycopy(doubles, 0, a, i, doubles.length)
          i += doubles.length
          this
        }
        def ~~(doubles: Array[Double], i0: Int, iN: Int): this.type = {
          val j0 = math.max(i0, 0)
          val jN = math.max(j0, math.min(doubles.length, iN))
          val n = jN - j0
          if (n > 0) {
            ensureAtLeast(i + n)
            System.arraycopy(doubles, j0, a, i, n)
            i += n
          }
          this
        }
        def ~~(coll: collection.TraversableOnce[Double]): this.type = {
          coll.foreach(this ~ _)
          this
        }
        def ~~(existing: Dbl): this.type = this ~~ existing.doubles

        def toAll[U >: All](that: All.Build[U]): All.Build[U] = {
          var j = 0
          while (j < i) {
            if (java.lang.Double.isNaN(a(j)) && java.lang.Double.isInfinite(a(j))) that ~ Null
            else that ~ (new Num(a(j), ""))
            j += 1
          }
          that
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

  sealed trait Obj extends Json {
    protected def myName = "object"
    def simple = false
    def size: Int
    def hasDuplicateKeys: Boolean
    def get(key: String): Option[Json]
    def foreach[U](f: (String, Json) => U): Unit
    def asMap: collection.Map[String, Json]
    def asFlatArray: Array[AnyRef]
  }
  private[jsonal] final class AtomicObj(val underlying: Array[AnyRef], myMap: collection.Map[String, Json] = null)
  extends java.util.concurrent.atomic.AtomicReference[collection.mutable.AnyRefMap[String, Json]](null) with Obj {
    private[this] def linearSearchOrNull(key: String): Json = {
      var i = 0
      while (i < underlying.length - 1) {
        if (key == (underlying(i) match { case s: String => s; case x => x.asInstanceOf[Str].text })) return underlying(i+1).asInstanceOf[Json]
        i += 2
      }
      null
    }
    def getOrNull(key: String): Json = {
      if (myMap ne null) myMap.getOrElse(key, null)
      else if (size < 6) linearSearchOrNull(key)
      else {
        var m = get()
        var gotMap = false
        do {
          if (m eq Obj.mapBuildingInProcess) linearSearchOrNull(key)
          else if (m eq null) {
            if (compareAndSet(null, Obj.mapBuildingInProcess)) {
              val arm = new collection.mutable.AnyRefMap[String, Json]()
              var i = 0
              while (i < underlying.length - 1) {
                arm += (underlying(i) match { case s: String => s; case x => x.asInstanceOf[Str].text }, underlying(i+1).asInstanceOf[Json])
                i += 2
              }
              set(arm)
              return arm.getOrNull(key)
            }
            else m = get()
          }
          else gotMap = true
        } while (!gotMap)
        m.getOrNull(key)
      }
    }
    def size = if (myMap eq null) underlying.size >> 1 else myMap.size
    override def apply(key: String): Jast = {
      val ans = getOrNull(key)
      if (ans eq null) JastError("no key: " + key) else ans
    }
    def hasDuplicateKeys: Boolean = size > 1 && (myMap eq null) && {
      if (size < 6) {
        var i = 2
        while (i < underlying.length -1) {
          val key = underlying(i) match { case s: String => s; case x => x.asInstanceOf[Str].text }
          var j = 0
          while (j < i) {
            if (key == (underlying(j) match { case s: String => s; case x => x.asInstanceOf[Str].text })) return true
            j += 2
          }
          i += 2
        }
        false
      }
      else {
        var m = get()
        if ((m eq null) || (m eq Obj.mapBuildingInProcess)) {
          val useWhatWeMake = compareAndSet(null, Obj.mapBuildingInProcess)
          val arm = new collection.mutable.AnyRefMap[String, Json]()
          var i = 0
          while (i < underlying.length - 1) {
            arm += (underlying(i) match { case s: String => s; case x => x.asInstanceOf[Str].text }, underlying(i+1).asInstanceOf[Json])
            i += 2
          }
          if (useWhatWeMake) set(arm)
          m = arm
        }
        m.size != size
      }
    }
    def get(key: String): Option[Json] = Option(getOrNull(key))
    def foreach[U](f: (String, Json) => U) {
      if (myMap eq null) {
        var i = 0
        while (i < underlying.length - 1) {
          f(underlying(i) match { case s: String => s; case x => x.asInstanceOf[Str].text }, underlying(i+1).asInstanceOf[Json])
          i += 2
        }
      }
      else myMap.foreach{ case (k,v) => f(k,v) }
    }
    def asMap: collection.Map[String, Json] = 
      if (myMap ne null) myMap
      else {
        val m = get()
        if (size < 6 || (m eq null) || (m eq Obj.mapBuildingInProcess)) {
          val useWhatWeMake = (size >= 6) && compareAndSet(null, Obj.mapBuildingInProcess)
          val arm = new collection.mutable.AnyRefMap[String, Json]()
          var i = 0
          while (i < underlying.length - 1) {
            arm += (underlying(i) match { case s: String => s; case x => x.asInstanceOf[Str].text }, underlying(i+1).asInstanceOf[Json])
            i += 2
          }
          if (useWhatWeMake) set(arm)
          arm        
        }
        else m
      }
    def asFlatArray: Array[AnyRef] =
      if (underlying ne null) underlying
      else {
        val a = new Array[AnyRef](myMap.size * 2)
        var i = 0
        myMap.foreach{ case (k,v) => a(i) = k; a(i+1) = v; i += 2 }
        a
      }
    private def jsonStringFromMap(sb: java.lang.StringBuilder) {
      if (myMap.size > 1) sb append "{ " else sb append '{'
      var first = true
      val it = myMap.iterator
      while (it.hasNext) {
        val kv = it.next
        if (!first) sb append ", "
        Str(kv._1).jsonString(sb)
        sb append ':'
        kv._2.jsonString(sb)
        first = false
      }
      if (myMap.size > 1) sb append " }" else sb append '}'
    }
      override def jsonPretty(pretty: PrettyJson, depth: Int) {
        // TODO--actually make this pretty.
        if (size == 0) pretty append "{}"
        else jsonString(pretty.lastLine)
      }
    override def jsonString(sb: java.lang.StringBuilder) {
      if (underlying ne null) {
        if (underlying.length > 3) sb append "{ " else sb append '{'
        var i = 0
        while (i < underlying.length-1) {
          if (i > 0) sb append ", "
          (underlying(i) match { case s: Str => s; case x => Str(x.asInstanceOf[String]) }).jsonString(sb)
          sb append ':'
          underlying(i+1).asInstanceOf[Json].jsonString(sb)
          i += 2
        }
        if (i > 2) sb append " }" else sb append '}'
      }
      else jsonStringFromMap(sb)
    }
    private def jsonBytesFromMap(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = {
      var b = if (bb.remaining >= 2) bb else refresh(bb)
      b put '{'.toByte
      if (myMap.size > 1) b put ' '.toByte
      var first = true
      val it = myMap.iterator
      while (it.hasNext) {
        val kv = it.next
        if (!first) {
          if (bb.remaining < 2) b = refresh(b)
          b put ','.toByte put ' '.toByte
        }
        b = Str(kv._1).jsonBytes(b, refresh)
        if (!bb.hasRemaining) b = refresh(b)
        b = kv._2.jsonBytes(b, refresh)
        first = false
      }
      if (myMap.size > 1) {
        if (b.remaining < 2) b = refresh(b)
        b put ' '.toByte put '}'.toByte
      }
      else {
        if (!b.hasRemaining) b = refresh(b)
        b put '}'.toByte
      }
    }
    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
      if (underlying ne null) {
        var b = if (bb.remaining >= 2) bb else refresh(bb)
        b put '{'.toByte
        if (underlying.length > 3) b put ' '.toByte
        var i = 0
        while (i < underlying.length-1) {
          if (i > 0) {
            if (b.remaining < 2) b = refresh(b)
            b put ','.toByte put ' '.toByte
          }
          else {
            if (!b.hasRemaining) b = refresh(b)
            b put ' '.toByte
          }
          b = (underlying(i) match { case s: Str => s; case x => Str(x.asInstanceOf[String]) }).jsonBytes(b, refresh)
          if (!b.hasRemaining) b = refresh(b)
          b put ':'.toByte
          underlying(i+1).asInstanceOf[Json].jsonBytes(b, refresh)
          i += 2
        }
        if (underlying.length > 3) {
          if (b.remaining < 2) b = refresh(b)
          b put ' '.toByte put '}'.toByte
        }
        else {
          if (!b.hasRemaining) b = refresh(b)
          b put '}'.toByte
        }        
      }
      else jsonBytesFromMap(bb, refresh)
    private def jsonCharsFromMap(bb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = {
      var b = if (bb.remaining >= 2) bb else refresh(bb)
      b put '{'
      if (myMap.size > 1) b put ' '
      var first = true
      val it = myMap.iterator
      while (it.hasNext) {
        val kv = it.next
        if (!first) {
          if (bb.remaining < 2) b = refresh(b)
          b put ',' put ' '
        }
        b = Str(kv._1).jsonChars(b, refresh)
        if (!bb.hasRemaining) b = refresh(b)
        b = kv._2.jsonChars(b, refresh)
        first = false
      }
      if (myMap.size > 1) {
        if (b.remaining < 2) b = refresh(b)
        b put ' ' put '}'
      }
      else {
        if (!b.hasRemaining) b = refresh(b)
        b put '}'
      }
    }
    override def jsonChars(bb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
      if (underlying ne null) {
        var b = if (bb.remaining >= 2) bb else refresh(bb)
        b put '{'
        if (underlying.length > 3) b put ' '
        var i = 0
        while (i < underlying.length-1) {
          if (i > 0) {
            if (b.remaining < 2) b = refresh(b)
            b put ',' put ' '
          }
          b = (underlying(i) match { case s: Str => s; case x => Str(x.asInstanceOf[String]) }).jsonChars(b, refresh)
          if (!b.hasRemaining) b = refresh(b)
          b put ':'
          b = underlying(i+1).asInstanceOf[Json].jsonChars(b, refresh)
          i += 2
        }
        if (underlying.length > 3) {
          if (b.remaining < 2) b = refresh(b)
          b put ' ' put '}'
        }
        else {
          if (!b.hasRemaining) b = refresh(b)
          b put '}'
        }
      }
      else jsonCharsFromMap(bb, refresh)
  }
  object Obj extends FromJson[Obj] with JsonBuildTerminator[Obj] {
    val empty: Obj = new AtomicObj(Array())
    private[jsonal] val mapBuildingInProcess = new collection.mutable.AnyRefMap[String, Json]()

    def apply(kvs: collection.Map[String, Json]) = new AtomicObj(null, kvs)
    def apply(keys: Array[String], values: Array[Json]): Jast = 
      if (keys.length != values.length) JastError("cannot create object with unequal numbers of keys and values")
      else {
        val a = new Array[AnyRef](keys.length*2)
        var i, j = 0
        while (i < keys.length) {
          a(j) = keys(i)
          a(j+1) = values(i)
          j += 2
          i += 1
        }
        new AtomicObj(a)
      }

    def ~(done: Obj.type) = empty
    def ~(key: Str, nul: scala.Null) = (new Build[Obj]) ~ (key, nul)
    def ~(key: String, nul: scala.Null) = (new Build[Obj]) ~ (key, nul)
    def ~(key: Str, js: Json) = (new Build[Obj]) ~ (key, js)
    def ~(key: String, js: Json) = (new Build[Obj]) ~ (key, js)
    def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]) = (new Build[Obj]) ~ (key, jser.jsonize(a))
    def ~[A](key: String, a: A)(implicit jser: Jsonize[A]) = (new Build[Obj]) ~ (key, jser.jsonize(a))
    def ~~(done: Obj.type) = empty
    def ~~(coll: collection.TraversableOnce[(Str,Json)]) = (new Build[Obj]) ~~ coll
    def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= String) = (new Build[Obj]) ~~ coll
    def ~~[A: Jsonize](coll: collection.TraversableOnce[(Str,A)]) = (new Build[Obj]) ~~ coll
    def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= String) = (new Build[Obj]) ~~ coll

    class Build[T >: Obj] {
      private[this] var i = 0
      private[this] var a: Array[AnyRef] = new Array[AnyRef](6)
      private[this] def append(key: AnyRef, js: Json): this.type = {
        if (i >= a.length-1) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
        a(i) = key
        a(i+1) = js
        i += 2
        this
      }
      def ~(done: JsonBuildTerminator[T]): T =
        new AtomicObj(if (i==a.length) a else java.util.Arrays.copyOf(a, i), null)
      def ~(key: Str, js: Json): this.type = append(key, js)
      def ~(key: String, js: Json): this.type = append(key, js)
      def ~(key: Str, nul: scala.Null): this.type = append(key, Null)
      def ~(key: String, nul: scala.Null): this.type = append(key, Null)
      def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]): this.type = this ~ (key, jser.jsonize(a))
      def ~[A](key: String, a: A)(implicit jser: Jsonize[A]): this.type = this ~ (key, jser.jsonize(a))
      def ~~(done: JsonBuildTerminator[T]): T = this ~ done
      def ~~(coll: collection.TraversableOnce[(Str,Json)]): this.type = {
        coll.foreach{ case (k,v) => this ~ (k,v) }
        this
      }
      def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= String): this.type = {
        coll.foreach{ case (k,v) => this ~ (ev(k),v) }
        this
      }
      def ~~[A: Jsonize](coll: collection.TraversableOnce[(Str,A)]): this.type = {
        coll.foreach{ case (k,a) => this ~ (k,a) }
        this
      }
      def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= String): this.type = {
        coll.foreach{ case (k,a) => this ~ (ev(k),a) }
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
