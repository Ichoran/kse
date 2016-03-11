// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

// Warning: this is a LONG file because of all the nested sealed traits!

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


/** Represents an object that can be mapped to JSON without error.
  * Methods for serializing the JSON representation are also provided.
  */
trait AsJson {
  /** The JSON representation of this object. */
  def json: Json

  /** Accumulate the serialized JSON representation of this object in a prettyprinter. */
  def jsonPretty(pretty: PrettyJson, depth: Int) { json.jsonPretty(pretty, depth) }

  /** Accumulate the serialized JSON representation of this object in a Java StringBuilder. */
  def jsonString(sb: java.lang.StringBuilder) { json.jsonString(sb) }

  /** Accumulate the serialized JSON representation of this object in a ByteBuffer.
    *
    * Note: if the `ByteBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 bytes more will be made available per call.
    */
  def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = json.jsonBytes(bb, refresh)

  /** Accumulate the serialized JSON representation of this object in a CharBuffer.
    *
    * Note: if the `CharBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 chars more will be made available per call.
    */
  def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = json.jsonChars(cb, refresh)
}


/** A type class that provides JSONizing functionality for a class. */
trait Jsonize[A] {
  /** Convert the object to its JSON representation. */
  def jsonize(a: A): Json

  /** Accumulate the serialized JSON representation of the object in a prettyprinter. */
  def jsonizePretty(a: A, pretty: PrettyJson, depth: Int) { jsonize(a).jsonPretty(pretty, depth) }

  /** Accumulate the serialized JSON representation of the object in a Java StringBuilder. */
  def jsonizeString(a: A, sb: java.lang.StringBuilder) { jsonize(a).jsonString(sb) }

  /** Accumulate the serialized JSON representation of the object in a ByteBuffer.
    *
    * Note: if the `ByteBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 bytes more will be made available per call.
    */
  def jsonizeBytes(a: A, bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = jsonize(a).jsonBytes(bb, refresh)

  /** Accumulate the serialized JSON representation of this object in a CharBuffer.
    *
    * Note: if the `CharBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 chars more will be made available per call.
    */
  def jsonizeChars(a: A, cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = jsonize(a).jsonChars(cb, refresh)
}

/** Classes or objects implementing this trait are able to deserialize objects from a JSON representation. */
trait FromJson[A] {
  /** Recover the object from its JSON representation. */
  def parse(input: Json): Either[JastError, A]

  /** Deserialize the object from a string containing its JSON representation, keeping track of the end of parsing. */
  def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Either[JastError, A] = Json.parse(input, i0, iN, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Deserialize the object from a string containing its JSON representation. */
  def parse(input: String): Either[JastError, A] = parse(input, 0, input.length, new FromJson.Endpoint(0))

  /** Deserialize the object from a ByteBuffer containing its JSON representation. */
  def parse(input: ByteBuffer): Either[JastError, A] = Json.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Deserialize the object from a CharBuffer containing its JSON representation. */
  def parse(input: CharBuffer): Either[JastError, A] = Json.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Deserialize the object from a InputStream containing its JSON representation, keeping track of the end of parsing. */
  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Either[JastError, A] = Json.parse(input, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }
}
object FromJson {
  /** A utility class that holds the last index of a input used for JSON parsing */
  case class Endpoint(var index: Long) {}
}

/** Jast is a Json Abstract Syntax Tree.
  *
  * It is the parent of both parse errors (`JastError`) and valid JSON values (`Json`).
  *
  * Because accessor methods are provided on `Jast`, and errors in access produce `JastError`, safe destructuring is easy:
  *
  * {{{
  * val maybeJson = Jast.parse("""{"red":[true, {"dish":27.5}], "blue":"fish"}""")
  * val redDish = maybeJson("red")(1)("dish")    // JSON representation of 27.5
  * val wrong   = maybeJson("blue")(3)("fish")   // JastError("Indexing into string")
  * val dishTwo = maybeJson \ "red" \ 1 \ "dish" // Alternative lookup syntax
  * }}}
  */
sealed trait Jast {
  /** True if and only if this represents a simple JSON value (null, true, false, number, or String) */
  def simple: Boolean

  /** True only if this is a JSON null */
  def isNull: Boolean

  /** Double representation of a JSON number, if it is a number; special NaN value otherwise.
    *
    * Note: test whether it was a JSON number using `Json.wasNumber(x)`
    */
  def double: Double

  /** Boolean value corresponding to this JSON object if it is a boolean JSON; `None` otherwise. */
  def bool: Option[Boolean]

  /** String corresponding to this JSON object if it is a JSON string, `None` otherwise. */
  def string: Option[String]

  /** Index into JSON array, if this is a JSON array.  Returns `JastError` otherwise. */
  def apply(i: Int): Jast

  /** Lookup of key in JSON object, if this is a JSON object.  Returns `JastError` if key not found or if this is not a JSON object. */
  def apply(key: String): Jast

  /** Lift-style alias for array lookup. */
  @inline final def \(i: Int): Jast = this apply i

  /** Lift-style alias for object key lookup. */
  @inline final def \(key: String): Jast = this apply key
}
/** High-level routines for converting strings and other serial formats into JSON ASTs. */
object Jast {
  /** Parse a portion of a string into a JSON AST, keeping track of the last parsed index. */
  def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Jast = JsonStringParser.Json(input, i0, iN, ep) match { case Left(e) => e; case Right(j) => j }

  /** Parse a string into a JSON AST.
    *
    * Note: if the JSON ends before the String does, that is not considered an error.
    */
  def parse(input: String): Jast = parse(input, 0, input.length, null)

  /** Parse from a `ByteBuffer` into a JSON AST.  TODO: actually implement this! */
  def parse(input: ByteBuffer): Jast = JsonByteBufferParser.Json(input) match { case Left(e) => e; case Right(j) => j }

  /** Parse from a `CharBuffer` into a JSON AST.  TODO: actually implement this! */
  def parse(input: CharBuffer): Jast = JsonCharBufferParser.Json(input) match { case Left(e) => e; case Right(j) => j }

  /** Parse from an `InputStream` into a JSON AST, keeping track of the last parsed index.  TODO: actually implement this! */
  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Jast = JsonInputStreamParser.Json(input, ep) match { case Left(e) => e; case Right(j) => j }

  /** Parse from an `InputStream` into a JSON AST.  TODO: actually implement this! */
  def parse(input: java.io.InputStream): Jast = parse(input, null)
}

/** Representation of an error that occured during JSON parsing or access. */
final case class JastError(msg: String, where: Long = -1L, because: Jast = Json.Null) extends Jast {
  def simple = false
  def isNull = false
  def double = Json.not_a_normal_NaN
  def bool = None
  def string = None
  def apply(i: Int) = this
  def apply(key: String) = this
}

/** This marker trait indicates that a JSON item being built should use type `T` as a stop token. */
trait JsonBuildTerminator[T] {}

/** Json is a JSON value.
  *
  * The subtypes of Json value are: `Json.Null`, `Json.Bool` (subdivided into `Json.Bool.True` and `Json.Bool.False`),
  * `Json.Str`, `Json.Num`, `Json.Arr`, and `Json.Obj`.
  *
  * Note: `Json.Arr` is further subdivided into `Json.Arr.All` and `Json.Arr.Dbl` for non-numeric and numeric arrays
  * respectively.
  *
  * See [[Jast]] for syntax to access array entries, look up maps, and so on.
  */
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
/** The companion object for Json provides a variety of ways to construct JSON values.
  *
  * Parsing: from strings, byte and char buffers, and input streams.
  *
  * One-shot creation: `apply` methods on corresponding Scala types (map -> JSON object, etc)
  *
  * Builders: Use syntax `Json ~ thing ~ another ~ another ~ Json` to build arrays; use
  * `Json ~ ("key", value1) ~ ("key2", value2) ~ Json` to build objects.  See `~` and `~~`
  * methods for more details.
  */
object Json extends FromJson[Json] with JsonBuildTerminator[Json] {
  private[jsonal] val not_a_normal_NaN_bits: Long = 0x7FF9000000000000L
  private[jsonal] val not_a_normal_NaN = java.lang.Double.longBitsToDouble(not_a_normal_NaN_bits)

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


  /** Tests whether a NaN `Double` reflects that a numeric value was absent, or is the best `Double` representation of the value.
    *
    * Note that `null` is used to correspond to `Double.NaN`, so `wasNumber` will be `true` for JSON null values!
    */
  def wasNumber(x: Double) = java.lang.Double.doubleToRawLongBits(x) != Json.not_a_normal_NaN_bits


  /** Returns an empty JSON value (in this case, a JSON null) */
  def apply(): Json = Null

  /** Returns a JSON boolean value */
  def apply(b: Boolean): Json = if (b) Bool.True else Bool.False

  /** Returns the JSON representation of a string */
  def apply(s: String): Json = Str(s)

  /** Returns the JSON representation of a Long */
  def apply(l: Long): Json = Num(l)

  /** Returns the JSON representation of a Double.  NaN and +-Infinty are mapped to JSON null. */
  def apply(d: Double): Json = Num(d)

  /** Returns the JSON representation of a BigDecimal. */
  def apply(bd: BigDecimal): Json = Num(bd)

  /** Wraps an array of `Json` (JSON values) as a JSON array */
  def apply(aj: Array[Json]): Json = Arr.All(aj)

  /** Wraps an array of doubles as a JSON array */
  def apply(xs: Array[Double]): Json = Arr.Dbl(xs)

  /** Wraps a String-Json map as a JSON object */
  def apply(kvs: Map[String, Json]): Json = Obj(kvs)

  /** Wraps an array of string keys and Json values as a JSON object.
    *
    * @return - A `Json.Obj`, if the key and value arrays were the same length, or `JastError` if not
    */
  def apply(keys: Array[String], values: Array[Json]): Jast = Obj(keys, values)


  /** Build an array of doubles as a JSON array, starting with this value */
  def ~(dbl: Double): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~ dbl

  /** Build an array of doubles as a JSON array, starting with this array */
  def ~~(doubles: Array[Double]): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~~ doubles

  /** Build an array of doubles as a JSON array, starting with this segment of this array */
  def ~~(doubles: Array[Double], i0: Int, iN: Int): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~~ (doubles, i0, iN)

  /** Build an array of doubles as a JSON array, starting with this collection */
  def ~~(coll: collection.TraversableOnce[Double]) = (new Arr.Dbl.Build[Json]) ~~ coll

  /** Build an array of doubles as a JSON array, starting with this JSON array of doubles */
  def ~~(existing: Arr.Dbl) = (new Arr.Dbl.Build[Json]) ~~ existing


  /** Build a JSON array, starting with a JSON value */
  def ~(js: Json) = (new Arr.All.Build[Json]) ~ js

  /** Build a JSON array, starting with a JSON null */
  def ~(nul: scala.Null) = (new Arr.All.Build[Json]) ~ Null

  /** Build an JSON array, starting with an object that can be converted to JSON */
  def ~[A: Jsonize](a: A) = (new Arr.All.Build[Json]) ~ a

  /** Build a JSON array, starting with an array of JSON values */
  def ~~(jses: Array[Json]) = (new Arr.All.Build[Json]) ~~ jses

  /** Build a JSON array, starting with a segment of an array of JSON values */
  def ~~(jses: Array[Json], i0: Int, iN: Int) = (new Arr.All.Build[Json]) ~~ (jses, i0, iN)

  /** Build a JSON array, starting with an array of objects that can be converted to JSON */
  def ~~[A: Jsonize](as: Array[A]) = (new Arr.All.Build[Json]) ~~ as

  /** Build a JSON array, starting with a segment of an array of objects that can be converted to JSON */
  def ~~[A: Jsonize](as: Array[A], i0: Int, iN: Int) = (new Arr.All.Build[Json]) ~~ (as, i0, iN)

  /** Build a JSON array, starting with a collection of JSON values */
  def ~~(coll: collection.TraversableOnce[Json]) = (new Arr.All.Build[Json]) ~~ coll

  /** Build a JSON array, starting with a collection of objects that can be converted to JSON */
  def ~~[A: Jsonize](coll: collection.TraversableOnce[A]) = (new Arr.All.Build[Json]) ~~ coll


  /** Build a JSON object, starting with this JSON string key and a JSON null value */
  def ~(key: Str, nul: scala.Null) = (new Obj.Build[Json]) ~ (key, nul)

  /** Build a JSON object, starting with this string key and a JSON null value */
  def ~(key: String, nul: scala.Null) = (new Obj.Build[Json]) ~ (key, nul)

  /** Build a JSON object starting with this JSON string key and JSON value */
  def ~(key: Str, js: Json) = (new Obj.Build[Json]) ~ (key, js)

  /** Build a JSON object starting with this string key and JSON value */
  def ~(key: String, js: Json) = (new Obj.Build[Json]) ~ (Str(key), js)

  /** Build a JSON object starting with this JSON string key and an object that can be converted to JSON */
  def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]) = (new Obj.Build[Json]) ~ (key, jser.jsonize(a))

  /** Build a JSON object starting with this string key and an object that can be converted to JSON */
  def ~[A](key: String, a: A)(implicit jser: Jsonize[A]) = (new Obj.Build[Json]) ~ (Str(key), jser.jsonize(a))

  /** Build a JSON object starting with the string / JSON value pairs in this collection */
  def ~~(coll: collection.TraversableOnce[(String,Json)]) = (new Obj.Build[Json]) ~~ coll

  /** Build a JSON object starting with the JSON string / JSON value pairs in this collection */
  def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= Str) = (new Obj.Build[Json]) ~~ coll

  /** Build a JSON object starting with the string / JSON-convertible-object pairs in this collection */
  def ~~[A: Jsonize](coll: collection.TraversableOnce[(String,A)]) = (new Obj.Build[Json]) ~~ coll

  /** Build a JSON object starting with the JSON string / JSON-convertible-object pairs in this collection */
  def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= Str) = (new Obj.Build[Json]) ~~ coll


  /** Parse this JSON value as itself.
    *
    * Note: a `Left(JastError)` is never actually produced.
    */
  def parse(input: Json): Either[JastError, Json] = Right(input)

  override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = JsonStringParser.Json(input, i0, iN, ep)
  override def parse(input: ByteBuffer) = JsonByteBufferParser.Json(input)
  override def parse(input: CharBuffer) = JsonCharBufferParser.Json(input)
  override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = JsonInputStreamParser.Json(input, ep)


  /** Represents a JSON null.  Only a single instance actually exists. */
  sealed abstract class Null extends Json {}
  /** The unique instance of the JSON null type. */
  final object Null extends Null with FromJson[Null] {
    final private[this] val myBytesSayNull = "null".getBytes
    final private[this] val myCharsSayNull = "null".toCharArray
    protected def myName = "null"

    override def isNull = true

    def simple = true

    override def double = Double.NaN

    override def jsonString(sb: java.lang.StringBuilder) { sb append "null" }

    override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
      (if (bb.remaining < 4) refresh(bb) else bb) put myBytesSayNull

    override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
      (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayNull

    override def toString = "null"

    /** Returns a `JastError` if this is not a JSON null */
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


  /** Represents a JSON boolean value (true or false) */
  sealed abstract class Bool extends Json { 
    protected def myName = "boolean"
    def simple = true
    def value: Boolean
  }
  /** Contains the two unique instances of JSON boolean types: `Bool.True` and `Bool.False` */
  object Bool extends FromJson[Bool] { 
    final private val myBytesSayTrue = "true".getBytes
    final private val myBytesSayFalse = "false".getBytes
    final private val myCharsSayTrue = "true".toCharArray
    final private val myCharsSayFalse = "false".toCharArray

    /** Creates a `Json.Bool` from a boolean */
    def apply(b: Boolean): Bool = if (b) True else False

    /** Pattern-matcher for JSON boolean values */
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

    /** Returns a `Left(JastError)` unless the input is a JSON boolean */
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


  /** Represents a JSON string value.  The value is stored as a plain String and encoded for serialization on demand. */
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
    /** Returns `Left(JastError)` unless the input is a `Json.Str` */
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


  /** Represents a JSON number.
    *
    * There is a mismatch between machine-level floating point, which is finite
    * precision but admits `NaN` and infinite values, and JSON numbers, which
    * are arbitrary precision but finite.
    *
    * Because of this, `Json.Num` stores not only the `Double` representation of
    * a number, but also the text representation if needed.  The internal encoding
    * also allows `Json.Num` to efficiently represent `Long`s.
    */
  class Num private[jsonal] (content: Double, text: String) extends Json {
    protected def myName = "number"
    def simple = true

    /** Returns `true` if this number is represented by a finite `Double` value */
    def isDouble: Boolean = (text ne null) && !java.lang.Double.isNaN(content) && !java.lang.Double.isInfinite(content)

    /** The `Double` value corresponding to this JSON number */
    override def double = if (text eq null) java.lang.Double.doubleToRawLongBits(content).toDouble else content

    /** Returns `true` if this number is represented exactly by a `Long` value */
    def isLong: Boolean = (text eq null) || (content.toLong == content)

    /** Returns a `Long` value corresponding to this JSON number.
      * Values too large end up as `Long.MaxValue`, those to small as `Long.MinValue`, and those
      * that are `NaN` end up as `0L`.  Fractional values are rounded as with `Double`'s `.toLong` method.
      */
    def long: Long = if (text eq null) java.lang.Double.doubleToRawLongBits(content) else content.toLong

    /** Returns a `Long` if this JSON number is exactly represented by a `Long`, or a fallback value if not */
    def longOr(fallback: Long): Long =
      if (text eq null) java.lang.Double.doubleToRawLongBits(content)
      else {
        val l = content.toLong
        if (l == content) l else fallback
      }

    /** Returns a BigDecimal value that corresponds to this JSON number.
      *
      * Note: this operation is not cached, so performance may suffer if it is called repeatedly.
      */
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

  /** The companion object to `Json.Num` provides apply methods to construct `Json.Num` instances.
    *
    * The pattern-matcher (`unapply`) extracts only the `Double` representation.  To obtain other
    * representations, use a match statement:
    *
    * {{{
        val big = BigDecimal("23984712893471894798123748921471892347128974")
        val j = Json.Num(big): Json
        j match {
          case n: Json.Num => n.big == big
          case _ => ???
        }
        j match {
          case Json.Num(n) => n == big  // false, Double does not have enough precision
          case _ => ???
        }
    * }}}
    */
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

    /** Return the JSON number corresponding to this Long */
    def apply(l: Long): Num = new Num(java.lang.Double.longBitsToDouble(l), null)

    /** Return the JSON number corresponding to this Double, or a JSON null if the Double is infinite or NaN */
    def apply(d: Double): Json = if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) Null else new Num(d, "")

    /** Return the JSON number corresponding to this BigDecimal */
    def apply(bd: BigDecimal): Num = new Num(bd.doubleValue, bd.toString)

    def unapply(js: Json): Option[Double] = js match {
      case n: Json.Num => Some(n.double)
      case _: Json.Null => Some(Double.NaN)
      case _ => None
    }

    /** Returns `Left(JastError)` unless the input is a `Json.Num` */    
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


  /** `Json.Arr` represents JSON arrays.  Due to the inefficiency of boxing numeric data, it has
    * two implementation classes: `Json.Arr.All` which holds arrays of arbitrary JSON values,
    * and `Json.Arr.Dbl` which holds only numeric values, and then only in `Double`s.
    */
  sealed trait Arr extends Json {
    protected def myName = "array"
    def simple = false

    /** The length of this array. */
    def size: Int
  }

  /** The companion object to `Json.Arr` provides ways to parse, construct, and build JSON
    * arrays.
    */
  object Arr extends FromJson[Arr] with JsonBuildTerminator[Arr] {
    /** Wraps an array of JSON values as a JSON array */
    def apply(aj: Array[Json]): Arr = All(aj)

    /** Wraps an array of Double values as a JSON array */
    def apply(xs: Array[Double]): Arr = Dbl(xs)

    /** Wraps an array of `Float` values as a JSON array, converting `Float` to `Double` via the closest decimal representation */
    def decimal(xs: Array[Float]): Arr = Dbl decimal xs

    /** Wraps an array of `Float` values as a JSON array, converting `Float` to `Double` via the closest binary representation.
      * Compared to `decimal`, this method is faster but will produce `Double` values that are slightly off from a decimal
      * representation, and thus will enlarge the size of a serialized version of the JSON array.
      */
    def exact(xs: Array[Float]): Arr = Dbl exact xs

    /** Creates an empty JSON array */
    def ~(me: Arr.type) = Dbl.empty

    /** Starts building a double-valued JSON array, beginning with this double value */
    def ~(dbl: Double): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~ dbl

    /** Creates an empty JSON array */
    def ~~(me: Arr.type) = Dbl.empty

    /** Starts building a double-valued JSON array, beginning with an array of doubles */
    def ~~(doubles: Array[Double]): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~~ doubles

    /** Starts building a double-valued JSON array, beginning with a segment of this array of doubles */
    def ~~(doubles: Array[Double], i0: Int, iN: Int): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~~ (doubles, i0, iN)

    /** Starts building a double-valued JSON array, beginning with the contents of this collection */
    def ~~(coll: collection.TraversableOnce[Double]) = (new Dbl.Build[Arr]) ~~ coll

    /** Starts building a double-valued JSON array, beginning with an existing double-valued JSON array */
    def ~~(existing: Dbl) = (new Dbl.Build[Arr]) ~~ existing


    /** Starts building a generic JSON array, beginning with this JSON value */
    def ~(js: Json) = (new All.Build[Arr]) ~ js

    /** Starts building a generic JSON array, beginning with null.*/
    def ~(nul: scala.Null) = (new All.Build[Arr]) ~ Null

    /** Starts building a generic JSON array, beginning with an object that can be converted to JSON */
    def ~[A: Jsonize](a: A) = (new All.Build[Arr]) ~ a

    /** Starts building a generic JSON array, beginning with an array of JSON values */
    def ~~(jses: Array[Json]) = (new All.Build[Arr]) ~~ jses

    /** Starts building a generic JSON array, beginning with a segment of an array of JSON values */
    def ~~(jses: Array[Json], i0: Int, iN: Int) = (new All.Build[Arr]) ~~ (jses, i0, iN)

    /** Starts building a generic JSON array, beginning with an array of objects that can be converted to JSON */
    def ~~[A: Jsonize](as: Array[A]) = (new All.Build[Arr]) ~~ as

    /** Starts building a generic JSON array, beginning with a segment of an array of objects that can be converted to JSON */
    def ~~[A: Jsonize](as: Array[A], i0: Int, iN: Int) = (new All.Build[Arr]) ~~ (as, i0, iN)

    /** Starts building a generic JSON array, beginning with the contents of this collection */
    def ~~(coll: collection.TraversableOnce[Json]) = (new All.Build[Arr]) ~~ coll

    /** Starts building a generic JSON array, beginning with the objects in this collection upon conversion to JSON */
    def ~~[A: Jsonize](coll: collection.TraversableOnce[A]) = (new All.Build[Arr]) ~~ coll


    /** Represents a JSON array of not-only-numeric values */
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

    /** The companion object to `Json.Arr.All` provides parsing, construction, and building methods for
      * arbitrary JSON arrays.
      */
    object All extends JsonBuildTerminator[All] {
      /** Creates JSON array from an array of JSON values */
      def apply(aj: Array[Json]) = new All(aj)

      /** Creates a new builder for JSON arrays */
      def builder = new Build[All]

      /** The empty generic JSON array */
      val empty = new All(new Array(0))


      /** Begin building a JSON array, starting with one JSON value */
      def ~(js: Json) = (new Build[All]) ~ js

      /** Begin building a JSON array, starting with a JSON null */
      def ~(nul: scala.Null) = (new Build[All]) ~ Null

      /** Begin building a JSON array, starting with an object that can be converted to JSON */
      def ~[A: Jsonize](a: A) = (new Build[All]) ~ a

      /** Begin building a JSON array, starting with an array of JSON values */
      def ~~(jses: Array[Json]) = (new Build[All]) ~~ jses

      /** Begin building a JSON array, starting with an array of objects that can be converted to JSON */
      def ~~[A: Jsonize](as: Array[A]) = (new Build[All]) ~~ as

      /** Begin building a JSON array, starting with a collection of JSON values */
      def ~~(coll: collection.TraversableOnce[Json]) = (new Build[All]) ~~ coll

      /** Begin building a JSON array, starting with a collection of objects that can be converted to JSON values */
      def ~~[A: Jsonize](coll: collection.TraversableOnce[A]) = (new Build[All]) ~~ coll


      /** A builder for JSON arrays that consist of JSON values */
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
        /** Complete building this array and return the array.
          *
          * Note that the terminal item is usually the name of the same object used to
          * begin the creation, e.g. `Json ~ Json("fish") ~ Json`.
          */
        def ~(done: JsonBuildTerminator[T]): T =
          new All(if (i==a.length) a else java.util.Arrays.copyOf(a, i))

        /** Add another JSON value to this array. */
        def ~(js: Json): this.type = {
          if (i >= a.length) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
          a(i) = js
          i += 1
          this
        }

        /** Add a JSON null to this array. */
        def ~(nul: scala.Null): this.type = this ~ Null

        /** Add to this array an object that can be converted to JSON */
        def ~[A](a: A)(implicit jser: Jsonize[A]): this.type = this ~ jser.jsonize(a)

        /** Complete building this array and return the array. */
        def ~~(done: JsonBuildTerminator[T]): T = this ~ done

        /** Add an array of JSON values ot this array. */
        def ~~(jses: Array[Json]): this.type = {
          ensureAtLeast(i + jses.length)
          System.arraycopy(jses, 0, a, i, jses.length)
          i += jses.length
          this
        }

        /** Add a segment of an array of JSON values to this array. */
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

        /** Add to this array an array of objects that can be converted to JSON */
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

        /** Add to this array a segment of an array of objects that can be converted to JSON */
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

        /** Add to this array a collection of JSON values */
        def ~~(coll: collection.TraversableOnce[Json]): this.type = {
          coll.foreach(this ~ _)
          this
        }

        /** Add to this array a collection of objects that can be converted to JSON values */
        def ~~[A: Jsonize](coll: collection.TraversableOnce[A]): this.type = {
          coll.foreach(x => this ~ x)
          this
        }
      }
    }


    /** `Json.Arr.Dbl` represents JSON arrays that consist solely of numbers and `null`s and
      * thus can be represented by `Double`s.
      */
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

    /** The companion object to `Json.Arr.Dbl` provides parsing, construction, and building methods for
      * JSON arrays that can be represented as `Double` values
      */
    object Dbl extends JsonBuildTerminator[Dbl] {
      /** Create a new JSON array from an array of `Double`s */
      def apply(xs: Array[Double]): Dbl = new Dbl(xs)

      /** Create a new JSON array from an array of `Long`s */
      def apply(xs: Array[Long]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toDouble; i += 1}
        new Dbl(ys)
      }

      /** Create a new JSON array from an array of `Int`s */
      def apply(xs: Array[Int]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toDouble; i += 1}
        new Dbl(ys)
      }

      /** Create a new JSON array from an array of `Float`s, using the closest decimal representation of the `Float` */
      def decimal(xs: Array[Float]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toString.toDouble; i += 1}
        new Dbl(ys)
      }

      /** Create a new JSON array from an array of `Float`s, using the closest binary representation of the `Float`.
        * This is faster than `decimal`, but if the `Float` was parsed from text will likely introduce errors and
        * in any case will result in a longer serialized form of the numbers.
        */
      def exact(xs: Array[Float]): Dbl = { 
        val ys = new Array[Double](xs.length)
        var i = 0
        while (i < xs.length) { ys(i) = xs(i).toString.toDouble; i += 1}
        new Dbl(ys)
      }

      /** Creates a new builder for JSON arrays of numbers */
      def builder = new Build[Dbl]

      /** An empty JSON array that could have contained only numbers */
      val empty = new Dbl(new Array[Double](0))

      /** Builds the empty JSON array that could have contained only numbers */
      def ~(me: Dbl.type) = empty

      /** Begin building a numeric JSON array, starting with this double */
      def ~(dbl: Double): Build[Dbl] = (new Build[Dbl]) ~ dbl

      /** Builds the empty JSON array that could have contained only numbers */
      def ~~(me: Dbl.type) = empty

      /** Begin building a numeric JSON array, starting with this array of doubles */
      def ~~(doubles: Array[Double]): Build[Dbl] = (new Build[Dbl]) ~~ doubles

      /** Begin building a numeric JSON array, starting with this segment of an array of doubles */
      def ~~(doubles: Array[Double], i0: Int, iN: Int): Build[Dbl] = (new Build[Dbl]) ~~ (doubles, i0, iN)

      /** Begin building a numeric JSON array, starting with this collection of Doubles */
      def ~~(coll: collection.TraversableOnce[Double]) = (new Build[Dbl]) ~~ coll

      /** Begin building a numeric JSON array, starting with an existing numeric JSON array */
      def ~~(existing: Dbl) = (new Build[Dbl]) ~~ existing.doubles


      /** A builder for numeric JSON arrays */
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

        /** Finish building this array and return it.  Note that the terminator is typically the same object used to begin
          * building, e.g. `Json.Arr ~ 2.7 ~ Json.Arr`
          */
        def ~(done: JsonBuildTerminator[T]): T =
          new Dbl(if (i==a.length) a else java.util.Arrays.copyOf(a, i))

        /** Adds another Double to this numeric JSON array */
        def ~(dbl: Double): this.type = {
          if (i >= a.length) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
          a(i) = dbl
          i += 1
          this
        }

        /** Finish building this array and return it.  Note that the terminator is typically the same object used to begin building. */
        def ~~(done: JsonBuildTerminator[T]): T = this ~ done

        /** Adds an array of doubles to this numeric JSON array */
        def ~~(doubles: Array[Double]): this.type = {
          ensureAtLeast(i + doubles.length)
          System.arraycopy(doubles, 0, a, i, doubles.length)
          i += doubles.length
          this
        }

        /** Adds a segment of an array of doubles to this numeric JSON array */
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

        /** Adds a collection of doubles to this numeric JSON array */
        def ~~(coll: collection.TraversableOnce[Double]): this.type = {
          coll.foreach(this ~ _)
          this
        }

        /** Adds an existing numeric JSON array to this numeric JSON array */
        def ~~(existing: Dbl): this.type = this ~~ existing.doubles

        /** Produces a builder of generic JSON arrays that has the same initial
          * contents as this numeric JSON array builder
          */
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

    /** Returns `Left(JastError)` unless the input is a `Json.Arr` */    
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


  /** Represents a JSON object.
    *
    * If the `Json.Obj` was created from a Scala map, this is just a thin wrapper over that map.
    * Otherwise, it is implemented as a single array of alternating keys and values and will
    * lazily (and thread-safely) create a backing map for key lookups if key lookups are performed.
    *
    * Note that `Json.Obj` uses `java.lang.concurrent.atomic.AtomicReference` to thread-safely and
    * lazily compute maps from keys to values.
    */
  sealed trait Obj extends Json {
    protected def myName = "object"
    def simple = false
    def size: Int

    /** Returns `true` if the JSON object has duplicate keys.  This requires map creation, so may be slow. */
    def hasDuplicateKeys: Boolean

    /** Returns `Some(value)` (a JSON value) if the corresponding key exists, or `None` otherwise */
    def get(key: String): Option[Json]

    /** Returns a JSON value if the corresponding key exists, or `null` otherwise */
    def getOrNull(key: String): Json

    /** Applies a function to each key-value pair in this JSON object */
    def foreach[U](f: (String, Json) => U): Unit

    /** Gets a map representation of this JSON object.  Duplicate keys will be discarded.  Use `hasDuplicateKeys` to detect discard of keys.
      *
      * Note: this map may be a mutable map used for lookups for this `Json.Obj`; actually modifying this map is
      * discouraged even when is possible.
      */
    def asMap: collection.Map[String, Json]

    /** Gets the underlying representation of key-value pairs.
      *
      * Note that although the `Array` is typed as `AnyRef`, even entries (starting at 0) are all strings,
      * and odd entries (starting at 1) are all JSON values.
      *
      * This array may be the same array that is used to back the `Json.Obj`, so modifying it is discouraged
      */
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

  /** The companion object to `Json.Obj` provides ways to parse, construct, and build JSON
    * objects.
    */
  object Obj extends FromJson[Obj] with JsonBuildTerminator[Obj] {
    private[jsonal] val mapBuildingInProcess = new collection.mutable.AnyRefMap[String, Json]()

    /** The empty JSON object. */
    val empty: Obj = new AtomicObj(Array())

    /** Creates a new JSON object from a map of String keys to JSON values. */
    def apply(kvs: collection.Map[String, Json]): Obj = new AtomicObj(null, kvs)

    /** Creates a new JSON object from a pair of arrays: one JSON keys, one Json values.
      *
      * Note: if the arrays are not equal length, a JastError will be returned instead.
      */
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

    /** Creates a new JSON object from a single array where keys and objects alternate.
      *
      * Note: all even indices must be `String`, and all odd indices must be `Json`.
      */
    def fromFlatArray(kvs: Array[AnyRef]): Obj = new AtomicObj(kvs, null)


    /** Creates the empty JSON object. */
    def ~(done: Obj.type) = empty

    /** Begins building a JSON object starting with the given JSON string key and a JSON null value */
    def ~(key: Str, nul: scala.Null) = (new Build[Obj]) ~ (key, nul)

    /** Begins building a JSON object starting with the given string key and a JSON null value */
    def ~(key: String, nul: scala.Null) = (new Build[Obj]) ~ (key, nul)

    /** Begins building a JSON object starting with the given JSON string key and a JSON value */    
    def ~(key: Str, js: Json) = (new Build[Obj]) ~ (key, js)

    /** Begins building a JSON object starting with the given string key and a JSON value */
    def ~(key: String, js: Json) = (new Build[Obj]) ~ (key, js)

    /** Begins building a JSON object starting with the given JSON string key and an object that can be converted to a JSON value */
    def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]) = (new Build[Obj]) ~ (key, jser.jsonize(a))

    /** Begins building a JSON object starting with the given string key and an object that can be converted to a JSON value */
    def ~[A](key: String, a: A)(implicit jser: Jsonize[A]) = (new Build[Obj]) ~ (key, jser.jsonize(a))

    /** Creates the empty JSON object. */
    def ~~(done: Obj.type) = empty

    /** Begins building a JSON object starting with the string keys and JSON values in a collection */
    def ~~(coll: collection.TraversableOnce[(String,Json)]) = (new Build[Obj]) ~~ coll

    /** Begins building a JSON object starting with the JSON string keys and JSON values in a collection.
      *
      * Note: the non-straightforward type signature is required to distinguish this case from the `String` case.
      */
    def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= Str) = (new Build[Obj]) ~~ coll

    /** Begins building a JSON object starting with a collection of string keys and objects that can be converted to JSON values */
    def ~~[A: Jsonize](coll: collection.TraversableOnce[(String,A)]) = (new Build[Obj]) ~~ coll

    /** Begins building a JSON object starting with a collection of JSON string keys and objects that can be converted to JSON values.
      *
      * Note: the non-straightforward type signature is required to distinguish this case from the `String` case.
      */
    def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= Str) = (new Build[Obj]) ~~ coll


    /** A builder for JSON objects. */
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

      /** Finish building this JSON object and return it.  Note that the terminator is typically the same
        * object used to begin building, e.g. `Json ~ ("fish", Json(2.7)) ~ Json`
        */
      def ~(done: JsonBuildTerminator[T]): T =
        new AtomicObj(if (i==a.length) a else java.util.Arrays.copyOf(a, i), null)

      /** Adds a JSON string key / JSON value pair to this JSON object */
      def ~(key: Str, js: Json): this.type = append(key, js)

      /** Adds a string key / JSON value pair to this JSON object */
      def ~(key: String, js: Json): this.type = append(key, js)

      /** Adds a JSON string key / JSON null pair to this JSON object */
      def ~(key: Str, nul: scala.Null): this.type = append(key, Null)

      /** Adds a string key / JSON null pair to this JSON object */
      def ~(key: String, nul: scala.Null): this.type = append(key, Null)

      /** Adds to this JSON object a JSON string key and an object convertible to JSON */
      def ~[A](key: Str, a: A)(implicit jser: Jsonize[A]): this.type = this ~ (key, jser.jsonize(a))

      /** Adds to this JSON object a string key and an object convertible to JSON */
      def ~[A](key: String, a: A)(implicit jser: Jsonize[A]): this.type = this ~ (key, jser.jsonize(a))


      /** Finish building this JSON object and return it.  Note that the terminator is typically the same
        * object used to begin building.
        */
      def ~~(done: JsonBuildTerminator[T]): T = this ~ done

      /** Adds the string key / JSON value pairs in this collection to this JSON object */
      def ~~(coll: collection.TraversableOnce[(String,Json)]): this.type = {
        coll.foreach{ case (k,v) => this ~ (k,v) }
        this
      }

      /** Adds the JSON string key / JSON value pairs in this collection to this JSON object */
      def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= Str): this.type = {
        coll.foreach{ case (k,v) => this ~ (ev(k),v) }
        this
      }

      /** Adds to this JSON object the pairs in this collection (string keys and objects convertible to JSON) */
      def ~~[A: Jsonize](coll: collection.TraversableOnce[(String,A)]): this.type = {
        coll.foreach{ case (k,a) => this ~ (k,a) }
        this
      }

      /** Adds to this JSON object the pairs in this collection (JSON string keys and objects convertible to JSON) */
      def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= Str): this.type = {
        coll.foreach{ case (k,a) => this ~ (ev(k),a) }
        this
      }
    }

    /** Returns `Left(JastError)` unless the input is a `Json.Arr` */
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

/** THIS IS A PLACEHOLDER FOR A REAL IMPLEMENTATION -- TODO: THE REAL IMPLEMENTATION! */
trait JsonGenericParser {
  def Json(a: Any): Either[JastError, kse.jsonal.Json] = ???
  def Json(a: Any, b: Any): Either[JastError, kse.jsonal.Json] = ???
  def Json(a: Any, b: Any, c: Any, d: Any): Either[JastError, kse.jsonal.Json] = ???
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

/** THIS IS A PLACEHOLDER FOR A REAL IMPLEMENTATION -- TODO: THE REAL IMPLEMENTATION! */
object JsonInputStreamParser extends JsonGenericParser {}

/** THIS IS A PLACEHOLDER FOR A REAL IMPLEMENTATION -- TODO: THE REAL IMPLEMENTATION! */
object JsonByteBufferParser extends JsonGenericParser {}

/** THIS IS A PLACEHOLDER FOR A REAL IMPLEMENTATION -- TODO: THE REAL IMPLEMENTATION! */
object JsonCharBufferParser extends JsonGenericParser {}
