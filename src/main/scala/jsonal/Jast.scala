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
  * It is fast: faster than Jackson, comparable to Boon in most cases.
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

import scala.util.control.NonFatal


/** Jast is a Json Abstract Syntax Tree.
  *
  * It is the parent of both parse errors (`JastError`) and valid JSON values (`Json`).
  *
  * Because accessor methods are provided on `Jast`, and errors in access produce `JastError`, safe destructuring is easy:
  *
  * {{{
  * import kse.jsonal._
  * val maybeJson = Jast.parse("""{"red":[true, {"dish":27.5}], "blue":"fish"}""")
  * val redDish = maybeJson("red")(1)("dish")    // JSON representation of 27.5
  * val wrong   = maybeJson("blue")(3)("fish")   // JastError("Indexing into string")
  * val dishTwo = maybeJson \ "red" \ 1 \ "dish" // Alternative Lift-style syntax
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

  /** Boolean value corresponding to this JSON value if it is a boolean JSON; `None` otherwise. */
  def bool: Option[Boolean]

  /** Boolean value corresponding to this JSON value if it has a value, default otherwise */
  def boolOr(default: Boolean): Boolean

  /** String corresponding to this JSON value if it is a JSON string, `None` otherwise. */
  def string: Option[String]

  /** String corresponding to this JSON value if it is a JSON string, default otherwise. */
  def stringOr(default: => String): String

  /** String corresponding to this JSON value if it is a JSON string, null otherwise. */
  def stringOrNull: String

  /** Index into JSON array, if this is a JSON array.  Returns `JastError` otherwise. */
  def apply(i: Int): Jast

  /** Lookup of key in JSON object, if this is a JSON object.  Returns `JastError` if key not found or if this is not a JSON object. */
  def apply(key: String): Jast

  /** Parse into a class given an implicit (or explicit) converter */
  def to[A](implicit fj: FromJson[A]): Either[JastError, A]

  /** Converts errors into JSON null values */
  def nullError: Json

  /** Lift-style alias for array lookup. */
  @inline final def \(i: Int): Jast = this apply i

  /** Lift-style alias for object key lookup. */
  @inline final def \(key: String): Jast = this apply key
}
/** High-level routines for converting strings and other serial formats into JSON ASTs. */
object Jast extends ParseToJast(false) {
  /** Parsing routines that take a relaxed approach to parsing numbers (everything goes into Double, even if inexact) */
  val relaxed: ParseToJast = new ParseToJast(true)
}

/** Representation of an error that occured during JSON parsing or access. */
final case class JastError(msg: String, where: Long = -1L, because: Jast = Json.Null) extends Jast {
  def simple = false
  def isNull = false
  def double = Json.not_a_normal_NaN
  def bool = None
  def boolOr(default: Boolean) = default
  def string = None
  def stringOr(default: => String) = default
  def stringOrNull: String = null
  def apply(i: Int) = this
  def apply(key: String) = this
  def to[A](implicit fj: FromJson[A]): Either[JastError, A] = Left(this)
  def nullError: Json = Json.Null

  override def toString = {
    var indent = 0
    var e: Jast = this
    val sb = new java.lang.StringBuilder
    while ((e ne null) && (e != Json.Null)) {
      if (indent > 0) { sb append '\n'; sb append " "*indent; sb append "because " }
      e match {
        case je: JastError =>
          sb append je.msg
          if (where >= 0) {
            sb append '\n'
            sb append " "*(indent + 2)
            sb append "near character "
            sb append je.where
          }
          e = je.because
        case jx =>
          sb append jx.toString
          e = Json.Null
      }
      indent += 2
    }
    sb.toString
  }
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
  def boolOr(default: Boolean) = default
  def string: Option[String] = None
  def stringOr(default: => String) = default
  def stringOrNull: String = null
  def apply(i: Int): Jast = JastError("Indexing into "+myName)
  def apply(key: String): Jast = JastError("Map looking on "+myName)
  def to[A](implicit fj: FromJson[A]): Either[JastError, A] = fj parse this
  def nullError: Json = this

  def json: Json = this
  override def toString = { val sb = new java.lang.StringBuilder; jsonString(sb); sb.toString }
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


  /** Returns a JSON value if a conversion exists, handling null inputs properly */
  def orNull[A](a: A)(implicit jser: Jsonize[A]): Json =
    if (a.asInstanceOf[AnyRef] eq null) Null
    else jser.jsonize(a)

  /** Applies automatic conversions to create a JSON value via a typeclass */
  def apply[A](a: A)(implicit jser: Jsonize[A]): Json = jser.jsonize(a)

  /** Returns an empty JSON value (in this case, a JSON null) */
  def apply(): Json = Null

  /** Returns a JSON boolean value */
  def apply(b: Boolean): Json = if (b) Bool.True else Bool.False

  /** Returns the JSON representation of a string */
  def apply(s: String): Json = Str(s)

  /** Returns the JSON representation of a Long */
  def apply(l: Long): Json = Num(l)

  /** Warns against putting `Float` into JSON without deciding what you want */
  @deprecated("for Float arguments.  Use .toString.toDouble for a nice decimal representation or .toDouble if you do not need one.", "0.3.0")
  def apply(f: Float): Json = Num(f.toDouble)

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


  /** Int can fit into Double just fine, so we allow this without warning */
  def ~(int: Int): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~ int.toDouble

  /** Warns against putting `Long` into JSON unguarded, as promotion to Long causes problems. */
  @deprecated("for Long arguments.  Use .toDouble to indicate that precision should be lost from this value, or use Arr.All to store Longs without loss of precision.", "0.3.0")
  def ~(long: Long): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~ long.toDouble

  /** Warns against putting `Float` into JSON unguarded, as promotion to Double causes problems. */
  @deprecated("for Float arguments.  Use .toString.toDouble for a nice decimal representation or .toDouble if you do not need one.", "0.3.0")
  def ~(flt: Float): Arr.Dbl.Build[Json] = (new Arr.Dbl.Build[Json]) ~ flt.toDouble

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

  /** Adds to this JSON object a JSON string key and a JSON abstract syntax tree element but only if the element is not null, NaN, empty, or an error */
  def ~?(key: Str, ja: Jast) = (new Obj.Build[Json]) ~? (key, ja)

  /** Adds to this JSON object a string key and a JSON abstract syntax tree element but only if the element is not null, NaN, empty, or an error */
  def ~?(key: String, ja: Jast) = (new Obj.Build[Json]) ~? (key, ja)

  /** Adds to this JSON object a JSON string key and string value but only if the string value is nonempty and non-null */
  def ~?(key: Str, value: String) = (new Obj.Build[Json]) ~? (key, value)

  /** Adds to this JSON object a string key and string value but only if the string value is nonempty and non-null */
  def ~?(key: String, value: String) = (new Obj.Build[Json]) ~? (key, value)

  /** Optionally adds to this JSON object a JSON string key and value of an object convertible to JSON */
  def ~?[A: Jsonize](key: Str, oa: Option[A]) = (new Obj.Build[Json]) ~? (key, oa)

  /** Optionally adds to this JSON object a string key and value of an object convertible to JSON */
  def ~?[A: Jsonize](key: String, oa: Option[A]) = (new Obj.Build[Json]) ~? (key, oa)

  /** Build a JSON object starting with the string / JSON value pairs in this collection */
  def ~~(coll: collection.TraversableOnce[(String,Json)]) = (new Obj.Build[Json]) ~~ coll

  /** Build a JSON object starting with the JSON string / JSON value pairs in this collection */
  def ~~[S](coll: collection.TraversableOnce[(S,Json)])(implicit ev: S =:= Str) = (new Obj.Build[Json]) ~~ coll

  /** Build a JSON object starting with the string / JSON-convertible-object pairs in this collection */
  def ~~[A: Jsonize](coll: collection.TraversableOnce[(String,A)]) = (new Obj.Build[Json]) ~~ coll

  /** Build a JSON object starting with the JSON string / JSON-convertible-object pairs in this collection */
  def ~~[A, S](coll: collection.TraversableOnce[(S,A)])(implicit jser: Jsonize[A], ev: S =:= Str) = (new Obj.Build[Json]) ~~ coll

  /** Begins building a JSON object starting with an existing JSON object. */
  def ~~(jo: Obj) = (new Obj.Build[Json]) ~~ jo


  /** Parse this JSON value as itself.
    *
    * Note: a `Left(JastError)` is never actually produced.
    */
  def parse(input: Json): Either[JastError, Json] = Right(input)

  override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = JsonStringParser.Json(input, i0, iN, ep)
  override def parse(input: ByteBuffer) = JsonByteBufferParser.Json(input)
  override def parse(input: CharBuffer) = JsonCharBufferParser.Json(input)
  override def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Either[JastError, Json] =
    JsonRecyclingParser.Json(JsonRecyclingParser recycleInputStream input, ep)

  private final class JsonFromJson extends FromJson[Json] {
    def parse(input: Json): Either[JastError, Json] = Json.parse(input)

    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
      JsonStringParser.Json(input, i0, iN, ep, relaxed = true)

    override def parse(input: ByteBuffer) = Json.parse(input)

    override def parse(input: CharBuffer) = Json.parse(input)

    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Either[JastError, Json] =
      JsonRecyclingParser.Json(JsonRecyclingParser recycleInputStream input, ep, relaxed = true)
  }

  /** Uses non-strict parsing of numbers (targeting Double for speed) */
  val relaxed: FromJson[Json] = new JsonFromJson

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

    override def equals(a: Any) = a.asInstanceOf[AnyRef] eq this

    /** Returns a `JastError` if this is not a JSON null */
    def parse(input: Json): Either[JastError, Null] =
      if (this eq input) Right(this)
      else Left(JastError("expected null"))

    override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) = 
      JsonStringParser.Null(input, i0, iN, ep)

    override def parse(input: ByteBuffer) = JsonByteBufferParser.Null(input)

    override def parse(input: CharBuffer) = JsonCharBufferParser.Null(input)

    override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) = 
      JsonRecyclingParser.Null(JsonRecyclingParser recycleInputStream input, ep)
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
      override def boolOr(default: Boolean) = true
      override def equals(a: Any) = a.asInstanceOf[AnyRef] eq this
      override def jsonString(sb: java.lang.StringBuilder) { sb append "true" }
      override def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer =
        (if (bb.remaining < 4) refresh(bb) else bb) put myBytesSayTrue
      override def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer =
        (if (cb.remaining < 4) refresh(cb) else cb) put myCharsSayTrue
    }

    case object False extends Bool {
      def value = false
      override val bool = Some(false)
      override def boolOr(default: Boolean) = false
      override def equals(a: Any) = a.asInstanceOf[AnyRef] eq this
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
      JsonRecyclingParser.Bool(JsonRecyclingParser recycleInputStream input, ep)
  }


  /** Represents a JSON string value.  The value is stored as a plain String and encoded for serialization on demand. */
  final case class Str(text: String) extends Json {
    protected def myName = "string"

    def simple = true

    override def string = Some(text)

    override def stringOr(default: => String) = text

    override def stringOrNull = text

    override def hashCode = text.hashCode

    override def equals(a: Any) = a match {
      case s: Str => text == s.text
      case _ => false
    }

    override def jsonString(sb: java.lang.StringBuilder) { Str.addJsonString(sb, text) }

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
    def addJsonString(sb: java.lang.StringBuilder, text: String) {
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
      JsonRecyclingParser.Str(JsonRecyclingParser recycleInputStream input, ep)
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
  class Num private[jsonal] (private val content: Double, private val text: String) extends Json {
    protected def myName = "number"
    def simple = true

    /** Returns `true` if this number is represented by a finite `Double` value */
    def isDouble: Boolean = (text eq null) || (!java.lang.Double.isNaN(content) && !java.lang.Double.isInfinite(content))

    /** Returns `true` if this number has stored textual representation */
    def explicitTextForm: Boolean = (text ne null) && !text.isEmpty

    /** The `Double` value corresponding to this JSON number */
    override def double = if (text eq null) java.lang.Double.doubleToRawLongBits(content).toDouble else content

    /** Returns `true` if this number is represented exactly by a `Long` value */
    def isLong: Boolean = (text eq null) || (text.isEmpty && content.toLong == content)

    /** Returns a `Long` value corresponding to this JSON number.
      * Values too large end up as `Long.MaxValue`, those to small as `Long.MinValue`, and those
      * that are `NaN` end up as `0L`.  Fractional values are rounded as with `Double`'s `.toLong` method.
      */
    def long: Long = if (text eq null) java.lang.Double.doubleToRawLongBits(content) else content.toLong

    /** Returns a `Long` if this JSON number is exactly represented by a `Long`, or a fallback value if not */
    def longOr(fallback: Long): Long =
      if (text eq null) java.lang.Double.doubleToRawLongBits(content)
      else fallback

    /** Returns a BigDecimal value that corresponds to this JSON number.
      *
      * Note: this operation is not cached, so performance may suffer if it is called repeatedly.
      */
    def big: BigDecimal =
      if (text eq null) BigDecimal(java.lang.Double.doubleToRawLongBits(content))
      else if (text.isEmpty) BigDecimal(content)
      else BigDecimal(text)

    override def toString =
      if (text eq null) java.lang.Double.doubleToRawLongBits(content).toString
      else if (text.isEmpty) content.toString
      else text

    override def hashCode = if (text eq null) java.lang.Double.doubleToRawLongBits(content).## else content.##

    override def equals(a: Any) = a match {
      case n: Num => 
        if (isDouble) n.isDouble && double == n.double
        else if (n.isDouble) false
        else Num.numericStringEquals(this.toString, n.toString)
      case _ => false
    }

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

    /** Return the JSON number corresponding to this Long */
    def apply(l: Long): Num = new Num(java.lang.Double.longBitsToDouble(l), null)

    /** Warns against unguarded conversion of Float--may produce unpleasant decimal expansions. */
    @deprecated("for Float arguments.  Use .toString.toDouble for a nice decimal representation or .toDouble if you do not need one.", "0.3.0")
    def apply(f: Float): Json = apply(f.toDouble)

    /** Return the JSON number corresponding to this Double, or a JSON null if the Double is infinite or NaN */
    def apply(d: Double): Json =
      if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) Null
      else {
        val l = d.toLong
        if (l == d) new Num(java.lang.Double.longBitsToDouble(l), null)
        else new Num(d, "")
      }

    /** Return the JSON number corresponding to this BigDecimal */
    def apply(bd: BigDecimal): Num = {
      val d = bd.doubleValue
      if (d == bd) {
        val l = d.toLong
        if (l == d && l == bd) new Num(java.lang.Double.longBitsToDouble(l), null)
        else new Num(d, "")
      }
      else if (d > (1L << 53)) {
        val l = bd.longValue
        if (l == bd) new Num(java.lang.Double.longBitsToDouble(l), null)
        else new Num(d, bd.toString)
      }
      else new Num(d, bd.toString)
    }

    /** Tests two non-empty strings for equality, assuming both are decimal representations of numbers.
      *
      * Note: if the two strings are NOT decimal representations of numbers, the results of this method are undefined.
      * (It is likely but not guaranteed that the method will return `false` even if the two strings are identical.)
      */
    def numericStringEquals(a: String, b: String): Boolean = {
      var i = 0  // Index for a
      var j = 0  // Index for b
      if (a.length < 1 || b.length < 1) return false
      if (a.charAt(0) == '-') i += 1
      if (b.charAt(0) == '-') j += 1
      if (i >= a.length || j >= b.length) return false
      var ca = a.charAt(i)  // Character at index of a
      var cb = b.charAt(j)  // Character at index of b
      if (i != j) {
        // Different signs.  They'd better both be zero
        return {
          if (ca == '0' && cb == '0') {
            while (i < a.length - 1 && (ca == '0' || ca == '.')) { i += 1; ca = a.charAt(i) }
            while (j < b.length - 1 && (cb == '0' || cb == '.')) { j += 1; cb = b.charAt(j) }
            (i == a.length - 1 || (ca | 0x20) == 'e') && (j == b.length - 1 || (cb | 0x20) == 'e')
          }
          else false
        }
      }
      var pa = 0  // Decimal point position for a
      var pb = 0  // Decimal point position for b
      if (ca == '0') {
        pa = -1
        if (i < a.length - 1 && a.charAt(i+1) != '.') return false
        else if (i < a.length - 2) {
          i += 2
          ca = a.charAt(i)
          while (ca == '0' && i < a.length-1) { i += 1; ca = a.charAt(i); pa -= 1 }          
        }
      }
      if (cb == '0') {
        pb = -1
        if (j < b.length - 1 && b.charAt(j+1) != '.') return false
        else if (j < b.length - 2) {
          j += 2
          cb = b.charAt(j)
          while (cb == '0' && j < b.length-1) { j += 1; cb = b.charAt(j); pb -= 1 }
        }
      }
      var fa = pa < 0  // Found a's decimal point?
      var fb = pb < 0  // Found b's decimal point?
      while (ca == cb && (ca | 0x20) != 'e' && i < a.length-1 && j < b.length-1) {
        i += 1
        ca = a.charAt(i)
        if (!fa) {
          pa += 1
          if (ca == '.') {
            fa = true
            if (i < a.length-1) {
              i += 1
              ca = a.charAt(i)
            }
          }
        }
        j += 1
        cb = b.charAt(j)
        if (!fb) {
          pb += 1
          if (cb == '.') {
            fb = true
            if (j < b.length-1) {
              j += 1
              cb = b.charAt(j)
            }
          }
        }
      }
      if (ca != cb && (ca | 0x20) != 'e' && (cb | 0x20) != 'e') return false  // Digits simply disagree
      // Capture any trailing zeros
      if (!(i >= a.length -1 && j >= b.length - 1)) {
        if (j >= b.length - 1 || (cb | 0x20) == 'e') {
          if (j >= b.length - 1) {
            if (!fb) pb += 1
            // Advance a off the end, make sure it's all zeros
            i += 1
            ca = a.charAt(i)
            if (!fa) {
              pa += 1
              if (ca == '.') {
                fa = true
                if (i < a.length - 1) {
                  i += 1
                  ca = a.charAt(i)
                }
              }
            }
          }
          while (i < a.length -1 && ca == '0') {
            i += 1
            ca = a.charAt(i)
            if (!fa) {
              pa += 1
              if (ca == '.') {
                fa = true
                if (i < a.length - 1) {
                  i += 1
                  ca = a.charAt(i)
                }
              }
            }
          }
          if (ca >= '1' && ca <= '9') return false  // Extra digits on a
        }
        else if (i >= a.length - 1 || (ca | 0x20) == 'e') {
          if (i >= a.length - 1) {
            if (!fa) pa += 1
            // Advance b off the end, make sure it's all zeros
            j += 1
            cb = b.charAt(j)
            if (!fb) {
              pb += 1
              if (cb == '.') {
                fb = true
                if (j < b.length - 1) {
                  j += 1
                  cb = b.charAt(j)
                }
              }
            }
          }
          while (j < b.length -1 && cb == '0') {
            j += 1
            cb = b.charAt(j)
            if (!fb) {
              pb += 1
              if (cb == '.') {
                fb = true
                if (j < b.length - 1) {
                  j += 1
                  cb = b.charAt(j)
                }
              }
            }
          }
          if (cb >= '1' && cb <= '9') return false  // Extra digits on b
        }        
      }
      if (pa > 0) pa -= 1
      if (pb > 0) pb -= 1
      if ((ca | 0x20) == 'e' && (cb | 0x20) == 'e') {
        if (i >= a.length - 1) return false
        i += 1
        ca = a.charAt(i)
        val nega =
          if (ca == '-' || ca == '+') {
            val ans = ca == '-'
            if (i >= a.length - 1) return false
            i += 1
            ca = a.charAt(i)
            ans
          }
          else false
        if (j >= b.length - 1) return false
        j += 1
        cb = b.charAt(j)
        val negb =
          if (cb == '-' || cb == '+') {
            val ans = cb == '-'
            if (j >= b.length - 1) return false
            j += 1
            cb = b.charAt(j)
            ans
          }
          else false
        if (a.length - i < 10 && b.length - j < 10) {
          val ea = a.substring(i).toInt
          val eb = b.substring(j).toInt
          (if (negb) pb - eb else pb + eb) == (if (nega) pa - ea else pa + ea)
        }
        else if (a.length - i < 18 && b.length - j < 18) {
          val ea = a.substring(i).toLong
          val eb = b.substring(j).toLong
          (if (negb) pb - eb else pb + eb) == (if (nega) pa - ea else pa + ea)
        }
        else {
          val ea = BigInt(a.substring(i))
          val eb = BigInt(b.substring(j))
          (if (negb) pb - eb else pb + eb) == (if (nega) pa - ea else pa + ea)
        }
      }
      else if ((ca | 0x20) == 'e') {
        if (i >= a.length - 1) return false
        i += 1
        ca = a.charAt(i)
        val nega =
          if (ca == '-' || ca == '+') {
            val ans = ca == '-'
            if (i >= a.length - 1) return false
            i += 1
            ca = a.charAt(i)
            ans
          }
          else false
        if (a.length - i < 10) {
          val ea = a.substring(i).toInt
          pb == (if (nega) pa - ea else pa + ea)
        }
        else if (a.length - i < 18) {
          val ea = a.substring(i).toLong
          pb == (if (nega) pa - ea else pa + ea)
        }
        else {
          val ea = BigInt(a.substring(i))
          pb == (if (nega) pa - ea else pa + ea)
        }
      }
      else if ((cb | 0x20) == 'e') {
        if (j >= b.length - 1) return false
        j += 1
        cb = b.charAt(j)
        val negb =
          if (cb == '-' || cb == '+') {
            val ans = cb == '-'
            if (j >= b.length - 1) return false
            j += 1
            cb = b.charAt(j)
            ans
          }
          else false
        if (b.length - j < 10) {
          val eb = b.substring(j).toInt
          pa == (if (negb) pb - eb else pb + eb)
        }
        else if (b.length - j < 18) {
          val eb = b.substring(j).toLong
          pa == (if (negb) pb - eb else pb + eb)
        }
        else {
          val eb = BigInt(b.substring(j))
          pa == (if (negb) pb - eb else pb + eb)
        }
      }
      else pa == pb
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
      JsonRecyclingParser.Num(JsonRecyclingParser recycleInputStream input, ep)

    /** Uses non-strict numeric parsing, storing all numbers in `Double` for speed */
    private final class NumFromJson extends FromJson[Num] {
      override def parse(input: Json): Either[JastError, Num] = Num.parse(input)
      override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
        JsonStringParser.Num(input, i0, iN, ep, relaxed = true)
      override def parse(input: ByteBuffer) = JsonByteBufferParser.Num(input, relaxed = true)
      override def parse(input: CharBuffer) = JsonCharBufferParser.Num(input, relaxed = true)
      override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
        JsonRecyclingParser.Num(JsonRecyclingParser recycleInputStream input, ep, relaxed = true)
    }

    val relaxed: FromJson[Num] = new NumFromJson
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

    /** Visits each element in this array. */
    def foreach[U](f: Json => U): Unit

    /** Selects some of the elements in this array. */
    def filter(p: Json => Boolean): Arr

    /** Produces an iterator over this array. */
    def iterator: Iterator[Json]
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

    /** Int can fit into Double just fine, so we allow this without warning */
    def ~(int: Int): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~ int.toDouble

    /** Warns against putting `Long` into JSON unguarded, as promotion to Long causes problems. */
    @deprecated("for Long arguments.  Use .toDouble to indicate that precision should be lost from this value, or use Arr.All to store Longs without loss of precision.", "0.3.0")
    def ~(long: Long): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~ long.toDouble

    /** Warns against putting `Float` into JSON unguarded, as promotion to Double causes problems. */
    @deprecated("for Float arguments.  Use .toString.toDouble for a nice decimal representation or .toDouble if you do not need one.", "0.3.0")
    def ~(flt: Float): Dbl.Build[Arr] = (new Dbl.Build[Arr]) ~ flt.toDouble

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
    final class All(val values: Array[Json]) extends Arr {
      def size = values.length
      override def apply(i: Int) = if (i < 0 || i >= values.length) JastError("bad index "+i) else values(i)
      def foreach[U](f: Json => U) { var i = 0; while (i < values.length) { f(values(i)); i += 1 } }
      def filter(p: Json => Boolean): All = {
        var i = 0
        while (i < values.length && p(values(i))) i += 1
        if (i == values.length) this
        else {
          val b = (if (i > 0) All ~~ (values, 0, i) else All.builder)
          i += 1
          while (i < values.length) { if (p(values(i))) b ~ values(i); i += 1 }
          b.result
        }
      }
      def iterator: Iterator[Json] = new scala.collection.AbstractIterator[Json] {
        private[this] var i = 0
        def hasNext = i < values.length
        def next = {
          val ans = values(i)
          i += 1
          ans
        }
      }
      override def hashCode = scala.util.hashing.MurmurHash3.arrayHash(values)
      override def equals(a: Any): Boolean = a match {
        case all: All => values.length == all.values.length && {
          var i = 0
          while (i < values.length) { if (values(i) != all.values(i)) return false; i += 1 }
          true
        }
        case dbl: Dbl => values.length == dbl.doubles.length && {
          var i = 0
          while (i < values.length) {
            values(i) match {
              case n: Num => n.double == dbl.doubles(i)
              case z: Null => if (!(java.lang.Double.isNaN(dbl.doubles(i)) || java.lang.Double.isInfinite(dbl.doubles(i)))) return false
              case _ => return false
            }
            i += 1
          }
          true
        }
        case _ => false
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

      /** Starts building a JSON array, beginning with a segment of an array of JSON values */
      def ~~(jses: Array[Json], i0: Int, iN: Int) = (new Build[Arr]) ~~ (jses, i0, iN)

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

        /** Finish building this arrray and return it.  See also `~` with a `JsonBuildTerminator`. */
        def result(): All =
          new All(if (i==a.length) a else java.util.Arrays.copyOf(a, i))

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
        else Num(doubles(i))
      def foreach[U](f: Json => U) { var i = 0; while (i < doubles.length) { f(Num(doubles(i))); i += 1 } }
      def foreach[A](f: Double => Unit)(implicit ev: A =:= Double) { var i = 0; while (i < doubles.length) { f(doubles(i)); i += 1 } }
      def filter(p: Json => Boolean): Dbl = {
        var i = 0
        while (i < doubles.length && p(Num(doubles(i)))) i += 1
        if (i == doubles.length) this
        else {
          val b = (if (i > 0) Dbl ~~ (doubles, 0, i) else Dbl.builder)
          i += 1
          while (i < doubles.length) { if (p(Num(doubles(i)))) b ~ doubles(i); i += 1 }
          b.result
        }
      }
      def filter[A](p: Double => Boolean)(implicit ev: A =:= Double): Dbl = {
        var i = 0
        while (i < doubles.length && p(doubles(i))) i += 1
        if (i == doubles.length) this
        else {
          val b = (if (i > 0) Dbl ~~ (doubles, 0, i) else Dbl.builder)
          i += 1
          while (i < doubles.length) { if (p(doubles(i))) b ~ doubles(i); i += 1 }
          b.result
        }
      }
      def iterator: Iterator[Json] = new scala.collection.AbstractIterator[Json] {
        private[this] var i = 0
        def hasNext = i < doubles.length
        def next = {
          val ans = Num(doubles(i))
          i += 1
          ans
        }
      }
      override def hashCode = {
        var h = scala.util.hashing.MurmurHash3.arraySeed
        var i = 0
        while (i < doubles.length) {
          h = scala.util.hashing.MurmurHash3.mix(h, if (java.lang.Double.isNaN(h) || java.lang.Double.isInfinite(h)) Null.hashCode else doubles(i).##)
          i += 1
        }
        scala.util.hashing.MurmurHash3.finalizeHash(h, doubles.length)
      }
      override def equals(a: Any): Boolean = a match {
        case all: All => all == this
        case dbl: Dbl => doubles.length == dbl.doubles.length && {
          var i = 0
          while (i < doubles.length) {
            if (java.lang.Double.isNaN(dbl.doubles(i)) && java.lang.Double.isNaN(doubles(i)))
              java.lang.Double.doubleToRawLongBits(dbl.doubles(i)) == java.lang.Double.doubleToRawLongBits(doubles(i))
            else doubles(i) == dbl.doubles(i)
            i += 1
          }
          true
        }
        case _ => false
      }
      override def jsonString(sb: java.lang.StringBuilder) {
        sb append '['
        var i = 0
        while (i < doubles.length) {
          if (i > 0) sb append ", "
          val d = doubles(i)
          val l = d.toLong
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) sb append "null"
          else if (d == l) sb append l.toString
          else sb.append(d.toString)
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
          val l = d.toLong
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) b = Null.jsonBytes(b, refresh)
          else b = loadByteBuffer((if (d == l) l.toString else d.toString).getBytes, b, refresh)
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
          val l  = d.toLong
          if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) c = Null.jsonChars(c, refresh)
          else c = loadCharBuffer((if (d == l) l.toString else d.toString).toCharArray, c, refresh)
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

      /** Int can fit into Double just fine, so we allow this without warning */
      def ~(int: Int): Build[Dbl] = (new Build[Dbl]) ~ int.toDouble

      /** Warns against putting `Long` into JSON unguarded, as promotion to Long causes problems. */
      @deprecated("for Long arguments.  Use .toDouble to indicate that precision should be lost from this value, or use Arr.All to store Longs without loss of precision.", "0.3.0")
      def ~(long: Long): Build[Dbl] = (new Build[Dbl]) ~ long.toDouble

      /** Warns against putting `Float` into JSON unguarded, as promotion to Double causes problems. */
      @deprecated("for Float arguments.  Use .toString.toDouble for a nice decimal representation or .toDouble if you do not need one.", "0.3.0")
      def ~(flt: Float): Build[Dbl] = (new Build[Dbl]) ~ flt.toDouble

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

        /** Finish building this array and return it.  See also `~` with a `JsonBuildTerminator`. */
        def result(): Dbl =
          new Dbl(if (i==a.length) a else java.util.Arrays.copyOf(a, i))

        /** Finish building this array and return it.  Note that the terminator is typically the same object used to begin
          * building, e.g. `Json.Arr ~ 2.7 ~ Json.Arr`
          */
        def ~(done: JsonBuildTerminator[T]): T =
          new Dbl(if (i==a.length) a else java.util.Arrays.copyOf(a, i))

        /** Int can fit into Double just fine, so we allow this without warning */
        def ~(int: Int): this.type = this ~ int.toDouble

        /** Warns against putting `Long` into JSON unguarded, as promotion to Long causes problems. */
        @deprecated("for Long arguments.  Use .toDouble to indicate that precision should be lost from this value, or use Arr.All to store Longs without loss of precision.", "0.3.0")
        def ~(long: Long): this.type = this ~ long.toDouble

        /** Warns against putting `Float` into JSON unguarded, as promotion to Double causes problems. */
        @deprecated("for Float arguments.  Use .toString.toDouble for a nice decimal representation or .toDouble if you do not need one.", "0.3.0")
        def ~(flt: Float): this.type = this ~ flt.toDouble

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
            that ~ Num(a(j))
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
      JsonRecyclingParser.Arr(JsonRecyclingParser recycleInputStream input, ep)

    private final class ArrFromJson extends FromJson[Arr] {
      override def parse(input: Json): Either[JastError, Arr] = Arr.parse(input)

      override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
        JsonStringParser.Arr(input, i0, iN, ep, relaxed = true)

      override def parse(input: ByteBuffer) = JsonByteBufferParser.Arr(input, relaxed = true)

      override def parse(input: CharBuffer) = JsonCharBufferParser.Arr(input, relaxed = true)

      override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
        JsonRecyclingParser.Arr(JsonRecyclingParser recycleInputStream input, ep, relaxed = true)
    }

    val relaxed: FromJson[Arr] = new ArrFromJson
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

    /** Folds an initial value through each key-value pair in this JSON object */
    def fold[A](zero: A)(f: (A, String, Json) => A): A

    /** Applies a function to each key-value pair in this JSON object */
    def foreach[U](f: (String, Json) => U): Unit

    /** Selects some of the key-value pairs in this JSON object to make a new one */
    def filter(p: (String, Json) => Boolean): Obj

    /** Keeping the same keys, transforms the values.  Returns the new Json Obj.
      *
      * If the old object was backed by an array, the same old object will be returned.
      * If it was backed by a map, a new object is returned (backed by an array).
      *
      * Note: this operation is NOT safe to use concurrently.
      */
    def transformValues(f: (String, Json) => Json): Obj

    /** Counts keys for validation purposes.  Optionally takes a set of keys to consider; otherwise will count all of them. */
    def countKeys(keyset: Option[collection.Set[String]] = None): collection.Map[String, Int]

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

    /** Iterates through the keys and values of this JSON object.
      *
      * If this object was read from a file, the original order will be traversed.  If constructed from a map, the map order will be followed.
      */
    def iterator: Iterator[(String, Json)]

    /** Checks whether the contents of this JSON object is the same as in the given map. */
    def equalsMap(m: collection.Map[String, Json]): Boolean

    /** Checks whether the contents of this JSON object is the same as in the given array.
      *
      * If `orderMatters` is set, the two must appear in the same order, and this JSON object must
      * be backed by an array, not a map.  If duplicate keys exist, `orderMatters` must be set.
      */
    def equalsArray(a: Array[AnyRef], orderMatters: Boolean): Boolean
  }
  private[jsonal] final class AtomicObj(val underlying: Array[AnyRef], myMap: collection.Map[String, Json] = null)
  extends java.util.concurrent.atomic.AtomicReference[collection.mutable.AnyRefMap[String, Json]](null) with Obj {
    private[this] def linearSearchOrNull(key: String): Json = {
      var i = 0
      while (i < underlying.length - 1) {
        if (key == underlying(i)) return underlying(i+1).asInstanceOf[Json]
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
                arm += (underlying(i).asInstanceOf[String], underlying(i+1).asInstanceOf[Json])
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
    def hasDuplicateKeys: Boolean = (myMap eq null) && underlying.length >= 4 && {
      if (underlying.length < 12) {
        var i = 2
        while (i < underlying.length -1) {
          val key = underlying(i).asInstanceOf[String]
          var j = 0
          while (j < i) {
            if (key == underlying(j)) return true
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
            arm += (underlying(i).asInstanceOf[String], underlying(i+1).asInstanceOf[Json])
            i += 2
          }
          if (useWhatWeMake) set(arm)
          m = arm
        }
        m.size != size
      }
    }
    def get(key: String): Option[Json] = Option(getOrNull(key))
    def fold[A](zero: A)(f: (A, String, Json) => A): A = {
      if (myMap eq null) myMap.foldLeft(zero)((a,sj) => f(a, sj._1, sj._2))
      else {
        var a = zero
        var i = 0
        while (i < underlying.length - 1) {
          a = f(a, underlying(i).asInstanceOf[String], underlying(i+1).asInstanceOf[Json])
          i += 1
        }
        a
      }
    }
    def foreach[U](f: (String, Json) => U) {
      if (myMap eq null) {
        var i = 0
        while (i < underlying.length - 1) {
          f(underlying(i).asInstanceOf[String], underlying(i+1).asInstanceOf[Json])
          i += 2
        }
      }
      else myMap.foreach{ case (k,v) => f(k,v) }
    }
    def transformValues(f: (String, Json) => Json): Obj = {
      if (myMap eq null) {
        get() match {
          case null =>
            var i = 0
            while (i < underlying.length - 1) {
              val k = underlying(i).asInstanceOf[String]
              val v = underlying(i+1).asInstanceOf[Json]
              val x = f(k,v)
              if (x ne v) underlying(i+1) = x
              i += 2
            }
            this
          case x if x eq Obj.mapBuildingInProcess => (new AtomicObj(asFlatArray, null)).transformValues(f)
          case arm: collection.mutable.AnyRefMap[String, Json] =>
            var i = 0
            while (i < underlying.length - 1) {
              val k = underlying(i).asInstanceOf[String]
              val v = underlying(i+1).asInstanceOf[Json]
              val x = f(k,v)
              if (x ne v) {
                underlying(i+1) = x
                arm(k) = v
              }
              i += 2
            }
            this
        }
      }
      else (new AtomicObj(asFlatArray, null)).transformValues(f)
    }
    def filter(p: (String, Json) => Boolean): Obj = {
      if (myMap ne null) new AtomicObj(null, myMap.filter{ case (k,v) => p(k,v) })
      else {
        var i = 0
        while (i < underlying.length - 1 && p(underlying(i).asInstanceOf[String], underlying(i+1).asInstanceOf[Json])) i += 2
        if (i >= underlying.length) this
        else {
          val b = Obj.builder
          b.appendFrom(underlying, 0, i >> 1)
          i += 2
          while (i < underlying.length - 1) {
            val k = underlying(i).asInstanceOf[String]
            val v = underlying(i+1).asInstanceOf[Json]
            b ~ (k,v)
            i += 2
          }
          b.result
        }
      }
    }
    def countKeys(keyset: Option[collection.Set[String]]): collection.Map[String, Int] = keyset match {
      case None =>
        if (myMap ne null) myMap.map{ case (k,v) => k -> 1 }
        else {
          val arm = new scala.collection.mutable.AnyRefMap[String, Int]
          foreach((k, _) => arm(k) = arm.getOrNull(k) + 1)
          arm
        }
      case Some(ks) =>
        if (myMap ne null) myMap.collect{ case (k, v) if ks(k) => k -> 1 }
        else {
          val arm = new scala.collection.mutable.AnyRefMap[String, Int]
          foreach((k, _) => if (ks(k)) arm(k) = arm.getOrNull(k) + 1)
          arm
        }
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
    def iterator: Iterator[(String, Json)] = 
      if (myMap ne null) myMap.iterator
      else new collection.AbstractIterator[(String, Json)] {
        private[this] var i = 0
        def hasNext = (i < underlying.length - 1)
        def next = {
          val key = underlying(i).asInstanceOf[String]
          val value = underlying(i+1).asInstanceOf[Json]
          i += 2
          (key, value)
        }
      }
    override def hashCode =
      if (hasDuplicateKeys) {
        var h = 938456718
        var i = 0
        while (i < underlying.length-1) {
          h = scala.util.hashing.MurmurHash3.mix(h, underlying(i).hashCode)
          h = scala.util.hashing.MurmurHash3.mix(h, underlying(i+1).hashCode)
          i += 2
        }
        scala.util.hashing.MurmurHash3.finalizeHash(h, underlying.length >>> 1)
      }
      else if (myMap ne null) {
        var h = 192375189
        var i = myMap.iterator
        while (i.hasNext) {
          val (k,v) = i.next
          h ^= scala.util.hashing.MurmurHash3.mix(k.hashCode, v.hashCode)
        }
        scala.util.hashing.MurmurHash3.finalizeHash(h, myMap.size)
      }
      else {
        var h = 192375189
        var i = 0
        while (i < underlying.length - 1) {
          h ^= scala.util.hashing.MurmurHash3.mix(underlying(i).hashCode, underlying(i+1).hashCode)
          i += 2
        }
        scala.util.hashing.MurmurHash3.finalizeHash(h, underlying.length >>> 1)
      }
    def equalsMap(m: collection.Map[String, Json]): Boolean =
      if (myMap ne null) m == myMap
      else if (hasDuplicateKeys) false
      else if (underlying.length < 12) {
        var i = 0
        while (i < underlying.length - 1) {
          val o = m.get(underlying(i).asInstanceOf[String])
          if (!o.isDefined) return false
          else if (o.get != underlying(i+1)) return false
          i += 2
        }
        true
      }
      else get() == m
    def equalsArray(a: Array[AnyRef], orderMatters: Boolean): Boolean =
      if (myMap ne null) {
        if (orderMatters) false
        else if ((a.length >>> 1) != myMap.size) false
        else {
          var i = 0
          while (i < a.length - 1) {
            val o = myMap.get(a(i).asInstanceOf[String])
            if (!o.isDefined) return false
            else if (o.get != a(i+1)) return false
            i += 2
          }
          true
        }
      }
      else if ((a.length >>> 1) != (underlying.length >>> 1)) false
      else {
        var i = 0
        if (orderMatters) {
          while (i < a.length - 1) {
            if (a(i) != underlying(i)) return false
            if (a(i+1) != underlying(i+1)) return false
            i += 2
          }
        }
        else {
          while (i < a.length - 1) {
            val o = getOrNull(a(i).asInstanceOf[String])
            if (o eq null) return false
            else if (o != a(i+1)) return false
            i += 2
          }
        }
        true
      }
    override def equals(a: Any): Boolean = a match {
      case o: Obj =>
        if (size != o.size) return false
        if (myMap ne null) o.equalsMap(myMap)
        else o.equalsArray(underlying, hasDuplicateKeys)
      case _ => false
    }
    private def jsonStringFromMap(sb: java.lang.StringBuilder) {
      if (myMap.size > 1) sb append "{ " else sb append '{'
      var first = true
      val it = myMap.iterator
      while (it.hasNext) {
        val kv = it.next
        if (!first) sb append ", "
        Str.addJsonString(sb, kv._1)
        sb append ':'
        kv._2.jsonString(sb)
        first = false
      }
      if (myMap.size > 1) sb append " }" else sb append '}'
    }
    override def jsonString(sb: java.lang.StringBuilder) {
      if (underlying ne null) {
        if (underlying.length > 3) sb append "{ " else sb append '{'
        var i = 0
        while (i < underlying.length-1) {
          if (i > 0) sb append ", "
          Str.addJsonString(sb, underlying(i).asInstanceOf[String])
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
          b = Str(underlying(i).asInstanceOf[String]).jsonBytes(b, refresh)
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
          b = Str(underlying(i).asInstanceOf[String]).jsonChars(b, refresh)
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

    /** Creates a new builder for JSON objects. */
    def builder: Build[Obj] = new Build[Obj]

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

    /** Adds to this JSON object a JSON string key and a JSON abstract syntax tree element but only if the element is not null, NaN, empty, or an error */
    def ~?(key: Str, ja: Jast) = (new Build[Obj]) ~? (key, ja)

    /** Adds to this JSON object a string key and a JSON abstract syntax tree element but only if the element is not null, NaN, empty, or an error */
    def ~?(key: String, ja: Jast) = (new Build[Obj]) ~? (key, ja)

    /** Adds to this JSON object a JSON string key and string value but only if the string value is nonempty and non-null */
    def ~?(key: Str, value: String) = (new Build[Obj]) ~? (key, value)

    /** Adds to this JSON object a string key and string value but only if the string value is nonempty and non-null */
    def ~?(key: String, value: String) = (new Build[Obj]) ~? (key, value)

    /** Optionally adds to this JSON object a JSON string key and value of an object convertible to JSON */
    def ~?[A: Jsonize](key: Str, oa: Option[A]) = (new Build[Obj]) ~? (key, oa)

    /** Optionally adds to this JSON object a string key and value of an object convertible to JSON */
    def ~?[A: Jsonize](key: String, oa: Option[A]) = (new Build[Obj]) ~? (key, oa)

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

    /** Begins building a JSON object starting with an existing JSON object. */
    def ~~(o: Obj) = (new Build[Obj]) ~~ o


    /** A builder for JSON objects. */
    class Build[T >: Obj] {
      private[this] var i = 0
      private[this] var a: Array[AnyRef] = new Array[AnyRef](6)
      private[this] def append(key: String, js: Json): this.type = {
        if (i >= a.length-1) a = java.util.Arrays.copyOf(a, ((a.length << 1) | a.length) & 0x7FFFFFFE)
        a(i) = key
        a(i+1) = js
        i += 2
        this
      }
      private[this] def append(key: Str, js: Json): this.type = append(key.text, js)

      private[this] def isNonEmptyJson(ja: Jast) = ja match {
        case _: Json.Bool => true 
        case s: Json.Str => !s.text.isEmpty
        case n: Json.Num => n.isDouble || n.explicitTextForm
        case a: Json.Arr => a.size > 0
        case o: Json.Obj => o.size > 0
        case _ => false
      }

      private[jsonal] def appendFrom(that: Array[AnyRef], i0: Int, n: Int): this.type = {
        val m = math.max(0, 2*math.min((that.length - i0) >> 1, n))
        if (i >= a.length - m) {
          var k = a.length
          while (i >= k - m && k < 0x7FFFFFFE) k = ((k << 1) | k) & 0x7FFFFFFE
          a = java.util.Arrays.copyOf(a, k)
          System.arraycopy(that, i0, a, i, 2*m)
          i += 2*m
        }
        this
      }

      /** Finish building this JSON object and return it.  See also `~` with a `JsonBuildTerminator`. */
      def result(): Obj =
        new AtomicObj(if (i==a.length) a else java.util.Arrays.copyOf(a,i), null)

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

      /** Adds to this JSON object a JSON string key and a JSON abstract syntax tree element but only if the element is not null, NaN, empty, or an error */
      def ~?(key: Str, ja: Jast): this.type = if (isNonEmptyJson(ja)) this ~ (key, ja.asInstanceOf[Json]) else this

      /** Adds to this JSON object a string key and a JSON abstract syntax tree element but only if the element is not null, NaN, empty, or an error */
      def ~?(key: String, ja: Jast): this.type = if (isNonEmptyJson(ja)) this ~ (key, ja.asInstanceOf[Json]) else this

      /** Adds to this JSON object a JSON string key and string value but only if the string value is nonempty and non-null */
      def ~?(key: Str, value: String): this.type = if ((value ne null) && !value.isEmpty) this ~ (key, Str(value)) else this

      /** Adds to this JSON object a string key and string value but only if the string value is nonempty and non-null */
      def ~?(key: String, value: String): this.type = if ((value ne null) && !value.isEmpty) this ~ (key, Str(value)) else this

      /** Optionally adds to this JSON object a JSON string key and value of an object convertible to JSON */
      def ~?[A](key: Str, oa: Option[A])(implicit jser: Jsonize[A]): this.type = if (oa.isDefined) this ~ (key, jser.jsonize(oa.get)) else this

      /** Optionally adds to this JSON object a string key and value of an object convertible to JSON */
      def ~?[A](key: String, oa: Option[A])(implicit jser: Jsonize[A]): this.type = if (oa.isDefined) this ~ (key, jser.jsonize(oa.get)) else this

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

      /** Adds to this JSON object the contents of another JSON object. */
      def ~~(o: Obj): this.type = {
        val ao = o.asInstanceOf[AtomicObj]
        if (ao.underlying ne null) { ao.foreach{ case (k,v) => this ~ (k,v) }; this }
        else appendFrom(ao.underlying, 0, ao.size)
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
      JsonRecyclingParser.Obj(JsonRecyclingParser recycleInputStream input, ep)

    private final class RelaxedObjFromJson extends FromJson[Obj] {
      override def parse(input: Json): Either[JastError, Obj] = Obj.parse(input)

      override def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint) =
        JsonStringParser.Obj(input, i0, iN, ep, relaxed = true)

      override def parse(input: ByteBuffer) = JsonByteBufferParser.Obj(input, relaxed = true)

      override def parse(input: CharBuffer) = JsonCharBufferParser.Obj(input, relaxed = true)

      override def parse(input: java.io.InputStream, ep: FromJson.Endpoint) =
        JsonRecyclingParser.Obj(JsonRecyclingParser recycleInputStream input, ep, relaxed = true) 
    }

    val relaxed: FromJson[Obj] = new RelaxedObjFromJson
  }
}
