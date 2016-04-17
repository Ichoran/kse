# Jsonal Tutorial

Jsonal is part of the Kerr Scala Extensions.  Jsonal is really fast.

It's pretty easy too:

```scala
import kse.jsonal._, JsonConverters._
val xs = Jast.parse("""{"data" : [1, 1.5, 2, 2.5]}""")("data").to[Array[Double]].right.get
val j = Json ~ ("data", xs.map(_ * 3)) ~ Json
```

To get it, add the following to your `build.sbt` file (or the equivalent to your
build tool of choice, or use this information to download jars manually):

```scala
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.ichoran" %% "kse" % "0.4-SNAPSHOT"
```

_**There is currently no stable released version.  0.4-SNAPSHOT is the latest live version.**_


## Introduction

Jsonal is a very fast JSON parsing library, usually comparable in speed to Boon
and considerably faster that Jackson, Play-Json, Argonaut, and even Jawn.

It's designed to provide serialization and deserialization of a Json AST.  The
root class for the AST hierarchy is `Jast`.  If you have a `String`, a
`ByteBuffer`, an `Array[Byte]`, a `CharBuffer`, an `Array[Char]`, or even
an `InputStream` containing valid JSON, you can get it like so:

```scala
import kse.jsonal._
Jast parse """{"fish": ["salmon", 42]}"""
```

`Jast` contains two subtraits: `JastError` and `Json`.  `JastError` contains
a `String` description of the error, optional positional information, and
possibly a child `Jast` that was the cause of this error.  For instance:

```scala
scala> val wrong = Jast parse """{"fish": {"salmon" = 42}}"""
wrong: kse.jsonal.Jast =
error reading value 1 (key fish) in object
  near character 9
  because object's key salmon not followed with ':'
    near character 19
```

`Json` corresponds to a JSON Value as defined at [www.json.org](http://www.json.org)
and in [RFC7159](https://tools.ietf.org/html/rfc7159).  Its subtraits are

* `Json.Null` to represent the JSON null value
* `Json.Bool` which has a boolean `value` method and is one of two objects, `Json.Bool.True` or `Json.Bool.False`
* `Json.Str` which has a `text` method
* `Json.Num` which can always give you the best approximating `Double` value with its `double` method but also can store and retrieve arbitrary precision numbers with other methods
* `Json.Arr` for JSON arrays; this is one of
  - `Json.Arr.Dbl` which wraps an array of doubles accessible via the `doubles` method
  - `Json.Arr.All` which wraps an array of `Json` values accessible via the `values` method
* `Json.Obj` for JSON objects, which can look up objects by (`String`) key but can also hold multiple values for the same key and keep track of key order.


## Working with `Json` syntax trees

### Universal accessors

`Jast` and its children implement a set of universal accessor methods that allow
you to optimistically try to look things up in a JSON syntax tree.  These are

* `isError` to tell you if this is actually an error state, not a JSON value
* `isNull` to tell if the value is `Null`
* `bool` to give an `Option[Boolean]` containing a `Boolean` value if available
* `double` to give the best approximating `Double` value
* `string` to give an `Option[String]` containing a `String` value if available
* `apply(index: Int)` to index into an array.  You can use the Lift-style `\` also.
* `apply(key: String)` to look up a value for a given key in an object.  Lift-style `\` works also.
* `nullError` to convert from a `JastError` to a `Null` if you don't care why something failed

and a few other methods.  These allow you to optimistically index into JSON ASTs and
only check at the end whether you got what you expected or whether you have an error.

Let's consider the following JSON:

```scala
val tree = Jast parse """{
  "fish": ["salmon", "cod", "herring", "tuna"],
  "price": 19.99,
  "store": {"name": "seafood market", "open": true, "location": null }
}"""
```

If we know the structure of the tree, we can index into it directly and pull out values:

```scala
scala> val myFish = tree("fish")(2).string
myFish: Option[String] = Some(herring)

scala> val myCost = tree("price").double
myCost: Double = 19.99

scala> val canGo = tree("store")("open").bool.getOrElse(false)
canGo: Boolean = true

scala> val wrongCanGo = tree("open").bool.getOrElse(false)
wrongCanGo: Boolean = false

scala> tree(42)("foo").nullError
res0: kse.jsonal.Json = null

scala> tree \ 42 \ "foo"
res1: kse.jsonal.Jast = Indexing into something that is not an array
```

### Pattern matching

Pattern matching can also be used to access parts of a JSON tree.  Note that the
namespace is nested (e.g. JSON values all start with `Json.`); if brevity is
your thing you can import the short forms.

Let's retrieve all the fish this time:

```scala
tree match {
  case o: Json.Obj => o("fish") match {
    case j: Json.Arr =>
      val b = Vector.newBuilder[String]
      j.foreach(ji => ji match {
        case s: Json.Str => b += s.text
        case _ => throw new Exception("Fish name is not a string?!")
      })
      b.result
    case _ => throw new Exception("fish isn't an array!")
  }
  case _ => throw new Exception("Tree isn't an object!")
}
```

Obviously this is quite verbose, but it provides full control over how the
tree is traversed and how errors are handled.

For a even more compact alternative, see the conversion section below!

### Numbers

JSON represents numbers in a human-friendly way, not a machine-friendly way.
Double precision floating point is ubiquitous, but JSON does not limit itself
to numbers that can be represented as a `Double`, nor can it represent the
non-finite `Double` values.

Because Jsonal is designed for high-performance parsing of numeric data, it
has to address this mismatch.

Non-finite `Double` values are all encoded as a JSON `null`.  Thus, values
of plus and minus `Infinity` are not preserved.

```scala
scala> val inf = Json(Double.PositiveInfinity)
inf: kse.jsonal.Json = null

scala> val ninf = Json(Double.NegativeInfinity)
ninf: kse.jsonal.Json = null

scala> val nan = Json(Double.NaN)
nan: kse.jsonal.Json = null
```

All JSON numbers will be decoded to a `Double` or `Long` representation, but by
default the full version will also be accessible with `Json.Num`'s `big` method (for a `BigDecimal`)
or simply the `toString` method.

```scala
scala> val jBig = Jast.parse("1234e999") match { case n: Json.Num => n; case _ => throw new Exception }
jBig: kse.jsonal.Json.Num = 1234e999

scala> val d = jBig.double
d: Double = Infinity

scala> val bd = jBig.big
bd: BigDecimal = 1.234E+1002

scala> val s = jBig.toString
s: String = 1234e999
```

If a `Json.Num` is a `Long` value, its `isLong` method will return true. Regardless
of whether a number is a `Long`, a `Long` approximation can be had with the
`long` method:

```scala
scala> val is = Seq(Json.Num(1234567890L).isLong, Json.Num(BigDecimal(1.7)).isLong)
is: Seq[Boolean] = List(true, false)

scala> Seq(Json.Num(1234567890L).long, Json.Num(BigDecimal(1.7)).long)
res8: Seq[Long] = List(1234567890, 1)
```

If you want fractional values to be rounded to the nearest `long`, you should use
`if (jn.isLong) jn.long else math.round(jn.double)`.

If a `Json.Num` has a finite `Double` approximation the `isDouble` method will return true
even if it is inexact, while the `isPrimitive` method will pick out anything that can
be represented exactly by either a `Long` or a `Double` (but be warned, this can
be rather slow as it often requires creation of a string corresponding to the approximating
`Double`).

```scala
scala> val ds = Seq(
     |   Json.Num(Long.MaxValue).isDouble,
     |   Json.Num(BigDecimal("1e1000")).isDouble,
     |   Json.Num(BigDecimal("1.01234567890123456789")).isDouble
     | )
ds: Seq[Boolean] = List(true, false, true)

scala> val ps = Seq(
     |   Json.Num(Long.MaxValue).isPrimitive,
     |   Json.Num(BigDecimal("1e1000")).isPrimitive,
     |   Json.Num(BigDecimal("1.01234567890123456789")).isPrimitive,
     |   Json.Num(BigDecimal("1.7")).isPrimitive
     | )
ps: Seq[Boolean] = List(true, false, false, true)
```

The situation is different with arrays.  If possible, arrays will be stored
as an array of Doubles inside a `Json.Arr.Dbl`.  Any values of `null` will be converted
to `Double.NaN`; during parsing, normally an exact `Double` representation is required,
but if an approximation is okay there is a `relaxed` set of parsers:

```scala
scala> val strictly = (Jast parse """[1, 1.0123456789012345]""").getClass
strictly: Class[_ <: kse.jsonal.Jast] = class kse.jsonal.Json$Arr$All

scala> val relaxedly = Jast.relaxed.parse("""[1, 1.0123456789012345]""").getClass
relaxedly: Class[_ <: kse.jsonal.Jast] = class kse.jsonal.Json$Arr$Dbl
```

### Traversing and transforming

The `Json.Arr` and `Json.Obj` methods contain a small set of high-level methods for
traversing or transforming the contents.

**The contents of arrays and objects is (usually) mutable.**  Usually they are
backed by arrays, and you can get access to those and change them.  Only do this
if you _really_ know what you're doing (e.g. speed is critical)!  Normally this would
be poor design, but Jsonal is for when _speed is critical_ so it exposes these
methods to you.

In addition, `Json.Arr` contains `foreach`, `filter`, and `iterator` methods; the first two are
specialized for speed if you know you have a `Json.Arr.Dbl`.

```scala
scala> import JsonConverters._
import JsonConverters._

scala> val a = (Json.Arr ~ "fish" ~ 2.7 ~ null ~ Json.Arr)
a: kse.jsonal.Json.Arr = ["fish", 2.7, null]

scala> a.foreach(println)
"fish"
2.7
null

scala> val simples = a.filter(_.simple)
simples: kse.jsonal.Json.Arr = ["fish", 2.7, null]

scala> val notDoubles = a.filter(_.double.isNaN)
notDoubles: kse.jsonal.Json.Arr = ["fish", null]

scala> val vec = a.iterator.toVector
vec: Vector[kse.jsonal.Json] = Vector("fish", 2.7, null)
```

`Json.Obj` contains the same set of methods, plus a `transformValues` method that
will modify the values in place if possible, or return a new object if not.
Since modification in place is not always possible, one should always use the returned
object:

```scala
scala> val o = tree match { case o: Json.Obj => o; case _ => ??? }
o: kse.jsonal.Json.Obj = { "fish":["salmon", "cod", "herring", "tuna"], "price":19.99, "store":{ "name":"seafood market", "open":true, "location":null } }

scala> val o2 = o.transformValues((key, value) => if (key == "price") Json("on sale for "+value.double.toString) else value)
o2: kse.jsonal.Json.Obj = { "fish":["salmon", "cod", "herring", "tuna"], "price":"on sale for on sale for 19.99", "store":{ "name":"seafood market", "open":true, "location":null } }

scala> val itChanged = tree
itChanged: kse.jsonal.Jast = { "fish":["salmon", "cod", "herring", "tuna"], "price":"on sale for on sale for 19.99", "store":{ "name":"seafood market", "open":true, "location":null } }
```

## Building JSON syntax trees

### Assembly

You can create a JSON value by putting the desired contents as an argument to `Json()`:

```scala
scala> import kse.jsonal._
import kse.jsonal._

scala> Json()
res0: kse.jsonal.Json = null

scala> Json(true)
res1: kse.jsonal.Json = true

scala> Json("salmon")
res2: kse.jsonal.Json = "salmon"

scala> Json(912837498173991L)
res3: kse.jsonal.Json = 912837498173991

scala> Json(1.415e-16)
res4: kse.jsonal.Json = 1.415E-16

scala> Json(Array(Json(false), Json("fish")))
res5: kse.jsonal.Json = [false, "fish"]

scala> Json(Map("fish" -> Json("salmon")))
res6: kse.jsonal.Json = {"fish":"salmon"}

scala> Json(Array(2.7, 3.3, Double.NaN))
res7: kse.jsonal.Json = [2.7, 3.3, null]
```

If you include the contents of `JsonConverters`, you can also generate Json from
anything that has implemented the `AsJson` trait, or for anything that has a
`Jsonize` typeclass:

```scala
scala> import JsonConverters._
import JsonConverters._

scala> Json(Vector("herring", "pike"))
res9: kse.jsonal.Json = ["herring", "pike"]

scala> class X extends AsJson { def json = Json(Map("X" -> true)) }
defined class X

scala> Json(new X)
res10: kse.jsonal.Json = {"X":true}

scala> implicit val jsonizeSymbol = new Jsonize[Symbol] { def jsonize(s: Symbol) = Json(s.toString) }
jsonizeSymbol: kse.jsonal.Jsonize[Symbol] = $anon$1@485f4044

scala> Json('widget)
res11: kse.jsonal.Json = "'widget"
```

Alternatively, you can create arrays and objects with builders: `Json.Arr.All.builder`,
`Json.Arr.Dbl.builder`, and `Json.Obj.builder` will get one for you, or you can simply state
the type you want to start building.

To add an element, use `~`.  To add a collection of elements, use `~~`.  To optionally
add a key-value pair only when the value is non-empty, use `~?`.  Maps take pairs `(String, Json)`,
while arrays take single values; either can take advantage of the presence of the `AsJson` trait
or the `Jsonize` typeclass.

To end, call `.result` on the builder, or re-state the type you were building with after a `~` or `~~` (either is fine).

Some examples:

```scala
scala> Json ~ "true" ~ true ~ 1 ~ Json
res12: kse.jsonal.Json = ["true", true, 1]

scala> Json ~ ("true", true) ~ ("false", 'widget) ~ Json
res13: kse.jsonal.Json = { "true":true, "false":"'widget" }

scala> // This one won't work: start and end types are not the same!

scala> Json.Obj ~ ("true", true) ~ ("false", 'widget) ~ Json
<console>:18: error: type mismatch;
 found   : kse.jsonal.Json.type
 required: kse.jsonal.JsonBuildTerminator[kse.jsonal.Json.Obj]
Note: kse.jsonal.Json >: kse.jsonal.Json.Obj (and kse.jsonal.Json.type <: kse.jsonal.JsonBuildTerminator[kse.jsonal.Json]), but trait JsonBuildTerminator is invariant in type T.
You may wish to define T as -T instead. (SLS 4.5)
       Json.Obj ~ ("true", true) ~ ("false", 'widget) ~ Json
                                                         ^

scala> Json.Obj ~ ("true", true) ~ ("false", 'widget) ~ Json.Obj
res15: kse.jsonal.Json.Obj = { "true":true, "false":"'widget" }

scala> Json ~? ("fish", Seq("pike", "tuna")) ~? ("wish", List.empty[String]) ~ Json
res16: kse.jsonal.Json = {"fish":["pike", "tuna"]}

scala> val b = Json ~~ Seq(true, false, false, false)
b: kse.jsonal.Json.Arr.All.Build[kse.jsonal.Json] = kse.jsonal.Json$Arr$All$Build@50ec5ab8

scala> b ~~ Iterator("this", "is", "more", "stuff")
res17: b.type = kse.jsonal.Json$Arr$All$Build@50ec5ab8

scala> b ~ Json
res18: kse.jsonal.Json = [true, false, false, false, "this", "is", "more", "stuff"]
```

Builders are mutable (as always!) so take care!


### Parsing

Parsing is handled by four separate high-performance implementations of parsing functionality:
`JsonStringParser`, `JsonCharBufferParser`, `JsonByteBufferParser`, and `JsonRecyclingParser`.

Of these, `JsonStringParser` is the fastest, followed by `JsonRecyclingParser` (which is designed to
take `InputStream`s but can be used in other contexts also), followed by the buffer parsers.  Note
that `JsonRecylingParser` uses `sun.misc.unsafe`, so you may wish to avoid it in certain contexts.

Normally, you never need to interface with these parsers directly.  `Jast` and each of the JSON value
types have their own `parse` methods on the companion object.  For `Jast`, every method simply returns
a `Jast`.  For the others, `parse` returns an `Either` with the desired type as the right branch
and `JastError` as the left.

Here are some examples of parsing:

```scala
scala> import kse.jsonal._
import kse.jsonal._

scala> Json.Bool parse "true"
res0: Either[kse.jsonal.JastError,kse.jsonal.Json.Bool] = Right(true)

scala> Jast parse "true"
res1: kse.jsonal.Jast = true

scala> Jast parse "grue"
res2: kse.jsonal.Jast =
invalid character: 'g'
  near character 0

scala> Json.Bool parse "grue"
res3: Either[kse.jsonal.JastError,kse.jsonal.Json.Bool] =
Left(Expected boolean but found character g
  near character 0)

scala> Jast parse java.nio.CharBuffer.wrap(Array('f', 'a', 'l', 's', 'e'))
res4: kse.jsonal.Jast = false

scala> Json.Arr parse java.nio.ByteBuffer.wrap("[2, 3, 2]".getBytes)
res5: Either[kse.jsonal.JastError,kse.jsonal.Json.Arr] = Right([2, 3, 2])

scala> Json.Str parse (new java.io.ByteArrayInputStream("\"salmon\"".getBytes))
res6: Either[kse.jsonal.JastError,kse.jsonal.Json.Str] = Right("salmon")

scala> Json.Obj parse """{"a": {"b": {"c": {"d": null}}}}"""
res7: Either[kse.jsonal.JastError,kse.jsonal.Json.Obj] = Right({"a":{"b":{"c":{"d":null}}}})
```

With `Jast` you can even parse a file (as long as you're happy with basic file error handling).

### User-defined objects

Scala classes that are not listed in `JsonConverters` can be converted to a JSON syntax tree
by having them implement the `AsJson` trait, or by providing a `Jsonize` typeclass for them.

In the former case, you simply define a `json` method on the class, which then works
with the JSON assembly methods:

```scala
scala> import kse.jsonal._, JsonConverters._
import kse.jsonal._
import JsonConverters._

scala> case class Person(first: String, last: String) extends AsJson {
     |   def json = Json ~ ("first name", first) ~ ("surname", last) ~ Json
     | }
defined class Person

scala> val obviously = Person("John", "Smith").json
obviously: kse.jsonal.Json = { "first name":"John", "surname":"Smith" }
```

In the latter case, if an implicit `Jsonize` typeclass is provided you can use `Json()`
to convert a class to its JSON representation:

```scala
scala> import kse.jsonal._, JsonConverters._
import kse.jsonal._
import JsonConverters._

scala> case class Person(first: String, last: String) {}
defined class Person

scala> implicit val jsonizePerson = new Jsonize[Person] {
     |   def jsonize(p: Person) = Json ~ p.last ~ p.first ~ Json
     | }
jsonizePerson: kse.jsonal.Jsonize[Person] = $anon$1@57a92d5a

scala> Json(Person("John", "Smith"))
res0: kse.jsonal.Json = ["Smith", "John"]

scala> Json ~ ("dude", Person("Bill", "Surfer")) ~ Json
res1: kse.jsonal.Json = {"dude":["Surfer", "Bill"]}
```

No macros are provided to make case classes work automatically, but it's usually
very easy to write a `json` method or `Jsonize` typeclass.

## Extracting structured data from JSON syntax trees

### Build-in data types

Data types that correspond perfectly to the JSON values can be extracted with
pattern matching or accessor methods.  In particular, numeric arrays can
be pattern-matched out for high-efficiency reading of numeric data.

In addition, the `asMap` method on `Json.Obj` can be handy to get a map view of
the data.

```scala
scala> import kse.jsonal._
import kse.jsonal._

scala> Jast.parse("[47, 13, 19291, null, 16, 2.2]") match {
     |   case jad: Json.Arr.Dbl => jad.doubles
     |   case _ => throw new Exception("oh noes")
     | }
res0: Array[Double] = Array(47.0, 13.0, 19291.0, NaN, 16.0, 2.2)

scala> Json.Bool(true).value
res1: Boolean = true

scala> Json.Str("herring").text
res2: String = herring

scala> Json.Num(1234567890L).double
res3: Double = 1.23456789E9

scala> (Json.Obj ~ ("one", Json(1)) ~ ("two", Json(2)) ~ Json.Obj).asMap
res4: scala.collection.Map[String,kse.jsonal.Json] = Map(two -> 2, one -> 1)
```

More complex data types can be pulled out with the `to` method.  This
requires a `FromJson` typeclass to convert from a JSON syntax tree to
a particular target class.  Typeclasses exist for the standard Scala
collections, basic data types, `Option`, `Either`, and several of the
most common time structures from `java.time`.

The requested type is returned in the `Right` branch; if there is an
error in parsing, a `JastError` is returned in the `Left` branch.

```scala
scala> import kse.jsonal._, JsonConverters._
import kse.jsonal._
import JsonConverters._

scala> Jast.parse("[47, 13, 19291, null, 16, 2.2]").to[Array[Double]]
res0: Either[kse.jsonal.JastError,Array[Double]] = Right([D@394d78aa)

scala> Jast.parse("[47, 13, 19291, null, 16, 2.2]").to[Vector[Double]]
res1: Either[kse.jsonal.JastError,Vector[Double]] = Right(Vector(47.0, 13.0, 19291.0, NaN, 16.0, 2.2))

scala> Jast.parse("true").to[Option[Boolean]]
res2: Either[kse.jsonal.JastError,Option[Boolean]] = Right(Some(true))

scala> Jast.parse("77.7").to[Either[Double, String]]
res3: Either[kse.jsonal.JastError,Either[Double,String]] = Right(Left(77.7))

scala> Jast.parse("""{"fish": [1, 2, 3], "wish": [3, 2, 1]}""").to[Map[String, Array[Double]]]
res4: Either[kse.jsonal.JastError,Map[String,Array[Double]]] = Right(Map(fish -> [D@35217067, wish -> [D@12957644))

scala> Jast.parse("""{"fish": [1, 2, 3], "wish": [3, 2, 1]}""").to[Map[String, collection.mutable.ArrayBuffer[Double]]]
res5: Either[kse.jsonal.JastError,Map[String,scala.collection.mutable.ArrayBuffer[Double]]] =
  Right(Map(fish -> ArrayBuffer(1.0, 2.0, 3.0), wish -> ArrayBuffer(3.0, 2.0, 1.0)))

scala> Jast.parse("\"PT99H44M2.2S\"").to[java.time.Duration]
res6: Either[kse.jsonal.JastError,java.time.Duration] = Right(PT99H44M2.2S)
```

### Additional or user-defined data types

The `to` method relies upon instances of the `FromJson` typeclass for the method
being parsed.  How complex it is to provide this functionality depends upon how
much error handling you want to perform.  Here is a simple example where correct
JSON is parsed, but various incorrect forms may also pass (e.g. JSON with severalkeys
keys corresponding to the same value).

Note that you can explicitly pass a `FromJson`--for example, a companion object--to
the `to` method even if it's not implicit.

```scala
scala> import kse.jsonal._, JsonConverters._
import kse.jsonal._
import JsonConverters._

scala> :paste
// Entering paste mode (ctrl-D to finish)

case class Person(first: String, last: String) extends AsJson {
  def json = Json ~ ("first", first) ~ ("last", last) ~ Json
}
object Person extends FromJson[Person] {
  def parse(j: Json): Either[JastError, Person] = Right(Person(
    j("first") match { case s: Json.Str => s.text; case _ => return Left(JastError("No first name")) },
    j("last") match { case s: Json.Str => s.text; case _ => return Left(JastError("No last name")) }
  ))
}

// Exiting paste mode, now interpreting.

defined class Person
defined object Person

scala> Json(Map("first" -> "John", "last" -> "Smith")).to(Person)
res1: Either[kse.jsonal.JastError,Person] = Right(Person(John,Smith))

scala> Person("Jane", "Doe").json.to(Person)
res2: Either[kse.jsonal.JastError,Person] = Right(Person(Jane,Doe))
```

However, to take advantage of more complex destructuring, you need to place the typeclass in scope implicitly:

```scala
scala> implicit val implicitPersonFromJson: FromJson[Person] = Person
implicitPersonFromJson: kse.jsonal.FromJson[Person] = Person$@5d4f6cb8

scala> (Json ~ Person("Yi", "Li") ~ Person("Mo", "Slo") ~ Json).to[Vector[Person]]
res3: Either[kse.jsonal.JastError,Vector[Person]] = Right(Vector(Person(Yi,Li), Person(Mo,Slo)))
scala> implicit val implicitPersonFromJson: FromJson[Person] = Person
implicitPersonFromJson: kse.jsonal.FromJson[Person] = Person$@5d4f6cb8

scala> (Json ~ Person("Yi", "Li") ~ Person("Mo", "Slo") ~ Json).to[Vector[Person]]
res3: Either[kse.jsonal.JastError,Vector[Person]] = Right(Vector(Person(Yi,Li), Person(Mo,Slo)))
```

Finally, note that instances of `FromJson` can parse from strings and `ByteBuffer`s and the like too:

```scala
scala> Person parse """{"first": "Arthur", "last": "Pendragon"}"""
res5: Either[kse.jsonal.JastError,Person] = Right(Person(Arthur,Pendragon))
```

## Printing and prettyprinting

### Basic output

All members of the JSON syntax tree will serialize themselves to valid JSON when
their `toString` method is called.  In addition, both `Json` and subclasses, plus
anything implementing `AsJson` can push its representation onto a `java.lang.StringBuilder`,
a `ByteBuffer` (with an operation to refresh the buffer when its full), or a `CharBuffer`.

Note: the Java StringBuilder was chosen because the JIT compiler will sometimes optimize
operations using it.  (Whether that actually matters in this library was not tested.)

Examples:

```scala
scala> import kse.jsonal._, JsonConverters._
import kse.jsonal._
import JsonConverters._

scala> val j = Json ~ true ~ (Json ~ ("fish", "salmon") ~ ("wish", true) ~ Json) ~ Json
j: kse.jsonal.Json = [true, { "fish":"salmon", "wish":true }]

scala> j.toString
res0: String = [true, { "fish":"salmon", "wish":true }]

scala> { val sb = new java.lang.StringBuilder
     |   j.jsonString(sb)
     |   sb.toString
     | }
res1: String = [true, { "fish":"salmon", "wish":true }]

scala> { val b = new Array[Byte](128)
     |   val bb = java.nio.ByteBuffer.wrap(b)
     |   j.jsonBytes(bb, _ => throw new Exception("no space left!"))
     |   new String(b, 0, bb.position)
     | }
res2: String = [true, { "fish":"salmon", "wish":true }]
```

### Pretty output

Jsonal includes a prettyprinting facility also, if a human may read the JSON output.
By default it has a two-space indent and a right margin of 78, but these can be set
if one creates a `PrettyJson` instance with different defaults.

```scala
scala> PrettyJson(j)
res3: String = [ true, { "fish": "salmon", "wish": true } ]

scala> val pj = new PrettyJson(indentation = 4, rightMargin = 10)
pj: kse.jsonal.PrettyJson = kse.jsonal.PrettyJson@18bc05f1

scala> { pj traverse j; pj.asString }
res4: String =
[ true,
    { "fish": "salmon",
        "wish": true
    } ]

scala> val pj2 = new PrettyJson(indentation = 2, rightMargin = 10)
pj2: kse.jsonal.PrettyJson = kse.jsonal.PrettyJson@4ce9a0fe

scala> { pj2 traverse j; pj2.asString }
res5: String =
[ true,
  { "fish": "salmon",
    "wish": true
  } ]
```

Note that despite being supposedly "pretty", the 4-space indent isn't lined up as well as the 2-space.

If you have a lot of numeric data, you may wish to use the `PrettyNumberJson` class, which will
store a list of object keys and array indices as history, and can use those to decide at what precision
to print a number.  For instance,

```scala
scala> val j2 = Json ~ ("x", 1.12345678) ~ ("y", 1.12345678) ~ ("x", Array(1.12345678, 98765432.1)) ~ Json
j2: kse.jsonal.Json = { "x":1.12345678, "y":1.12345678, "x":[1.12345678, 9.87654321E7] }

scala> // Format a max 3 of digits after decimal point, and max 5 digits after the first

scala> val three = PrettyNumberJson.FormatTo(3, 5)
three: kse.jsonal.PrettyNumberJson.FormatTo = FormatTo(3,5)

scala> val pnj = new PrettyNumberJson(contextFormatter = _ match {
     |   case "x" :: more => three
     |   case _ => PrettyNumberJson.defaultFormatter
     | }, rightMargin = 10)
pnj: kse.jsonal.PrettyNumberJson = kse.jsonal.PrettyNumberJson@16b1471e

scala> { pnj traverse j2; pnj.asString }
res6: String =
{ "x": 1.123,
  "y": 1.12345678,
  "x": [
    1.123,
    9.87654e+07
  ] }
```

This is far slower than standard output, but it can be worth it.
