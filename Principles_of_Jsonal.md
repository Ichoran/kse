# Principles Underlying the Design and Implementation of Jsonal

Jsonal is a highly opinionated JSON parser, printer, and Abstract Syntax Tree (AST).

It is intended to excel at being an intermediate format for data, including numeric data.  The design goals for Jsonal are:

1. **Be scrupulous.**  Adhere perfectly to the JSON specification.
2. **Be efficient.**  JSON I/O should never be the bottleneck.
3. **Be concise.**  Place the focus on the logic of what to accomplish.
4. **Be helpful.**  Provide powerful, flexible methods for the most common operations.  Provide comprehensible error messages if anything goes wrong.
5. **Be mathematical.** Understand numbers and precision and provide ways to work with them.

Jsonal rejects other valuable goals as incompatible with its primary goals:

1. **Be immutable.**  Immutability aids consistency, but it is not always compatible with efficiency.
2. **Be reshapeable.**   Reshaping an AST is expensive without structural sharing, but structural sharing is not always efficient.
3. **Be idealistic.**  If something is RECOMMENDED, that's nice, but we won't assume it.

If you wish to accomplish these goals, which complement each other nicely, convert to another AST.

In addition, Jsonal does not have a streaming mode.  If a single file is too big to process in memory, you should use another approach.

## Why Jsonal?

No other JSON parser/printer/AST for Scala fulfills the design goals.  (Jawn comes close.)

# How can Jsonal...

## Be Scrupulous?

JSON parsers and ASTs often take shortcuts that enable them to represent only JSON that avoids "SHOULD NOT" directives in the specification.  These are not, strictly speaking, compliant parsers.  Because Jsonal insists upon supporting the entire JSON specification, it must:

1. Represent numbers as a non-primitive type.  JSON allows arbitrary numbers; Jsonal must also.
2. Not embed NaN and +/- Infinity bare in JSON.  These are not allowed; they must be quoted as strings, or be `null`.
3. Not represent a JSON object as a single-valued unordered map.  JSON is a linear format, and does not forbid multiple values with the same key, so the order and number of values for the same key must be preserved.
4. Preserve the actual value of a number, not some internal approximation thereof.

Jsonal is not so scrupulous as to:

1. Preserve whitespace.  It is semantically irrelevant.
2. Preserve the encoding of strings.  Again, the choice of unicode escape vs. plain unicode is semantically irrelevant.

## Be Efficient?

All parsing and printing routines are written by hand to optimize speed.  (`sun.misc.Unsafe` is used, but only for InputStreams.)  But there are additional choices that that are dictated by efficiency:

1. Parsing big numbers can be slow.  No need to unless they're wanted, so big numbers must be stored as a `String` or similar format.
2. Parsing small numbers can be fast and happen in one pass.  Small numbers should thus be parsed and stored as a `Double` or similar.
3. Parsing arrays of numbers creates a lot of boxes.  Numbers should be unboxed into a primitive array when possible.
4. Constructing key-value maps is slow.  Parsing must be into an array.
5. Looking up keys in objects is slow.  Lookup must be from a map.  (Lazily created based on the array.)
6. Extra layers of boxing, or exceptions, for errors are slow.  The core data type must represent errors also (but outside of the JSON types).

## Be Concise?

The JSON specification is for a serialized data format, and it talks in terms of several basic types.  Jsonal uses exactly the same types (but not necessarily only those types), and basic operators to build and destructure JSON.

1. The basic hierarchy of JSON data types should map exactly to the types in the specification, and have the same properties.  A JSON value is `Json`.  A complete set of subtypes are `Null`, `True`, `False`, `Str`, `Num`, `Arr`, and `Obj`, corresponding exactly to the seven JSON types (in abbreviated form).
2. Building a single JSON value is as simple as `Json(x)`, where `x` is an appropriate type.
3. Building a composite value is accomplished with builders where you just list the values: `Json ~ x ~ y ~ Json`.  Note that the builders are delimited with `Json`.

## Be Helpful?

The JSON specification does not map perfectly onto Scala's data types.  Therefore, some adjustment to the JSON type hierarchy and some utility methods are advisable.

1. Values that might be JSON or might be in error are of type `Jast` (JSON Abstract Syntax Tree).  This encapsulates an error state as well as correct states for lookups that may fail (e.g. looking up a missing key).
2. Natural destructuring is provided by `apply` methods for keys and values; on failure, a `Jast` is returned (upon which all destructuring methods are no-ops and just preserve the original error).
3. Parsers and printers are provided for most common data types or wrappers thereof: `String`, `ByteBuffer`, `CharBuffer`, `InputStream`.  A prettyprinter is provided also.
4. Converters from common types are provided (mostly via typeclasses), so you don't have to select which type of JSON value you're building.  Usually there is a single obvious choice.

## Be Mathematical?

The mismatch beween JSON's "numbers are decimal numbers of arbitrary size" and computing's heavy reliance upon `Double` provides some challenges for using JSON as a data exchange format for number-heavy data.

1. Mathematics is mostly done with Doubles, so I/O of Doubles should be fast and easy.
2. Many physical sciences have an idea of precision or significant figures which, if applied, can discard roundoff error and/or allow more compact JSON files.  What is known about precision can be supplied.
3. Floats are also sometimes used in place of Doubles.  But those may have rounding errors.  It shouldn't be easy to mistake one for the other.
4. Arrays of Doubles should go to-and-from arrays of Doubles. The `Arr` type thus has subtypes `All` (for arbitrary `Json` values) and `Dbl` (for Doubles).
