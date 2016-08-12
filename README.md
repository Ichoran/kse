# Kerr Scala Extensions (KSE)

Does KSE work? [![Build Status](https://semaphoreci.com/api/v1/ichoran/kse/branches/master/badge.svg)](https://semaphoreci.com/ichoran/kse)

The Kerr Scala Extensions contain everything that the Scala standard library forgot that I have had time to create.

Want KSE?

If you're using SBT, make sure you have the following resolver in your build.sbt:

```scala
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
```

and add

```scala
libraryDependencies += "com.github.ichoran" %% "kse" % "0.4-SNAPSHOT"
```

If you're using something else, you can probably figure it out from the above.  Or just fork the repository and run `sbt package`.


## Jsonal

Jsonal is a JSON parser tailored for rapid reading of numeric data.  It's _really_ fast.

It's also supposed to be pretty easy to use.  Basic syntax looks like this:

```scala
import kse.jsonal._
val jp = Jast.parse("""[true, {"fish": ["herring", "cod"]}]""")
jp(1)("fish")(0).string   // Some(cod)

import JsonConverters._
val jq = Json ~ true ~
  ( Json ~ ("fish", Json(Array("herring", "cod"))) ~ Json )
  ~ Json
jp == jq  // true
```

Curious about the design goals and how they were met?  [Read more!](docs/Principles_of_Jsonal.md)

Or perhaps you want to [read the tutorial](docs/Jsonal_Tutorial.md).

## Maths

Maths contains numeric routines involving statistics and such that everyone
should have access to, plus handy postfix operators for some of the most
common mathematical operators.  Here's an example:

```scala
import kse.maths._
(3.sq + 9.7.sq).sqrt  // 10.15332457867865
cdfNormal(2)          // 0.9772498680518208
```

## Flow

Flow contains various control constructs including the desperately-lacking
`tap` and pipe (here, `fn`) operators, stackless flow control with `Hop`, and
a biased Either clone, `Ok`.

Here's an example:

```scala
import kse.flow._

okay[String]{ implicit hop =>
  List(3).map{ x => if (x>2) hop("bad"); x+5 }
}  // No("bad")

okay[String]{ implicit hop =>
  List(3).map{ x => if (x<2) hop("bad"); x+5 }
}  // Yes(List(8))
```

## Eio

Eio ("Easy I/O") contains various extension methods for Java files.  It was
written for systems stuck on Java 6, and as such probably doesn't have that
many advantages over Java 8's NIO2 routines.

```scala
import kse.eio._

"test.txt".file.slurp    // Yes[Vector[String]] containing file contents
```

## Test

The tests use JUnit, but are written with their own framework for not very
compelling reasons.  These then hook into JUnit.

## Goals

KSE is intended to add the batteries that the Scala standard library forgot--things
that practically every non-trivial project will want, or ought to want.  These include

* Zero-cost abstractions enabling higher-level operations on primitives and arrays (requires macros)
* Flow control that supports fluent and non-fluent styles equally well (tap, etc.)
* Flow control that supports chained return and exception-based styles equally well
* Core data types for validation and hiding/wrapping/showing/typing existing data
* Mutable analogs of core data types (e.g. tuples)
* High-level support for mutable operations on collections
* High-performance collections for basic data types
* Richer support of operations on primitive types (e.g. packing them into each other)
* Basic math that goes beyond trigonometry (erf, etc.)
* Enough statistics to address typical questions of robustness and estimation
* Possibly some basic machine learning and optimization
* Enough visualization to look at data of typical types
* Support for common and simple I/O including filesystem / archives / JSON
* Support for simple performant parsing of binary and textual data
* Facilities for performance testing

Not all of these things exist currently, but together with the current library this set of
facilities would make rare the times when one would have to reach for a collection of unrelated
and poorly interoperating libraries just to get basic computing tasks done.  Note that the
items are a mix of classical computer science topics (e.g. efficient collections), 
computational mathematics ("advanced" math and statistics), everyday housekeeping tasks
(JSON, zip file support, etc.), and ways to tell that what you're doing is what you mean
to be doing (visualization, benchmarking).

There are other libraries that provide extensive facilities for almost all of these things,
but very often basic capability in each area is all that you need to continue making progress
on your actual problem.  Letting you (or me) make that progress without having to
assemble a massive coalition of libraries and interface between them all is the purpose
of this project.
