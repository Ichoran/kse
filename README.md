# Kerr Scala Extensions (KSE)

Does KSE work? [![Build Status](https://semaphoreci.com/api/v1/ichoran/kse/branches/master/badge.svg)](https://semaphoreci.com/ichoran/kse)

The Kerr Scala Extensions contain everything that the Scala standard library forgot that I have had time to create.


## Jsonal

Jsonal is a JSON parser tailored for rapid reading of numeric data.  It's _really_ fast.

It's also supposed to be pretty easy to use.  Basic syntax looks like this:

```scala
import kse.jsonal._
val jp = Jast.parse("""[true, {"fish": ["herring", "cod"]}]""")
jp(1)("fish")(0).string   // cod

import JsonConverters._
val jq = Json ~ true ~
  ( Json ~ ("fish", Json(Array("herring", "cod"))) ~ Json )
  ~ Json
jp == jq  // true
```

Curious about the design goals and how they were met?  [Read more!](Principles_of_Jsonal.md)

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
