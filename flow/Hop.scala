// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse.flow

import scala.util.control.ControlThrowable

/** Marker trait for control flow classes that work by throwing stackless exceptions. */
trait HopStackless {
  /** Test if this hop is the same as a throwable */
  def is(t: Throwable): Boolean
}

/** Allows a payload to go along with a hop. */
trait HopHasValue[@specialized(Int, Long) A] { 
  /** The value to carried when an exception is thrown. */
  def value: A
}

/** An exception class that contains a value. */
abstract class Hopped[@specialized(Int, Long) A] extends ControlThrowable with HopStackless with HopHasValue[A] {}

/** Throws a stackless exception when applied; this should be caught by whichever code created this instance. */
trait HopOnly extends HopStackless {
  /** Throws an exception to leave the local execution context. */
  def hop(): Nothing
}

/** `Hop` provides customizable non-local return capability via stackless exceptions.
  * This class typically contains a private mutable field that can carry a value out when it is thrown.
  * 
  * The use-cases for `Hop` are similar to those for non-local `return`, and performance is similar,
  * but there are two major advantages.  First, `return` only works from within a closure
  * passed to another piece of code, while a `Hop` can be called from anywhere that has
  * access to the `Hop` instance.  Second, a `Hop` can be handled as a separate path
  * from the main execution, and thus the resulting value can be treated as an error
  * condition even if it has the same type as the normal return value.
  * 
  * Example:
  * {{{
  * import kse.flow._
  * val m = collection.mutable.HashMap("Joe" -> 1)
  * def who(s: String)(implicit nobody: Hop[String]) = m.get(s) match {
  *   case Some(i) => i
  *   case None => nobody(s)
  * }
  * okayWith(""){ implicit nobody => who("Joe") }  // Returns Ok[String,Int] = Yes(1)
  * okayWith(""){ implicit nobody => who("Moe") }  // Returns Ok[String,Int] = No(Moe)
  * okayWith("?"){ implicit nobody => nobody.hop() }    // Returns Ok[String,Int] = No(?)
  * }}}
  * 
  * This pattern is particularly useful when performing many operations which
  * you want to assume will succeed, but where at least one failure is a fairly common
  * outcome.  Unlike standard (unchecked) exceptions, the standard methods only generate
  * a `Hop` in a context where the exception will be caught, so the exception cannot
  * accidentally escape the context where it should be handled.  Unlike conditional
  * return constructs such as `Option`, `Either`, and `Ok`, one need not explicitly
  * handle every failing branch.  The result, when used in the correct context, is
  * safe, performant, uncluttered code.
  */
trait Hop[@specialized(Int, Long) A] extends HopStackless {
  /** Throws a stackless exception, carrying the supplied value */
  def apply(a: A): Nothing
  /** Typically throws a stackless exception with the supplied value, but may be overridden to not throw or only sometimes throw */
  def on(a: A): Unit
  /** Detects whether a throwable is in fact this Hop, and if so returns a Hopped; if not, return null.
    * This method should return null if and only if `is` returns false.
    */
  def as(t: Throwable): Hopped[A]
}

/** A trait specifically to handle errors that have no information.
  * Example:
  * {{{
  * def getBoth[A,B](a: Option[A], b: Try[B]): Option[(A,B)] =
  *   probably{ implicit oops => (a.grab, b.grab) }
  * }}}
  * 
  * Best used when there are many operations each individually unlikely to fail, but
  * where the overall computation is not highly certain to complete, and when there
  * is no useful information to return aside from the fact that something went wrong.
  */
trait Oops extends HopOnly {}

/** Thrown by special Oops instance that will throw real exceptions instead of itself. */
class OopsException extends RuntimeException("Uncaught Oops.") {}

