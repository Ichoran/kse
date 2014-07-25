// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse

import scala.language.experimental.macros

import scala.util.control.{ NonFatal, ControlThrowable }

package flow {
  /** Marker trait for control flow classes that work by throwing stackless exceptions. */
  trait HopStackless  // marker trait for control flow classes
  
  /** An exception class that contains a value. */
  abstract class Hopped[@specialized(Int, Long) A] extends ControlThrowable with HopStackless { def value: A }

  /** Throws a stackless exception when applied; this should be caught by whichever code created this instance. */
  trait Hopper extends HopStackless {
    /** Throws an exception to leave the local execution context. */
    def apply(): Nothing
  }

  /** On `apply`, throws a stackless exception that carries a value; this should be caught by whichever code created this instance. */
  trait HopWith[@specialized(Int, Long) A] {
    /** Throws a value-carrying exception to leave the local execution context. */
    def apply(a: A): Nothing
  }
  
  /** Can throw stackless exceptions with values like [[HopsWith]] but provides a facility for changing the result type. */
  sealed trait HopChain[@specialized(Int, Long) A] extends HopWith[A] {
    /** Creates a new `HopChain` that takes a `B` instead of an `A`. */
    def from[B](f: B => A): HopChain[B]
  }

  /** Holds a value that will be carried by a stackless exception; the value is viewable in advance. */
  trait HopView[@specialized(Int, Long) A] extends Hopper {
    /** The value to be carried when an exception is thrown. */
    def value: A
  }
  
  /** `Hop` provides customizable non-local return capability via stackless exceptions.
    * This class contains a mutable field that can carry a value out when it is thrown.
    * 
    * The use-cases for `Hop` are similar to those for `return`, and performance is similar,
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
    * okayWith("???"){ implicit nobody => who("Joe") }  // Returns Ok[String,Int] = Yes(1)
    * okayWith("???"){ implicit nobody => who("Moe") }  // Returns Ok[String,Int] = No(Moe)
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
  trait Hop[@specialized(Int, Long) A] extends HopView[A] with HopWith[A] with Hopper {
    /** Sets the value to be carried when an exception is thrown. */
    def value(a: A): this.type
    /** Alters the value to be carried according to the function `f`. */
    def valueFn(f: A => A): this.type
  }
  
  /** A trait specifically to handle errors that probably have no information, 
    * but you can provide your own to accept a `Long` if you wish.
    * The standard control flow functions that give an `Oops` will give one that
    * ignores any information stored in it, and will return only `-1` when caught.
    * 
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
  trait Oops extends Hop[Long] {}
  
  
  /** Provides default behavior for validating an [[Ok]], namely to throw an exception if a disfavored value is found. */
  trait LowPriorityOkValidation {
    private[flow] val genericInvalidOkThrowsException = new Ok.ValidateOkay[Any] { def incorrect(any: Any) { throw new NotOkException(any) } }
    implicit def defaultInvalidOkThrowsException[N]: Ok.ValidateOkay[N] = genericInvalidOkThrowsException.asInstanceOf[Ok.ValidateOkay[N]]
  }
}

/** A nice tutorial should go here.  For now, just browse everything. */
package object flow extends LowPriorityOkValidation {
  /** Allows `grab` as an alternative to `get` on `Option`: `grab` will throw an available `Oops` if the `Option` is empty. */
  implicit class OptionCanHop[A](val underlying: Option[A]) extends AnyVal {
    /** Retrieve the value from this option or throw an `Oops` otherwise. */
    def grab(implicit oops: Oops): A = if (underlying.isDefined) underlying.get else oops()
    /** Convert to [[Ok]] with `Unit` for the disfavored branch. */
    def toOk: Ok[Unit, A] = underlying match { case Some(a) => Yes(a); case _ => Ok.UnitNo }
  }

  /** Allows alternatives to `get` on `Try`. */
  implicit class TryCanHop[A](val underlying: scala.util.Try[A]) extends AnyVal {
    /** Throws an available `Oops` if the `Try` is a `Failure`, gives the `Success` value otherwise. */
    def grab(implicit oops: Oops): A = underlying match {
      case scala.util.Success(a) => a
      case _ => oops()
    }
    /** Throws a `Failure` value with an available `Hop`; gives the `Success` value otherwise. */
    def orHop(implicit hop: HopWith[Throwable]): A = underlying match {
      case scala.util.Success(a) => a
      case scala.util.Failure(t) => hop(t)
    }
    /** Convert to [[Ok]] with `Success` favored. */
    def toOk: Ok[Throwable, A] = underlying match {
      case scala.util.Success(a) => Yes(a)
      case scala.util.Failure(t) => No(t)
    }
  }
  
  /** Supplies a method on `Either` to convert it to an [[Ok]]. */
  implicit class EitherCanBeOk[L,R](val underlying: scala.util.Either[L,R]) extends AnyVal {
    /** Convert to [[Ok]] with `Right` favored. */
    def toOk: Ok[L, R] = underlying match {
      case scala.util.Right(r) => Yes(r)
      case scala.util.Left(l) => No(l)
    }
  }
  
  /** Allows alternatives to `yes` on [[Ok]] */
  implicit class OkCanHop[N,Y](val underlying: Ok[N,Y]) extends AnyVal {
    /** Throws an available `Oops` if the [[Ok]] is a `No`, gives the `Yes` value otherwise. */
    def grab(implicit oops: Oops): Y = if (underlying.isOk) underlying.yes else oops()
    /** Throws a `No` value with an available `Hop`; gives the `Yes` value otherwise. */
    def orHop(implicit hop: HopWith[N]): Y = if (underlying.isOk) underlying.yes else hop(underlying.no)
  }
  
  
  /** Provides standard control-flow methods that should exist on Object. */
  implicit class EverythingCanTapAndSuch[A](val underlying: A) extends AnyVal {
    /** Transforms self according to the function `f`. */
    def fn[Z](f: A => Z) = f(underlying)
    
    /** Executes a side effect that depends on self, and returns self */
    def tap(f: A => Any) = { f(underlying); underlying }

    /** Transforms self according to `pf` only for those values where `pf` is defined. */
    def partFn(pf: PartialFunction[A,A]) = if (pf.isDefinedAt(underlying)) pf(underlying) else underlying
    
    /** Transforms self according to `f` for those values where `p` is true. */
    def pickFn(p: A => Boolean)(f: A => A) = if (p(underlying)) f(underlying) else underlying
    
    /** Wraps the value in an `Option`, discarding values where `p` is false. */
    def optIf(p: A => Boolean) = if (p(underlying)) Some(underlying) else None
    
    /** Wraps the value in an [[Ok]], in a `Yes` if `p` is true, otherwise in a `No`. */
    def okIf(p: A => Boolean): Ok[A,A] = if (p(underlying)) Yes(underlying) else No(underlying)
    
    /** Tries to cast (without numeric conversion), placing the value in a `Yes` if it succeeds, or leaving it in a `No` if it fails. */
    def okAs[Z](implicit tg: scala.reflect.ClassTag[Z]): Ok[A,Z] = {
      try { underlying match {
        case _: Unit => if (tg.runtimeClass == classOf[Unit]) Ok.UnitYes.asInstanceOf[Yes[Z]] else No(underlying)
        case z: Boolean => if (tg.runtimeClass == classOf[Boolean]) Yes(z).asInstanceOf[Yes[Z]] else No(underlying)
        case b: Byte => if (tg.runtimeClass == classOf[Byte]) Yes(b).asInstanceOf[Yes[Z]] else No(underlying)
        case s: Short => if (tg.runtimeClass == classOf[Short]) Yes(s).asInstanceOf[Yes[Z]] else No(underlying)
        case c: Char => if (tg.runtimeClass == classOf[Char]) Yes(c).asInstanceOf[Yes[Z]] else No(underlying)
        case i: Int => if (tg.runtimeClass == classOf[Int]) Yes(i).asInstanceOf[Yes[Z]] else No(underlying)
        case l: Long => if (tg.runtimeClass == classOf[Long]) Yes(l).asInstanceOf[Yes[Z]] else No(underlying)
        case f: Float => if (tg.runtimeClass == classOf[Float]) Yes(f).asInstanceOf[Yes[Z]] else No(underlying)
        case d: Double => if (tg.runtimeClass == classOf[Double]) Yes(d).asInstanceOf[Yes[Z]] else No(underlying)
        case _ =>
          val a = underlying.asInstanceOf[AnyRef]  // Need to handle null this way or we get a MatchError
          if ((a ne null) && tg.runtimeClass.isAssignableFrom(a.getClass)) Yes(a.asInstanceOf[Z]) else No(underlying)
      }}
      catch { case cce: ClassCastException => No(underlying) }
    }
    
    /** If `p` is true, replace the value with `default`. */
    def defaultIf(p: A => Boolean)(default: => A) = if (p(underlying)) default else underlying
    
    /** If `p` is true, continue, otherwise throw an Oops */
    def must(p: A => Boolean)(implicit oops: Oops) = if (p(underlying)) underlying else oops()

    /** If `p` is true, send the value as an implicit `Hop`; otherwise return self */
    def hopIf(p: A => Boolean)(implicit hop: HopWith[A]) = if (p(underlying)) hop(underlying) else underlying

    /** Perform a side-effect `g`, typically to clean up a resource, after
      * transforming self according to `f`. 
      * Example: {{{ scala.io.Source.fromFile(file).tidy(_.close)( _.getLines.toVector ) }}}
      */
    def tidy[Z](g: A => Any)(f: A => Z) = try { f(underlying) } finally { g(underlying) }
  }
  
  
  /** C-style for loop (translated to while loop via macro).
    * Example: {{{ cFor(1)(_ <= 10)(_ + 1)(i => println("Line "+i)) }}}
    */
  def cFor[A](zero: A)(p: A => Boolean)(next: A => A)(f: A => Unit): Unit = macro ControlFlowMacroImpl.cFor[A]
  
  /** Indexed array iterator--provides both the element and the index at each step.
    * Example:
    * {{{ 
    * val arr = Array(5,4,3,2,1,0)
    * var s = 0
    * aFor(arr){ (x,i) => s += x*i }
    * println(s)
    * }}}
    */
  def aFor[A](array: Array[A])(f: (A,Int) => Unit): Unit = macro ControlFlowMacroImpl.aFor[A]
  
  /** Indexed loop, counting up from 0. 
    * Example: {{{ nFor(3)(println) // Prints 0 1 2 on separate lines }}}
    */
  def nFor(count: Int)(f: Int => Unit): Unit = macro ControlFlowMacroImpl.nFor
  
  /** Loop over an iterator.
    * Example: {{{ iFor( io.Source.fromFile(f).getLines )( line => if (line.length>0) builder += line ) }}}
    */
  def iFor[A](iterator: Iterator[A])(f: A => Unit): Unit = macro ControlFlowMacroImpl.iFor[A]
   
  
  private val myOops: Hopped[Long] with Oops = new Hopped[Long] with Oops {
    def apply(): Nothing = throw this
    def apply(a: Long): Nothing = throw this
    def value = -1L
    def value(a: Long) = this
    def valueFn(f: Long => Long) = this
  }
  
  /** Use this when you want an [[Oops]] that will throw an
    * IllegalArgumentException instead of a stackless exception; it will not 
    * be caught by the methods that catch stackless exceptions.
    */
  val oopsThrowingRealException = new Oops {
    def apply(): Nothing = throw new IllegalArgumentException
    def apply(a: Long): Nothing = throw new IllegalArgumentException
    def value = -1L
    def value(a: Long) = this
    def valueFn(f: Long => Long) = this
  }


  /** Call when something has gone wrong, in a context where an implicit `Oops` is available for error handling.
    *  Example: {{{ if (!myFile.exists) OOPS }}}
    */
  def OOPS(implicit oops: Oops): Nothing = oops()
  
  /** Execute a side-effect when something has gone wrong, in a context where an implicit `Oops` will be thrown in case of error; allow the `Oops` to continue. */
  def tapOops[A, U](f: => A)(aside: => U)(implicit oops: Oops): A = try{ f } catch { case t if t eq oops => aside; throw t }
  
  /** Call when an implicit `Hop` is available and you wish to execute a non-local return of `value`. */
  def HOP[@specialized(Int, Long) A](value: A)(implicit hop: HopWith[A]): Nothing = hop(value)
  
  /** Execute a side-effect when a non-local `Hop` has been thrown, but allow it to continue. */
  def tapHop[A, B, U](f: => A)(sideEffect: B => U)(implicit hop: HopView[B]): A = try { f } catch { case t if t eq hop => sideEffect(hop.value); throw t }


  /** Catch any `Oops`es that happen within this block and convert to `Option`. */
  def oopsless[A](f: => A)(implicit oops: Oops): Option[A] = try { Some(f) } catch { case t if t eq oops => None }
  
  /** Catch any `Oops`es that happen within this block and fill in a default value. */
  def oopslessOr[A](default: => A)(f: => A)(implicit oops: Oops) = try { f } catch { case t if t eq oops => default }
  
  /** Catch any 'Oops`es that happen within this block and fill in `null`; the block must return an `AnyRef`. */
  def oopslessOrNull[A >: Null](f: => A)(implicit oops: Oops): A = try { f } catch { case t if t eq oops => null }
  
  /** Do something with a side-effect, ignoring any `Oops` that is thrown (aside from it short-circuiting execution). */
  def oopslessDo[U](f: => U)(implicit oops: Oops): Unit = try { f; () } catch { case t if t eq oops => }

  /** Get an `Oops` to use in a block of code.
    * It is your responsibility not to store the `Oops` or pass it to another thread.
    * The result will be packed in an `Option` (`None` if the `Oops` was thrown).
    * If nested, the `Oops` will be caught at the innermost level.
    */
  def probably[A](f: Oops => A): Option[A] = try { Some(f(myOops)) } catch { case t if t eq myOops => None }
  
  /** Get an `Oops` to use in a block of code.
    * It is your responsibility not to store the `Oops` or pass it to another thread.
    * Use a default value instead if the `Oops` is thrown.
    * If nested, the `Oops` will be caught at the innermost level.
    */
  def probablyOr[A](default: => A)(f: Oops => A): A = try { f(myOops) } catch { case t if t eq myOops => default }
  
  /** Get an `Oops` to use in a block of code.
    * It is your responsibility not to store the `Oops` or pass it to another thread.
    * Return `null` if the `Oops` was thrown; the code block must return an `AnyRef`.
    * If nested, the `Oops` will be caught at the innermost level.
    */
  def probablyOrNull[A >: Null](f: Oops => A): A = try { f(myOops) } catch { case t if t eq myOops => null }
  
  /** Get an `Oops` to use in a block of side-effecting code, but do nothing
    * (aside from short-circuiting execution) if it is thrown. 
    * It is your responsibility not to store the `Oops` or pass it to another thread.
    * If nested, the `Oops` will be caught at the innermost level.
    */
  def probablyDo[U](f: Oops => U): Unit = try { f(myOops); () } catch { case t if t eq myOops => }
  
  /** Run a series of operations, stopping on the first one not to throw an `Oops`.
    * It is your responsibility not to store the `Oops` or pass it to another thread.
    * If nested, the `Oops` will be caught at the innermost level.
    */
  def probablyOne[A](fs: Seq[Oops => A])(default: => A): A = {
    val it = fs.iterator
    while (it.hasNext) {
      try { 
        val f = it.next
        return f(myOops)
      }
      catch { case t if t eq myOops => }
    }
    default
  }
  
  
  private sealed class HoppingImpl[A] extends Hopped[A] with HopChain[A] {
    private[this] var myValue: A = _
    def value = myValue
    def apply(a: A): Nothing = { myValue = a; throw this }
    def from[B](f: B => A): HopChain[B] = new HoppingMapImpl[A,A,B](this, f)
  }
  private final class HoppingMapImpl[A, B, C](original: Hopped[A] with HopChain[B], map: C => B) extends Hopped[A] with HopChain[C] {
    def value = original.value
    def apply(c: C): Nothing = original(map(c))
    def from[D](f: D => C): HopChain[D] = new HoppingMapImpl[A,C,D](this, f)
  }
  
  private final class HopValImplRef[A <: AnyRef](val value: A) extends Hopped[A] with HopView[A] {
    def apply() = throw this
  }

  private final class HopValImplInt(val value: Int) extends Hopped[Int] with HopView[Int] {
    def apply() = throw this
  }
  
  private final class HopValImplLong(val value: Long) extends Hopped[Long] with HopView[Long] {
    def apply() = throw this
  }
  
  private final class HopLazyImpl[A](valueGen: => A) extends Hopped[A] with HopView[A] {
    lazy val value = valueGen
    def apply() = throw this
  }
  
  private sealed class HopImplRef[A <: AnyRef](var value: A) extends Hopped[A] with Hop[A] {
    def apply() = throw this
    def apply(a: A) = { value = a; throw this }
    def value(a: A) = { value = a; this }
    def valueFn(f: A => A) = { value = f(value); this }
  }

  private sealed class HopImplInt(var value: Int) extends Hopped[Int] with Hop[Int] {
    def apply() = throw this
    def apply(a: Int) = { value = a; throw this }
    def value(a: Int) = { value = a; this }
    def valueFn(f: Int => Int) = { value = f(value); this }
  }

  private sealed class HopImplLong(var value: Long) extends Hopped[Long] with Hop[Long] {
    def apply() = throw this
    def apply(a: Long) = { value = a; throw this }
    def value(a: Long) = { value = a; this }
    def valueFn(f: Long => Long) = { value = f(value); this }
  }


  /** Provides a `HopChain` shortcut to return a value from a block of code. */
  def hopTo[A](f: HopChain[A] => A): A = {
    val hop = new HoppingImpl[A]
    try { f(hop) } catch { case t if t eq hop => hop.value }
  }
  
  /** Provides a `Hop` shortcut, with an initial value, to return from a block of code. */
  def hopOr[A <: AnyRef](initial: A)(f: Hop[A] => A): A = {
    val hop = new HopImplRef[A](initial)
    try { f(hop) } catch { case t if t eq hop => hop.value }
  }
  /** Provides a `Hop` shortcut, with an initial value, to return from a block of code that returns an `Int`. */
  def hopOr(initial: Int)(f: Hop[Int] => Int): Int = {
    val hop = new HopImplInt(initial)
    try { f(hop) } catch { case t if t eq hop => hop.value }
  }
  /** Provides a `Hop` shortcut, with an initial value, to return from a block of code that returns a 'Long`. */
  def hopOr(initial: Long)(f: Hop[Long] => Long): Long = {
    val hop = new HopImplLong(initial)
    try { f(hop) } catch { case t if t eq hop => hop.value }
  }
  
  /** Assists with creating a block of code with a `Hop` using the `okay` method. */
  trait OkayDispatcher[N] { def apply[Y](f: HopWith[N] => Y): Ok[N,Y] }
  private val GenericOkayDispatch = new OkayDispatcher[Any] {
    def apply[Y](f: HopWith[Any] => Y): Ok[Any, Y] = {
      val hop = new HoppingImpl[Any]
      try { Yes(f(hop)) } catch { case t if t eq hop => No(hop.value) }
    }
  }
  
  /** Constructs an [[Ok]] via a `Hop`: `Hop`s will return a `No` value, while a successful result in a `Yes`.
    * Example:
    * {{{
    * okay[String]{ implicit hop =>
    *   if (!file.exists) hop(file.getName)
    *   file.length
    * }
    * }}}
    */
  def okay[N] = GenericOkayDispatch.asInstanceOf[OkayDispatcher[N]]
  
  /** Constructs an [[Ok]] via a `Hop` with an existing immutable return value. */
  def okayOr[N,Y](no: N)(yes: HopView[N] => Y): Ok[N,Y] = {
    val hop: HopView[N] = (no match {
      case n: Int => new HopValImplInt(n)
      case n: Long => new HopValImplLong(n)
      case _ => new HopValImplRef(no.asInstanceOf[AnyRef])
    }).asInstanceOf[HopView[N]]
    try { Yes(yes(hop)) } catch { case t if t eq hop => No(hop.value) }
  }
  
  /** Constructs an [[Ok]] via a `Hop` with a return value that is not constructed unless it is viewed or the `Hop` is thrown. */
  def okayLazy[N,Y](no: => N)(yes: HopView[N] => Y): Ok[N,Y] = {
    val hop = new HopLazyImpl(no)
    try { Yes(yes(hop)) } catch { case t if t eq hop => No(hop.value) }
  }
  
  /** Constructs an [[Ok]] via a mutable `Hop` with a default starting value. */
  def okayWith[N <: AnyRef,Y](no: N)(yes: Hop[N] => Y): Ok[N,Y] = {
    val hop = new HopImplRef(no)
    try { Yes(yes(hop)) } catch { case t if t eq hop => No(hop.value) }
  }
  
  /** Constructs an [[Ok]] via a mutable `Hop` with a default starting value (`Int` hop only). */
  def okayWith[Y](no: Int)(yes: Hop[Int] => Y): Ok[Int,Y] = {
    val hop = new HopImplInt(no)
    try { Yes(yes(hop)) } catch { case t if t eq hop => No(hop.value) }
  }
  
  /** Constructs an [[Ok]] via a mutable `Hop` with a default starting value (`Long` hop only). */
  def okayWith[Y](no: Long)(yes: Hop[Long] => Y): Ok[Long,Y] = {
    val hop = new HopImplLong(no)
    try { Yes(yes(hop)) } catch { case t if t eq hop => No(hop.value) }
  }
  
  /** Constructs an [[Ok]] by catching any uses of a pre-existing `Hop` for the `No` case. */
  def okayHere[N,Y](yes: Hop[N] => Y)(implicit hop: Hop[N]): Ok[N,Y] = try { Yes(yes(hop)) } catch { case t if t eq hop => No(hop.value) }
  
  /** Assists with creating custom validators for [[Ok]]s. */
  trait ValidateDispatcher[N] { def apply[Y](f: HopView[Seq[N]] with Ok.ValidateOkay[N] => Y): Ok[Seq[N],Y] }
  private final class HopValValidatingImpl[N](val value: collection.mutable.ArrayBuffer[N]) extends Hopped[Seq[N]] with HopView[Seq[N]] with Ok.ValidateOkay[N] {
    def apply() = throw this
    def incorrect(n: N) { value += n }
  }
  /** Constructs a context where validating `Ok`s leads to collecting disfavored values that can then be read out.
    * This method is incompletely documented and may be removed.
    */
  def validating[N] = new ValidateDispatcher[N] {
    def apply[Y](f: HopView[Seq[N]] with Ok.ValidateOkay[N] => Y): Ok[Seq[N],Y] = {
      val fvvi = new HopValValidatingImpl(new collection.mutable.ArrayBuffer[N])
      try { Yes(f(fvvi)) } catch { case t if t eq fvvi => No(fvvi.value) }
    }
  }

  /** Equivalent to `Try{...}` but stores exceptions in an [[Ok]] instead of a `Try`. */
  def safe[A](a: => A): Ok[Throwable, A] = try { Yes(a) } catch { case t if NonFatal(t) => No(t) }
  
  /** Equivalent to `Try{...}.toOption` */
  def safeOption[A](a: => A): Option[A] = try { Some(a) } catch { case t if NonFatal(t) => None }
  
  /** Equivalent to `Try{...}` but maps exceptions or hops into a disfavored value and stores results in an [[Ok]]. */
  def safeOk[N,Y](no: Throwable => N)(yes: HopWith[N] => Y): Ok[N,Y] = {
    val hop = new HoppingImpl[N]
    try{ Yes(yes(hop)) }
    catch {
      case t if t eq hop => No(hop.value)
      case t if NonFatal(t) => No(no(t))
    }
  }
}

