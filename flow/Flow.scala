// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse

import scala.language.experimental.macros

import scala.util.control.{ NonFatal, ControlThrowable }

package flow {
  /** Marker trait for control flow classes that work by throwing stackless exceptions. */
  trait FlowStackless  // marker trait for control flow classes
  
  /** An exception class that contains a value. */
  abstract class Flew[@specialized(Int, Long) A] extends ControlThrowable with FlowStackless { def value: A }

  /** Throws a stackless exception when applied; this should be caught by whichever code created this instance. */
  trait Flowable extends FlowStackless {
    /** Throws an exception to leave the local execution context. */
    def apply(): Nothing
  }

  /** On `apply`, throws a stackless exception that carries a value; this should be caught by whichever code created this instance. */
  trait FlowWith[@specialized(Int, Long) A] {
    /** Throws a value-carrying exception to leave the local execution context. */
    def apply(a: A): Nothing
  }
  
  /** Can throw stackless exceptions with values like [[FlowWith]] but provides a facility for changing the result type. */
  sealed trait FlowChain[@specialized(Int, Long) A] extends FlowWith[A] {
    /** Creates a new `FlowChain` that takes a `B` instead of an `A`. */
    def from[B](f: B => A): FlowChain[B]
  }

  /** Holds a value that will be carried by a stackless exception; the value is viewable in advance. */
  trait FlowView[@specialized(Int, Long) A] extends Flowable {
    /** The value to be carried when an exception is thrown. */
    def value: A
  }
  
  /** `Flow` provides customizable non-local return capability via stackless exceptions.
    * This class contains a mutable field that can carry a value out when it is thrown.
    * 
    * The use-cases for Flow are similar to those for `return`, and performance is similar,
    * but there are two major advantages.  First, `return` only works from within a closure
    * passed to another piece of code, while a `Flow` can be called from anywhere that has
    * access to the `Flow` instance.  Second, a `Flow` can be handled as a separate path
    * from the main execution, and thus the resulting value can be treated as an error
    * condition even if it has the same type as the normal return value.
    * 
    * Example:
    * {{{
    * import kse.flow._
    * val m = collection.mutable.HashMap("Joe" -> 1)
    * def who(s: String)(implicit nobody: Flow[String]) = m.get(s) match {
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
    * a `Flow` in a context where the exception will be caught, so the exception cannot
    * accidentally escape the context where it should be handled.  Unlike conditional
    * return constructs such as `Option`, `Either`, and `Ok`, one need not explicitly
    * handle every failing branch.  The result, when used in the correct context, is
    * safe, performant, uncluttered code.
    */
  trait Flow[@specialized(Int, Long) A] extends FlowView[A] with FlowWith[A] with Flowable {
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
  trait Oops extends Flow[Long] {}
  
  /** This class is undocumented and may be removed. */
  class Flop(s: String) extends RuntimeException(s) {
    private var extra: List[String] = List(s)
    def more(s: String): Nothing = { extra = s :: extra; throw this }
    def more(f: Flop): Nothing = {
      extra = f.extra.map("< " + _) ::: extra.map("> " + _) ::: List("Neither worked: ")
      throw this
    }
    def more(fs: Seq[Flop]): Nothing = {
      val n = fs.length.toString.length
      extra = (this +: fs).zipWithIndex.toList.reverse.flatMap{ case (f,i) => f.extra.map(("%0"+n+"d").format(i) + " " + _) } ::: List("None of "+n+" worked:")
      throw this
    }
    override def getMessage = extra.reverse.mkString("\n...")
  }

  /** This class is undocumented and may be removed. */
  class Doable[A](val underlying: () => A) extends AnyVal {
    def until(p: A => Boolean) = { var a = underlying(); while (!p(a)) a = underlying(); a }
  }
  object Do {
    def apply[A](f: => A) = new Doable(() => f)
  }
  
  /** Provides default behavior for validating an [[Ok]], namely to throw an exception if a disfavored value is found. */
  trait LowPriorityOkValidation {
    private[flow] val genericInvalidOkThrowsException = new Ok.ValidateOkay[Any] { def incorrect(any: Any) { throw new NotOkException(any) } }
    implicit def defaultInvalidOkThrowsException[N]: Ok.ValidateOkay[N] = genericInvalidOkThrowsException.asInstanceOf[Ok.ValidateOkay[N]]
  }
}

package object flow extends LowPriorityOkValidation {
  /** Allows `grab` as an alternative to `get` on `Option`: `grab` will throw an available `Oops` if the `Option` is empty. */
  implicit class OptionIsFlowable[A](val underlying: Option[A]) extends AnyVal {
    def grab(implicit oops: Oops): A = if (underlying.isDefined) underlying.get else oops()
  }

  /** Allows alternatives to `get` on `Try`. */
  implicit class TryIsFlowable[A](val underlying: scala.util.Try[A]) extends AnyVal {
    /** Throws an available `Oops` if the `Try` is a `Failure`, gives the `Success` value otherwise. */
    def grab(implicit oops: Oops): A = underlying match {
      case scala.util.Success(a) => a
      case _ => oops()
    }
    /** Throws a `Failure` value with an available `Flow`; gives the `Success` value otherwise. */
    def orFlow(implicit flow: FlowWith[Throwable]): A = underlying match {
      case scala.util.Success(a) => a
      case scala.util.Failure(t) => flow(t)
    }
  }
  
  /** Allows alternatives to `yes` on [[Ok]] */
  implicit class OkIsFlowable[N,Y](val underlying: Ok[N,Y]) extends AnyVal {
    /** Throws an available `Oops` if the [[Ok]] is a `No`, gives the `Yes` value otherwise. */
    def grab(implicit oops: Oops): Y = if (underlying.isOk) underlying.yes else oops()
    /** Throws a `No` value with an available `Flow`; gives the `Yes` value otherwise. */
    def orFlow(implicit flow: FlowWith[N]): Y = if (underlying.isOk) underlying.yes else flow(underlying.no)
  }
  
  
  /** Provides standard methods that should exist on Object. */
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

    /** If `p` is true, send the value as an implicit `Flow`; otherwise return self */
    def flowIf(p: A => Boolean)(implicit flow: FlowWith[A]) = if (p(underlying)) flow(underlying) else underlying

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
  
  private val myOops: Flew[Long] with Oops = new Flew[Long] with Oops {
    def apply(): Nothing = throw this
    def apply(a: Long): Nothing = throw this
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
  
  /** Call when an implicit `Flow` is available and you wish to execute a non-local return of `value`. */
  def FLOW[@specialized(Int, Long) A](value: A)(implicit flow: FlowWith[A]): Nothing = flow(value)
  
  /** Execute a side-effect when a non-local `Flow` has been thrown, but allow it to continue. */
  def tapFlow[A, B, U](f: => A)(aside: B => U)(implicit flow: FlowView[B]): A = try { f } catch { case t if t eq flow => aside(flow.value); throw t }


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
  
  
  private sealed class FlowingImpl[A] extends Flew[A] with FlowChain[A] {
    private[this] var myValue: A = _
    def value = myValue
    def apply(a: A): Nothing = { myValue = a; throw this }
    def from[B](f: B => A): FlowChain[B] = new FlowingMapImpl[A,A,B](this, f)
  }
  private final class FlowingMapImpl[A, B, C](original: Flew[A] with FlowChain[B], map: C => B) extends Flew[A] with FlowChain[C] {
    def value = original.value
    def apply(c: C): Nothing = original(map(c))
    def from[D](f: D => C): FlowChain[D] = new FlowingMapImpl[A,C,D](this, f)
  }
  
  private final class FlowValImplRef[A <: AnyRef](val value: A) extends Flew[A] with FlowView[A] {
    def apply() = throw this
  }

  private final class FlowValImplInt(val value: Int) extends Flew[Int] with FlowView[Int] {
    def apply() = throw this
  }
  
  private final class FlowValImplLong(val value: Long) extends Flew[Long] with FlowView[Long] {
    def apply() = throw this
  }
  
  private final class FlowLazyImpl[A](valueGen: => A) extends Flew[A] with FlowView[A] {
    lazy val value = valueGen
    def apply() = throw this
  }
  
  private sealed class FlowImplRef[A <: AnyRef](var value: A) extends Flew[A] with Flow[A] {
    def apply() = throw this
    def apply(a: A) = { value = a; throw this }
    def value(a: A) = { value = a; this }
    def valueFn(f: A => A) = { value = f(value); this }
  }

  private sealed class FlowImplInt(var value: Int) extends Flew[Int] with Flow[Int] {
    def apply() = throw this
    def apply(a: Int) = { value = a; throw this }
    def value(a: Int) = { value = a; this }
    def valueFn(f: Int => Int) = { value = f(value); this }
  }

  private sealed class FlowImplLong(var value: Long) extends Flew[Long] with Flow[Long] {
    def apply() = throw this
    def apply(a: Long) = { value = a; throw this }
    def value(a: Long) = { value = a; this }
    def valueFn(f: Long => Long) = { value = f(value); this }
  }


  /** Provides a `FlowChain` shortcut to return a value from a block of code. */
  def flowTo[A](f: FlowChain[A] => A): A = {
    val flow = new FlowingImpl[A]
    try { f(flow) } catch { case t if t eq flow => flow.value }
  }
  
  /** Provides a `Flow` shortcut, with an initial value, to return from a block of code. */
  def flowOr[A <: AnyRef](initial: A)(f: Flow[A] => A): A = {
    val flow = new FlowImplRef[A](initial)
    try { f(flow) } catch { case t if t eq flow => flow.value }
  }
  /** Provides a `Flow` shortcut, with an initial value, to return from a block of code that returns an `Int`. */
  def flowOr(initial: Int)(f: Flow[Int] => Int): Int = {
    val flow = new FlowImplInt(initial)
    try { f(flow) } catch { case t if t eq flow => flow.value }
  }
  /** Provides a `Flow` shortcut, with an initial value, to return from a block of code that returns a 'Long`. */
  def flowOr(initial: Long)(f: Flow[Long] => Long): Long = {
    val flow = new FlowImplLong(initial)
    try { f(flow) } catch { case t if t eq flow => flow.value }
  }
  
  /** Assists with creating a block of code with a `Flow` using the `okay` method. */
  trait OkayDispatcher[N] { def apply[Y](f: FlowWith[N] => Y): Ok[N,Y] }
  private val GenericOkayDispatch = new OkayDispatcher[Any] {
    def apply[Y](f: FlowWith[Any] => Y): Ok[Any, Y] = {
      val flow = new FlowingImpl[Any]
      try { Yes(f(flow)) } catch { case t if t eq flow => No(flow.value) }
    }
  }
  
  /** Constructs an [[Ok]] via a `Flow`: `Flow`s will return a `No` value, while a successful result in a `Yes`.
    * Example:
    * {{{
    * okay[String]{ implicit flow =>
    *   if (!file.exists) flow(file.getName)
    *   file.length
    * }
    * }}}
    */
  def okay[N] = GenericOkayDispatch.asInstanceOf[OkayDispatcher[N]]
  
  /** Constructs an [[Ok]] via a `Flow` with an existing immutable return value. */
  def okayOr[N,Y](no: N)(yes: FlowView[N] => Y): Ok[N,Y] = {
    val flow: FlowView[N] = (no match {
      case n: Int => new FlowValImplInt(n)
      case n: Long => new FlowValImplLong(n)
      case _ => new FlowValImplRef(no.asInstanceOf[AnyRef])
    }).asInstanceOf[FlowView[N]]
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  
  /** Constructs an [[Ok]] via a `Flow` with a return value that is not constructed unless it is viewed or the `Flow` is thrown. */
  def okayLazy[N,Y](no: => N)(yes: FlowView[N] => Y): Ok[N,Y] = {
    val flow = new FlowLazyImpl(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  
  /** Constructs an [[Ok]] via a mutable `Flow` with a default starting value. */
  def okayWith[N <: AnyRef,Y](no: N)(yes: Flow[N] => Y): Ok[N,Y] = {
    val flow = new FlowImplRef(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  
  /** Constructs an [[Ok]] via a mutable `Flow` with a default starting value (`Int` flow only). */
  def okayWith[Y](no: Int)(yes: Flow[Int] => Y): Ok[Int,Y] = {
    val flow = new FlowImplInt(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  
  /** Constructs an [[Ok]] via a mutable `Flow` with a default starting value (`Long` flow only). */
  def okayWith[Y](no: Long)(yes: Flow[Long] => Y): Ok[Long,Y] = {
    val flow = new FlowImplLong(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  
  /** Constructs an [[Ok]] by catching any uses of a pre-existing `Flow` for the `No` case. */
  def okayHere[N,Y](yes: Flow[N] => Y)(implicit flow: Flow[N]): Ok[N,Y] = try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  
  /** Assists with creating custom validators for [[Ok]]s. */
  trait ValidateDispatcher[N] { def apply[Y](f: FlowView[Seq[N]] with Ok.ValidateOkay[N] => Y): Ok[Seq[N],Y] }
  private final class FlowValValidatingImpl[N](val value: collection.mutable.ArrayBuffer[N]) extends Flew[Seq[N]] with FlowView[Seq[N]] with Ok.ValidateOkay[N] {
    def apply() = throw this
    def incorrect(n: N) { value += n }
  }
  /** Constructs a context where validating `Ok`s leads to collecting disfavored values that can then be read out.
    * This method is incompletely documented and may be removed.
    */
  def validating[N] = new ValidateDispatcher[N] {
    def apply[Y](f: FlowView[Seq[N]] with Ok.ValidateOkay[N] => Y): Ok[Seq[N],Y] = {
      val fvvi = new FlowValValidatingImpl(new collection.mutable.ArrayBuffer[N])
      try { Yes(f(fvvi)) } catch { case t if t eq fvvi => No(fvvi.value) }
    }
  }

  /** Equivalent to `Try{...}` but stores exceptions in an [[Ok]] instead of a `Try`. */
  def safe[A](a: => A): Ok[Throwable, A] = try { Yes(a) } catch { case t if NonFatal(t) => No(t) }
  
  /** Equivalent to `Try{...}.toOption` */
  def safeOption[A](a: => A): Option[A] = try { Some(a) } catch { case t if NonFatal(t) => None }
  
  /** Equivalent to `Try{...}` but maps exceptions or flows into a disfavored value and stores results in an [[Ok]]. */
  def safeOk[N,Y](no: Throwable => N)(yes: FlowWith[N] => Y): Ok[N,Y] = {
    val flow = new FlowingImpl[N]
    try{ Yes(yes(flow)) }
    catch {
      case t if t eq flow => No(flow.value)
      case t if NonFatal(t) => No(no(t))
    }
  }
  
  /** This method is undocumented and may be removed. */
  def FLOP(s: String) = throw new Flop(s)
}

