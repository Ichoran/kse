// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse

import scala.language.experimental.macros
import scala.language.postfixOps

import scala.util.control.NonFatal

/**
  * Control flow abstractions for Scala.
  * 
  * Style guide: naming schemes for methods and variables.
  *   toX  --  converts to a new object X
  *   asX  --  converts losslessly (possibly just by casting) to a new object X
  *   inX  --  creates a box of type X around object
  *   x    --  getter for x
  *   xTo  --  create a new object with a newly supplied value for x
  *   xFn  --  create a new object with an updated value for x
  *   x_   --  setter (chainable) for x
  *   x_=  --  setter for x that returns unit
  *   xOp  --  mutate x with a given operation
  * 
  *   myX  --  private storage for x
  *   xi   --  i'th value of x in a loop
  *   i0   --  start index (inclusive)
  *   i1   --  end index (inclusive!)
  *   iN   --  end index (exclusive)
  *   x0   --  initial or default value
  *   xs   --  more than one x  
  * 
  * tap and fn _never_ apply to the contents of an object, _always_ the object.  They will
  * be called peek (if returning self) or foreach (if not) instead of tap, and map (if returning
  * a new object) or mop (for mutable Op, if mutating in place) instead of fn.  Collections with
  * single named variables should prefer the collections names (map, mop, etc.) instead of the
  * decorated variable names (valueFn, valueOp, etc.).
  */
package flow {
  
  private[flow] final class HopImpl[A] extends Hopped[A] with HopKey[A, Unit] {
    protected var myValue: A = null.asInstanceOf[A]
    final def value = myValue
    final def apply(a: A) = { myValue = a; throw this }
    final def is(t: Throwable): Boolean = this eq t
    final def as(t: Throwable): Hopped[A] = if (this eq t) this else null
    final def reKey[X] = this.asInstanceOf[HopKey[A, X]]
  }

  private[flow] final class HopImplInt extends Hopped[Int] with HopKey[Int, Unit] {
    protected var myValue = 0
    final def value = myValue
    final def apply(a: Int) = { myValue = a; throw this }
    final def is(t: Throwable): Boolean = this eq t
    final def as(t: Throwable): Hopped[Int] = if (this eq t) this else null
    final def reKey[X] = this.asInstanceOf[HopKey[Int, X]]
  }

  private[flow] final class HopImplLong extends Hopped[Long] with HopKey[Long, Unit] {
    protected var myValue = 0L
    final def value = myValue
    def apply(a: Long) = { myValue = a; throw this }
    final def is(t: Throwable): Boolean = this eq t
    final def as(t: Throwable): Hopped[Long] = if (this eq t) this else null
    final def reKey[X] = this.asInstanceOf[HopKey[Long, X]]
  }
  
  object UnboundHopSupplier {
    def of[@specialized(Int, Long) A](implicit impl: HopSpecAdapter1[A]): Hop[A] = impl.create()
    def keyed[@specialized(Int, Long) A, X](implicit impl: HopSpecAdapter1[A]): HopKey[A, X] = impl.keyed[X]
  }
  
  trait HopSpecAdapter1[@specialized(Int, Long) A] { 
    private[flow] def hopOut(f: Hop[A] => A): A
    private[flow] def hopKeyOut[X](f: HopKey[A, X] => A): A
    private[flow] def create(): Hop[A]
    private[flow] def keyed[X]: HopKey[A, X]
  }
  
  private final class HopSpec1Any[A] extends HopSpecAdapter1[A] {
    def hopOut(f: Hop[A] => A): A = {
      val hop: Hop[A] = new HopImpl[A]
      try { f(hop) } catch { case t if hop is t => hop as t value }
    }
    def hopKeyOut[X](f: HopKey[A, X] => A): A = {
      val hop: HopKey[A, X] = (new HopImpl[A]).reKey[X]
      try { f(hop) } catch { case t if hop is t => hop as t value }
    }
    def create(): Hop[A] = new HopImpl[A]
    def keyed[X]: HopKey[A, X] = (new HopImpl[A]).reKey[X]
  }
  
  private final class HopSpec1Int extends HopSpecAdapter1[Int] {
    def hopOut(f: Hop[Int] => Int): Int = {
      val hop: Hop[Int] = new HopImplInt
      try { f(hop) } catch { case t if hop is t => hop as t value }
    }
    def hopKeyOut[X](f: HopKey[Int, X] => Int): Int = {
      val hop: HopKey[Int, X] = (new HopImplInt).reKey[X]
      try{ f(hop) } catch { case t if hop is t => hop as t value }
    }
    def create(): Hop[Int] = new HopImplInt
    def keyed[X]: HopKey[Int, X] = (new HopImplInt).reKey[X]
  }
  
  private final class HopSpec1Long extends HopSpecAdapter1[Long] {
    def hopOut(f: Hop[Long] => Long): Long = {
      val hop: Hop[Long] = new HopImplLong
      try { f(hop) } catch { case t if hop is t => hop as t value }
    }
    def hopKeyOut[X](f: HopKey[Long, X] => Long): Long = {
      val hop: HopKey[Long, X] = (new HopImplLong).reKey[X]
      try{ f(hop) } catch { case t if hop is t => hop as t value }
    }
    def create(): Hop[Long] = new HopImplLong
    def keyed[X]: HopKey[Long, X] = (new HopImplLong).reKey[X]
  }
  
  trait HopSpecAdapter2[@specialized(Int, Long) A, @specialized(Int, Long) B] {
    private[flow] def hopOn[C](onwards: B => A)(f: Hop[B] => C)(hop: Hop[A]): C
    private[flow] def hopKeyOn[C, X](onwards: B => A)(f: HopKey[B, X] => C)(hop: HopKey[A, X]): C
  }
  
  private final class HopSpec2AnyAny[A, B] extends HopSpecAdapter2[A, B] {
    def hopOn[C](onwards: B => A)(f: Hop[B] => C)(hop: Hop[A]): C = {
      val hip = hop match { case _: HopImpl[_] => hop.asInstanceOf[Hop[B]]; case _ => (new HopImpl[B]): Hop[B] }
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
    def hopKeyOn[C, X](onwards: B => A)(f: HopKey[B, X] => C)(hop: HopKey[A, X]): C = {
      val hip: HopKey[B, X] = hop match { case _: HopImpl[_] => hop.asInstanceOf[HopKey[B, X]]; case _ => (new HopImpl[B]).reKey[X] }
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
  }
  
  private final class HopSpec2IntAny[B] extends HopSpecAdapter2[Int, B] {
    def hopOn[C](onwards: B => Int)(f: Hop[B] => C)(hop: Hop[Int]): C = {
      val hip = (new HopImpl[B]): Hop[B]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
    def hopKeyOn[C, X](onwards: B => Int)(f: HopKey[B, X] => C)(hop: HopKey[Int, X]): C = {
      val hip = (new HopImpl[B]).reKey[X]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
  }
  
  private final class HopSpec2LongAny[B] extends HopSpecAdapter2[Long, B] {
    def hopOn[C](onwards: B => Long)(f: Hop[B] => C)(hop: Hop[Long]): C = {
      val hip = (new HopImpl[B]): Hop[B]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
    def hopKeyOn[C, X](onwards: B => Long)(f: HopKey[B, X] => C)(hop: HopKey[Long, X]): C = {
      val hip = (new HopImpl[B]).reKey[X]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
  }
  
  private final class HopSpec2AnyInt[A] extends HopSpecAdapter2[A, Int] {
    def hopOn[C](onwards: Int => A)(f: Hop[Int] => C)(hop: Hop[A]): C = {
      val hip = (new HopImplInt): Hop[Int]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
    def hopKeyOn[C, X](onwards: Int => A)(f: HopKey[Int, X] => C)(hop: HopKey[A, X]): C = {
      val hip = (new HopImplInt).reKey[X]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
  }
  
  private final class HopSpec2AnyLong[A] extends HopSpecAdapter2[A, Long] {
    def hopOn[C](onwards: Long => A)(f: Hop[Long] => C)(hop: Hop[A]): C = {
      val hip = (new HopImplLong): Hop[Long]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
    def hopKeyOn[C, X](onwards: Long => A)(f: HopKey[Long, X] => C)(hop: HopKey[A, X]): C = {
      val hip = (new HopImplLong).reKey[X]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
  }
  
  private final class HopSpec2IntInt extends HopSpecAdapter2[Int, Int] {
    def hopOn[C](onwards: Int => Int)(f: Hop[Int] => C)(hop: Hop[Int]): C = {
      try { f(hop) } catch { case t if hop is t => hop(onwards(hop as t value)) }
    }
    def hopKeyOn[C, X](onwards: Int => Int)(f: HopKey[Int, X] => C)(hop: HopKey[Int, X]): C = {
      try { f(hop) } catch { case t if hop is t => hop(onwards(hop as t value)) }
    }
  }
  
  private final class HopSpec2IntLong extends HopSpecAdapter2[Int, Long] {
    def hopOn[C](onwards: Long => Int)(f: Hop[Long] => C)(hop: Hop[Int]): C = {
      val hip = (new HopImplLong): Hop[Long]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
    def hopKeyOn[C, X](onwards: Long => Int)(f: HopKey[Long, X] => C)(hop: HopKey[Int, X]): C = {
      val hip = (new HopImplLong).reKey[X]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
  }
  
  private final class HopSpec2LongInt extends HopSpecAdapter2[Long, Int] {
    def hopOn[C](onwards: Int => Long)(f: Hop[Int] => C)(hop: Hop[Long]): C = {
      val hip = (new HopImplInt): Hop[Int]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
    def hopKeyOn[C, X](onwards: Int => Long)(f: HopKey[Int, X] => C)(hop: HopKey[Long, X]): C = {
      val hip = (new HopImplInt).reKey[X]
      try { f(hip) } catch { case t if hip is t => hop(onwards(hip as t value)) }
    }
  }
  
  private final class HopSpec2LongLong extends HopSpecAdapter2[Long, Long] {
    def hopOn[C](onwards: Long => Long)(f: Hop[Long] => C)(hop: Hop[Long]): C = {
      try { f(hop) } catch { case t if hop is t => hop(onwards(hop as t value)) }
    }
    def hopKeyOn[C, X](onwards: Long => Long)(f: HopKey[Long, X] => C)(hop: HopKey[Long, X]): C = {
      try { f(hop) } catch { case t if hop is t => hop(onwards(hop as t value)) }
    }
  }
  
  trait Priority3HopSpecs {
    private val myImpl_HopSpec2AnyAny = new HopSpec2AnyAny[Any, Any]
    implicit def impl_HopSpec2AnyAny[A, B] = myImpl_HopSpec2AnyAny.asInstanceOf[HopSpecAdapter2[A, B]]
  }
  trait Priority2HopSpecs extends Priority3HopSpecs {
    private val myImpl_HopSpec1Any = new HopSpec1Any[Any]
    implicit def impl_HopSpec1Any[A]: HopSpecAdapter1[A] = myImpl_HopSpec1Any.asInstanceOf[HopSpecAdapter1[A]]
    
    private val myImpl_HopSpec2AnyInt = new HopSpec2AnyInt[Any]
    private val myImpl_HopSpec2AnyLong = new HopSpec2AnyLong[Any]
    private val myImpl_HopSpec2IntAny = new HopSpec2IntAny[Any]
    private val myImpl_HopSpec2LongAny = new HopSpec2LongAny[Any]
    implicit def impl_HopSpec2AnyInt[A] = myImpl_HopSpec2AnyInt.asInstanceOf[HopSpecAdapter2[A, Int]]
    implicit def impl_HopSpec2AnyLong[A] = myImpl_HopSpec2AnyLong.asInstanceOf[HopSpecAdapter2[A, Long]]
    implicit def impl_HopSpec2IntAny[A] = myImpl_HopSpec2IntAny.asInstanceOf[HopSpecAdapter2[Int, A]]
    implicit def impl_HopSpec2LongAny[A] = myImpl_HopSpec2LongAny.asInstanceOf[HopSpecAdapter2[Long, A]]
  }
  trait Priority1HopSpecs extends Priority2HopSpecs {
    implicit val impl_HopSpec1Int: HopSpecAdapter1[Int] = new HopSpec1Int
    implicit val impl_HopSpec1Long: HopSpecAdapter1[Long] = new HopSpec1Long
    
    implicit val impl_HopSpec2IntInt: HopSpecAdapter2[Int, Int] = new HopSpec2IntInt
    implicit val impl_HopSpec2IntLong: HopSpecAdapter2[Int, Long] = new HopSpec2IntLong
    implicit val impl_HopSpec2LongInt: HopSpecAdapter2[Long, Int] = new HopSpec2LongInt
    implicit val impl_HopSpec2LongLong: HopSpecAdapter2[Long, Long] = new HopSpec2LongLong
  }
}

/** A nice tutorial should go here.  For now, just browse everything. */
package object flow extends Priority1HopSpecs {
  /** Allows `grab` as an alternative to `get` on `Option`: `grab` will throw an available `Oops` if the `Option` is empty. */
  implicit class OptionCanHop[A](private val underlying: Option[A]) extends AnyVal {
    /** Retrieve the value from this option or throw an `Oops` otherwise. */
    @inline def grab(implicit oops: Oops): A = if (underlying.isDefined) underlying.get else oops.hop()
    /** Retrieve the value from this option or hop with specified value. */
    @inline def orHop[B](default: => B)(implicit hop: Hop[B]): A = underlying match { case Some(a) => a; case None => hop(default); null.asInstanceOf[A] }
    /** Convert to [[Ok]] with `Unit` for the disfavored branch. */
    @inline def toOk: Ok[Unit, A] = underlying match { case Some(a) => Yes(a); case _ => Ok.UnitNo }
  }

  /** Allows alternatives to `get` on `Try`. */
  implicit class TryCanHop[A](private val underlying: scala.util.Try[A]) extends AnyVal {
    /** Throws an available `Oops` if the `Try` is a `Failure`, gives the `Success` value otherwise. */
    @inline def grab(implicit oops: Oops): A = underlying match {
      case scala.util.Success(a) => a
      case _ => oops.hop()
    }
    /** Throws a `Failure` value with an available `Hop`; gives the `Success` value otherwise. */
    @inline def orHop(implicit hop: Hop[Throwable]): A = underlying match {
      case scala.util.Success(a) => a
      case scala.util.Failure(t) => hop(t); null.asInstanceOf[A]
    }
    /** Convert to [[Ok]] with `Success` favored. */
    @inline def toOk: Ok[Throwable, A] = underlying match {
      case scala.util.Success(a) => Yes(a)
      case scala.util.Failure(t) => No(t)
    }
  }
  
  /** Supplies a method on `Either` to convert it to an [[Ok]]. */
  implicit class EitherCanBeOk[L,R](private val underlying: scala.util.Either[L,R]) extends AnyVal {
    /** Convert to [[Ok]] with `Right` favored. */
    @inline def toOk: Ok[L, R] = underlying match {
      case scala.util.Right(r) => Yes(r)
      case scala.util.Left(l) => No(l)
    }
  }
  
  /** Allows alternatives to `yes` on [[Ok]] */
  implicit class OkCanHop[N,Y](private val underlying: Ok[N,Y]) extends AnyVal {
    /** Throws an available `Oops` if the [[Ok]] is a `No`, gives the `Yes` value otherwise. */
    @inline def grab(implicit oops: Oops): Y = if (underlying.isOk) underlying.yes else oops.hop()
    /** Throws a `No` value with an available `Hop`; gives the `Yes` value otherwise. */
    @inline def orHop(implicit hop: Hop[N]): Y = if (underlying.isOk) underlying.yes else { hop(underlying.no); null.asInstanceOf[Y] }
  }
  
  
  /** Provides standard control-flow methods that should exist on Object. */
  implicit class EverythingCanTapAndSuch[A](private val underlying: A) extends AnyVal {
    /** Transforms self according to the function `f`. */
    @inline def fn[Z](f: A => Z) = f(underlying)
    
    /** Executes a side effect that depends on self, and returns self */
    @inline def tap(f: A => Any) = { f(underlying); underlying }

    /** Transforms self according to `pf` only for those values where `pf` is defined. */
    @inline def partFn(pf: PartialFunction[A,A]) = if (pf.isDefinedAt(underlying)) pf(underlying) else underlying
    
    /** Transforms self according to `f` for those values where `p` is true. */
    @inline def pickFn(p: A => Boolean)(f: A => A) = if (p(underlying)) f(underlying) else underlying
    
    /** Wraps the value in an `Option`, discarding values where `p` is false. */
    @inline def optIf(p: A => Boolean) = if (p(underlying)) Some(underlying) else None
    
    /** Performs a side-effecting operation if a conditional is true. */
    @inline def doIf(p: A => Boolean)(f: A => Unit) { if (p(underlying)) f(underlying) }
    
    /** Wraps the value in an [[Ok]], in a `Yes` if `p` is true, otherwise in a `No`. */
    @inline def okIf(p: A => Boolean): Ok[A,A] = if (p(underlying)) Yes(underlying) else No(underlying)
    
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
          if ((a ne null) && tg.runtimeClass.isInstance(a)) Yes(a.asInstanceOf[Z]) else No(underlying)
      }}
      catch { case cce: ClassCastException => No(underlying) }
    }
    
    /** If `p` is true, replace the value with `default`. */
    @inline def defaultIf(p: A => Boolean)(default: => A) = if (p(underlying)) default else underlying
    
    /** If `p` is true, continue, otherwise throw an Oops */
    @inline def must(p: A => Boolean)(implicit oops: Oops) = if (p(underlying)) underlying else oops.hop()

    /** If `p` is true, send the value as an implicit `Hop`; otherwise return self */
    @inline def hopIf(p: A => Boolean)(implicit hop: Hop[A]) = { if (p(underlying)) hop(underlying); underlying }

    /** Perform a side-effect `g`, typically to clean up a resource, after
      * transforming self according to `f`. 
      * Example: {{{ scala.io.Source.fromFile(file).tidy(_.close)( _.getLines.toVector ) }}}
      */
    @inline def tidy[Z](g: A => Any)(f: A => Z) = try { f(underlying) } finally { g(underlying) }
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
   
  
  private val myOops: Oops = new Hopped[Unit] with Oops {
    def hop(): Nothing = throw this
    def is(t: Throwable) = this eq t
    def value = ()
  }
  
  /** Use this when you want an [[Oops]] that will throw a (stack-bearing)
    * `OopsException` instead of a stackless exception; it will not 
    * be caught by the methods that catch stackless exceptions.
    */
  val oopsThrowingRealException = new Oops {
    def hop(): Nothing = throw new OopsException
    def is(t: Throwable) = false
  }


  /** Call when something has gone wrong, in a context where an implicit `Oops` is available for error handling.
    *  Example: {{{ if (!myFile.exists) OOPS }}}
    */
  def OOPS(implicit oops: Oops): Nothing = oops.hop()
  
  /** Execute a side-effect when something has gone wrong in `f`, in a context where an implicit `Oops` will be thrown in case of error; allow the `Oops` to continue. */
  def tapOops[A, U](f: => A)(sideEffect: => U)(implicit oops: Oops): A = try{ f } catch { case t if t eq oops => sideEffect; throw t }
  
  /** Execute a side-effect when a non-local `Hop` is thrown inside `f`, but allow it to continue. */
  def tapHop[A, B, U](f: => A)(sideEffect: B => U)(implicit hop: Hop[B]): A = try { f } catch { case t if hop is t => sideEffect(hop as t value); throw t }


  /** Catch any `Oops`es that happen within this block and convert to `Option`. */
  def oopsless[A](f: => A)(implicit oops: Oops): Option[A] = try { Some(f) } catch { case t if t eq oops => None }
  
  /** Catch any `Oops`es that happen within this block and fill in a default value. */
  def oopslessOr[A](default: => A)(f: => A)(implicit oops: Oops) = try { f } catch { case t if t eq oops => default }
  
  /** Catch any 'Oops`es that happen within this block and fill in `null`; the block must return an `AnyRef`. */
  def oopslessOrNull[A >: Null](f: => A)(implicit oops: Oops): A = try { f } catch { case t if t eq oops => null }
  
  /** Do something with a side-effect and let us know if it succeeded (`true`) or if it short-circuited execution (`false`). */
  def oopslessTry[U](f: => U)(implicit oops: Oops): Boolean = try { f; true } catch { case t if t eq oops => false }
  

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
  
  /** Get an `Oops` to use in a block of side-effecting code, and let us know
    * whether the block succeded (`true`) or short-circuted (`false`) by being thrown.
    * It is your responsibility not to store the `Oops` or pass it to another thread.
    * If nested, the `Oops` will be caught at the innermost level.
    */
  def probablyTry[U](f: Oops => U): Boolean = try { f(myOops); true } catch { case t if t eq myOops => false }
  
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
  
  /** Hops with the implicit `hop` in scope */
  def HOP[A](value: A)(implicit hop: Hop[A]): Nothing = hop(value)
  
  /** Traps the implicit `hop` inside an expression, converting it to an `Ok` */
  def hopless[A, B](f: => A)(implicit hop: Hop[B]) = hop.hopless(f)
  
  trait HoplessDispatcher[X] { def apply[A, B](f: => A)(implicit hop: HopKey[B, X]) }
  private val GenericHoplessDispatch = new HoplessDispatcher[Any] {
    def apply[A, B](f: => A)(implicit hop: HopKey[B, Any]) = hop.hopless(f)
  }
  
  /** Traps the implicit `hop` with the specified type key, converting it to an `Ok` */
  def hoplessKey[X] = GenericHoplessDispatch.asInstanceOf[HoplessDispatcher[X]]
  
  /** Provides a `Hop` of a different return type given an existing `Hop` and a conversion from the new return type to the old one. */
  @inline def hopOn[@specialized(Int, Long) A, @specialized(Int, Long) B, C](onwards: B => A)(f: Hop[B] => C)(implicit hop: Hop[A], impl: HopSpecAdapter2[A, B]): C =
    impl.hopOn(onwards)(f)(hop)
  
  trait HopKeyOnDispatcher[X] {
    def apply[@specialized(Int, Long) A, @specialized(Int, Long) B, C](onwards: B => A)(f: HopKey[B, X] => C)(implicit hop: HopKey[A, X], impl: HopSpecAdapter2[A, B]): C
  }
  private val GenericHopKeyOnDispatch = new HopKeyOnDispatcher[Any] {
    def apply[@specialized(Int, Long) A, @specialized(Int, Long) B, C](onwards: B => A)(f: HopKey[B, Any] => C)(implicit hop: HopKey[A, Any], impl: HopSpecAdapter2[A, B]): C =
      impl.hopKeyOn(onwards)(f)(hop)
  }
  
  
  /** The same as `hopOn` except with a marker type parameter. */
  @inline def hopKeyOn[X] = GenericHopKeyOnDispatch.asInstanceOf[HopKeyOnDispatcher[X]]
  
  /** Provides a `Hop` shortcut to return from a block of code. */
  @inline def hopOut[@specialized(Int, Long) A](f: Hop[A] => A)(implicit impl: HopSpecAdapter1[A]): A = 
    impl.hopOut(f)
    
  @inline def hopKeyOut[@specialized(Int, Long) A, X](f: HopKey[A, X] => A)(implicit impl: HopSpecAdapter1[A]): A =
    impl.hopKeyOut(f)
  
  /** Assists with creating a block of code with a `Hop` using the `okay` method. */
  sealed trait OkayDispatcher[N] { def apply[Y](f: Hop[N] => Y): Ok[N,Y] }
  private val GenericOkayDispatch = new OkayDispatcher[Any] {
    def apply[Y](f: Hop[Any] => Y): Ok[Any, Y] = {
      val hop = (new HopImpl[Any]): Hop[Any]
      try { Yes(f(hop)) } catch { case t if hop is t => No(hop as t value) }
    }
  }
  
  /** Constructs an [[Ok]] via a `Hop`: `Hop`s will return a `No` value, while a successful result in a `Yes`.
    * Example:
    * {{{
    * okay[String]{ hop =>
    *   if (!file.exists) hop(file.getName)
    *   file.length
    * }
    * }}}
    * Note: the return values are not specialized.
    * Note: the hop instance is type-keyed to the particular invocation so they won't cross-contaminate.
    */
  def okay[N] = GenericOkayDispatch.asInstanceOf[OkayDispatcher[N]]
  
  sealed trait OkayKeyDispatcher[N, X] { def apply[Y](f: HopKey[N, X] => Y): Ok[N, Y] }
  private val GenericOkayKeyDispatch = new OkayKeyDispatcher[Any, Any] {
    def apply[Y](f: HopKey[Any, Any] => Y): Ok[Any, Y] = {
      val hop = (new HopImpl[Any]).reKey[Any]
      try { Yes(f(hop)) } catch { case t if hop is t => No(hop as t value) }
    }
  }
  
  /** Like `okay` but uses a keyed hop. */
  def okayKey[N, X] = GenericOkayKeyDispatch.asInstanceOf[OkayKeyDispatcher[N, X]]
  
  /** Equivalent to `Try{...}` but stores exceptions in an [[Ok]] instead of a `Try`. */
  def safe[A](a: => A): Ok[Throwable, A] = try { Yes(a) } catch { case t if NonFatal(t) => No(t) }
  
  /** Equivalent to `Try{...}.toOption` */
  def safeOption[A](a: => A): Option[A] = try { Some(a) } catch { case t if NonFatal(t) => None }
  
  /** Equivalent to `Try{...}` but maps exceptions or hops into a disfavored value and stores results in an [[Ok]]. */
  def safeHop[N, Y](no: Throwable => N)(yes: Hop[N] => Y): Ok[N, Y] = {
    val hop = (new HopImpl[N]): Hop[N]
    try{ Yes(yes(hop)) }
    catch {
      case t if hop is t => No(hop as t value)
      case t if NonFatal(t) => No(no(t))
    }
  }
  
  /** Assists with creating a block of code with a `HopKey` using the `safeKeyHop` method. */
  sealed trait SafeDispatcher[X] { def apply[N, Y](no: Throwable => N)(yes: HopKey[N, X] => Y): Ok[N,Y] }
  private val GenericSafeDispatcher = new SafeDispatcher[Any] {
    def apply[N, Y](no: Throwable => N)(yes: HopKey[N, Any] => Y): Ok[N,Y] = {
      val hop = (new HopImpl[N]).reKey[Any]
      try{ Yes(yes(hop)) }
      catch {
        case t if hop is t => No(hop as t value)
        case t if NonFatal(t) => No(no(t))
      }
    }
  }
  
  /** The same as `safeHop` but uses a type key for the hop instance. */
  def safeHopKey[X] = GenericSafeDispatcher.asInstanceOf[SafeDispatcher[X]]
}
