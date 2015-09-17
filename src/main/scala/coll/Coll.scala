// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-2015 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse

import scala.language.implicitConversions

import kse.typecheck._
import kse.flow._

package coll {
  /** A general way to defer a computation but cache the result. */
  class Lazy[A](gen: => A) {
    lazy val value = gen
    def map[B](f: A => B) = Lazy(f(value))
    def flatMap[B](f: A => Lazy[B]) = Lazy(f(value).value)
    def foreach[B](f: A => B) { f(value) }
  }
  object Lazy {
    def apply[A](gen: => A) = new Lazy(gen)
  }
  
  /** Caches expensive computations that are cleared when memory gets especially tight (via SoftReference); not thread-safe */
  class Soft[T,U](t: T)(gen: T => U) {
    private[this] var myCache = new java.lang.ref.SoftReference(gen(t))
    def apply(): U = {
      var u = myCache.get()
      if (u==null) {
        u = gen(t)
        myCache = new java.lang.ref.SoftReference(u)
      }
      u
    }
  }
  object Soft {
    def apply[T,U](t: T)(gen: T => U) = new Soft(t)(gen)
  }
  
  /** Hides data from case classes */
  class Mute[A](val value: A) {
    override def toString = "..."
    override def hashCode = 1239182
    override def equals(a: Any) = a match {
      case _: Mute[_] => true
      case _ => false
    }
  }
  object Mute {
    def apply[A](a: A) = new Mute(a)
  }
  
  /** Holds mutable data (would be better if standard library exposed this!) */
  sealed abstract class Mu[@specialized A] {
    def value: A
    def value_=(a: A): Unit
    def value_(a: A): this.type = { value = a; this }
    def op(f: A => A): this.type = { value = f(value); this }
    override def toString = value.toString
    override def hashCode = value.##
  }
  object Mu {
    object MuUnit extends Mu[Unit] { def value: Unit = (); def value_=(u: Unit) {} }
    final class MuBoolean(init: Boolean) extends Mu[Boolean] { var value = init }
    final class MuByte(init: Byte) extends Mu[Byte] { var value = init }
    final class MuShort(init: Short) extends Mu[Short] { var value = init }
    final class MuChar(init: Char) extends Mu[Char] { var value = init }
    final class MuInt(init: Int) extends Mu[Int] { var value = init }
    final class MuLong(init: Long) extends Mu[Long] { var value = init }
    final class MuFloat(init: Float) extends Mu[Float] { var value = init }
    final class MuDouble(init: Double) extends Mu[Double] { var value = init }
    final class MuAny[A](init: A) extends Mu[A] { var value = init }
    def apply(u: Unit): Mu[Unit] = MuUnit
    def apply(z: Boolean): Mu[Boolean] = new MuBoolean(z)
    def apply(b: Byte): Mu[Byte] = new MuByte(b)
    def apply(s: Short): Mu[Short] = new MuShort(s)
    def apply(c: Char): Mu[Char] = new MuChar(c)
    def apply(i: Int): Mu[Int] = new MuInt(i)
    def apply(l: Long): Mu[Long] = new MuLong(l)
    def apply(f: Float): Mu[Float] = new MuFloat(f)
    def apply(d: Double): Mu[Double] = new MuDouble(d)
    def apply[A](a: A): Mu[A] = new MuAny(a)
  }
  
  
  /** Provides Option-like capabilities, but mutably. */
  sealed abstract class Mopt[@specialized A] {
    def value: A
    def value_=(a: A): Unit
    def clear: this.type
    def copy: Mopt[A]
    
    protected def emptyHash: Int

    final var ok: Boolean = false
    final def off = { ok = false; this }
    final def on = { ok = true; this }
    final def value_(a: A): this.type = { value = (a); this }
    final def :=(a: A): this.type = { ok = true; value = a; this }

    final def grab(implicit oops: Oops) = if (ok) value else OOPS
    final def getOr(a: => A) = if (ok) value else a
    final def getOrSet(a: => A) = { if (!ok) { value = a; ok = true }; value }
    final def get = if (ok) value else throw new NoSuchElementException("Mopt")

    final def mop(f: A => A): this.type = { if (ok) { value = f(value) }; this }
    final def peek(f: A => Unit): this.type = { if (ok) { f(value) }; this }
    final def reject(p: A => Boolean): this.type = { if (ok && p(value)) ok = false; this }
    final def exists(p: A => Boolean) = ok && p(value)

    final def toOption = if (ok) Some(value) else None
    final def toOk: Ok[Unit, A] = if (ok) Yes(value) else Ok.UnitNo
    
    final override def equals(a: Any) = if (ok) (a == value) else a match {
      case m: Mopt[_] => !m.ok && m.emptyHash == emptyHash
      case _ => false
    }
    final override def toString = if (ok) "<"+value.toString+">" else "<_>"
    final override def hashCode = if (ok) value.## else emptyHash
  }
  object Mopt {
    def unanimous(mopts: Mopt[_]*): Boolean = {
      var successes = 0
      var failures = 0
      iFor(mopts.iterator){ m =>
        if (failures > 0 || !m.ok) {
          failures += 1
          m.ok = false
        }
        else successes += 1
      }
      if (failures == 0) true
      else if (successes == 0) false
      else {
        iFor(mopts.iterator){ m =>
          if (m.ok) {
            m.ok = false
            successes -= 1
            if (successes == 0) return false
          }
        }
        false
      }
    }
    class MoptAny[A] extends Mopt[A] {
      var value: A = null.asInstanceOf[A]
      def clear = { ok = false; value = null.asInstanceOf[A]; this }
      def copy = { val m = new MoptAny[A]; m.ok = ok; m.value = value; m }
      def emptyHash = 0x9BB1B7F8
    }
    class MoptUnit extends Mopt[Unit] {
      var value: Unit = ()
      def clear = { ok = false; this }
      def copy = { val m = new MoptUnit; m.ok = ok; m }
      def emptyHash = 0x980D1F98
    }
    class MoptBoolean extends Mopt[Boolean] {
      var value: Boolean = false
      def clear = { ok = false; value = false; this }
      def copy = { val m = new MoptBoolean; m.ok = ok; m.value = value; m }
      def emptyHash = 0x62BC6BE9
    }
    class MoptByte extends Mopt[Byte] {
      var value: Byte = 0
      def clear = { ok = false; value = 0; this }
      def copy = { val m = new MoptByte; m.ok = ok; m.value = value; m }
      def emptyHash = 0x93D80D8B
    }
    class MoptShort extends Mopt[Short] {
      var value: Short = 0
      def clear = { ok = false; value = 0; this }
      def copy = { val m = new MoptShort; m.ok = ok; m.value = value; m }
      def emptyHash = 0xD1E297D0
    }
    class MoptChar extends Mopt[Char] {
      var value: Char = 0
      def clear = { ok = false; value = 0; this }
      def copy = { val m = new MoptChar; m.ok = ok; m.value = value; m }
      def emptyHash = 0xDC165BDF
    }
    class MoptInt extends Mopt[Int] {
      var value: Int = 0
      def clear = { ok = false; value = 0; this }
      def copy = { val m = new MoptInt; m.ok = ok; m.value = value; m }
      def emptyHash = 0xE456E5B9
    }
    class MoptLong extends Mopt[Long] {
      var value: Long = 0
      def clear = { ok = false; value = 0; this }
      def copy = { val m = new MoptLong; m.ok = ok; m.value = value; m }
      def emptyHash = 0x62080488
    }
    class MoptFloat extends Mopt[Float] {
      var value: Float = Float.NaN
      def clear = { ok = false; value = Float.NaN; this }
      def copy = { val m = new MoptFloat; m.ok = ok; m.value = value; m }
      def emptyHash = 0x28976EA9
    }
    class MoptDouble extends Mopt[Double] {
      var value: Double = Double.NaN
      def clear = { ok = false; value = Double.NaN; this }
      def copy = { val m = new MoptDouble; m.ok = ok; m.value = value; m }
      def emptyHash = 0x4A0FFD76
    }
    def apply(u: Unit) = (new MoptUnit).on
    def apply(z: Boolean) = (new MoptBoolean) := z
    def apply(b: Byte) = (new MoptByte) := b
    def apply(s: Short) = (new MoptShort) := s
    def apply(c: Char) = (new MoptChar) := c
    def apply(i: Int) = (new MoptInt) := i
    def apply(l: Long) = (new MoptLong) := l
    def apply(f: Float) = (new MoptFloat) := f
    def apply(d: Double) = (new MoptDouble) := d
    def apply[A <: AnyRef](a: A) = (new MoptAny[A]) := a
    def apply(o: Option[Unit]) = { val m = new MoptUnit; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Boolean]) = { val m = new MoptBoolean; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Byte]) = { val m = new MoptByte; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Short]) = { val m = new MoptShort; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Char]) = { val m = new MoptChar; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Int]) = { val m = new MoptInt; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Long]) = { val m = new MoptLong; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Float]) = { val m = new MoptFloat; if (o.isDefined) { m := o.get }; m }
    def apply(o: Option[Double]) = { val m = new MoptDouble; if (o.isDefined) { m := o.get }; m }
    def apply[A <: AnyRef](oa: Option[A]) = { val m = new MoptAny[A]; if (oa.isDefined) { m := oa.get }; m }
    def empty[@specialized A](implicit iv: ImplicitValue[Mopt[A], Mopt.type]) = iv.value
  }

  trait Stepper[@specialized A] { self =>
    def step(f: A => Unit): Boolean
    def peek(g: A => Unit) = new Stepper[A] {
      def step(f: A => Unit): Boolean = self.step{ a => g(a); f(a) }
    }
    def foreach[U](f: A => U) {
      while(step(a => { f(a); () })) {}
    }
    def filter(p: A => Boolean) = new Stepper[A] {
      def step(f: A => Unit): Boolean = {
        var good = false
        while (self.step(a => if ({ good = p(a); good }) f(a)) && !good) {}
        good
      }
    }
    def withFilter(p: A => Boolean) = filter(p)
    def ++(s: Stepper[A]): Stepper[A] = new Stepper[A] {
      private[this] var current = self
      def step(f: A => Unit): Boolean = {
        val ans = current.step(f)
        if (!ans && (current ne s)) {
          current = s
          current.step(f)
        }
        else ans
      }
    }
  }

}

package object coll {
  implicit def unmuteAnythingMuted[A](mute: Mute[A]) = mute.value
  
  implicit val unitMoptInstanceProvider = new ImplicitValue[Mopt[Unit], Mopt.type] { def value = new Mopt.MoptUnit }
  implicit val booleanMoptInstanceProvider = new ImplicitValue[Mopt[Boolean], Mopt.type] { def value = new Mopt.MoptBoolean }
  implicit val byteMoptInstanceProvider = new ImplicitValue[Mopt[Byte], Mopt.type] { def value = new Mopt.MoptByte }
  implicit val shortMoptInstanceProvider = new ImplicitValue[Mopt[Short], Mopt.type] { def value = new Mopt.MoptShort }
  implicit val charMoptInstanceProvider = new ImplicitValue[Mopt[Char], Mopt.type] { def value = new Mopt.MoptChar }
  implicit val intMoptInstanceProvider = new ImplicitValue[Mopt[Int], Mopt.type] { def value = new Mopt.MoptInt }
  implicit val longMoptInstanceProvider = new ImplicitValue[Mopt[Long], Mopt.type] { def value = new Mopt.MoptLong }
  implicit val floatMoptInstanceProvider = new ImplicitValue[Mopt[Float], Mopt.type] { def value = new Mopt.MoptFloat }
  implicit val doubleMoptInstanceProvider = new ImplicitValue[Mopt[Double], Mopt.type] { def value = new Mopt.MoptDouble }
  private val genericMoptInstanceProvider = new ImplicitValue[Mopt[AnyRef], Mopt.type] { def value = new Mopt.MoptAny }
  implicit def anyrefMoptInstanceProvider[A <: AnyRef] = genericMoptInstanceProvider.asInstanceOf[ImplicitValue[Mopt[A], Mopt.type]]
  
  implicit class OptionConvertsToMoptUnit(private val underlying: Option[Unit]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptByte(private val underlying: Option[Byte]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptShort(private val underlying: Option[Short]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptChar(private val underlying: Option[Char]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptInt(private val underlying: Option[Int]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptLong(private val underlying: Option[Long]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptFloat(private val underlying: Option[Float]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptDouble(private val underlying: Option[Double]) extends AnyVal { def toMopt = Mopt(underlying) }
  implicit class OptionConvertsToMoptAny[A <: AnyRef](private val underlying: Option[A]) extends AnyVal { def toMopt = Mopt(underlying) }
  
  implicit class Tuple1UtilityMethods[A](private val underlying: A) extends AnyVal {
    @inline def also[Z](f: A => Z) = underlying -> f(underlying)
    @inline def tup[Z](z: Z) = (underlying, z)
  }
  
  implicit class Tuple2UtilityMethods[A,B](val underlying: (A,B)) extends AnyVal {
    @inline def _1To(value: A) = (value, underlying._2)
    @inline def _2To(value: B) = (underlying._1, value)
    @inline def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2)
    @inline def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2))
    @inline def eachFn[Y,Z](f: A => Y, g: B => Z) = (f(underlying._1), g(underlying._2))
    @inline def fold[Z](f: (A,B) => Z) = f(underlying._1, underlying._2)
    @inline def also[Z](f: (A,B) => Z) = (underlying._1, underlying._2, f(underlying._1, underlying._2))
    @inline def _without1 = underlying._2
    @inline def _without2 = underlying._1
    @inline def tup[Z](z: Z) = (underlying._1, underlying._2, z)
  }
  implicit class Tuple2IdenticalUtilityMethods[A](val underlying: (A,A)) extends AnyVal {
    @inline def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2))
    @inline def reduce(f: (A,A) => A) = f(underlying._1, underlying._2)
  }
  
  implicit class Tuple3UtilityMethods[A,B,C](val underlying: (A,B,C)) extends AnyVal {
    @inline def _1To(value: A) = (value, underlying._2, underlying._3)
    @inline def _2To(value: B) = (underlying._1, value, underlying._3)
    @inline def _3To(value: C) = (underlying._1, underlying._2, value)
    @inline def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2, underlying._3)
    @inline def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2), underlying._3)
    @inline def _3Fn[Z](f: C => Z) = (underlying._1, underlying._2, f(underlying._3))
    @inline def eachFn[A1, B1, C1](f: A => A1, g: B => B1, h: C => C1) = (f(underlying._1), g(underlying._2), h(underlying._3))
    @inline def fold[Z](f: (A,B,C) => Z) = f(underlying._1, underlying._2, underlying._3)
    @inline def also[Z](f: (A,B,C) => Z) = (underlying._1, underlying._2, underlying._3, fold(f))
    @inline def _without1 = (underlying._2, underlying._3)
    @inline def _without2 = (underlying._1, underlying._3)
    @inline def _without3 = (underlying._1, underlying._2)
    @inline def tup[Z](z: Z) = (underlying._1, underlying._2, underlying._3, z)
  }
  implicit class Tuple3IdenticalUtilityMethods[A](val underlying: (A,A,A)) extends AnyVal {
    @inline def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2), f(underlying._3))
    @inline def reduce(f: (A,A) => A) = f( f(underlying._1, underlying._2), underlying._3 )
  }
    
  implicit class Tuple4UtilityMethods[A,B,C,D](val underlying: (A,B,C,D)) extends AnyVal {
    @inline def _1To(value: A) = (value, underlying._2, underlying._3, underlying._4)
    @inline def _2To(value: B) = (underlying._1, value, underlying._3, underlying._4)
    @inline def _3To(value: C) = (underlying._1, underlying._2, value, underlying._4)
    @inline def _4To(value: D) = (underlying._1, underlying._2, underlying._3, value)
    @inline def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2, underlying._3, underlying._4)
    @inline def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2), underlying._3, underlying._4)
    @inline def _3Fn[Z](f: C => Z) = (underlying._1, underlying._2, f(underlying._3), underlying._4)
    @inline def _4Fn[Z](f: D => Z) = (underlying._1, underlying._2, underlying._3, f(underlying._4))
    @inline def eachFn[A1, B1, C1, D1](f: A => A1, g: B => B1, h: C => C1, i: D => D1) = (f(underlying._1), g(underlying._2), h(underlying._3), i(underlying._4))
    @inline def fold[Z](f: (A,B,C,D) => Z) = f(underlying._1, underlying._2, underlying._3, underlying._4)
    @inline def also[Z](f: (A,B,C,D) => Z) = (underlying._1, underlying._2, underlying._3, underlying._4, fold(f))
    @inline def _without1 = (underlying._2, underlying._3, underlying._4)
    @inline def _without2 = (underlying._1, underlying._3, underlying._4)
    @inline def _without3 = (underlying._1, underlying._2, underlying._4)
    @inline def _without4 = (underlying._1, underlying._2, underlying._3)
    @inline def tup[Z](z: Z) = (underlying._1, underlying._2, underlying._3, underlying._4, z)
  }
  implicit class Tuple4IdenticalUtilityMethods[A](val underlying: (A,A,A,A)) extends AnyVal {
    @inline def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2), f(underlying._3), f(underlying._4))
    @inline def reduce(f: (A,A) => A) = f( f(underlying._1, underlying._2), f(underlying._3, underlying._4) )
  }
  
  implicit class Tuple5UtilityMethods[A,B,C,D,E](val underlying: (A,B,C,D,E)) extends AnyVal {
    @inline def _1To(value: A) = (value, underlying._2, underlying._3, underlying._4, underlying._5)
    @inline def _2To(value: B) = (underlying._1, value, underlying._3, underlying._4, underlying._5)
    @inline def _3To(value: C) = (underlying._1, underlying._2, value, underlying._4, underlying._5)
    @inline def _4To(value: D) = (underlying._1, underlying._2, underlying._3, value, underlying._5)
    @inline def _5To(value: E) = (underlying._1, underlying._2, underlying._3, underlying._4, value)
    @inline def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2, underlying._3, underlying._4, underlying._5)
    @inline def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2), underlying._3, underlying._4, underlying._5)
    @inline def _3Fn[Z](f: C => Z) = (underlying._1, underlying._2, f(underlying._3), underlying._4, underlying._5)
    @inline def _4Fn[Z](f: D => Z) = (underlying._1, underlying._2, underlying._3, f(underlying._4), underlying._5)
    @inline def _5Fn[Z](f: E => Z) = (underlying._1, underlying._2, underlying._3, underlying._4, f(underlying._5))
    @inline def eachFn[A1, B1, C1, D1, E1](f: A => A1, g: B => B1, h: C => C1, i: D => D1, j: E => E1) = (f(underlying._1), g(underlying._2), h(underlying._3), i(underlying._4), j(underlying._5))
    @inline def fold[Z](f: (A,B,C,D,E) => Z) = f(underlying._1, underlying._2, underlying._3, underlying._4, underlying._5)
    @inline def _without1 = (underlying._2, underlying._3, underlying._4, underlying._5)
    @inline def _without2 = (underlying._1, underlying._3, underlying._4, underlying._5)
    @inline def _without3 = (underlying._1, underlying._2, underlying._4, underlying._5)
    @inline def _without4 = (underlying._1, underlying._2, underlying._3, underlying._5)
    @inline def _without5 = (underlying._1, underlying._2, underlying._3, underlying._4)
    @inline def tup[Z](z: Z) = (underlying._1, underlying._2, underlying._3, underlying._4, underlying._5, z)
  }
  implicit class Tuple5IdenticalUtilityMethods[A](val underlying: (A,A,A,A,A)) extends AnyVal {
    @inline def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2), f(underlying._3), f(underlying._4), f(underlying._5))
    @inline def reduce(f: (A,A) => A) = f( f( f(underlying._1, underlying._2), f(underlying._3, underlying._4) ), underlying._5 )
  }
  
  implicit class KseRichIterator[A](private val underlying: Iterator[A]) extends AnyVal {
    def takeTo(p: A => Boolean): Iterator[A] = new collection.AbstractIterator[A] {
      private[this] var foundLast = false
      def hasNext = !foundLast && underlying.hasNext
      def next = {
        if (foundLast) throw new NoSuchElementException("next on empty iterator")
        val ans = underlying.next
        foundLast = p(ans)
      ans
      }
    }
    def stepper = new Stepper[A] {
      def step(f: A => Unit) = if (underlying.hasNext) { f(underlying.next); true } else false
    }
    def step(f: A => Unit) = if (underlying.hasNext) { f(underlying.next); true } else false
  }
}
