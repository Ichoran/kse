// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-2014 Rex Kerr, HHMI/JFRC, and UCSF.

package kse

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
    private[this] var cache = new java.lang.ref.SoftReference(gen(t))
    def apply(): U = {
      var u = cache.get()
      if (u==null) {
        u = gen(t)
        cache = new java.lang.ref.SoftReference(u)
      }
      u
    }
  }
  object Soft {
    def apply[T,U](t: T)(gen: T => U) = new Soft(t)(gen)
  }
  
  
  /** Provides Option-like capabilities, but mutably. */
  abstract class Mopt[@specialized A] {
    def value: A
    def value_=(a: A): Unit
    def blank: this.type
    def copy: Mopt[A]
    
    protected def emptyHash: Int

    var ok: Boolean = false
    def off = { ok = false; this }
    def on = { ok = true; this }
    def value(a: A): this.type = { value = (a); this }

    def grab(implicit oops: Oops) = if (ok) value else oops
    def getOr(a: => A) = if (ok) value else a
    def getOrSet(a: => A) = { if (!ok) { value = a }; value }
    def apply() = if (ok) value else throw new java.util.NoSuchElementException("Mopt")

    def :=(a: A): this.type = { ok = true; value = a; this }
    def xform(f: A => A): this.type = { if (ok) { value = f(value) }; this }
    def tap(f: A => Unit): this.type = { if (ok) { f(value) }; this }
    def reject(p: A => Boolean): this.type = { if (ok && p(value)) ok = false; this }
    def exists(p: A => Boolean) = ok && p(value)

    def toOption = if (ok) Some(value) else None
    def toOk: Ok[Unit, A] = if (ok) Yes(value) else Ok.UnitNo
    
    override def equals(a: Any) = if (ok) (a == value) else a match {
      case m: Mopt[_] => !m.ok && m.emptyHash == emptyHash
      case _ => false
    }
    override def toString = if (ok) "<"+value.toString+">" else "<_>"
    override def hashCode = if (ok) value.## else emptyHash
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
      def blank = { ok = false; value = null.asInstanceOf[A]; this }
      def copy = { val m = new MoptAny[A]; m.ok = ok; m.value = value; m }
      def emptyHash = 0x9BB1B7F8
    }
    class MoptUnit extends Mopt[Unit] {
      var value: Unit = ()
      def blank = { ok = false; this }
      def copy = { val m = new MoptUnit; m.ok = ok; m }
      def emptyHash = 0x980D1F98
    }
    class MoptBoolean extends Mopt[Boolean] {
      var value: Boolean = false
      def blank = { ok = false; value = false; this }
      def copy = { val m = new MoptBoolean; m.ok = ok; m.value = value; m }
      def emptyHash = 0x62BC6BE9
    }
    class MoptByte extends Mopt[Byte] {
      var value: Byte = 0
      def blank = { ok = false; value = 0; this }
      def copy = { val m = new MoptByte; m.ok = ok; m.value = value; m }
      def emptyHash = 0x93D80D8B
    }
    class MoptShort extends Mopt[Short] {
      var value: Short = 0
      def blank = { ok = false; value = 0; this }
      def copy = { val m = new MoptShort; m.ok = ok; m.value = value; m }
      def emptyHash = 0xD1E297D0
    }
    class MoptChar extends Mopt[Char] {
      var value: Char = 0
      def blank = { ok = false; value = 0; this }
      def copy = { val m = new MoptChar; m.ok = ok; m.value = value; m }
      def emptyHash = 0xDC165BDF
    }
    class MoptInt extends Mopt[Int] {
      var value: Int = 0
      def blank = { ok = false; value = 0; this }
      def copy = { val m = new MoptInt; m.ok = ok; m.value = value; m }
      def emptyHash = 0xE456E5B9
    }
    class MoptLong extends Mopt[Long] {
      var value: Long = 0
      def blank = { ok = false; value = 0; this }
      def copy = { val m = new MoptLong; m.ok = ok; m.value = value; m }
      def emptyHash = 0x62080488
    }
    class MoptFloat extends Mopt[Float] {
      var value: Float = Float.NaN
      def blank = { ok = false; value = Float.NaN; this }
      def copy = { val m = new MoptFloat; m.ok = ok; m.value = value; m }
      def emptyHash = 0x28976EA9
    }
    class MoptDouble extends Mopt[Double] {
      var value: Double = Double.NaN
      def blank = { ok = false; value = Double.NaN; this }
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
    def apply[A <: AnyRef](a: A) = (new MoptAny[A]) := a
    def empty[@specialized A](implicit iv: ImplicitValue[Mopt[A], Mopt.type]) = iv.value
  }
}

package object coll {
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
  
  trait Tuple1LowPriorityImplicits[A] extends Any {
    def underlying: A
    def also[Z](f: A => Z) = underlying -> f(underlying)
  }
  
  implicit class Tuple1UtilityMethods[A](val underlying: A) extends AnyVal with Tuple1LowPriorityImplicits[A] {}
  
  implicit class Tuple2UtilityMethods[A,B](val underlying: (A,B)) extends AnyVal {
    def _1(value: A) = (value, underlying._2)
    def _2(value: B) = (underlying._1, value)
    def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2)
    def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2))
    def eachFn[Y,Z](f: A => Y)(g: B => Z) = (f(underlying._1), g(underlying._2))
    def fold[Z](f: (A,B) => Z) = f(underlying._1, underlying._2)
    def also[Z](f: (A,B) => Z) = (underlying._1, underlying._2, f(underlying._1, underlying._2))
  }
  implicit class Tuple2IdenticalUtilityMethods[A](val underlying: (A,A)) extends AnyVal {
    def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2))
  }
  
  implicit class Tuple3UtilityMethods[A,B,C](val underlying: (A,B,C)) extends AnyVal {
    def _1(value: A) = (value, underlying._2, underlying._3)
    def _2(value: B) = (underlying._1, value, underlying._3)
    def _3(value: C) = (underlying._1, underlying._2, value)
    def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2, underlying._3)
    def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2), underlying._3)
    def _3Fn[Z](f: C => Z) = (underlying._1, underlying._2, f(underlying._3))
    def eachFn[A1, B1, C1](f: A => A1)(g: B => B1)(h: C => C1) = (f(underlying._1), g(underlying._2), h(underlying._3))
    def fold[Z](f: (A,B,C) => Z) = f(underlying._1, underlying._2, underlying._3)
    def also[Z](f: (A,B,C) => Z) = (underlying._1, underlying._2, underlying._3, fold(f))
  }
  implicit class Tuple3IdenticalUtilityMethods[A](val underlying: (A,A,A)) extends AnyVal {
    def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2), f(underlying._3))
  }
    
  implicit class Tuple4UtilityMethods[A,B,C,D](val underlying: (A,B,C,D)) extends AnyVal {
    def _1(value: A) = (value, underlying._2, underlying._3, underlying._4)
    def _2(value: B) = (underlying._1, value, underlying._3, underlying._4)
    def _3(value: C) = (underlying._1, underlying._2, value, underlying._4)
    def _4(value: D) = (underlying._1, underlying._2, underlying._3, value)
    def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2, underlying._3, underlying._4)
    def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2), underlying._3, underlying._4)
    def _3Fn[Z](f: C => Z) = (underlying._1, underlying._2, f(underlying._3), underlying._4)
    def _4Fn[Z](f: D => Z) = (underlying._1, underlying._2, underlying._3, f(underlying._4))
    def eachFn[A1, B1, C1, D1](f: A => A1)(g: B => B1)(h: C => C1)(i: D => D1) = (f(underlying._1), g(underlying._2), h(underlying._3), i(underlying._4))
    def fold[Z](f: (A,B,C,D) => Z) = f(underlying._1, underlying._2, underlying._3, underlying._4)
    def also[Z](f: (A,B,C,D) => Z) = (underlying._1, underlying._2, underlying._3, underlying._4, fold(f))
  }
  implicit class Tuple4IdenticalUtilityMethods[A](val underlying: (A,A,A,A)) extends AnyVal {
    def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2), f(underlying._3), f(underlying._4))
  }
  
  implicit class Tuple5UtilityMethods[A,B,C,D,E](val underlying: (A,B,C,D,E)) extends AnyVal {
    def _1(value: A) = (value, underlying._2, underlying._3, underlying._4, underlying._5)
    def _2(value: B) = (underlying._1, value, underlying._3, underlying._4, underlying._5)
    def _3(value: C) = (underlying._1, underlying._2, value, underlying._4, underlying._5)
    def _4(value: D) = (underlying._1, underlying._2, underlying._3, value, underlying._5)
    def _5(value: E) = (underlying._1, underlying._2, underlying._3, underlying._4, value)
    def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2, underlying._3, underlying._4, underlying._5)
    def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2), underlying._3, underlying._4, underlying._5)
    def _3Fn[Z](f: C => Z) = (underlying._1, underlying._2, f(underlying._3), underlying._4, underlying._5)
    def _4Fn[Z](f: D => Z) = (underlying._1, underlying._2, underlying._3, f(underlying._4), underlying._5)
    def _5Fn[Z](f: E => Z) = (underlying._1, underlying._2, underlying._3, underlying._4, f(underlying._5))
    def eachFn[A1, B1, C1, D1, E1](f: A => A1)(g: B => B1)(h: C => C1)(i: D => D1)(j: E => E1) = (f(underlying._1), g(underlying._2), h(underlying._3), i(underlying._4), j(underlying._5))
    def fold[Z](f: (A,B,C,D,E) => Z) = f(underlying._1, underlying._2, underlying._3, underlying._4, underlying._5)
  }
  implicit class Tuple5IdenticalUtilityMethods[A](val underlying: (A,A,A,A,A)) extends AnyVal {
    def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2), f(underlying._3), f(underlying._4), f(underlying._5))
  }
}
