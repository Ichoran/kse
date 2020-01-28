// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-2015 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse

import scala.reflect.ClassTag
import scala.util.{Try, Success, Failure}

import scala.language.implicitConversions
import scala.language.higherKinds

import kse.typecheck._
import kse.flow._

package coll {
  /** A class that captures the idea of a constant or computed value that can stand in for the original */
  abstract class Itself[@specialized A] {
    def itself: A
    override def toString = itself.toString
    override def hashCode = itself.##
  }
  trait LowPriorityItselfConversions {
    implicit def itselfCanBeAnything[@specialized A](it: Itself[A]): A = it.itself
    implicit def anythingCanBeItself[@specialized A](a: A): Itself[A] = Itself.value(a)    
  }
  object Itself extends LowPriorityItselfConversions {
    def value[@specialized A](a: A): Itself[A] = new Itself[A] { def itself = a }
    def is[@specialized A](a: => A): Itself[A] = new Itself[A] { private lazy val mySelf = a; def itself = mySelf }
    def from[@specialized A](a: => A): Itself[A] = new Itself[A] { def itself = a }
  }

  /** A trait that expresses that a class can copy itself. */
  trait Copy[+A <: Copy[_]] {
    def copy: A
  }

  /** A general way to defer a computation but cache the result. */
  class Lazy[A](gen: => A) {
    lazy val value = gen
    def map[B](f: A => B) = Lazy(f(value))
    def flatMap[B](f: A => Lazy[B]) = Lazy(f(value).value)
    def foreach[B](f: A => B) { f(value) }
    def asItself: Itself[A] = Itself from value
  }
  object Lazy {
    def apply[A](gen: => A) = new Lazy(gen)
  }
  
  /** Caches expensive computations that are cleared when memory gets especially tight (via SoftReference); not thread-safe */
  class Soft[T, U](t: T)(gen: T => U) {
    private[this] var myCache = new java.lang.ref.SoftReference(gen(t))
    def apply(): U = {
      var u = myCache.get()
      if (u==null) {
        u = gen(t)
        myCache = new java.lang.ref.SoftReference(u)
      }
      u
    }
    def asItself: Itself[U] = Itself from apply()
  }
  object Soft {
    def apply[T,U](t: T)(gen: T => U) = new Soft(t)(gen)
  }

  /** Clearable, chainable, thread-safe caching with side-state.  Fairly heavyweight operation, so use for work that is significant. */
  sealed trait Hold[V] {
    /** Indicate that this value should be recomputed next access. */
    def invalidate(): Unit

    /** Indicate that this value, and everything it depends on, should be released and recomputed. */
    def release(): Unit

    /** Get a count of the number of computations (which serves as a timestamp of sorts) */
    def stamp(): Long

    /** Get the current value and current count of computations */
    def get(): (V, Long)

    /** Get the current value */
    def apply(): V

    /** Produce a new Hold that depends on this one. */
    final def map[A](f: V => A): Hold[A] = Hold.map(this)(f)
  }
  object Hold {
    sealed abstract class Checking[H, V, C](hidden: H) extends Hold[V] {
      // These things must only be used while synchronized
      protected var h: H = hidden
      protected var v: Any = Checking.invalidated
      protected var n: Long = 0L
      protected def reusable(context: C): Boolean    // Called EXACTLY ONCE
      protected def advance(context: C): V           // May NOT call reusable()
      // End of things that must be used synchronized

      final def invalidate() { this.synchronized { if (!v.isInstanceOf[Checking.Empty]) v = Checking.invalidated } }

      final def stamp(): Long = this.synchronized { n }

      final protected def get(context: C): (V, Long) = this.synchronized {
        if (!reusable(context) || (v.isInstanceOf[Checking.Empty])) {    // Note--reusable comes first so it is called exactly once
          v = advance(context)
          n += 1
        }
        (v.asInstanceOf[V], n)
      }

      final protected def apply(context: C): V = this.synchronized {
        if (!reusable(context) || (v.isInstanceOf[Checking.Empty])) {   // Note--reusable comes first so it is called exactly once
          v = advance(context)
          n += 1
        }
        v.asInstanceOf[V]
      }
    }
    object Checking {
      private[Hold] class Empty {}
      private[Hold] val invalidated = new Empty {}
      private[Hold] val released = new Empty {}
    }

    sealed abstract class Caching[H, V](hidden: H)(gen: H => V) extends Checking[H, V, Unit](hidden) {
      protected def reusable(): Boolean
      final def release() { invalidate() }
      final def reusable(context: Unit): Boolean = reusable()
      final def advance(context: Unit): V = gen(h)
      def get(): (V, Long) = get(())
      def apply(): V = apply(())
    }

    sealed abstract class Mapping[V, C] extends Checking[Long, V, C](0L) {
      protected def gatherContext(): C         // Must NOT use within synchronized!
      protected def extract(context: C): Long
      protected def forget(): Unit

      final def release() {
        val done = this.synchronized { 
          val ans = v.asInstanceOf[AnyRef] eq Checking.released
          if (!ans) v = Checking.released
          ans
        }
        if (!done) forget()
      }

      protected def reusable(context: C): Boolean = {
        val m = extract(context)
        val ans = m == h
        if (!ans) h = m
        ans
      }

      final def get(): (V, Long) = get(gatherContext())

      final def apply(): V = apply(gatherContext())
    }

    def apply[V](gen: => V): Hold[V] =
      new Caching[Unit, V](())(_ => gen) { protected def reusable() = true }

    def tested[V](zero: V)(test: V => Boolean)(next: V => V): Hold[V] =
      new Caching[V, V](zero)(next) {
        protected def reusable() =
          if (v.isInstanceOf[Checking.Empty]) false
          else {
            h = v.asInstanceOf[V]
            test(h)
          }
      }

    final class Counted(uses: Long) {
      def apply[V](gen: => V): Hold[V] =
        new Caching[Long, V](0L)(_ => gen) {
          protected def reusable() = {
            if (h < uses) { h += 1; true }
            else { h = 0; false }
          }
        }
    }
    def counted(uses: Long) = new Counted(uses)

    final class Timed(timeout: java.time.Duration) {
      def apply[V](gen: => V): Hold[V] =
        new Caching[java.time.Instant, V](java.time.Instant.EPOCH)(_ => gen) {
          protected def reusable() = {
            val now = java.time.Instant.now
            val ans = timeout.compareTo(java.time.Duration.between(h, now)) >= 0
            if (!ans) h = now
            ans
          }
        }
    }
    def timed(timeout: java.time.Duration) = new Timed(timeout)

    def okay[N, Y](gen: => Ok[N, Y]): Hold[Ok[N, Y]] =
      new Caching[Unit, Ok[N, Y]](())(_ => gen) {
        protected def reusable() = (!v.isInstanceOf[Checking.Empty]) && v.asInstanceOf[Ok[N, Y]].isOk
      }

    def option[A](gen: => Option[A]): Hold[Option[A]] =
      new Caching[Unit, Option[A]](())(_ => gen) {
        protected def reusable() = (!v.isInstanceOf[Checking.Empty]) && v.asInstanceOf[Option[A]].isDefined
      }

    def preserve[V](hV: Hold[V])(p: V => Boolean): Hold[V] =
      new Checking[Long, V, (V, Long)](0L) {
        protected def advance(context: (V, Long)) = context._1
        protected def reusable(context: (V, Long)) = {
          val ans = context._2 == h
          if (!ans) h = context._2
          ans
        }
        def release() { hV.release(); invalidate() }
        def get(): (V, Long) = {
          val failed = this.synchronized{ !v.isInstanceOf[Checking.Empty] && !p(v.asInstanceOf[V]) }
          if (failed) hV.release()
          get(hV.get())
        }
        def apply(): V = {
          val failed = this.synchronized{ !v.isInstanceOf[Checking.Empty] && !p(v.asInstanceOf[V]) }
          if (failed) hV.release()
          apply(hV.get())
        }
      }

    def map[V, A](hV: Hold[V])(f: V => A): Hold[A] =
      new Mapping[A, (V, Long)] {
        protected def forget() { hV.release() }
        protected def gatherContext() = hV.get()
        protected def extract(context: (V, Long)) = context._2
        protected def advance(context: (V, Long)) = f(context._1)
      }

    def map[V, U, A](hV: Hold[V], hU: Hold[U])(f: (V, U) => A): Hold[A] =
      new Mapping[A, ((V, Long), (U, Long))] {
        protected def forget() { hV.release(); hU.release() }
        protected def gatherContext() = (hV.get(), hU.get())
        protected def extract(context: ((V, Long), (U, Long))) = context._1._2 + context._2._2
        protected def advance(context: ((V, Long), (U, Long))) = f(context._1._1, context._2._1)
      }

    def map[V, U, T, A](hV: Hold[V], hU: Hold[U], hT: Hold[T])(f: (V, U, T) => A): Hold[A] =
      new Mapping[A, ((V, Long), (U, Long), (T, Long))] {
        protected def forget() { hV.release(); hU.release(); hT.release() }
        protected def gatherContext() = (hV.get(), hU.get(), hT.get())
        protected def extract(context: ((V, Long), (U, Long), (T, Long))) = context._1._2 + context._2._2 + context._3._2
        protected def advance(context: ((V, Long), (U, Long), (T, Long))) = f(context._1._1, context._2._1, context._3._1)
      }

    def map[V, U, T, S, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S])(f: (V, U, T, S) => A): Hold[A] =
      new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long))] {
        protected def forget() { hV.release(); hU.release(); hT.release(); hS.release() }
        protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get())
        protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long))) =
          context._1._2 + context._2._2 + context._3._2 + context._4._2
        protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long))) =
          f(context._1._1, context._2._1, context._3._1, context._4._1)
      }

    def map[V, U, T, S, R, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S], hR: Hold[R])(f: (V, U, T, S, R) => A): Hold[A] =
      new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long))] {
        protected def forget() { hV.release(); hU.release(); hT.release(); hS.release(); hR.release() }
        protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get(), hR.get())
        protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long))) =
          context._1._2 + context._2._2 + context._3._2 + context._4._2 + context._5._2
        protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long))) = 
          f(context._1._1, context._2._1, context._3._1, context._4._1, context._5._1)
      }

    def map[V, U, T, S, R, Q, A](hV: Hold[V], hU: Hold[U], hT: Hold[T], hS: Hold[S], hR: Hold[R], hQ: Hold[Q])(f: (V, U, T, S, R, Q) => A): Hold[A] =
      new Mapping[A, ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long))] {
        protected def forget() { hV.release(); hU.release(); hT.release(); hS.release(); hR.release(); hQ.release() }
        protected def gatherContext() = (hV.get(), hU.get(), hT.get(), hS.get(), hR.get(), hQ.get())
        protected def extract(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long))) =
          context._1._2 + context._2._2 + context._3._2 + context._4._2 + context._5._2 + context._6._2
        protected def advance(context: ((V, Long), (U, Long), (T, Long), (S, Long), (R, Long), (Q, Long))) = 
          f(context._1._1, context._2._1, context._3._1, context._4._1, context._5._1, context._6._1)
      }
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

  trait Walker[@specialized A] { self =>
    def step(f: A => Unit): Boolean
    def peek(g: A => Unit) = new Walker[A] {
      def step(f: A => Unit): Boolean = self.step{ a => g(a); f(a) }
    }
    def foreach[U](f: A => U) {
      while(step(a => { f(a); () })) {}
    }
    def filter(p: A => Boolean) = new Walker[A] {
      def step(f: A => Unit): Boolean = {
        var good = false
        while (self.step(a => if ({ good = p(a); good }) f(a)) && !good) {}
        good
      }
    }
    def withFilter(p: A => Boolean) = filter(p)
    def ++(s: Walker[A]): Walker[A] = new Walker[A] {
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

  sealed abstract class Q[+A] { self =>
    def alive: Boolean
    def value: A
    def opt: Option[A] = if (alive) Some(value) else None
    def live: Q[A] = new Q.Zombie(self, true)
    def dead: Q[A] = new Q.Zombie(self, false)
    def fix: Q[A] = if (alive) new Q.Alive(value) else new Q.Dead(value)
    def as[A1 >: A](a: A1): Q[A1] = new Q.Alive(a)
    def or[A1 >: A](that: Q[A1]): Q[A1] = new Q.Or(self, that)
    def map[B](f: A => B): Q[B] = new Q.Mapped(self, f)
    def flatMap[B](f: A => Q[B]): Q[B] = new Q.Flat(self, f)
    def filter(p: A => Boolean): Q[A] = new Q.Filter(self, p)
    def cache: Q[A] = new Q.Cache(this)
    final def withFilter(p: A => Boolean): Q[A] = filter(p)
    def foreach[U](f: A => U) { if (alive) f(value) }
    override def equals(a: Any) = a match {
      case q: Q[_] => alive == q.alive && value == q.value
      case _ => false
    }
    def merge[B,Z](b: Q[B])(op: (A,B) => Z): Q[Z] = new Q.Merge2(self, b, op)
    def merge[B,C,Z](b: Q[B], c: Q[C])(op: (A, B, C) => Z): Q[Z] = new Q.Merge3(self, b, c, op)
    def merge[B,C,D,Z](b: Q[B], c: Q[C], d: Q[D])(op: (A, B, C, D) => Z): Q[Z] = new Q.Merge4(self, b, c, d, op)
    def merge[B,C,D,E,Z](b: Q[B], c: Q[C], d: Q[D], e: Q[E])(op: (A, B, C, D, E) => Z): Q[Z] = new Q.Merge5(self, b, c, d, e, op)
    def merge[B,C,D,E,F,Z](b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F])(op: (A, B, C, D, E, F) => Z): Q[Z] =
      new Q.Merge6(self, b, c, d, e, f, op)
    def merge[B,C,D,E,F,G,Z](b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F], g: Q[G])(op: (A, B, C, D, E, F, G) => Z): Q[Z] =
      new Q.Merge7(self, b, c, d, e, f, g, op)
    def merge[B,C,D,E,F,G,H,Z](b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F], g: Q[G], h: Q[H])(op: (A, B, C, D, E, F, G, H) => Z): Q[Z] =
      new Q.Merge8(self, b, c, d, e, f, g, h, op)
    def merge[B,C,D,E,F,G,H,I,Z](b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F], g: Q[G], h: Q[H], i: Q[I])(op: (A, B, C, D, E, F, G, H, I) => Z): Q[Z] =
      new Q.Merge9(self, b, c, d, e, f, g, h, i, op)
    override def hashCode =
      if (alive) value.hashCode ^ 0x515B2B41
      else (if (value.asInstanceOf[AnyRef] eq null) 0 else value.hashCode) ^ 0x4a2B5B51
    override def toString =
      if (alive) "_" + value.toString + "_"
      else if (value.asInstanceOf[AnyRef] eq null) "(null)"
      else "(" + value.toString + ")"
  }
  object Q {
    private[kse] class Cache[+A](gen: Q[A]) extends Q[A] {
      private[this] var myValue: AnyRef = null
      private[this] var myAlive = -1
      private[this] def mySet { myAlive = if (gen.alive) 1 else 0; myValue = gen.value.asInstanceOf[AnyRef] }
      def alive = synchronized { if (myAlive < 0) mySet; myAlive == 1 }
      def value = synchronized { if (myAlive < 0) mySet; myValue.asInstanceOf[A] }
      override def cache = this
    }

    def apply[A](value: A): Q[A] = new Alive(value)
    def eval[A](value: => A): Q[A] = new Eval(() => value)
    def empty[A](default: A): Q[A] = new Dead(default)
    def set[A](value: A): Vary[A] = { val v = new Vary(value); v.alive = true; v }
    def unset[A](default: A): Vary[A] = new Vary(default);
    private[kse] class Alive[+A](val value: A) extends Q[A] { def alive = true; override def cache: Q[A] = this }
    private[kse] class Dead[+A](val value: A) extends Q[A] { def alive = false; override def cache: Q[A] = this }
    private[kse] class Zombie[+A](that: Q[A], val alive: Boolean) extends Q[A] { def value = that.value; override def cache: Q[A] = this }
    private[kse] class Eval[+A](evaluate: () => A) extends Q[A] {
      def alive = true
      def value = evaluate()
    }
    class Vary[A](zero: A) extends Q[A] {
      var alive: Boolean = false
      def aliveTo(live: Boolean): this.type = { alive = live; this }
      def aliveFn(f: Boolean => Boolean): this.type = { alive = f(alive); this }
      private[this] var myValue = zero
      def value = myValue
      def value_=(a: A) { myValue = a; alive = true }
      def valueTo(a: A): this.type = { value = a; alive = true; this }
      def valueFn(f: A => A): this.type = { value = f(value); alive = true; this }
    }
    private[kse] class Or[+A](primary: Q[A], secondary: Q[A]) extends Q[A] {
      def alive = primary.alive || secondary.alive
      def value = if (primary.alive) primary.value else if (secondary.alive) secondary.value else primary.value
    }
    private[kse] class Mapped[+B, +A](that: Q[A], mapping: A => B) extends Q[B] {
      def alive = that.alive
      def value = mapping(that.value)
      override def map[C](f: B => C): Q[C] = new Q.Mapped(that, mapping andThen f)
    }
    private[kse] class Flat[+B, +A](that: Q[A], mapping: A => Q[B]) extends Q[B] {
      def alive = that.alive && mapping(that.value).alive
      def value = mapping(that.value).value
    }
    private[kse] class Filter[+A](it: Q[A], predicate: A => Boolean) extends Q[A] {
      def alive = it.alive && predicate(it.value)
      def value = it.value
      override def filter(p: A => Boolean): Q[A] = new Filter(it, (a: A) => predicate(a) && p(a))
    }
    private[kse] class Merge2[+A, +B, Z](a: Q[A], b: Q[B], op: (A, B) => Z) extends Q[Z] {
      def alive = a.alive && b.alive
      def value = op(a.value, b.value)
    }
    private[kse] class Merge3[+A, +B, +C, Z](a: Q[A], b: Q[B], c: Q[C], op: (A, B, C) => Z) extends Q[Z] {
      def alive = a.alive && b.alive && c.alive
      def value = op(a.value, b.value, c.value)
    }
    private[kse] class Merge4[+A, +B, +C, +D, Z](a: Q[A], b: Q[B], c: Q[C], d: Q[D], op: (A, B, C, D) => Z) extends Q[Z] {
      def alive = a.alive && b.alive && c.alive && d.alive
      def value = op(a.value, b.value, c.value, d.value)
    }
    private[kse] class Merge5[+A, +B, +C, +D, +E, Z](
      a: Q[A], b: Q[B], c: Q[C], d: Q[D], e: Q[E], op: (A, B, C, D, E) => Z
    ) extends Q[Z] {
      def alive = a.alive && b.alive && c.alive && d.alive && e.alive
      def value = op(a.value, b.value, c.value, d.value, e.value)
    }
    private[kse] class Merge6[+A, +B, +C, +D, +E, +F, Z](
      a: Q[A], b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F], op: (A, B, C, D, E, F) => Z
    ) extends Q[Z] {
      def alive = a.alive && b.alive && c.alive && d.alive && e.alive && f.alive
      def value = op(a.value, b.value, c.value, d.value, e.value, f.value)
    }
    private[kse] class Merge7[+A, +B, +C, +D, +E, +F, +G, Z](
      a: Q[A], b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F], g: Q[G], op: (A, B, C, D, E, F, G) => Z
    ) extends Q[Z] {
      def alive = a.alive && b.alive && c.alive && d.alive && e.alive && f.alive && g.alive
      def value = op(a.value, b.value, c.value, d.value, e.value, f.value, g.value)
    }
    private[kse] class Merge8[+A, +B, +C, +D, +E, +F, +G, +H, Z](
      a: Q[A], b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F], g: Q[G], h: Q[H], op: (A, B, C, D, E, F, G, H) => Z
    ) extends Q[Z] {
      def alive = a.alive && b.alive && c.alive && d.alive && e.alive && f.alive && g.alive && h.alive
      def value = op(a.value, b.value, c.value, d.value, e.value, f.value, g.value, h.value)
    }
    private[kse] class Merge9[+A, +B, +C, +D, +E, +F, +G, +H, +I, Z](
      a: Q[A], b: Q[B], c: Q[C], d: Q[D], e: Q[E], f: Q[F], g: Q[G], h: Q[H], i: Q[I], op: (A, B, C, D, E, F, G, H, I) => Z
    ) extends Q[Z] {
      def alive = a.alive && b.alive && c.alive && d.alive && e.alive && f.alive && g.alive && h.alive && i.alive
      def value = op(a.value, b.value, c.value, d.value, e.value, f.value, g.value, h.value, i.value)
    }
  }

  /*
  sealed abstract class RingBuffer[@specialized(Int, Long, Float, Double) A] {
    protected var underlying: Array[A] = null
    protected var i0: Int = -1
    protected var i1: Int = -1
    final def internalIndex(i: Int): Int =
      if (i0 < 0) -1
      else if (i < 0) i1 + i + 1 match {
        case x if x < 0 =>
          if (i0 <= i1) -1
          else {
            val y = x + underlying.length
            if (y >= i0) y else -1
          }
        case x          =>
          if (i0 > i1 || x >= i0) x else -1
      }
      else i0 + i match {
        case x if x >= underlying.length =>
          if (i0 <= i1) -1
          else {
            val y = x - underlying.length
            if (y <= i1) y else -1
          }
        case x =>
          if (i0 > i1 || x <= i1) x else -1
      }
    def intoArray(a: Array[A]): Unit
    def makeArray(n: Int): Array[A]
    final def embiggen(): this.type = {
      val m = length
      val n = 0x7FFFFFE & ((m << 1) | m | 4)
      val a = makeArray(n)
      intoArray(a)
      underlying = a
      my0 = if (m == 0) -1 else 0
      my1 = m-1
      this
    }
    final def shrinkwrap(): this.type = {
      val m = length
      if (m == underlying.length) this
      else {
        val a = makeArray(m)
        intoArray(a)
        underlying = a
        my0 = if (m == 0) -1 else 0
        my1 = m - 1
        this
      }
    }
    def apply(i: Int): A
    def update(i: Int, a: A): Unit
    def push(a: A): this.type
    def pop(): A
    def prepush(a: Int): this.type
    def prepop(): A
    final def isEmpty = i0 < 0
    final def length: Int = if (i0 < 0) 0 else if (i1 >= i0) 1+i1-i0 else underlying.length-(i1-i0+1)
    def iterator(): Iterator[A] = new collection.AbstractIterator[A] {
      var i = i0
      val stop = i1
      val buf = underlying
      def hasNext = i >= 0
      def next = {
        val ans = buf(i)
        if (i == i1) i = -1
        else {
          i += 1
          if (i >= buf.length) i = 0
        }
        ans
      }
    }
    def map[@specialized(Int, Long, Float, Double) B](f: A => B)(implicit tag: reflect.ClassTag[B]): RingBuffer[B]
    def filterMe(f: A => Boolean, sense: Boolean = true): this.type
    def mapMe(f: A => A): this.type
    def toArray: Array[A]
  }
  object RingBuffer {
    final class RingOfInt private (initialBuffer: Array[Int], initial0: Int, initial1: Int) extends RingBuffer[Int] {
      underlying = initialBuffer
      i0 = initial0
      i1 = initial1
      def intoArray(a: Array[Int]) {
        if (i0 <= i1) System.arraycopy(myBuffer, i0, a, 0, m)
        else {
          System.arraycopy(myBuffer, i0, a, 0, myBuffer.length - i0)
          System.arraycopy(myBuffer, 0, a, myBuffer.length - i0, i1 + 1)
        }
      }
      def apply(i: Int) = myBuffer(internalIndex(i))
      def update(i: Int, a: Int) { myBuffer(internalIndex(i)) = a }
      def push(a: Int): this.type = {
        var i = i1+1
        if (i == myBuffer.length) i = 0
        if (i == i0) {
          embiggen()
          i1 += 1
        }
        else i1 = i
        myBuffer(i1) = a
        this
      }
      def pop(a: Int): Int = {
        ???
      }
    }
  }
  */
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
  
  implicit class Tuple6UtilityMethods[A,B,C,D,E,F](val underlying: (A,B,C,D,E,F)) extends AnyVal {
    @inline def _1To(value: A) = (value, underlying._2, underlying._3, underlying._4, underlying._5, underlying._6)
    @inline def _2To(value: B) = (underlying._1, value, underlying._3, underlying._4, underlying._5, underlying._6)
    @inline def _3To(value: C) = (underlying._1, underlying._2, value, underlying._4, underlying._5, underlying._6)
    @inline def _4To(value: D) = (underlying._1, underlying._2, underlying._3, value, underlying._5, underlying._6)
    @inline def _5To(value: E) = (underlying._1, underlying._2, underlying._3, underlying._4, value, underlying._6)
    @inline def _6To(value: F) = (underlying._1, underlying._2, underlying._3, underlying._4, underlying._5, value)
    @inline def _1Fn[Z](f: A => Z) = (f(underlying._1), underlying._2, underlying._3, underlying._4, underlying._5, underlying._6)
    @inline def _2Fn[Z](f: B => Z) = (underlying._1, f(underlying._2), underlying._3, underlying._4, underlying._5, underlying._6)
    @inline def _3Fn[Z](f: C => Z) = (underlying._1, underlying._2, f(underlying._3), underlying._4, underlying._5, underlying._6)
    @inline def _4Fn[Z](f: D => Z) = (underlying._1, underlying._2, underlying._3, f(underlying._4), underlying._5, underlying._6)
    @inline def _5Fn[Z](f: E => Z) = (underlying._1, underlying._2, underlying._3, underlying._4, f(underlying._5), underlying._6)
    @inline def _6Fn[Z](f: F => Z) = (underlying._1, underlying._2, underlying._3, underlying._4, underlying._5, f(underlying._6))
    @inline def eachFn[A1, B1, C1, D1, E1, F1](f: A => A1, g: B => B1, h: C => C1, i: D => D1, j: E => E1, k: F => F1) = (f(underlying._1), g(underlying._2), h(underlying._3), i(underlying._4), j(underlying._5), k(underlying._6))
    @inline def fold[Z](f: (A,B,C,D,E,F) => Z) = f(underlying._1, underlying._2, underlying._3, underlying._4, underlying._5, underlying._6)
    @inline def _without1 = (underlying._2, underlying._3, underlying._4, underlying._5, underlying._6)
    @inline def _without2 = (underlying._1, underlying._3, underlying._4, underlying._5, underlying._6)
    @inline def _without3 = (underlying._1, underlying._2, underlying._4, underlying._5, underlying._6)
    @inline def _without4 = (underlying._1, underlying._2, underlying._3, underlying._5, underlying._6)
    @inline def _without5 = (underlying._1, underlying._2, underlying._3, underlying._4, underlying._6)
    @inline def _without6 = (underlying._1, underlying._2, underlying._3, underlying._4, underlying._5)
    @inline def tup[Z](z: Z) = (underlying._1, underlying._2, underlying._3, underlying._4, underlying._5, underlying._6, z)
  }
  implicit class Tuple6IdenticalUtilityMethods[A](val underlying: (A,A,A,A,A,A)) extends AnyVal {
    @inline def sameFn[Z](f: A => Z) = (f(underlying._1), f(underlying._2), f(underlying._3), f(underlying._4), f(underlying._5), f(underlying._6))
    @inline def reduce(f: (A,A) => A) = f( f( f( f(underlying._1, underlying._2), f(underlying._3, underlying._4) ), underlying._5 ), underlying._6 )
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
    def walker = new Walker[A] {
      def step(f: A => Unit) = if (underlying.hasNext) { f(underlying.next); true } else false
    }
    def step(f: A => Unit) = if (underlying.hasNext) { f(underlying.next); true } else false
  }

  implicit class RichCollectionOfTuple4[
    A, B, C, D, T, CC[T] <: collection.Traversable[T]
  ](private val underlying: CC[(A, B, C, D)]) extends AnyVal {
    def unzip4[CCA, CCB, CCC, CCD](
      implicit
        cbfa: collection.generic.CanBuildFrom[CC[(A, B, C, D)], A, CCA],
        cbfb: collection.generic.CanBuildFrom[CC[(A, B, C, D)], B, CCB],
        cbfc: collection.generic.CanBuildFrom[CC[(A, B, C, D)], C, CCC],
        cbfd: collection.generic.CanBuildFrom[CC[(A, B, C, D)], D, CCD]
    ): (CCA, CCB, CCC, CCD) = {
      val bfa = cbfa()
      val bfb = cbfb()
      val bfc = cbfc()
      val bfd = cbfd()
      underlying.foreach{ case (a, b, c, d) =>
        bfa += a
        bfb += b
        bfc += c
        bfd += d
      }
      (bfa.result(), bfb.result(), bfc.result(), bfd.result())
    }
  }

  implicit class RichCollectionOfTuple5[
    A, B, C, D, E, T, CC[T] <: collection.Traversable[T]
  ](private val underlying: CC[(A, B, C, D, E)]) extends AnyVal {
    def unzip5[CCA, CCB, CCC, CCD, CCE](
      implicit
        cbfa: collection.generic.CanBuildFrom[CC[(A, B, C, D, E)], A, CCA],
        cbfb: collection.generic.CanBuildFrom[CC[(A, B, C, D, E)], B, CCB],
        cbfc: collection.generic.CanBuildFrom[CC[(A, B, C, D, E)], C, CCC],
        cbfd: collection.generic.CanBuildFrom[CC[(A, B, C, D, E)], D, CCD],
        cbfe: collection.generic.CanBuildFrom[CC[(A, B, C, D, E)], E, CCE]
    ): (CCA, CCB, CCC, CCD, CCE) = {
      val bfa = cbfa()
      val bfb = cbfb()
      val bfc = cbfc()
      val bfd = cbfd()
      val bfe = cbfe()
      underlying.foreach{ case (a, b, c, d, e) =>
        bfa += a
        bfb += b
        bfc += c
        bfd += d
        bfe += e
      }
      (bfa.result(), bfb.result(), bfc.result(), bfd.result(), bfe.result())
    }
  }

  implicit class RichCollectionOfTuple6[
    A, B, C, D, E, F, T, CC[T] <: collection.Traversable[T]
  ](private val underlying: CC[(A, B, C, D, E, F)]) extends AnyVal {
    def unzip6[CCA, CCB, CCC, CCD, CCE, CCF](
      implicit
        cbfa: collection.generic.CanBuildFrom[CC[(A, B, C, D, E, F)], A, CCA],
        cbfb: collection.generic.CanBuildFrom[CC[(A, B, C, D, E, F)], B, CCB],
        cbfc: collection.generic.CanBuildFrom[CC[(A, B, C, D, E, F)], C, CCC],
        cbfd: collection.generic.CanBuildFrom[CC[(A, B, C, D, E, F)], D, CCD],
        cbfe: collection.generic.CanBuildFrom[CC[(A, B, C, D, E, F)], E, CCE],
        cbff: collection.generic.CanBuildFrom[CC[(A, B, C, D, E, F)], F, CCF]
    ): (CCA, CCB, CCC, CCD, CCE, CCF) = {
      val bfa = cbfa()
      val bfb = cbfb()
      val bfc = cbfc()
      val bfd = cbfd()
      val bfe = cbfe()
      val bff = cbff()
      underlying.foreach{ case (a, b, c, d, e, f) =>
        bfa += a
        bfb += b
        bfc += c
        bfd += d
        bfe += e
        bff += f
      }
      (bfa.result(), bfb.result(), bfc.result(), bfd.result(), bfe.result(), bff.result())
    }
  }

  implicit class CanSplitWithIndicator[A, CC[A]](xs: CC[A])(implicit trav: CC[A] => collection.TraversableOnce[A]) {
    import collection.generic.CanBuildFrom
    def equivalenceSplit[B](indicator: A => B)(implicit cbf: CanBuildFrom[CC[A], A, CC[A]], cbf2: CanBuildFrom[CC[A], CC[A], CC[CC[A]]]): CC[CC[A]] = {
      val ccb = cbf2()
      var first = true
      var last = null.asInstanceOf[B]
      var cb: collection.mutable.Builder[A, CC[A]] = null
      trav(xs).foreach{ x =>
        val i = indicator(x)
        if (first || last != i) {
          if (!first) ccb += cb.result()
          else first = false
          cb = cbf()
          cb += x
          last = i
        }
        else cb += x
      }
      if (cb != null) ccb += cb.result()
      ccb.result()
    }

    def distinctSplit[B](indicator: A => B)(implicit cbf: CanBuildFrom[CC[A], A, CC[A]], cbf2: CanBuildFrom[CC[A], CC[A], CC[CC[A]]]): CC[CC[A]] = {
      val ccb = cbf2()
      val seen = collection.mutable.HashSet.empty[B]
      var cb: collection.mutable.Builder[A, CC[A]] = null
      trav(xs).foreach{ x =>
        val i = indicator(x)
        if (seen contains i) {
          if (cb ne null) ccb += cb.result()
          cb = null
          seen.clear
        }
        if (cb eq null) cb = cbf()
        seen += i
        cb += x
      }
      if (cb ne null) ccb += cb.result()
      ccb.result()
    }

    def splitUnless(indicator: (A, A) => Boolean)(implicit cbf: CanBuildFrom[CC[A], A, CC[A]], cbf2: CanBuildFrom[CC[A], CC[A], CC[CC[A]]]): CC[CC[A]] = {
      val ccb = cbf2()
      var last: A = null.asInstanceOf[A]
      var cb: collection.mutable.Builder[A, CC[A]] = null
      trav(xs).foreach{ x =>
        if (cb eq null) {
          cb = cbf()
        }
        else if (!indicator(last, x)) {
          ccb += cb.result()
          cb = cbf()
        }
        cb += x
        last = x
      }
      if (cb ne null) ccb += cb.result()
      ccb.result()
    }
  }
  implicit class StringCanSplitWithIndicator(xs: String) {
    import collection.generic.CanBuildFrom
    def equivalenceSplit[B](indicator: Char => B): Array[String] = {
      val sb = Array.newBuilder[String]
      var i = 0
      while (i < xs.length) {
        val b = indicator(xs.charAt(i))
        var j = i + 1
        while (j < xs.length && indicator(xs.charAt(j)) == b) j += 1
        sb += xs.substring(i, j)
        i = j
      }
      sb.result()
    }
  }

  implicit class CanDeduplicateArrayPrefix[A: reflect.ClassTag](array: Array[A]) {
    def deduplicatedPrefix: (Array[A], Int, Array[A]) = {
      val coded = array.indices.groupBy(i => array(i))
      val candidates = coded.
        collect{ case (_, xs) if xs.length > 2 => xs }.
        toArray.sortBy(_.head)
      val lookup = candidates.zipWithIndex.
        flatMap{ case (xs, n) => xs.map(_ -> n) }.toMap
      if (!(lookup contains 0)) (Array.empty[A], 0, array)
      else {
        val starts = candidates.head
        val intervals = starts.indices.tail.map{ ii =>
          val delta = starts(ii) - starts(0)
          delta ->
            (ii until starts.length by ii).takeWhile{ jj => 
              starts(jj) - starts(jj-ii) == delta && 
              (0 until delta).forall(k => lookup contains k) &&
              (0 until delta).forall(k => lookup.get(k) == lookup.get(starts(jj)+k))
            }.map(ii => starts(ii)).toArray
        }.filter(_._2.length > 0)
        intervals.sortBy{ case (delta, si) => -((si.last + delta) * starts.length.toLong) + delta }.headOption match {
          case None => (Array.empty, 0, array)
          case Some((delta, si)) => (array.take(delta), si.length + 1, array.drop(si.last + delta))
        }
      }
    }
  }

  implicit class CanUnmixOk[N, Y, CC[_]](cc: CC[Ok[N, Y]])(implicit trav: CC[Ok[N, Y]] => collection.TraversableOnce[Ok[N, Y]]) {
    import collection.generic.CanBuildFrom
    def unmix(implicit cbfn: CanBuildFrom[CC[Ok[N, Y]], N, CC[N]], cbfy: CanBuildFrom[CC[Ok[N, Y]], Y, CC[Y]]) = {
      val bfn = cbfn()
      val bfy = cbfy()
      cc.foreach{
        case Yes(y) => bfy += y
        case No(n)  => bfn += n
      }
      (bfn.result(), bfy.result())
    }
    def unmixWithIndex(implicit cbfn: CanBuildFrom[CC[Ok[N, Y]], (N, Int), CC[(N, Int)]], cbfy: CanBuildFrom[CC[Ok[N, Y]], (Y, Int), CC[(Y, Int)]]) = {
      val bfn = cbfn()
      val bfy = cbfy()
      var i = 0
      cc.foreach{
        case Yes(y) => bfy += (y -> i); i += 1
        case No(n)  => bfn += (n -> i); i += 1
      }
      (bfn.result(), bfy.result())
    }
  }

  implicit class CanUnmixArrayOk[N, Y](private val a: Array[Ok[N, Y]]) extends AnyVal {
    def unmix(implicit ntag: reflect.ClassTag[N], ytag: reflect.ClassTag[Y]): (Array[N], Array[Y]) = {
      val is = new Array[Int](a.length)
      var iA = 0
      var iB = is.length - 1
      var i = 0
      while (i < a.length) {
        if (a(i).isOk) { is(iA) = i; iA += 1 }
        else           { is(iB) = i; iB -= 1 }
        i += 1
      }
      val ys = new Array[Y](iA)
      val ns = new Array[N](a.length - iA)
      i = 0
      while (i < ys.length) { ys(i) = a(is(i)).yes; i += 1 }
      i = 0
      while (i < ns.length) { ns(i) = a(is(is.length - 1 - i)).no; i += 1 }
      (ns, ys)
    }
    def unmixWithIndex(implicit ntag: reflect.ClassTag[(N, Int)], ytag: reflect.ClassTag[(Y, Int)]): (Array[(N, Int)], Array[(Y, Int)]) = {
      val is = new Array[Int](a.length)
      var iA = 0
      var iB = is.length - 1
      var i = 0
      while (i < a.length) {
        if (a(i).isOk) { is(iA) = i; iA += 1 }
        else           { is(iB) = i; iB -= 1 }
        i += 1
      }
      val ys = new Array[(Y, Int)](iA)
      val ns = new Array[(N, Int)](a.length - iA)
      i = 0
      while (i < ys.length) {
        val j = is(i)
        ys(i) = (a(j).yes, j)
        i += 1
      }
      i = 0
      while (i < ns.length) { 
        val j = is(is.length - 1 - i)
        ns(i) = (a(j).no, j)
        i += 1
      }
      (ns, ys)
    }
    def unmixIndices: (Array[Int], Array[Int]) = {
      val is = new Array[Int](a.length)
      var iA = 0
      var iB = is.length - 1
      var i = 0
      while (i < a.length) {
        if (a(i).isOk) { is(iA) = i; iA += 1 }
        else           { is(iB) = i; iB -= 1 }
        i += 1
      }
      if (iA == is.length) (new Array[Int](0), is)
      else {
        val yis = java.util.Arrays.copyOf(is, iA)
        val nis = new Array[Int](is.length - iA)
        i = 0
        while (i < nis.length) {
          nis(i) = is(is.length - 1 - i)
          i += 1
        }
        (nis, yis)
      }
    }
  }

  implicit class CanUnmixEither[L, R, CC[_]](cc: CC[Either[L, R]])(implicit trav: CC[Either[L, R]] => collection.TraversableOnce[Either[L, R]]) {
    import collection.generic.CanBuildFrom
    def unmix(implicit cbfl: CanBuildFrom[CC[Either[L, R]], L, CC[L]], cbfr: CanBuildFrom[CC[Either[L, R]], R, CC[R]]) = {
      val bfl = cbfl()
      val bfr = cbfr()
      cc.foreach{
        case Right(r) => bfr += r
        case Left(l)  => bfl += l
      }
      (bfl.result(), bfr.result())
    }
    def unmixWithIndex(implicit cbfl: CanBuildFrom[CC[Either[L, R]], (L, Int), CC[(L, Int)]], cbfr: CanBuildFrom[CC[Either[L, R]], (R, Int), CC[(R, Int)]]) = {
      val bfl = cbfl()
      val bfr = cbfr()
      var i = 0
      cc.foreach{
        case Right(r) => bfr += (r -> i); i += 1
        case Left(l)  => bfl += (l -> i); i += 1
      }
      (bfl.result(), bfr.result())
    }
  }


  implicit class CanUnmixArrayEither[L, R](private val a: Array[Either[L, R]]) extends AnyVal {
    def unmix(implicit ntag: reflect.ClassTag[L], ytag: reflect.ClassTag[R]): (Array[L], Array[R]) = {
      var i = 0
      var ynum = 0
      while (i < a.length) { if (a(i).isRight) ynum += 1; i += 1 }
      var ns = new Array[L](a.length - ynum)
      var ys = new Array[R](ynum)
      var ni = 0
      var yi = 0
      i = 0
      while (i < a.length) {
        a(i) match {
          case Right(y) => ys(yi) = y; yi += 1
          case Left(n)  => ns(ni) = n; ni += 1
        }
        i += 1
      }
      (ns, ys)
    }
    def unmixWithIndex(implicit ntag: ClassTag[(L, Int)], ytag: ClassTag[(R, Int)]): (Array[(L, Int)], Array[(R, Int)]) = {
      var i = 0
      var ynum = 0
      while (i < a.length) { if (a(i).isRight) ynum += 1; i += 1 }
      var ns = new Array[(L, Int)](a.length - ynum)
      var ys = new Array[(R, Int)](ynum)
      var ni = 0
      var yi = 0
      i = 0
      while (i < a.length) {
        a(i) match {
          case Right(y) => ys(yi) = (y, i); yi += 1
          case Left(n)  => ns(ni) = (n, i); ni += 1
        }
        i += 1
      }
      (ns, ys)
    }
    def unmixIndices: (Array[Int], Array[Int]) = {
      val is = new Array[Int](a.length)
      var iA = 0
      var iB = is.length - 1
      var i = 0
      while (i < a.length) {
        if (a(i).isRight) { is(iA) = i; iA += 1 }
        else              { is(iB) = i; iB -= 1 }
        i += 1
      }
      if (iA == is.length) (new Array[Int](0), is)
      else {
        val yis = java.util.Arrays.copyOf(is, iA)
        val nis = new Array[Int](is.length - iA)
        i = 0
        while (i < nis.length) {
          nis(i) = is(is.length - 1 - i)
          i += 1
        }
        (nis, yis)
      }
    }
  }

  implicit class CanUnmixTry[A, CC[_]](cc: CC[Try[A]])(implicit trav: CC[Try[A]] => collection.TraversableOnce[Try[A]]) {
    import collection.generic.CanBuildFrom
    def unmix(implicit cbff: CanBuildFrom[CC[Try[A]], Throwable, CC[Throwable]], cbfs: CanBuildFrom[CC[Try[A]], A, CC[A]]) = {
      val bff = cbff()
      val bfs = cbfs()
      cc.foreach{
        case Success(s) => bfs += s
        case Failure(t) => bff += t
      }
      (bff.result(), bfs.result())
    }
    def unmixWithIndex(implicit cbff: CanBuildFrom[CC[Try[A]], (Throwable, Int), CC[(Throwable, Int)]], cbfs: CanBuildFrom[CC[Try[A]], (A, Int), CC[(A, Int)]]) = {
      val bff = cbff()
      val bfs = cbfs()
      var i = 0
      cc.foreach{
        case Success(s) => bfs += (s -> i); i += 1
        case Failure(t) => bff += (t -> i); i += 1
      }
      (bff.result(), bfs.result())
    }
  }

  implicit class CanUnmixArrayTry[A](private val a: Array[Try[A]]) extends AnyVal {
    def unmix(implicit atag: ClassTag[A]): (Array[Throwable], Array[A]) = {
      val good = new Array[A](a.length)
      var bad: Array[Throwable] = null
      var i = 0
      var j = 0
      var k = 0
      while (i < a.length) {
        a(i) match {
          case Success(x) => good(j) = x; j += 1
          case Failure(t) =>
            if (bad == null) bad = new Array[Throwable](8)
            else if (k >= bad.length) bad = java.util.Arrays.copyOf(bad, if (bad.length >= Int.MaxValue/2) Int.MaxValue -2 else bad.length*2)
            bad(k) = t; k += 1
        }
        i += 1
      }
      if (j == good.length) (new Array[Throwable](0), good)
      else (
        if (k == bad.length) bad else java.util.Arrays.copyOf(bad, k),
        { val temp = new Array[A](j); System.arraycopy(good, 0, temp, 0, j); temp }
      )
    }
    def unmixWithIndex(implicit atag: ClassTag[A]): (Array[(Throwable, Int)], Array[(A, Int)]) = {
      var i = 0
      var ynum = 0
      while (i < a.length) { if (a(i).isSuccess) ynum += 1; i += 1 }
      var ns = new Array[(Throwable, Int)](a.length - ynum)
      var ys = new Array[(A, Int)](ynum)
      var ni = 0
      var yi = 0
      i = 0
      while (i < a.length) {
        a(i) match {
          case Success(x) => ys(yi) = (x, i); yi += 1
          case Failure(t) => ns(ni) = (t, i); ni += 1
        }
        i += 1
      }
      (ns, ys)
    }
    def unmixIndices: (Array[Int], Array[Int]) = {
      val is = new Array[Int](a.length)
      var iA = 0
      var iB = is.length - 1
      var i = 0
      while (i < a.length) {
        if (a(i).isSuccess) { is(iA) = i; iA += 1 }
        else                { is(iB) = i; iB -= 1 }
        i += 1
      }
      if (iA == is.length) (new Array[Int](0), is)
      else {
        val yis = java.util.Arrays.copyOf(is, iA)
        val nis = new Array[Int](is.length - iA)
        i = 0
        while (i < nis.length) {
          nis(i) = is(is.length - 1 - i)
          i += 1
        }
        (nis, yis)
      }
    }
  }
}
