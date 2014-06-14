// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse

import scala.util.control.{ NonFatal, ControlThrowable }

package flow {
  trait FlowMarker  // marker trait for control flow classes
  
  abstract class Flew[@specialized(Int, Long) A] extends ControlThrowable with FlowMarker { def value: A }

  trait Flowable extends FlowMarker {
    def apply(): Nothing
  }

  trait FlowWith[@specialized(Int, Long) A] {
    def apply(a: A): Nothing
  }
  
  sealed trait FlowChain[@specialized(Int, Long) A] extends FlowWith[A] {
    def from[B](f: B => A): FlowChain[B]
  }

  trait FlowView[@specialized(Int, Long) A] extends Flowable {
    def value: A
  }
  
  trait Flow[@specialized(Int, Long) A] extends FlowView[A] with FlowWith[A] with Flowable {
    def value(a: A): this.type
    def valueFn(f: A => A): this.type
  }
  
  trait Oops extends Flow[Long] {}
  
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

  class Doable[A](val underlying: () => A) extends AnyVal {
    def until(p: A => Boolean) = { var a = underlying(); while (!p(a)) a = underlying(); a }
  }
  object Do {
    def apply[A](f: => A) = new Doable(() => f)
  }
  
  trait LowPriorityOkValidation {
    private[flow] val genericInvalidOkThrowsException = new Ok.ValidateOkay[Any] { def incorrect(any: Any) { throw new NotOkException(any) } }
    implicit def defaultInvalidOkThrowsException[N]: Ok.ValidateOkay[N] = genericInvalidOkThrowsException.asInstanceOf[Ok.ValidateOkay[N]]
  }
}

package object flow extends LowPriorityOkValidation {
  private val myOops: Flew[Long] with Oops = new Flew[Long] with Oops {
    def apply(): Nothing = throw this
    def apply(a: Long): Nothing = throw this
    def value = -1L
    def value(a: Long) = this
    def valueFn(f: Long => Long) = this
  }

  def OOPS(implicit oops: Oops): Nothing = oops()
  def tapOops[A, U](f: => A)(aside: => U)(implicit oops: Oops): A = try{ f } catch { case t if t eq oops => aside; throw t }
  def FLOW[@specialized(Int, Long) A](value: A)(implicit flow: FlowWith[A]): Nothing = flow(value)
  def tapFlow[A, B, U](f: => A)(aside: B => U)(implicit flow: FlowView[B]): A = try { f } catch { case t if t eq flow => aside(flow.value); throw t }

  def oopsless[A](f: => A)(implicit oops: Oops): Option[A] = try { Some(f) } catch { case t if t eq oops => None }
  def oopslessOr[A](default: => A)(f: => A)(implicit oops: Oops) = try { f } catch { case t if t eq oops => default }
  def oopslessOrNull[A >: Null](f: => A)(implicit oops: Oops): A = try { f } catch { case t if t eq oops => null }
  def oopslessDo[U](f: => U)(implicit oops: Oops): Unit = try { f; () } catch { case t if t eq oops => }

  def probably[A](f: Oops => A): Option[A] = try { Some(f(myOops)) } catch { case t if t eq myOops => None }
  def probablyOr[A](default: => A)(f: Oops => A): A = try { f(myOops) } catch { case t if t eq myOops => default }
  def probablyOrNull[A >: Null](f: Oops => A): A = try { f(myOops) } catch { case t if t eq myOops => null }
  def probablyDo[U](f: Oops => U): Unit = try { f(myOops); () } catch { case t if t eq myOops => }
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
  
  implicit class OptionIsFlowable[A](val underlying: Option[A]) extends AnyVal {
    def grab(implicit oops: Oops): A = if (underlying.isDefined) underlying.get else oops()
  }
  implicit class TryIsFlowable[A](val underlying: scala.util.Try[A]) extends AnyVal {
    def grab(implicit ack: Flowable): A = underlying match {
      case scala.util.Success(a) => a
      case _ => ack()
    }
    def orFlow(implicit flow: FlowWith[Throwable]): A = underlying match {
      case scala.util.Success(a) => a
      case scala.util.Failure(t) => flow(t)
    }
  }
  implicit class OkIsFlowable[N,Y](val underlying: Ok[N,Y]) extends AnyVal {
    def grab(implicit oops: Oops): Y = if (underlying.isOk) underlying.yes else oops()
    def orFlow(implicit ack: FlowWith[N]): Y = if (underlying.isOk) underlying.yes else ack(underlying.no)
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


  def flowTo[A](generate: FlowChain[A] => A): A = {
    val flow = new FlowingImpl[A]
    try { generate(flow) } catch { case t if t eq flow => flow.value }
  }
  def flowOr[A <: AnyRef](initial: A)(generate: Flow[A] => A): A = {
    val flow = new FlowImplRef[A](initial)
    try { generate(flow) } catch { case t if t eq flow => flow.value }
  }
  def flowOr(initial: Int)(generate: Flow[Int] => Int): Int = {
    val flow = new FlowImplInt(initial)
    try { generate(flow) } catch { case t if t eq flow => flow.value }
  }
  def flowOr(initial: Long)(generate: Flow[Long] => Long): Long = {
    val flow = new FlowImplLong(initial)
    try { generate(flow) } catch { case t if t eq flow => flow.value }
  }
  
  trait OkayDispatcher[N] { def apply[Y](f: FlowWith[N] => Y): Ok[N,Y] }
  private val GenericOkayDispatch = new OkayDispatcher[Any] {
    def apply[Y](f: FlowWith[Any] => Y): Ok[Any, Y] = {
      val flow = new FlowingImpl[Any]
      try { Yes(f(flow)) } catch { case t if t eq flow => No(flow.value) }
    }
  }
  def okay[N] = GenericOkayDispatch.asInstanceOf[OkayDispatcher[N]]
  def okayOr[N,Y](no: N)(yes: FlowView[N] => Y): Ok[N,Y] = {
    val flow: FlowView[N] = (no match {
      case n: Int => new FlowValImplInt(n)
      case n: Long => new FlowValImplLong(n)
      case _ => new FlowValImplRef(no.asInstanceOf[AnyRef])
    }).asInstanceOf[FlowView[N]]
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  def okayLazy[N,Y](no: => N)(yes: FlowView[N] => Y): Ok[N,Y] = {
    val flow = new FlowLazyImpl(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  def okayWith[N <: AnyRef,Y](no: N)(yes: Flow[N] => Y): Ok[N,Y] = {
    val flow = new FlowImplRef(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  def okayWith[Y](no: Int)(yes: Flow[Int] => Y): Ok[Int,Y] = {
    val flow = new FlowImplInt(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  def okayWith[Y](no: Long)(yes: Flow[Long] => Y): Ok[Long,Y] = {
    val flow = new FlowImplLong(no)
    try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  }
  def okayHere[N,Y](yes: Flow[N] => Y)(implicit flow: Flow[N]): Ok[N,Y] = try { Yes(yes(flow)) } catch { case t if t eq flow => No(flow.value) }
  
  trait ValidateDispatcher[N] { def apply[Y](f: FlowView[Seq[N]] with Ok.ValidateOkay[N] => Y): Ok[Seq[N],Y] }
  private final class FlowValValidatingImpl[N](val value: collection.mutable.ArrayBuffer[N]) extends Flew[Seq[N]] with FlowView[Seq[N]] with Ok.ValidateOkay[N] {
    def apply() = throw this
    def incorrect(n: N) { value += n }
  }
  def validating[N] = new ValidateDispatcher[N] {
    def apply[Y](f: FlowView[Seq[N]] with Ok.ValidateOkay[N] => Y): Ok[Seq[N],Y] = {
      val fvvi = new FlowValValidatingImpl(new collection.mutable.ArrayBuffer[N])
      try { Yes(f(fvvi)) } catch { case t if t eq fvvi => No(fvvi.value) }
    }
  }

  def safe[A](a: => A): Ok[Throwable, A] = try { Yes(a) } catch { case t if NonFatal(t) => No(t) }
  def safeOption[A](a: => A): Option[A] = try { Some(a) } catch { case t if NonFatal(t) => None }
  def safeOk[N,Y](no: Throwable => N)(yes: FlowWith[N] => Y): Ok[N,Y] = {
    val flow = new FlowingImpl[N]
    try{ Yes(yes(flow)) }
    catch {
      case t if t eq flow => No(flow.value)
      case t if NonFatal(t) => No(no(t))
    }
  }
  
  def FLOP(s: String) = throw new Flop(s)
}

