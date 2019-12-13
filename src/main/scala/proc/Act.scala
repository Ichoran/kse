// This file is distributed under the BSD 3-clause license
// Copyright 2019 Rex Kerr and Calico Life Sciences

package kse.proc

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable.TreeMap
import scala.util.control.NonFatal

import kse.flow._
import kse.maths._
import kse.maths.stats._
import kse.maths.fits._

/** An Act is an error-prone producer of heavyweight values.
  * It can estimate the workload to produce itself, returning
  * NaN if it doesn't know.  (Units are arbitrary).  It receives
  * information about the resources it may use  when running in order
  * to control its evaluation.
  *
  * It is customary to catch all nonfatal errors and pack them into the
  * error state.
  */
trait Act[A] { outer =>
  type E
  def act(provisions: Act.Provision): Ok[E, A]
  final def act(): Ok[E, A] = act(Act.Provision.simple)

  def identifier: Option[String]
  def quantify(): Double = Double.NaN
  protected def error(message: String): E
  override def toString = identifier match {
    case Some(s) => s
    case _ => getClass.getName + "@" + System.identityHashCode(this).toHexString
  }

  def once(): Act.Once[A] = new Act.Once[A] {
    type E = outer.E
    override def identifier = outer.identifier
    override def quantify() = outer.quantify()
    protected def error(message: String) = outer.error(message)
    def actUnsync(provisions: Act.Provision) = outer.act(provisions)
  }
}
object Act {
  trait Stringly[A] extends Act[A] {
    type E = String
    protected def error(message: String) = message
  }

  final class Simply[A](a: => A, val identifier: Option[String] = None)
  extends Act[A] with Stringly[A] {
    def act(provisions: Act.Provision): Ok[String, A] =
      try { Yes(a) }
      catch { case e if NonFatal(e) => No(e.explain()) }
  }
  object Simply {
    def apply[A](a: => A) = new Simply(a)
    def apply[A](a: => A, ident: String) = new Simply(a, Some(ident))
  }

  final class Directly[A](f: => Ok[String, A], val identifier: Option[String] = None)
  extends Act[A] with Stringly[A] {
    def act(provisions: Act.Provision): Ok[String, A] =
      try { f }
      catch { case e if NonFatal(e) => No(e.explain()) }
  }
  object Directly {
    def apply[A](f: => Ok[String, A]) = new Directly(f)
    def apply[A](f: => Ok[String, A], ident: String) = new Directly(f, Some(ident))
  }


  sealed trait Synchronously[A] extends Act[A] {
    protected def actUnsync(provisions: Act.Provision): Ok[E, A]
    protected val synchronizer = new AnyRef()
    def act(provisions: Act.Provision): Ok[E, A] = synchronizer.synchronized{ actUnsync(provisions) }
  }

  /** An Act.Once only runs itself once and caches its result. It will run
    * only once even if the result is an error.
    */
  trait Once[A] extends Synchronously[A] {
    protected var resultCache: Ok[E, A] = null
    override def act(provisions: Act.Provision): Ok[E, A] = synchronizer.synchronized{
      if (resultCache eq null) resultCache = actUnsync(provisions)
      resultCache
    }
    override def once(): this.type = this
  }
  object Once {
    /** An Act.Once.From starts with a value; once it has run
      * it will discard that value and remember the result forever.
      */
    sealed abstract class From[I >: Null <: AnyRef, F, A](f: I => Ok[F, A], err: String => F, val identifier: Option[String] = None)
    extends Once[A] {
      type E = F
      override def error(message: String) = err(message)
      override def once(): this.type = this
    }
    object From {
      sealed private[proc] class Impl[I >: Null <: AnyRef, F, A](f: I => Ok[F, A], err: String => F, identifier: Option[String])
      extends From[I, F, A](f, err, identifier) with On[I, A] {
        def actOn(i: I, provisions: Act.Provision): Ok[E, A] =
          try { f(i) }
          catch { case e if NonFatal(e) => No(error(e.explain())) }
      }

      def apply[I >: Null <: AnyRef](input: => I) = new Supply[I](input)
      final class Supply[I >: Null <: AnyRef](input: => I) {
        def via[F, A](f: I => Ok[F, A])(err: String => F, ident: I => Option[String] = _ => None): From[I, F, A] = {
          val i: I = input
          val ans = new Impl[I, F, A](f, err, ident(i))
          ans.supply(i)
          ans
        }
        def via[A](f: I => Ok[String, A], ident: I => Option[String]): From[I, String, A] = {
          val i: I = input
          val ans = new Impl[I, String, A](f, identity, ident(i))
          ans.supply(i)
          ans
        }
        def via[A](f: I => Ok[String, A]): From[I, String, A] = {
          val i: I = input
          val ans = new Impl[I, String, A](f, identity, None)
          ans.supply(i)
          ans
        }
        def simply[A](f: I => A, ident: I => Option[String] = _ => None): From[I, String, A] = {
          val i = input
          val g = (i: I) => try { Yes(f(i)) } catch { case e if NonFatal(e) => No(e.explain()) }
          val ans = new Impl[I, String, A](g, identity, ident(i))
          ans.supply(i)
          ans
        }
      }
    }
  }


  /** An Act.On can run on an argument.  It discards the argument once used.  It doesn't cache the answer.
    */
  trait On[I >: Null <: AnyRef, A] extends Act.Synchronously[A] {
    protected var supplyCache: I = null
    def supply(i: I): Boolean = if (i == null) false else synchronizer.synchronized {
      if (supplyCache != null) false
      else { supplyCache = i; true }
    }
    final def loaded: Boolean = synchronizer.synchronized { supplyCache != null }
    protected def actOn(input: I, provisions: Act.Provision): Ok[E, A]
    protected def actUnsync(provisions: Act.Provision): Ok[E, A] = {
      if (supplyCache == null) No(error("no input"))
      else {
        val ans = actOn(supplyCache, provisions)
        supplyCache = null
        ans
      }
    }
  }

  case class Provision(threads: Int, completeBy: Option[java.time.Instant]) {}
  object Provision {
    val simple = new Provision(1, None)
    def apply(i: Int) = new Provision(i, None)
  }
}


/*
trait Acts[Z >: Null <: AnyRef] extends Act[Z] {
  type A
  type K <: Act[A]
  type N
  type R <: Acts.Record[A, K, N, E]

  protected def initialRecord(): R
  protected def finalizeRecord(record: R, queue: Seq[K]): Ok[(E, Seq[K]), Z]

  protected def initialActs: Ok[E, Iterator[K]]
  protected def checkAct(k: K, timeBudget: Double = NaN): Ok[N, Double]
  protected def actMore(record: R, k: K)(result: Ok[k.E, A]): Ok[E, Iterator[K]]

  protected def actImpl(provisions: Act.Provision): Ok[(E, Seq[K]), Z]
}
object Acts {
  trait Record[A, K <: Act[A], N, E] {
    def no(act: K)(n: N): Ok[E, Unit]
    def ran(act: K)(result: Ok[act.E, A]): Ok[E, Unit]
  }

  trait Timed[Z >: Null <: AnyRef] extends Acts[Z] {
    protected val quantityModel = EstXM()
    protected val constantModel = EstM.empty
    protected val nanModel = EstM.empty
    protected val affineModel = new FitTX()
    protected def predictTime(quantity: Double): Double =
      if (quantity.isNaN) {
        if (nanModel.n > 0) nanModel.mean
        else if (constantModel.n > 0) constantModel.mean
        else Double.NaN
      }
      else if (constantModel.n == 0) {
        if (nanModel.n > 0) nanModel.mean else Double.NaN
      }
      else if (constantModel.n < 5) constantModel.n
      else {
        val cv = constantModel.mean
        val ce = constantModel.sd
        val av = affineModel.xt(quantity)
        val ae = affineModel.xDeviation(quantity)
        val w =
          if (ce.finite && ae.finite && ce > 0 && ae > 0) {
            if (quantity >= quantityModel.min && quantity <= quantityModel.max) ae / (ce + ae)
            else {
              val outside = if (quantity < quantityModel.min) quantityModel.min - quantity else quantity-quantityModel.max
              val extrap = outside / (0.5*(quantityModel.max - quantityModel.min))
              val wce = ce / (1+extrap)
              ae / (wce + ae)
            }
          }
          else 0.5

        cv*w + av*(1-w)
      }
    protected def learnTime(quantity: Double, dt: Double): this.type = {
      if (dt.finite) {
        if (quantity.finite) {
          quantityModel += quantity
          constantModel += dt
          affineModel += (quantity, dt)
        }
        else nanModel += quantity
      }
      this
    }
    protected def notEnoughTime(needed: Double, estimated: Double): N
    protected def checkActTime(k: K, timeBudget: Double): Ok[N, Unit] = 
      if (timeBudget.nan) Ok.UnitYes
      else {
        dt = predictTime(k.quantity()) max 0
        if (dt.nan) Ok.UnitYes
        else if (timeBudget <= dt * Timed.SafetyFactor) No(notEnoughTime(dt * Timed.SafetyFactor, timeBudget))
        else Ok.UnitYes
      }
    protected def checkActContent(k: K): Ok[N, Unit]
    final def checkAct(k: K, timeBudget: Double = Double.NaN): Ok[N, Unit] = 
      if (!timeBudget.nan) checkActTime(k, timeBudget).flatMap(_ => checkActContent(k))
  }
  object Timed {
    val SafetyFactor = 1.2
  }

  trait Gathered[Z >: Null <: AnyRef] extends Acts[Z] {
    type E = Gathered.Error[A, K]
    type N = Gathered.Skipped[A, K]
    type R = Gathered.Record[A, K]
    protected def initialRecord() = new Gathered.Record[A, K]()
  }
  object Gathered {
    class Record[A, K <: Act[A]]() extends Acts.Record[A, K, Skipped[A, K], Error[A, K]] {
      protected val payload = new java.util.concurrent.ConcurrentLinkedQueue[Item[A, K]]()
      def no(act: K)(n: Skipped[A, K]): Ok[Error[A, K], Unit] = {
        payload add n
        Ok.UnitYes
      }
      def ran(act: K)(result: Ok[act.E, A]): Ok[Error[A, K], Unit] = {
        result match {
          case Yes(a) => payload add Worked.apply[A, K](act)(a)
          case No(e)  => payload add Failed.apply[A, K](act)(e)
        }
        Ok.UnitYes
      }
      def retrieve() = {
        val vb = Vector.newBuilder[Item[A, K]]
        var more = true
        while (more) {
          val x = payload.poll()
          if (x eq null) more = false
          else vb += x
        }
        vb.result()
      }
    }

    sealed trait Item[A, K <: Act[A]] {}

    final class Skipped[A, K <: Act[A]](val act: K)(val reason: Skipped.Reason[A, K]) extends Item[A, K] {}
    object Skipped {
      trait Reason[A, K <: Act[A]] {
        type Why
        def why: Why
      }
    }

    final class Failed[A, K <: Act[A], KE] private (val act: K)(val err: KE) extends Item[A, K] {}
    object Failed {
      def apply[A, K <: Act[A]](act: K)(err: act.E) = new Failed[A, K, act.E](act)(err)
    }

    final class Worked[A, K <: Act[A]](val act: K)(val result: A) extends Item[A, K] {}
    object Worked {
      def apply[A, K <: Act[A]](act: K)(result: A) = new Worked[A, K](act)(result)
    }

    final class Remaining[A, K <: Act[A]](val acts: Seq[K]) extends Item[A, K] {}
    object Remaining {
      def apply[A, K <: Act[A]](acts: Seq[K]) = new Remaining[A, K](acts)
    }

    class Error[A, K <: Act[A]]()
  }

  trait Sequentially[Z >: Null <: AnyRef] extends Acts[Z] with Timed[Z] {
    protected def actImpl(provisions: Act.Provision): Ok[(E, Seq[K]), Z] = {
      val queue = scala.collection.mutable.Queue.empty[K]
      val record = initialRecord()
      initialActs.mapNo(e => (e, queue)).?.foreach(queue += _)
      while (queue.nonEmpty) {
        val k = queue.dequeue
        val timeBudget = provisions.completeBy match {
          case None => Double.NaN
          case Some(i) => Duration.between(i, Instant.now).toNanos * 1e-9
        }
        checkAct(k, timeBudget) match {
          case No(n) =>
            record.no(k)(n).mapNo(e => (e, queue)).?
          case _ =>
            val t0 = if (timeBudget.nan) -1L else System.nanoTime
            val result = k.act(provisions)
            record.ran(k)(result).mapNo(e => (e, queue)).?
            actMore(record, k)(result).mapNo(e => (e, queue)).?.foreach(queue += _)
            if (t0 >= 0) {
              val dt = 1e-9*(System.nanoTime - t0)

            }
        }
      }
      finalizeRecord(record, queue)
    }
  }
  object Sequentially {
    class Impl[Z >: Null <: AnyRef]() extends Gathered[Z] {}
  }

  trait Concurrently[Z >: Null <: AnyRef] extends Acts[Z] {

  }
}
*/


/*
abstract class Acts[A, K <: Act[A], N, P <: Acts.Pick[A, K, N], X, R <: Acts.Record[A, N, X]](val zero: R)
extends Act[X] {

}
object Acts {
  trait Pick[A, K <: Act[A], N] {
    def check(k: K): Option[Ok[String, N]]
  }

  trait Record[A, N] {
    type X
    def initialize(): this.type
    def incorporate(status: Ok[Ok[String, N], Ok[String, A]]): Ok[String, this.type]
    def finalize(): X
  }
}
*/

/*
abstract class Acts[A, X <: Act[A], N <: Acts.Not[X], B, R <: Acts.Report[A, X, B]] extends Act.On[Acts.AsSeq[X], R] {
  type State <: Acts.Stately[A, X]
  protected def initialize(): Ok[String, State]
  protected def select(state: State, act: X): Option[N]
  protected def elaborate(state: State, result: Ok[String, A]): Ok[String, Seq[X]]
  protected def accumulate(state: State, result: Ok[String, A]): Ok[String, State]
  protected def conclude(state: State): Ok[String, R]
}
object Acts {
  trait AsSeq[X] {
    def asSeq: Seq[X]
  }
  trait Stately[A, X <: Act[A]] {
    def prepare: Unit
    def add(value: )
    def results: Vector[Ok[(X, N), Ok[String, A]]]
  }
  class Report[A, X <: Act[A], B](
    val results: Vector[Ok[(X, N), Ok[String, A]]],
    val summary: B
  ) {
    lazy val (successes, failures, skips): (TreeMap[Int, A], TreeMap[Int, String], TreeMap[Int, X]) = {
      var succb = TreeMap.empty[Int, A]
      var failb = TreeMap.empty[Int, String]
      var skipb = TreeMap.empty[Int, X]
      var i = 0
      var it = results.iterator
      while (it.hasNext) {
        it.next match {
          case No(x) => skipb += ((i, x))
          case Yes(y) => y match {
            case No(e) => failb += ((i, e))
            case Yes(a) => succb += ((i, a))
          }
        }
        i += 1
      }
      (succb, failb, skipb)
    }
  }
}
*/

/*
abstract class Acts[B, R <: Act.Recursive[Unit, B, _] with Act.Solo[B], S, Z](sequencer: Acts.AsSeq[S, R]) extends Act[S, Z] {
  type Extra
  type State
  type Info

  protected def preamble(state: State): Ok[String, Info]
  protected def interpret(result: Ok[String, B]): Ok[String, Info]
  protected def conclusion(state: State): Ok[String, Info]

  protected def initialize(extra: Extra): State
  protected def accumulate(state: State, info: Info): Ok[String, State]
  protected def synthesize(state: State): Ok[String, Z]

  protected def actOn(seq: Seq[R], extra: Extra, res: Act.Resource[Unit]): Ok[String, Z] = {
    val e = new Engine(seq, res.cores, res.completeBy, initialize(extra).?)
    e.state = accumulate(e.state, preamble.?).?
    e.await.?
    e.state = accumulate(e.state, conclusion.?)
    synthesize(e.state).?
  }
}
object Acts {
  trait AsSeq[S, R <: Act.Solo[_]] {
    def apply(s: S): Seq[R]
  }
  object AsSeq {
    def of[R <: Act.Once[_]](rs: Seq[R]): AsSeq[Unit, R] = new AsSeq[Unit, R] { def apply(u: Unit) = rs }
  }

  abstract class Once[B, R <: Act.Recursive[Unit, B, _] with Act.Once[B], Z](sequence: Seq[R])
  extends Acts[B, R, Unit, Z](AsSeq of sequence) with Act.Once[Z] {}
}
*/
