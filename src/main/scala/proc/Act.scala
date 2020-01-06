// This file is distributed under the BSD 3-clause license
// Copyright 2019 Rex Kerr and Calico Life Sciences

package kse.proc

import java.time.{Duration, Instant}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Queue
import scala.util.control.NonFatal

import kse.flow._
import kse.maths._
import kse.maths.stats._
import kse.maths.fits._


/** An Act is a computation that can run once and is safe for concurrent access.
  * It is assumed to work via side-effects, so it doesn't return any value;
  * it just reports on its state.
  */
abstract class Act {
  type E

  def name: String
  def cost: Double

  protected val myStatus = new AtomicReference[Ok[E, Act.Outcome]](Act.Outcome.Before.yes)
  def status: Act.Outcome = myStatus.get() match {
    case Yes(y) => y
    case No(e) => Act.Outcome.Failed
  }

  protected def handler(t: Throwable): Option[E]
  protected def excuse(message: String): E

  /** Call all `Impl` methods, including this one, ONLY while you own the `Act.Outcome.Running` flag! */
  protected def actImpl(provisions: Act.Provision): Ok[E, Boolean]

  def act(provisions: Act.Provision): Ok[E, Act.Outcome] = {
    var result: Ok[E, Act.Outcome] = null
    while (result eq null) {
      myStatus.get() match {
        case Yes(Act.Outcome.Before) =>
          if (myStatus.compareAndSet(Act.Outcome.Before.yes, Act.Outcome.Running.yes)) {
            result =
              try { 
                actImpl(provisions).map{ b => 
                  if (b) Act.Outcome.Succeeded
                  else   Act.Outcome.Skipped
                }
              }
              catch {
                case e if NonFatal(e) => handler(e) match {
                  case Some(x) => No(x)
                  case None => throw e
                }
              }
            myStatus.set(result)
          }
        case Yes(Act.Outcome.Running) =>
          Thread.`yield`
        case x =>
          result = x
      }
    }
    result
  }
  final def act(): Ok[E, Act.Outcome] = act(Act.Provision.empty)

  override def toString =
    if (name.nonEmpty) name + status.actText
    else getClass.getName + "@" + System.identityHashCode(this).toHexString + status.actText
}
object Act {
  sealed trait Outcome { def actText: String; lazy val yes = Yes(this) }
  object Outcome {
    case object  Before    extends Outcome  { def actText = "";             override def toString = "has not acted" }
    sealed trait Complete  extends Outcome  {}
    case object  Succeeded extends Complete { def actText = " (succeeded)"; override def toString = "act succeeded" }
    case object  Failed    extends Complete { def actText = " (failed)";    override def toString = "act failed" }
    case object  Running   extends Outcome  { def actText = " (running)";   override def toString = "acting" }
    case object  Skipped   extends Outcome  { def actText = " (skipped)";   override def toString = "skipped" }
  }

  case class Provision(threads: Int, completeBy: Option[java.time.Instant]) {}
  object Provision {
    val empty = new Provision(1, None)
    def apply(i: Int) = new Provision(i max 1, None)
    def apply(t: java.time.Instant) = new Provision(1, Some(t))
  }


  trait Stringly extends Act {
    type E = String
    final protected def excuse(message: String) = message
    final protected def handler(t: Throwable): Option[String] = Some(t.explain())
  }


  final class Like private[proc] (computation: => Unit, val name: String = "", val cost: Double = Double.NaN)
  extends Act with Stringly {
    protected def actImpl(provisions: Act.Provision): Ok[E, Boolean] =
      if (provisions.completeBy.exists(t => t isBefore Instant.now)) Yes(false)
      else {
        computation
        Yes(true)
      }
  }

  def like(computation: => Unit)                             = new Like(computation)
  def like(computation: => Unit, name: String)               = new Like(computation, name)
  def like(computation: => Unit, cost: Double)               = new Like(computation, cost = cost)
  def like(computation: => Unit, name: String, cost: Double) = new Like(computation, name, cost)


  final class TemporaryInputSupplier[I](private val input: () => I) extends AnyVal {
    def map[O](fn: I => O):                                       Arrow[I, O] with Stringly = new Arrow.Map(input, fn, "",   Arrow.nanEstimator)
    def map[O](fn: I => O, name: String):                         Arrow[I, O] with Stringly = new Arrow.Map(input, fn, name, Arrow.nanEstimator)
    def map[O](fn: I => O,               estimator: I => Double): Arrow[I, O] with Stringly = new Arrow.Map(input, fn, "",   estimator)
    def map[O](fn: I => O, name: String, estimator: I => Double): Arrow[I, O] with Stringly = new Arrow.Map(input, fn, name, estimator)
    def flatMap[O](fn: I => Ok[String, O]):                                       Arrow[I, O] with Stringly = new Arrow.FlatMap(input, fn, "",   Arrow.nanEstimator)
    def flatMap[O](fn: I => Ok[String, O], name: String):                         Arrow[I, O] with Stringly = new Arrow.FlatMap(input, fn, name, Arrow.nanEstimator)
    def flatMap[O](fn: I => Ok[String, O],               estimator: I => Double): Arrow[I, O] with Stringly = new Arrow.FlatMap(input, fn, "",   estimator)
    def flatMap[O](fn: I => Ok[String, O], name: String, estimator: I => Double): Arrow[I, O] with Stringly = new Arrow.FlatMap(input, fn, name, estimator)
  }

  def from[I](input: => I) = new TemporaryInputSupplier(() => input)
}


/** An Arrow is an Act that starts with a predefined input and produces,
  * after acting, an output.  It also can optionally give a numeric score
  * that represents an estimate of the amount of work it is to create an
  * output.  Ideally, this number would be linearly related to the
  * execution time (possibly with a constant offset).
  */
abstract class Arrow[I, O](input: () => I, estimator: I => Double = (i: I) => Double.NaN, val name: String = "")
extends Act {
  protected val myInput = new AtomicReference[Ok[() => I, I]](No(input))
  protected val myOutput = new AtomicReference[Ok[E, O]](null)
  protected val myCost = new AtomicLong(Arrow.UninitializedNaNBits)

  /** Call only when you hold both the `Act.Outcome.Running` flag AND the `myInput == null` flag */
  private[this] def ensureCostIsSetImpl(i: I) {
    var retries = 3
    while (retries > 0) {
      retries -= 1
      if (myCost.compareAndSet(Arrow.UninitializedNaNBits, Arrow.RunningNaNBits)) {
        myCost.set( 
          try{ val c = estimator(i); if (c.isNaN) Arrow.DoubleNaNBits else c.bits }
          catch { case e if NonFatal(e) => Arrow.DoubleNaNBits }
        )
        retries = 0
      }
      else myCost.get() match {
        case Arrow.RunningNaNBits => Thread.`yield`
        case Arrow.UninitializedNaNBits =>
        case _ => retries = 0
      }
    }
  }

  final def cost: Double = {
    var retries = 10
    while (retries > 0) {
      retries -= 1
      myCost.get() match {
        case Arrow.UninitializedNaNBits =>
          if (myStatus.compareAndSet(Act.Outcome.Before.yes, Act.Outcome.Running.yes)) {
            val x = myInput.get()
            if (x != null) {
              if (myInput.compareAndSet(x, null)) {
                val i = x match {
                  case Yes(y) => y
                  case No(fn) =>
                    try { fn() }
                    catch {
                      case e if NonFatal(e) =>
                        myInput.set(No(fn))
                        myStatus.set(Act.Outcome.Before.yes)
                        return Double.NaN
                    }
                }
                ensureCostIsSetImpl(i)
                myInput.set(Yes(i))
              }
            }
            myStatus.set(Act.Outcome.Before.yes)
          }
        case Arrow.RunningNaNBits => Thread.`yield`
        case x => return x.binary64
      }
    }
    Double.NaN
  }

  final def output: Ok[Option[E], O] = myOutput.get() match {
    case null => No(None)
    case y: Yes[O] => y
    case No(e) => No(Some(e))
  }

  /** Assume when implementing this that we're holding the `Act.Outcome.Running` flag */
  protected def actArrowImpl(in: I, provisions: Act.Provision): Ok[E, Option[O]]

  /** Only call this when guarded by holding the `Act.Outcome.Running` flag (i.e. you CASed it into place) */
  protected final def actImpl(provisions: Act.Provision): Ok[E, Boolean] = {
    var retries = 10
    while (retries > 0) {
      retries -= 1
      myInput.get() match {
        case null => // Bad implementation or we computed the cost concurrently and memory consistency hasn't caught up yet
          Thread.`yield`
        case x =>
          if (myInput.compareAndSet(x, null)) {
            val i = x match {
              case Yes(y) => y
              case No(fn) =>
                try { fn() }
                catch {
                  case t if NonFatal(t) =>
                    myInput.set(No(fn))
                    myCost.compareAndSet(Arrow.UninitializedNaNBits, Arrow.DoubleNaNBits)
                    val e = No(handler(t).getOrElse(excuse(s"Exception before $name could act:\n${t.explain()}")))
                    myOutput.set(e)
                    return e
                }
            }
            actArrowImpl(i, provisions) match {
              case n: No[E] =>
                myOutput.set(n)
                ensureCostIsSetImpl(i)
                return n
              case Yes(Some(o)) =>
                myOutput.set(Yes(o))
                ensureCostIsSetImpl(i)
                return Yes(true)
              case Yes(None) => myInput.set(Yes(i)); return Yes(false)
            }
          }
          else Thread.`yield`
      }
    }
    No(excuse(s"input not available to $this"))
  }
}
object Arrow {
  val DoubleNaNBits = Double.NaN.bits
  val UninitializedNaNBits = Double.NaN.bits ^ 0x3
  val RunningNaNBits = Double.NaN.bits ^ 0x5
  val nanEstimator = (a: Any) => Double.NaN

  private[proc] class Map[I, O](input: () => I, op: I => O, name: String, estimator: I => Double)
  extends Arrow[I, O](input, estimator, name) with Act.Stringly {
    protected def actArrowImpl(in: I, provisions: Act.Provision): Ok[E, Option[O]] =
      safe{ Option(op(in)) }.mapNo(e => excuse(e.explain()))
  }

  private[proc] class FlatMap[I, O](input: () => I, op: I => Ok[String, O], name: String, estimator: I => Double)
  extends Arrow[I, O](input, estimator, name) with Act.Stringly {
    protected def actArrowImpl(in: I, provisions: Act.Provision): Ok[E, Option[O]] =
      try{ op(in).map(o => Option(o)) } catch { case e if NonFatal(e) => No(excuse(e.explain())) }
  }
  def apply[I, O](input: => I, op: I => O, name: String = "", estimator: I => Double = (i: I) => Double.NaN): Arrow[I, O] with Act.Stringly =
    new Map[I, O](() => input, op, name, estimator)
}


/** Quiver is an Act (actually an Arrow) that runs a bunch of Arrows.  (Yes, it's a play on words.)
  *
  * Be very careful while interacting with the concurrency mechanisms.
  * It's safest to use the builder methods to create one of the two
  * primary subclasses, `Quiver.Sequential` and `Quiver.Concurrent`.
  */
abstract class Quiver[KI, KO, K <: Arrow[KI, KO], Z <: Quiver.Summary](initial: () => Seq[K], name: String = "")
extends Arrow[Seq[K], Z](initial, _ => Double.NaN, name) {
  type KE
  type KR <: Quiver.Result[KI, KO, K, KE]

  private[proc] val progress = new Quiver.Progress[KO, KI, K, KE, KR, E]

  private[proc] def reload(act: K)(result: KO): Seq[K]
  private[proc] def fatality(act: K)(e: act.E): Option[E]

  protected def getExecutors(): Seq[Quiver.Executor[KI, KO, K, KE]]
  protected def summaryImpl(summary: (Option[E], Vector[KR], Vector[K])): Ok[E, Z]

  protected def actOnQueueImpl(provisions: Act.Provision): Ok[E, (Option[E], Vector[KR], Vector[K])] = {
    val execs = getExecutors()
    if (execs.isEmpty) No(excuse(s"$name could not create any executors"))
    else {
      execs.foreach(_.begin(execs.length))
      execs.foreach(_.await)
      var e: Option[E] = None
      val osb = Vector.newBuilder[KR]
      val isb = Vector.newBuilder[K]
      progress.synchronized {
        e = progress.dead
        progress.dead = None
        progress.bored = 0
        while (progress.outputs.nonEmpty) osb += progress.outputs.dequeue
        while (progress.queue.nonEmpty) isb += progress.queue.dequeue
      }
      Yes((e, osb.result, isb.result))
    }
  }

  final protected def actArrowImpl(in: Seq[K], provisions: Act.Provision): Ok[E, Option[Z]] = {
    val i = in.iterator
    while (i.hasNext) progress.queue add i.next
    actOnQueueImpl(provisions).flatMap(summaryImpl).map(z => Some(z))
  }
}
object Quiver {
  // Use ONLY when synchronized!
  final protected[proc] class Progress[KI, KO, K <: Arrow[KI, KO], KE, KR <: Result[KI, KO, K, KE], E]() {
    var bored = 0
    var dead: Option[E] = None
    val queue = new Queue[K]()
    val outputs = new Queue[KR]()
    var provisions: Option[Provisions] = None
  }

  trait Summary {}
  trait Result[KI, KO, K <: Arrow[KI, KO], KE] {}
  trait Executor[KI, KO, K <: Arrow[KI, KO], KE] {
    val quiver: Quiver[KI, KO, K, _]
    def begin(allBored: Int): Unit
    def await(): Unit
  }
  object Executor {
    final class Sequential[KI, KO, K <: Arrow[KI, KO], KE](val quiver: Quiver[KI, KO, K, _])
    extends Executor[KI, KO, K, KE] {
      def begin(allBored: Int): Unit = quiver.progress.synchronized {
        val p = quiver.progress
        while (p.queue.nonEmpty && p.dead.isEmpty) {
          val k = p.queue.dequeue
          val status = quiver.provisions match { case Some(prov) => k.act(prov); case _ => k.act() }
          status match {
            case Yes(state) =>
            case No(error)  => quiver.fatality(k)(error) match {
              case None
            }
          }
          // TODO--make this actually run the arrow and process the output
        }
      }
      def await() : Unit = ()   // Already ran everything when we began because we're sequential
    }
  }
}


/*

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
    sealed abstract class From[I >: Null <: AnyRef, F, A](f: I => Ok[F, A], err: String => F, val identifier: Option[String] = None, val quantifier: I => Double = _ => Double.NaN)
    extends Once[A] {
      type E = F
      override def error(message: String) = err(message)
      override def once(): this.type = this
    }
    object From {
      sealed private[proc] class Impl[I >: Null <: AnyRef, F, A](f: I => Ok[F, A], err: String => F, identifier: Option[String], quantifier: I => Double)
      extends From[I, F, A](f, err, identifier) with On[I, A] {
        override def quantify(): Double = synchronizer.synchronized{
          if (supplyCache == null) Double.NaN)
          else 
        }
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


  class Predictor[A, K <: Act[A]] {
    protected val quantityModel = EstXM()
    protected val constantModel = EstM.empty
    protected val nanModel = EstM.empty
    protected val affineModel = new FitTX()

    def apply(quantity: Double): Double = {
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
    }
    def apply(act: K): Double = apply(act.quantify())

    def learn(quantity: Double, observed: Double): Unit = {
      if (dt.finite) {
        if (quantity.finite) {
          quantityModel += quantity
          constantModel += dt
          affineModel += (quantity, dt)
        }
        else nanModel += quantity
      }
    }
  }
}


class [A, K <: Act[A]] {}



class Acts[A, K <: Act[A], N, Z]

trait Acts[Z >: Null <: AnyRef] extends Act[Z] {

  protected val results = new ConcurrentLinkedQueue[Acts.Result[A, K, N]]()

  protected def summarize(): Z
}
object Acts {
  sealed trait Result[A, K <: Act[A], +N] {}
  final class Worked[A, K <: Act[A]](val act: K)(val result: A) extends Result[A, K, Nothing] {}
  final class Failed[A, K <: Act[A]](val act: K)(val error: act.E) extends Result[A, K, Nothing] {}
  final class Skipped[A, K <: Act[A], +N](val act: K)(val why: N) extends Result[A, K, N] {}


  class Record[A, K <: Act[A], X](zero: X) {
    protected val x = new AtomicReference(zero)
  }

  abstract class Abstract[A, K <: Act[A], Z >: Null <: AnyRef](initial: Iterator[K])
  extends Acts[A, K, Z] with Act.On[Iterator[K], Z] {

  }
}
*/


/*
trait Acts[Z >: Null <: AnyRef] extends Act[Z] {
  type A
  type K <: Act[A]
  type N
  type R <: Acts.Record[A, K, N, E]

  protected def initialRecord(): R
  protected def finalizeRecord(record: R, queue: Seq[K]): Ok[(E, Seq[K]), Z]

  protected def initialActs: Ok[E, Iterator[K]]
  protected def checkAct(k: K, timeBudget: Double = Double.NaN): Ok[N, Double]
  protected def actMore(record: R, k: K)(result: Ok[k.E, A]): Ok[E, Iterator[K]]

  protected def actImpl(provisions: Act.Provision): Ok[(E, Seq[K]), Z]
}
object Acts {
  trait Once[Z >: Null <: AnyRef] extends Acts[Z] with Act.Once[Z] {
    def actUnsync(provisions: Act.Provision): Ok[(E, Seq[K]), Z] = actImpl(provisions)
  }

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
        val dt = predictTime(k.quantity()) max 0
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
