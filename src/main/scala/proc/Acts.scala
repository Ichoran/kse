// This file is distributed under the BSD 3-clause license
// Copyright 2019, 2020 Rex Kerr and Calico Life Sciences

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

/** Acts handles running multiple instances of Act, either in serial or in parallel,
  * optionally gathering timing information in order to stop in time.
  */
abstract class Acts[KO, K <: Act[KO], R] private[proc] (input: Iterator[K], val name: String)
extends Act[R] {
  type S

  private val queue = {
    val ans = new Queue[K]()
    while (input.hasNext) ans += input.next
    ans
  }
  private val outputs = new Queue[Acts.Result[KO, K]]()
  private var count = 0
  private var interesting = 0
  private var killer: Option[E] = None

  protected def initialState(): S
  protected def finalState(s: S, killed: Boolean): S
  private[this] var myState = initialState()
  protected final def mapState(f: S => S) = synchronized { myState = f(myState) }

  protected def fatality(act: K)(error: act.E): Option[E]
  protected def process(act: K)(result: Acts.Result[KO, K])(state: S): S
  protected def expand(act: K)(result: KO)(state: S): Seq[K]
  protected def timedOut(act: K)(error: act.E): Boolean

  final protected def actImpl(provisions: Act.Provision): Ok[E, R] = {
    if (provisions.threads > 1) actImplParallel(provisions)
    else actImplSequential(provisions)
    synchronized { killer match {
      case Some(e) =>
        queue.clear()
        outputs.clear()
        killer = None
        myState = finalState(myState, killed = true)
        No(e)
      case _ =>
        val tb = Vector.newBuilder[Acts.Result.Bad[KO, K]]
        val fb = Vector.newBuilder[Acts.Result.Bad[KO, K]]
        val sb = Vector.newBuilder[Acts.Result.Good[KO, K]]
        while (outputs.nonEmpty) outputs.dequeue match {
          case g: Acts.Result.Good[_, _] =>  sb += g
          case b: Acts.Result.Bad[_, _] =>
            if (timedOut(b.act)(b.err)) tb += b
            else fb += b
        }
        val qb = Vector.newBuilder[K]
        while (queue.nonEmpty) qb += queue.dequeue
        val results = Acts.Results(qb.result, tb.result, fb.result, sb.result)
        val ans = complete(results, myState)
        myState = finalState(myState, killed = false)
        ans
    }}
  }

  protected def actImplParallel(provisions: Act.Provision): Unit = {
    val p = provisions.copy(threads = 1)
    synchronized{ interesting = provisions.threads }
    val workers = Array.fill(provisions.threads)(new Thread {
      override def run(): Unit = {
        actImplReentrant(p)
      }
    });
    { var i = 0; while (i < workers.length) { workers(i).start(); i += 1 } }
    { var i = 0; while (i < workers.length) { workers(i).join(); i += 1 } }
  }

  protected def actImplSequential(provisions: Act.Provision): Unit = {
    interesting = 1
    actImplReentrant(provisions)
  }

  protected def actImplReentrant(provisions: Act.Provision): Unit = {
    var more = true
    var lively = true
    try {
      while (more && synchronized { killer.isEmpty } && !provisions.expired()) {
        synchronized { queue.dequeueFirst(_ => true) } match {
          case Some(k) =>
            val index = synchronized {
              if (!lively) {
                lively = true
                interesting += 1
              }
              val x = count
              count += 1
              x
            }
            val result = safe{ k.act(provisions) }
            synchronized{ result match {
              case No(e) => if (killer.isEmpty) killer = Some(handler(e))
              case Yes(y) => y match {
                case Yes(ko) =>
                  val result = Acts.Result.Good(k, index, ko)
                  outputs += result
                  myState = process(k)(result)(myState)
                  queue ++= expand(k)(ko)(myState)
                case No(ke) => fatality(k)(ke) match {
                  case Some(e) => if (killer.isEmpty) killer = Some(e)
                  case None =>
                    val result = Acts.Result.Bad[KO, K](k, index)(ke)
                    outputs += result
                    myState = process(k)(result)(myState)
                }
              }
            }}
          case _ =>
            synchronized {
              if (lively) {
                lively = false
                interesting -= 1
              }
              else if (interesting <= 0) more = false
              else Thread.`yield`()
            }
        }
      }
    }
    finally {
      if (lively) synchronized { interesting -= 1 }
    }
  }

  protected def complete(results: Acts.Results[KO, K], state: S): Ok[E, R]
}
object Acts {
  final case class Results[KO, K <: Act[KO]](
    pending: Vector[K],
    timeouts: Vector[Result.Bad[KO, K]],
    failures: Vector[Result.Bad[KO, K]],
    successes: Vector[Result.Good[KO, K]]
  ) {
    lazy val ordered: Vector[Result[KO, K]] = (timeouts ++ failures ++ successes).sortBy(_.index)
  }

  sealed abstract class Result[KO, K <: Act[KO]](val act: K, val index: Long) {
    def isGood: Boolean
    def isBad: Boolean
    def good: Option[KO]
    def bad: Option[act.E]
  }
  object Result {
    final class Good[KO, K <: Act[KO]] private (k: K, i: Long)(val out: KO) extends Result[KO, K](k, i) {
      def isGood = true
      def isBad = false
      def good = Some(out)
      def bad = None
    }
    object Good {
      def apply[KO, K <: Act[KO]](act: K, i: Long, out: KO) = new Good[KO, K](act, i)(out)
    }
    final class Bad[KO, K <: Act[KO]] private (k: K, i: Long) extends Result[KO, K](k, i) {
      private var myErr: act.E = _
      def err: act.E = myErr
      def isGood = false
      def isBad = true
      def good = None
      def bad = Some(err)
    }
    object Bad {
      def apply[KO, K <: Act[KO]](act: K, i: Long)(err: act.E) = {
        val ans = new Bad[KO, K](act, i)
        ans.myErr = err.asInstanceOf[ans.act.E]  // Compiler can't tell that act is exactly ans.act
        ans
      }
    }
  }
}
