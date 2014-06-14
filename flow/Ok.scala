// This file is distributed under the BSD 3-clause license.  See file LICENSE.

package kse.flow

import scala.util.control.{NonFatal, ControlThrowable}

/** An `Ok` holds either of two possibilities, either the favored one, `Yes`,
  * or an alternative, `No`.  The use cases are much like those of
  * `scala.util.Either` with a few important differences: since the favored
  * (second) branch is the default, you can use for comprehensions; a richer
  * set of methods are provided; and each of the two branches does not remember
  * what the other alternative might have been, simplifying the code. 
  */
sealed trait Ok[+N, +Y] {
  
  /** True if this holds a `Y` which can be retrieved with `yes` */
  def isOk: Boolean
  
  /** Retrieves a stored `Y` value if available
    * @throws NoSuchElementException if this is an instance of `No`
    */
  def yes: Y
  
  /** Retrieves a stored `N` value if available
    * @throws NoSuchElementException if this is an instance of `Yes`
    */
  def no: N
  
  /** Provides a method to bail out if this is not the favored type. */
  def valid[M >: N](implicit validator: Ok.ValidateOkay[M]): this.type
  
  
  /** Produces a result by applying `f` to the disfavored value or `g`
    * to the favored one, depending on which is available.
    */
  def fold[A](f: N => A)(g: Y => A): A
  
  /** Returns whichever value is stored by finding a common supertype. */
  def merge[A](implicit evn: N <:< A, evy: Y <:< A): A
  
  /** Retrieves a stored `Y` value or produces one from a `N` using `f`. */
  def yesOr[Z >: Y](f: N => Z): Z
  
  /** Retrieves a stored `N` value or produces one from a `Y` using `f`. */
  def noOr[M >: N](f: Y => M): M
  
  
  /** Maps a favored value from `Y` to `Z` using `f`; does nothing to a disfavored value. */
  def map[Z](f: Y => Z): Ok[N, Z]
  
  /** Maps a disfavored value from `Y` to `Z` using `f`; does nothing to a favored value. */
  def mapNo[M](f: N => M): Ok[M, Y]
  
  /** Turn a favored value into a `Z` or into a disfavored `M` via `f`; leaves a disfavored value alone. */
  def flatMap[M >: N, Z](f: Y => Ok[M, Z]): Ok[M, Z]

  /** Turn a disfavored value into a `M` or into a favored `Z` via `f`; leaves a favored value alone. */
  def flatMapNo[M, Z >: Y](f: N => Ok[M, Z]): Ok[M, Z]


  /** Selects only favored values selected by `p`; an implicit conversion 
    * must be available to produce a disfavored value from a favored value
    * not selected by `p`.
    */
  def filter[M >: N](p: Y => Boolean)(implicit noify: Ok.Defaulter[M,Y]): Ok[M, Y]
  
  /** Selects only favored values selected by `p`; an implicit conversion 
    * must be available to produce a disfavored value from a favored value
    * not selected by `p`.  This simply defers to `filter`.
    */
  def withFilter[M >: N](p: Y => Boolean)(implicit noify: Ok.Defaulter[M,Y]): Ok[M, Y] = filter[M](p)(noify)
  
  /** With nested `Ok`s of the form `Ok[A,Ok[B,Z]]`, un-nest the possible 
    * disfavored values into the common supertype `M` of `A` and `B`.
    */
  def flatten[M >: N, Z](implicit ev: Y <:< Ok[M,Z]): Ok[M, Z]
  
  /** With nested `Ok`s of the form `Ok[Ok[N,A],B]`, un-nest the possible 
    * favored values into the common supertype `Z` of `A` and `B`.
    */
  def flattenNo[M, Z >: Y](implicit ev: N <:< Ok[M,Z]): Ok[M, Z]
  
  
  /** Apply an operation only to a favored value. */
  def foreach[A](f: Y => A): Unit
  
  /** Apply an operation only to a disfavored value. */
  def foreachNo[A](f: N => A): Unit
  
  
  /** Apply an operation only to a favored value and return the same `Ok`. */
  def tap[A](f: Y => A): this.type
  
  /** Apply an operation only to a disfavored value and return the same `Ok`. */
  def tapNo[A](f: N => A): this.type
  
  
  /** True only if a value is favored and passes test `p`. */
  def exists(p: Y => Boolean): Boolean
  
  /** True if a value is favored and passes `p`, or if the value is disfavored. */
  def forall(p: Y => Boolean): Boolean
  
  
  /** Converts any favored value covered by `pf` into a disfavored value. */
  def reject[M >: N](pf: PartialFunction[Y, M]): Ok[M, Y]
  
  /** Converts any disfavored value covered by `pf` into a favored value. */
  def accept[Z >: Y](pf: PartialFunction[N, Z]): Ok[N, Z]
  
  
  /** Converts to a `scala.util.Either`. */
  def toEither: Either[N, Y] = this match {
    case Yes(y) => Right(y)
    case No(n) => Left(n)
  }
  
  /** Converts to an `Option` by dropping any disfavored value. */
  def toOption: Option[Y] = this match {
    case Yes(y) => Some(y)
    case _ => None
  }
  
  /** Converts to a `scala.util.Try` by wrapping a disfavored value in a [[NotOkException]]. */
  def toTry: scala.util.Try[Y] = this match {
    case Yes(y) => scala.util.Success(y)
    case No(n) => scala.util.Failure(new NotOkException(no))
  }
  
  /** Switches which alternative is favored and which is not. */
  def swap: Ok[Y,N] = this match {
    case Yes(y) => No(y)
    case No(n) => Yes(n)
  }
}


/** The favored alternative from among the two possible in an [[Ok]]. */
final case class Yes[+Y](yes: Y) extends Ok[Nothing, Y] {
  def isOk = true
  def no = throw new NoSuchElementException("Attempt to retrieve No case when Yes")
  def valid[M](implicit validator: Ok.ValidateOkay[M]): this.type = this

  def fold[A](f: Nothing => A)(g: Y => A) = g(yes)
  def merge[A](implicit evn: Nothing <:< A, evy: Y <:< A): A = evy(yes)
  
  def yesOr[Z >: Y](f: Nothing => Z) = yes
  def noOr[M](f: Y => M) = f(yes)
  
  def map[Z](f: Y => Z): Ok[Nothing, Z] = Yes(f(yes))
  def mapNo[M](f: Nothing => M): Ok[M, Y] = this
  
  def flatMap[M, Z](f: Y => Ok[M, Z]): Ok[M, Z] = f(yes)
  def flatMapNo[M, Z >: Y](f: Nothing => Ok[M, Z]): Ok[M, Z] = this
  
  def filter[M](p: Y => Boolean)(implicit noify: Ok.Defaulter[M,Y]): Ok[M, Y] = if (p(yes)) this else noify(yes)
  
  def flatten[M, Z](implicit ev: Y <:< Ok[M,Z]): Ok[M, Z] = ev(yes)
  def flattenNo[M, Z >: Y](implicit ev: Nothing <:< Ok[M,Z]): Ok[M, Z] = this
  
  def foreach[A](f: Y => A): Unit = { f(yes); () }
  def foreachNo[A](f: Nothing => A): Unit = {}
  
  def tap[A](f: Y => A): this.type = { f(yes); this }
  def tapNo[A](f: Nothing => A): this.type = this
  
  def exists(f: Y => Boolean) = f(yes)
  def forall(f: Y => Boolean) = f(yes)
  
  def reject[M](pf: PartialFunction[Y, M]): Ok[M, Y] = if (pf.isDefinedAt(yes)) No(pf(yes)) else this
  def accept[Z >: Y](pf: PartialFunction[Nothing, Z]): Ok[Nothing, Z] = this
  
  /** Specifies what the other (disfavored) alternative should be. */
  def alt[N]: Ok[N,Y] = this
}


/** The disfavored alternative from among the two possible in an [[Ok]]. */
final case class No[+N](no: N) extends Ok[N, Nothing] {
  def isOk = false
  def yes = throw new NoSuchElementException("Attempt to retrieve Yes case when No")
  def valid[M >: N](implicit validator: Ok.ValidateOkay[M]): this.type = { validator.incorrect(no); this }
  
  def fold[A](f: N => A)(g: Nothing => A) = f(no)
  def merge[A](implicit evn: N <:< A, evy: Nothing <:< A): A = evn(no)
  
  def yesOr[Z](f: N => Z) = f(no)
  def noOr[M >: N](f: Nothing => M) = no
  
  def map[Z](f: Nothing => Z): Ok[N, Z] = this
  def mapNo[M](f: N => M): Ok[M, Nothing] = No(f(no))
  
  def flatMap[M >: N, Z](f: Nothing => Ok[M, Z]): Ok[M, Z] = this
  def flatMapNo[M, Z](f: N => Ok[M, Z]): Ok[M, Z] = f(no)
  
  def filter[M >: N](p: Nothing => Boolean)(implicit noify: Ok.Defaulter[M,Nothing]) = this
  
  def flatten[M >: N, Z](implicit ev: Nothing <:< Ok[M,Z]): Ok[M, Z] = this
  def flattenNo[M, Z](implicit ev: N <:< Ok[M,Z]): Ok[M, Z] = ev(no)
  
  def foreach[A](f: Nothing => A): Unit = {}
  def foreachNo[A](f: N => A): Unit = { f(no); () }
  
  def tap[A](f: Nothing => A): this.type = this
  def tapNo[A](f: N => A): this.type = { f(no); this }
  
  def exists(f: Nothing => Boolean) = false
  def forall(f: Nothing => Boolean) = true
  
  def reject[M >: N](pf: PartialFunction[Nothing, M]): Ok[M, Nothing] = this
  def accept[Z](pf: PartialFunction[N, Z]): Ok[N, Z] = if (pf.isDefinedAt(no)) Yes(pf(no)) else this.asInstanceOf[Ok[N, Z]]
  
  /** Specifies what the other (favored) alternative should be. */
  def alt[Y]: Ok[N,Y] = this
}

/** Used to convert a disfavored alternative into an `Exception` so that [[Ok]] can be mapped into `scala.util.Try`. */
class NotOkException[N](val no: N) extends Exception {
  override def toString = "ichi.core.NotOkException("+no.toString+")"
}


object Ok {
  /** The canonical disfavored unit value. */
  val UnitNo = No(())
  
  /** The canonical favored unit value */
  val UnitYes = Yes(())
  
  
  /** Explicit handler for noticing disfavored values. */
  trait ValidateOkay[N] {
    /** Called if a disfavored branch is found during validation.
      * May throw an exception but is not required to.
      */
    def incorrect(n: N): Unit
  }
  
  
  /** Implicit handler for rejecting favored values. */
  trait Defaulter[N,-Y] { def apply(yes: Y): No[N] }
  
  private val DefaultUnitToUnit = new Defaulter[Unit, Any] { def apply(yes: Any) = UnitNo }
  /** Enables returning a `Unit` on the [[No]] branch when filtering. */
  implicit def defaultToUnit[Y]: Defaulter[Unit, Y] = DefaultUnitToUnit
  
  
  /** Converts an `Option` to a disfavored value.  `None` maps to a content-free (`Unit`) favored value. */
  def ifNot[N](o: Option[N]) = o match {
    case Some(n) => No(n)
    case None => UnitYes
  }
  
  /** Converts an `Option` to a favored value.  `None` maps to a content-free (`Unit`) disfavored value. */
  def from[Y](o: Option[Y]) = o match {
    case Some(y) => Yes(y)
    case None => UnitNo
  }
  
  /** Converts an `Either` to an [[Ok]], favoring the `Right` alternative. */
  def from[N,Y](e: Either[N,Y]) = e match {
    case Left(n) => No(n)
    case Right(y) => Yes(y)
  }
  
  /** Converts a `Try` to an [[Ok]]; the disfavored alternative is a `Throwable`. */
  def from[Y](t: scala.util.Try[Y]) = t match {
    case scala.util.Success(y) => Yes(y)
    case scala.util.Failure(t) => No(t)
  }
  
  /** Given a bunch of [[Ok]]s, return either all the `No` values if there are
    * any (disfavored result); otherwise return all the `Yes` values (favored result).
    */
  def gather[N,Y](oks: Ok[N,Y]*) = {
    val nos = oks.collect{ case No(n) => n }
    if (nos.size > 0) No(nos) else Yes(oks.map(_.yes))
  }
}
