package kse.flow

import scala.util.control.{NonFatal, ControlThrowable}

sealed trait Ok[+N, +Y] {
  def isOk: Boolean
  def yes: Y
  def no: N
  def valid[M >: N](implicit validator: Ok.ValidateOkay[M]): this.type

  def fold[A](f: N => A)(g: Y => A): A
  def merge[A](implicit evn: N <:< A, evy: Y <:< A): A
  
  def yesOr[Z >: Y](f: N => Z): Z
  def noOr[M >: N](f: Y => M): M
  
  def map[Z](f: Y => Z): Ok[N, Z]
  def mapNo[M](f: N => M): Ok[M, Y]
  
  def flatMap[M >: N, Z](f: Y => Ok[M, Z]): Ok[M, Z]
  def flatMapNo[M, Z >: Y](f: N => Ok[M, Z]): Ok[M, Z]
  
  def flatten[M >: N, Z](implicit ev: Y <:< Ok[M,Z]): Ok[M, Z]
  def flattenNo[M, Z >: Y](implicit ev: N <:< Ok[M,Z]): Ok[M, Z]

  def foreach[A](f: Y => A): Unit
  def foreachNo[A](f: N => A): Unit
  
  def tap[A](f: Y => A): this.type
  def tapNo[A](f: N => A): this.type

  def exists(f: Y => Boolean): Boolean
  def forall(f: Y => Boolean): Boolean
  
  def reject[M >: N](pf: PartialFunction[Y, M]): Ok[M, Y]
  def accept[Z >: Y](pf: PartialFunction[N, Z]): Ok[N, Z]
  
  def toEither: Either[N, Y] = this match {
    case Yes(y) => Right(y)
    case No(n) => Left(n)
  }
  def toOption: Option[Y] = this match {
    case Yes(y) => Some(y)
    case _ => None
  }
  def toTry: scala.util.Try[Y] = this match {
    case Yes(y) => scala.util.Success(y)
    case No(n) => scala.util.Failure(new NotOkException(no))
  }
  def swap: Ok[Y,N] = this match {
    case Yes(y) => No(y)
    case No(n) => Yes(n)
  }
}

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
  
  def alt[N]: Ok[N,Y] = this
}

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
  
  def alt[Y]: Ok[N,Y] = this
}

class NotOkException[N](val no: N) extends Exception {
  override def toString = "ichi.core.NotOkException("+no.toString+")"
}

object Ok {
  val UnitNo = No(())
  val UnitYes = Yes(())
  
  trait ValidateOkay[N] {
    def incorrect(n: N): Unit
  }
  
  def ifNot[N](o: Option[N]) = o match {
    case Some(n) => No(n)
    case None => UnitYes
  }
  def from[Y](o: Option[Y]) = o match {
    case Some(y) => Yes(y)
    case None => UnitNo
  }
  def from[N,Y](e: Either[N,Y]) = e match {
    case Left(n) => No(n)
    case Right(y) => Yes(y)
  }
  def from[Y](t: scala.util.Try[Y]) = t match {
    case scala.util.Success(y) => Yes(y)
    case scala.util.Failure(t) => No(t)
  }
  
  def gather[N,Y](oks: Ok[N,Y]*) = {
    val nos = oks.collect{ case No(n) => n }
    if (nos.size > 0) No(nos) else Yes(oks.map(_.yes))
  }
}
