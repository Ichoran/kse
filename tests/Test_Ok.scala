package kse.tests

import kse.flow._

object Test_Ok extends Test_Kse {
  class TypeIdentity[T](val t: T) {
    def ident[U](implicit ev: U =:= T) {}
  }
  trait T { override def equals(a: Any) = a.getClass == this.getClass }
  class U extends T {}
  class V extends T {}
  class W extends T {}

  def test_isOk = Yes("fish").isOk && !No("dog").isOk

  def test_yes = (Yes("fish").yes == "fish") && (safe { No("dog").alt[Int].yes == 0 } match { case No(t: NoSuchElementException) => true; case _ => false })

  def test_no = (No("fish").no == "fish") && (safe { Yes("dog").alt[Int].no == 0 } match { case No(t: NoSuchElementException) => true; case _ => false })

  def test_valid: Boolean = {
    var called: String = null
    implicit val validator = new Ok.ValidateOkay[String] {
      def incorrect(s: String) { called = s }
    }
    Yes("fish").valid
    if (called ne null) return false
    No("dog").valid
    called == "dog"
  }

  def test_fold = Yes("fish").fold(_ => false)(_ => true) && No("dog").fold(_ => true)(_ => false)

  def test_merge = {
    val yn = new TypeIdentity( Yes(new U).alt[V].merge )
    val ny = new TypeIdentity( No(new V).alt[U].merge )
    yn.ident[T]
    ny.ident[T]
    yn.t == (new U) && ny.t == (new V)
  }

  def test_yesOr = Yes("fish").alt[Int].yesOr(_.toString) == "fish" && No(123).alt[String].yesOr(_.toString) == "123"

  def test_noOr = Yes("fish").alt[Int].noOr(_.length) == 4 && No(123).alt[String].noOr(_.toInt) == 123

  def test_map = Yes("fish").map(_.length) == Yes(4) && No("dog").alt[Int].map(_ + 2) == No("dog")

  def test_mapNo = Yes("fish").alt[Int].mapNo(_ + 2) == Yes("fish") && No("dog").mapNo(_.length) == No(3)

  def test_flatMap = 
    Yes("fish").flatMap(s => if (s.length < 3) Yes(s.reverse) else No(s)) == No("fish") &&
    Yes("fish").flatMap(s => if (s.length > 3) Yes(s.reverse) else No(s)) == Yes("hsif") &&
    No("dog").flatMap(s => ???) == No("dog")

  def test_flatMapNo =
    No("dog").flatMapNo(s => if (s.length < 2) No(s.reverse) else Yes(s)) == Yes("dog") &&
    No("dog").flatMapNo(s => if (s.length > 2) No(s.reverse) else Yes(s)) == No("god") &&
    Yes("fish").flatMapNo(s => ???) == Yes("fish")

  def test_filter = {
    import Ok.defaultToUnit
    Yes("fish").filter(_.length > 3) == Yes("fish") &&
    Yes("fish").filter(_.length < 3) == No(()) &&
    No(()).filter(_ => true) == No(()) &&
    No(()).filter(_ => false) == No(())
  }

  def test_withFilter = {
    import Ok.defaultToUnit
    (new TypeIdentity(No(()).alt[String].withFilter(_ => true).noOr(_ => ???))).ident[Unit]
    (new TypeIdentity(No(()).alt[String].withFilter(_ => false).noOr(_ => ???))).ident[Unit]
    Yes("fish").withFilter(_.length > 3).yesOr(_ => "dog") == "fish" &&
    Yes("fish").withFilter(_.length < 3).yesOr(_ => "dog") == "dog"
  }

  def test_flatten = {
    val yy = Yes(Yes(new U).alt[V]).alt[W]
    val yn = Yes(No(new V).alt[U]).alt[W]
    val n = No(new W).alt[Ok[V,U]]
    val tyy = new TypeIdentity(yy.flatten)
    val tyn = new TypeIdentity(yn.flatten)
    val tn = new TypeIdentity(n.flatten)
    tyy.ident[Ok[T,U]]
    tyn.ident[Ok[T,U]]
    tn.ident[Ok[T,U]]
    tyy.t == Yes(new U) && tyn.t == No(new V) && tn.t == No(new W)
  }

  def test_flattenNo = {
    val nn = No(No(new U).alt[V]).alt[W]
    val ny = No(Yes(new V).alt[U]).alt[W]
    val y = Yes(new W).alt[Ok[U,V]]
    val tnn = new TypeIdentity(nn.flattenNo)
    val tny = new TypeIdentity(ny.flattenNo)
    val ty = new TypeIdentity(y.flattenNo)
    tnn.ident[Ok[U,T]]
    tny.ident[Ok[U,T]]
    ty.ident[Ok[U,T]]
    tnn.t == No(new U) && tny.t == Yes(new V) && ty.t == Yes(new W)
  }

  def test_foreach = {
    var one = "fish"
    var two = "dog"
    Yes("fish").foreach(two = _)
    No("dog").foreach(one = _)
    one == "fish" && two == "fish"
  }

  def test_foreachNo = {
    var one = "fish"
    var two = "dog"
    Yes("fish").foreachNo(two = _)
    No("dog").foreachNo(one = _)
    one == "dog" && two == "dog"
  }

  def test_tap = {
    var one = "fish"
    var two = "dog"
    val y = Yes("fish")
    val n = No("dog")
    (y.tap(two = _) eq y) && (n.tap(one = _) eq n) && one == "fish" && two == "fish"
  }

  def test_tapNo = {
    var one = "fish"
    var two = "dog"
    val y = Yes("fish")
    val n = No("dog")
    (y.tapNo(two = _) eq y) && (n.tapNo(one = _) eq n) && one == "dog" && two == "dog"
  }

  def test_exists = Yes("fish").exists(_.length > 3) && !Yes("fish").exists(_.length < 3) && !No("dog").exists(_ => ???)

  def test_forall = Yes("fish").forall(_.length > 3) && !Yes("fish").forall(_.length < 3) && No("dog").forall(_ => ???)

  def test_reject =
    Yes("fish").alt[Int].reject{ case x if x.length < 3 => 0 } == Yes("fish") &&
    Yes("fish").alt[Int].reject{ case x if x.length > 3 => 0 } == No(0) &&
    No(1).alt[String].reject{ case _ => ??? } == No(1)

  def test_accept =
    No("dog").alt[Int].accept{ case x if x.length < 2 => 1 } == No("dog") &&
    No("dog").alt[Int].accept{ case x if x.length > 2 => 1 } == Yes(1) &&
    Yes(0).alt[String].accept{ case _ => ??? } == Yes(0)

  def test_toEither = {
    import scala.util._
    val ty = new TypeIdentity(Yes("fish").alt[Int].toEither)
    val tn = new TypeIdentity(No(1).alt[String].toEither)
    ty.ident[Either[Int,String]]
    tn.ident[Either[Int,String]]
    ty.t == Right("fish") && tn.t == Left(1)
  }

  def test_toOption = {
    val ty = new TypeIdentity(Yes("fish").alt[Int].toOption)
    val tn = new TypeIdentity(No(1).alt[String].toOption)
    ty.ident[Option[String]]
    tn.ident[Option[String]]
    ty.t == Some("fish") && tn.t == None
  }

  def test_toTry = {
    import scala.util._
    val ex = new Exception
    val ty = new TypeIdentity(Yes("fish").alt[Int].toTry)
    val tn = new TypeIdentity(No("dog").alt[Int].toTry)
    val tt = new TypeIdentity(No(ex).alt[Int].toTry)
    ty.ident[Try[String]]
    tn.ident[Try[Int]]
    tt.ident[Try[Int]]
    ty.t == Success("fish") && 
      (tn.t match { case Failure(x: NotOkException[_]) => x.no match { case s: String => s == "dog"; case _ => false }; case _ => false }) && 
      (tt.t match { case Failure(x: Exception) => x eq ex; case _ => false })
  }

  def test_swap = Yes("fish").alt[Int].swap == No("fish") && No("dog").alt[Int].swap == Yes("dog")

  def test_alt = {
    val ty = new TypeIdentity(Yes("fish").alt[Int])
    val tn = new TypeIdentity(No("dog").alt[Int])
    ty.ident[Ok[Int,String]]
    tn.ident[Ok[String,Int]]
    true
  }

  def test_unitYesNo = Ok.UnitYes == Yes(()) && Ok.UnitNo == No(())

  def test_ifNot = Ok.ifNot(Option("fish")) == No("fish") && Ok.ifNot(None) == Ok.UnitYes

  def test_from_Option = Ok.from(Some("fish")) == Yes("fish") && Ok.from(None) == Ok.UnitNo

  def test_from_Either = {
    import scala.util._
    val e: Either[Int, String] = Right("fish")
    val f: Either[Int, String] = Left(1)
    val ty = new TypeIdentity(Ok.from(e))
    val tn = new TypeIdentity(Ok.from(f))
    ty.ident[Ok[Int, String]]
    tn.ident[Ok[Int, String]]
    ty.t == Yes("fish") && tn.t == No(1)
  }

  def test_from_Try = {
    import scala.util._
    val a = Try{ 1 / 1 }
    val b = Try{ 1 / 0 }
    val t = new TypeIdentity(Ok.from(a))
    t.ident[Ok[Throwable,Int]]
    t.t == Yes(1) && (Ok.from(b) match { case No(x: ArithmeticException) => true; case _ => false })
  }

  def test_gather =
    Ok.gather(Yes("salmon"), Yes("herring"), Yes("cod")) == Yes(Seq("salmon", "herring", "cod")) &&
    Ok.gather(No("labrador"), No("terrier"), No("poodle")) == No(Seq("labrador", "terrier", "poodle")) &&
    Ok.gather(Yes("salmon"), No("poodle"), Yes("perch"), No("chihuahua")) == No(Seq("poodle", "chihuahua"))

  def test_Option_toOk = Option("fish").toOk == Yes("fish") && Option[String](null).toOk == Ok.UnitNo

  def test_Either_toOk = {
    import scala.util._
    val e: Either[Int, String] = Right("fish")
    val f: Either[Int, String] = Left(1)
    val ty = new TypeIdentity(e.toOk)
    val tn = new TypeIdentity(f.toOk)
    ty.ident[Ok[Int, String]]
    tn.ident[Ok[Int, String]]
    ty.t == Yes("fish") && tn.t == No(1)
  }

  def test_Try_toOk = {
    import scala.util._
    val a = Try{ 1 / 1 }
    val b = Try{ 1 / 0 }
    val t = new TypeIdentity(a.toOk)
    t.ident[Ok[Throwable,Int]]
    t.t == Yes(1) && (b.toOk match { case No(x: ArithmeticException) => true; case _ => false })
  }

  def main(args: Array[String]) { typicalMain(args) }
}
