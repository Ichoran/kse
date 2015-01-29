package kse.tests

import scala.util._

import kse.flow._

object Test_Hop extends Test_Kse {
  def test_oopsThrowingRealException = Try{ OOPS(oopsThrowingRealException) } match { case Failure(t: OopsException) => true; case _ => false }

  def test_OOPS = {
    implicit val o = oopsThrowingRealException
    Try { OOPS } match { case Failure(t: OopsException) => true; case _ => false }
  }

  private val myOops = new Hopped[Unit] with Oops {
    def value = ()
    def apply()  = { throw this }
  }

  def test_tapOops = {
    var count = 0
    var effect = false
    var affect = false
    implicit val o = myOops
    try { tapOops{ count += 1 }{ affect = true } } catch { case t if t eq o => count += 2 }
    try { tapOops{ count += 4; OOPS }{ effect = true } } catch { case t if t eq o => count += 8 }
    count == 13 && effect && !affect
  }

  private val myHop = new Hopped[String] with Hop[String] {
    private[this] var myValue: String = null
    def value = myValue
    def value_(v: String): this.type = { myValue = value; this }
    def valueOp(f: String => String): this.type = { myValue = f(myValue); this }
    def apply() = { throw this }
    def apply(s: String) = { myValue = s; throw this }
  }

  def test_HOP: Boolean = {
    implicit val h: Hop[String] = myHop
    try { h("frog") } catch { case t if t eq h => return h.value == "frog" }
    false
  }

  def test_tapHop = {
    implicit val h: Hop[String] = myHop
    var count = 0
    var effect = false
    var affect = false
    try { tapHop{ count += 1; () }{ (s: String) => affect = true } } catch { case t if t eq h => count += 2 }
    try { tapHop{ count += 4; HOP("kangaroo"); () }{ (s: String) => effect = s == "kangaroo" } } catch { case t if t eq h => count += 8 }
    count == 13 && effect && !affect
  }

  def test_oopsless: Boolean = {
    implicit val o = myOops
    oopsless{ "fish" } == Some("fish") && oopsless[String]{ OOPS } == None
  }

  def test_oopslessOr = {
    implicit val o = myOops
    oopslessOr("dog"){ "fish" } == "fish" && oopslessOr("dog"){ OOPS } == "dog"
  }

  def test_oopslessOrNull = {
    implicit val o = myOops
    oopslessOrNull{ "fish" } == "fish" && oopslessOrNull[String]{ OOPS } == null
  }

  def test_oopslessDo = {
    implicit val o = myOops
    var effect = false
    var affect = false
    oopslessDo{ effect = true; OOPS; affect = true }
    effect && !affect
  }

  def test_probably = probably{ implicit oops => "fish" } == Some("fish") && probably[String]{ implicit oops => OOPS } == None

  def test_probablyOr = probablyOr("dog"){ implicit oops => "fish" } == "fish" && probablyOr("dog"){ implicit oops => OOPS } == "dog"

  def test_probablyOrNull = probablyOrNull{ implicit oops => "fish" } == "fish" && probablyOrNull[String]{ implicit oops => OOPS } == null

  def test_probablyDo = {
    var effect = false
    var affect = false
    probablyDo{ implicit oops => effect = true; OOPS; affect = true }
    effect && !affect
  }

  def test_probablyOne = {
    var effect = false
    var affect = false
    val answer: Unit = probablyOne(Seq[Oops => Unit](implicit oops => effect = true, implicit oops => OOPS, implicit oops => affect = true))(())
    effect && !affect
  }

  def test_hopTo =
    Seq("fish", "dog").map( x => hopTo[Int]( hop => if (x == "fish") 0 else hop.from((s: String) => s.length).apply(x) ) ) == Seq(0, 3)
    
  def test_hopOr =
    Seq("fish", "dog", "cow").map(x => hopOr("cricket")( hop => if (x == "fish") x else if (x == "cow") hop() else hop("jerboa") )) == Seq("fish", "jerboa", "cricket") &&
    Seq(1, 2, 3).map(x => hopOr(0)( hop => if (x == 1) x else if (x == 3) hop() else hop(-1) )) == Seq(1, -1, 0)
    Seq(1L, 2L, 3L).map(x => hopOr(0L)( hop => if (x == 1) x else if (x == 3) hop(); else hop(-1) )) == Seq(1L, -1L, 0L)
    
  def test_okay = okay[String]{ _ => "frog" } == Yes("frog") && okay[String]{ hop => hop("dog"); "frog" } == No("dog")
  
  def test_okayOr =
    okayOr(0){ _ => "frog" } == Yes("frog") && okayOr(0){ hop => hop(); "frog" } == No(0) &&
    okayOr(0L){ _ => "frog" } == Yes("frog") && okayOr(0L){ hop => hop(); "frog" } == No(0L) &&
    okayOr("dog"){ _ => "frog" } == Yes("frog") && okayOr("dog") { hop => hop(); "frog" } == No("dog")
  
  def test_okayLazy = {
    var count = 0
    val correct =
      okayLazy{ count += 1; "grasshopper" }(_ => "frog") == Yes("frog") &&
      okayLazy{ count += 2; "grasshopper" }{ hop => hop(); "dog" } == No("grasshopper")
    correct && count == 2
  }
  
  def test_okayWith =
    Seq("fish", "dog", "cow").map(x => okayWith("cricket")( hop => if (x == "fish") x else if (x == "cow") hop() else hop("jerboa") )) == Seq(Yes("fish"), No("jerboa"), No("cricket")) &&
    Seq(1, 2, 3).map(x => okayWith(0)( hop => if (x == 1) x else if (x == 3) hop() else hop(-1) )) == Seq(Yes(1), No(-1), No(0))
    Seq(1L, 2L, 3L).map(x => okayWith(0L)( hop => if (x == 1) x else if (x == 3) hop(); else hop(-1) )) == Seq(Yes(1L), No(-1L), No(0L))
  
  def test_okayHere =
    okayWith("fish")(implicit hop => okayHere((_: Hop[String]) => "frog")) == Yes(Yes("frog")) &&
    okayWith("fish")(implicit hop => okayHere{(_: Hop[String]) => hop("jerboa"); "frog"}) == Yes(No("jerboa")) &&
    okayWith("fish")(implicit hop => okayHere{(_: Hop[String]) => hop(); "frog" }) == Yes(No("fish")) &&
    okayWith("fish"){ implicit hop => hop(); okayHere((_: Hop[String]) => "frog") } == No("fish")

  def main(args: Array[String]) { typicalMain(args) }
}
