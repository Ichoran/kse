package kse.tests

import scala.util._

import kse.flow._

object Test_Flow extends Test_Kse {
  def test_Flow_Option =
    probably { implicit oops => Option("fish").grab } == Some("fish") &&
    probably { implicit oops => Option[String](null).grab } == None &&
    okay[Boolean] { implicit hop => Option("fish").orHop(true) } == Yes("fish") &&
    okay[Boolean] { implicit hop => Option[String](null).orHop(true) } == No(true)

  def test_Flow_Try = {
    val s = Try{ 1 / 1 }
    val f = Try{ 1 / 0 }
    probably{ implicit oops => s.grab } == Some(1) &&
    probably{ implicit oops => f.grab } == None &&
    okay[Throwable]{ implicit hop => s.orHop } == Yes(1) &&
    (okay[Throwable]{ implicit hop => f.orHop } match { case No(t: ArithmeticException) => true; case _ => false })
  }

  def test_Flow_Ok = {
    val y = Yes("fish").alt[Int]
    val n = No(0).alt[String]
    probably{ implicit oops => y.grab } == Some("fish") &&
    probably{ implicit oops => n.grab } == None &&
    okay[Int]{ implicit hop => y.orHop } == Yes("fish") &&
    okay[Int]{ implicit hop => n.orHop } == No(0)
  }

  def test_Everything_Extension = {
    val i = 5

    def inc(j: Int) = j+1
    var a = 0
    val b: Byte = 1
    val c: Char = '2'
    val s: Short = 3
    val f: Float = 4f
    val l: Long = 6L
    val d: Double = 7d

    var did = false
    var didnt = false

    (i fn inc) == i+1 &&
    i.tap(x => a = x + 1) == i && (a == i+1)
    i.partFn{ case x if x < 0 => -x } == i && i.partFn{ case x if x > 0 => -x } == -i &&
    i.pickFn(_ < 0){ - _ } == i && i.pickFn(_ > 0){ - _ } == -i &&
    { i.doIf(_ < 0){_ => didnt = true }; i.doIf(_ > 0){_ => did = true }; did && !didnt } &&
    i.optIf(_ < 0) == None && i.optIf(_ > 0) == Some(i) &&
    i.okIf(_ < 0) == No(i) && i.okIf(_ > 0) == Yes(i) &&
    ((): Any).okAs[Unit] == Yes(()) && ((): Any).okAs[AnyRef] == No(()) &&
    (b: Any).okAs[Byte] == Yes(b) && (b: Any).okAs[Short] == No(b) &&
    (c: Any).okAs[Char] == Yes(c) && (c: Any).okAs[Short] == No(c) &&
    (s: Any).okAs[Short] == Yes(s) && (s: Any).okAs[Char] == No(s) &&
    (f: Any).okAs[Float] == Yes(f) && (f: Any).okAs[Double] == No(f) &&
    (i: Any).okAs[Int] == Yes(i) && (i: Any).okAs[Long] == No(i) &&
    (l: Any).okAs[Long] == Yes(l) && (l: Any).okAs[AnyRef] == No(l) &&
    (d: Any).okAs[Double] == Yes(d) && (d: Any).okAs[AnyRef] == No(d) &&
    (Some("fish"): Any).okAs[Option[String]] == Yes(Some("fish")) && (Some("fish"): Any).okAs[Try[String]] == No(Some("fish"))
  }

  def test_safe = safe{ 1/1 } == Yes(1) && (safe{ 1/0 } match { case No(t: ArithmeticException) => true; case _ => false })
  
  def test_safeOption = safeOption{ 1/1 } == Some(1) && safeOption{ 1/0 } == None

  def test_safeHop =
    safeHop(_ => false){ hop => 1/1 } == Yes(1) &&
    safeHop(_ => false){ hop => 1/0 } == No(false) &&
    safeHop(_ => false){ hop => hop(true); 1/1 } == No(true) &&
    safeHop(_ => false){ hop => hop(true); 1/0 } == No(true)
  
  def test_safeHopKey = {
    trait A {}
    trait B {}
    def ha(implicit hop: HopKey[Int, A]) { hop(1) }
    def hb(implicit hop: HopKey[Int, B]) { hop(2) }
    Seq(-1.0, 0.0, 1.0, Double.NaN).map(x =>
      safeHopKey[A](_ => 0){ implicit hop => safeHopKey[B](_ => -1){ implicit hip =>
        if (x < 0) ha;
        if (x > 0) hb;
        if (x.isNaN) throw new IllegalArgumentException
        x
      }}
    ) =?= Seq(No(1), Yes(Yes(0.0)), Yes(No(2)), Yes(No(-1)))
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Flow_from_JUnit {
  @org.junit.Test
  def test() { Test_Flow.main(Array()) }
}
