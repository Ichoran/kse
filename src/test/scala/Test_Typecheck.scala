package kse.tests

import kse.typecheck._
import kse.flow._

object Test_Typecheck extends Test_Kse {
  trait T {}
  trait U {}

  def f2[T: Union2[Int, String]#Apply](t: T) = t match { case i: Int => i; case s: String => s.length }
  def f3[T: Union3[Int, String, Float]#Apply](t: T) = t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt }
  def f4[T: Union4[Int, String, Float, Char]#Apply](t: T) =
    t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt; case c: Char => if (c.isDigit) c-'0' else -1 }
  def f5[T: Union5[Int, String, Float, Char, Unit]#Apply](t: T) =
    t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt; case c: Char => if (c.isDigit) c-'0' else -1; case _: Unit => 0 }

  def test_Union2 = f2(1) == 1 && f2("fish") == 4

  def test_Union3 = f3(1) == 1 && f3("fish") == 4 && f3(2.7f) == 3

  def test_Union4 = f4(1) == 1 && f4("fish") == 4 && f4(2.7f) == 3 && f4('9') == 9

  def test_Union5 = f5(1) == 1 && f5("fish") == 4 && f5(2.7f) == 3 && f5('9') == 9 && f5(()) == 0

  def test_ImplicitValue = {
    implicit val a = new ImplicitValue[Int, T] { def value = 5 }
    implicit val b = new ImplicitValue[Int, U] { def value = 6 }
    def ft(implicit iv: ImplicitValue[Int, T]) = iv.value
    def fu(implicit iv: ImplicitValue[Int, U]) = iv.value
    ft + fu == 11
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Typecheck_from_JUnit {
  @org.junit.Test
  def test() { Test_Typecheck.main(Array()) }
}
