package kse.tests

import scala.util._

import kse.flow._

object Test_Flow_Macros extends Test_Kse {
  def test_cFor = {
    val a = Array(0,0,0)
    cFor(0)(_ < a.length)(_ + 1)(i => a(i) = i+1)
    val b = Array(0,0,0)
    var i = 0
    cFor(0)(_ < b.length)(_ + 1)(i => b(i) = i+1)
// TODO--fix me!  I fail!
//    val c = Array(0,0,0)
//    cFor(0)(_ < c.length)(_ + 1)(i => c(i) = { val j = i; { val i = 1; i+j } })
    i == 0 &&
    a.toList == List(1,2,3) &&
    b.toList == a.toList // &&
//    c.toList == a.toList
  }

  def test_aFor = {
    val a = Array(0,0,0)
    aFor(a)((x,i) => a(i) = i+1)
    val b = Array(0,0,0)
    var i = 0
    aFor(b)((x,i) => b(i) = i+1)
    i == 0 &&
    a.toList == List(1,2,3) &&
    b.toList == a.toList
  }
  
  def test_nFor = {
    val a = Array(0,0,0)
    nFor(a.length)(i => a(i) = i+1)
    val b = Array(0,0,0)
    var i = 0
    nFor(b.length)(i => b(i) = i+1)
    i == 0 &&
    a.toList == List(1,2,3) &&
    b.toList == a.toList
  }
  
  def test_iFor = {
    val lb = List.newBuilder[Int]
    iFor((1 to 3).iterator)(x => lb += x)
    val lb2 = List.newBuilder[Int]
    var x = 0
    iFor((1 to 3).iterator)(x => lb2 += x)
    x == 0 &&
    lb.result == List(1,2,3) &&
    lb2.result == List(1,2,3)
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Flow_Macros_from_JUnit {
  @org.junit.Test
  def test() { Test_Flow_Macros.main(Array()) }
}