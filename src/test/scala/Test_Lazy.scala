package kse.tests

import kse.coll._

object Test_Lazy extends Test_Kse {
  def test_basics: Boolean = {
    var count = 0
    val l = new Lazy[String]({ count += 1; "cat" })
    val test1 = l.value == "cat"
    val test2 = l.value != "dog"
    var count2 = 0
    val m = Lazy{ count2 += 1; "fish" }
    val test3 = m.value == "fish"
    val test4 = m.value != "bird"
    test1 && test2 && count == 1 && test3 && test4 && count2 == 1
  }

  def test_map: Boolean = {
    var count, count2 = 0
    val l = Lazy{ count += 1; "fish" }
    val test1 = count == 0
    val m = l.map(x => { count2 += 1; x.length })
    val test2 = count == 0
    val test3 = count2 == 0
    m.value == 4 && test1 && test2 && test3 && count == 1 && count2 == 1
  }

  def test_flatMap: Boolean = {
    var count, count2 = 0
    val l = Lazy{ count += 1; "fish" }
    val test1 = count == 0
    val m = l.flatMap(x => { count2 += 1; Lazy(x.length) })
    val test2 = count == 0
    val test3 = count2 == 0
    m.value == 4 && test1 && test2 && test3 && count == 1 && count2 == 1
  }
  
  def test_foreach: Boolean = {
    var count = 0
    var fishlen = 0
    val l = Lazy{ count += 1; "fish" }
    val test1 = count == 0
    l.foreach{ x => fishlen = x.length }
    test1 && count == 1 && fishlen == 4
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Lazy_from_JUnit {
  @org.junit.Test
  def test() { Test_Lazy.main(Array()) }
}
