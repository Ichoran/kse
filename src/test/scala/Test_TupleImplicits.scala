package kse.tests

import kse.coll._

object Test_TupleImplicits extends Test_Kse {
  def test_tuple1 = {
    "fish".also(_.length) == ("fish", 4)
  }

  def test_tuple2 = {
    val t2 = ("fish", 4)
    t2._1To("dog") == ("dog", 4) && t2._2To(3) == ("fish", 3) &&
    t2._1Fn(_.length + 1) == (5, 4) && t2._2Fn("!" * _) == ("fish", "!!!!") &&
    t2.eachFn(_.length + 1, "!" * _) == (5, "!!!!") &&
    t2.fold(_ * _) == "fishfishfishfish" &&
    t2.also(_.length == _) == ("fish", 4, true) &&
    t2._without1 == 4 && t2._without2 == "fish" &&
    t2._2Fn(_.toString).sameFn(_.length) == (4, 1) &&
    t2._2Fn(_.toString).reduce(_ + _) == "fish4"
  }
  
  def test_tuple3 = {
    val t3 = ("fish", 4, true)
    t3._1To("dog") == t3.copy(_1 = "dog") && t3._2To(3) == t3.copy(_2 = 3) && t3._3To(false) == t3.copy(_3 = false) &&
    t3._1Fn(_.length + 1) == t3.copy(_1 = 5) && t3._2Fn("!" * _) == t3.copy(_2 = "!!!!") && t3._3Fn(_.toString) == t3.copy(_3 = "true") &&
    t3.eachFn(_.length + 1, "!" * _, _.toString) == (5, "!!!!", "true") &&
    t3.fold(_ * _ + _.toString) == "fishfishfishfishtrue" &&
    t3.also(_.length.toDouble + _ / _.toString.length) == ("fish", 4, true, 5.0) &&
    t3._without1 == (4, true) && t3._without2 == ("fish", true) && t3._without3 == ("fish", 4) &&
    t3.eachFn(_ == "dog", _ == 4, x => x).sameFn(! _) == (true, false, false) &&
    t3.eachFn(_ == "dog", _ == 4, x => x).reduce(_ | _)
  }

  def test_tuple4 = {
    val t4 = ("fish", 4, true, 5.0)
    t4._1To("dog") == t4.copy(_1 = "dog") && t4._2To(3) == t4.copy(_2 = 3) && t4._3To(false) == t4.copy(_3 = false) && t4._4To(2.0) == t4.copy(_4 = 2.0) &&
    t4._1Fn(_.length + 1) == t4.copy(_1 = 5) && t4._2Fn("!" * _) == t4.copy(_2 = "!!!!") && t4._3Fn(_.toString) == t4.copy(_3 = "true") && t4._4Fn(_.toFloat) == t4.copy(_4 = 5f) &&
    t4.eachFn(_.length + 1, "!" * _, _.toString, _.toFloat) == (5, "!!!!", "true", 5f) &&
    t4.fold(_ * _ + _.toString * _.toInt) == "fishfishfishfishtruetruetruetruetrue" &&
    t4.also((a,b,c,d) => if (c && b < d) Some(a) else None) == ("fish", 4, true, 5.0, Option("fish")) &&
    t4._without1 == (4, true, 5.0) && t4._without2 == ("fish", true, 5.0) && t4._without3 == ("fish", 4, 5.0) && t4._without4 == ("fish", 4, true) &&
    t4.eachFn(_ == "dog", _ == 4, x => x, _.isNaN).sameFn(! _) == (true, false, false, true) &&
    t4.eachFn(_ == "dog", _ == 4, x => x, _.isNaN).reduce(_ | _)
  }

  def test_tuple5 = {
    val t5 = ("fish", 4, true, 5.0, Option("fish"))
    t5._1To("dog") == t5.copy(_1 = "dog") && t5._2To(3) == t5.copy(_2 = 3) && t5._3To(false) == t5.copy(_3 = false) && t5._4To(2.0) == t5.copy(_4 = 2.0) && t5._5To(None) == t5.copy(_5 = Option.empty[String]) &&
    t5._1Fn(_.length + 1) == t5.copy(_1 = 5) && t5._2Fn("!" * _) == t5.copy(_2 = "!!!!") && t5._3Fn(_.toString) == t5.copy(_3 = "true") && t5._4Fn(_.toFloat) == t5.copy(_4 = 5f) && t5._5Fn(_.get) == t5.copy(_5 = "fish") &&
    t5.eachFn(_.length + 1, "!" * _, _.toString, _.toFloat, _.get) == (5, "!!!!", "true", 5f, "fish") &&
    t5.fold(_ * _ + _.toString * _.toInt + _.get) == "fishfishfishfishtruetruetruetruetruefish" &&
    t5._without1 == (4, true, 5.0, Option("fish")) && t5._without2 == ("fish", true, 5.0, Option("fish")) && t5._without3 == ("fish", 4, 5.0, Option("fish")) && t5._without4 == ("fish", 4, true, Option("fish")) && t5._without5 == ("fish", 4, true, 5.0) &&
    t5.eachFn(_ == "dog", _ == 4, x => x, _.isNaN, _.isDefined).sameFn(! _) == (true, false, false, true, false) &&
    t5.eachFn(_ == "dog", _ == 4, x => x, _.isNaN, _.isDefined).reduce(_ | _)
  }

  def main(args: Array[String]) { typicalMain(args) }  
}