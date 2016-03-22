// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr and Calico Life Sciences.

package kse.tests

import scala.util._

import kse.jsonal._

object Test_Jsonal extends Test_Kse {
  case class N(var count: Int) {
    private[this] var negative = 0
    def --(): this.type = {
      if (count > 0) count -= 1
      else {
        negative += 1
        if (negative > 1024*1024) throw new Exception("AAAAAAH!  Went over by at least " + negative)
      }
      this
    }
    def ++(): this.type = {
      if (count > 0 && count < Int.MaxValue) count += 1
      this
    }
  }
  def mkNull(r: Random, n: N): Json.Null = { n.--; Json.Null }
  def mkBool(r: Random, n: N): Json.Bool = { n.--; Json.Bool(r.nextBoolean) }
  def mkStr(r: Random, n: N): Json.Str = { n.--; Json.Str(r.nextInt(5) match {
    case 0 => ""
    case 1 => Array.fill(r.nextInt(10)){ r.nextInt(65536).toChar }.mkString
    case 2 => Array.fill(1 << r.nextInt(6)){ r.nextInt(128).toChar }.mkString
    case _ => Array.fill(r.nextInt(10)){ r.nextPrintableChar }.mkString
  })}
  def mkNum(r: Random, n: N): Json = { n.--; r.nextInt(4) match {
    case 0 => Json.Num(r.nextLong)
    case 1 => Json.Num(BigDecimal((r.nextInt(9)+'1').toChar.toString + Array.fill(1 << r.nextInt(6)){ (r.nextInt(10) + '0').toChar }.mkString))
    case _ => Json.Num(java.lang.Double.longBitsToDouble(r.nextLong))
  }}
  def mkArrA(r: Random, d: Int, n: N): Json.Arr.All = { n.--; Json.Arr.All(Array.fill(r.nextInt(10)){ mkVal(r, d, n) }) }
  def mkArrD(r: Random, d: Int, n: N): Json.Arr.Dbl = {
    n.--
    if (r.nextBoolean)
      Json.Arr.Dbl(Array.fill(r.nextInt(20)){ java.lang.Double.longBitsToDouble(r.nextLong) })
    else
      Json.Arr.Dbl(Array.fill(r.nextInt(20)){ r.nextDouble })
  }
  def mkArr(r: Random, d: Int, n: N): Json.Arr = if (r.nextBoolean) mkArrA(r, d, n) else mkArrD(r, d, n)
  def mkObj(r: Random, d: Int, n: N): Json.Obj = {
    n.--
    r.nextInt(3) match {
      case 0 => Json.Obj( Array.fill(r.nextInt(20)){ (mkStr(r, n.++).text, mkVal(r, d, n))}.toMap )
      case 1 => ((Json.Obj.builder /: (0 to r.nextInt(20))){ (o, _) => o ~ (mkStr(r, n.++).text, mkVal(r, d, n)) }).result
      case _ => Json.Obj ~~ Seq.fill(r.nextInt(10)){ (Array("fish", "dish", "wish").apply(r.nextInt(3)), mkVal(r, d, n)) } ~~ Json.Obj
    }
  }
  def mkVal(r: Random, d: Int, n: N): Json = {
    r.nextInt(if (d > 0 && n.count > 0) 6 else 4) match {
      case 1 => mkBool(r, n)
      case 2 => mkStr(r, n)
      case 3 => mkNum(r, n)
      case 4 => mkArr(r, d-1, n)
      case 5 => mkObj(r, d-1, n)
      case _ => mkNull(r, n)
    }
  }

  def subtest(a: Jast, b: Jast, indent: Int = 0) {
    a match {
      case c: Json.Arr => b match {
        case d: Json.Arr => 
          val badb = Array.newBuilder[(Int, Jast, Jast)]
          val n = math.max(c.size, d.size)
          var i = 0
          while (i < n) {
            if (c(i) != d(i)) badb += ((i, c(i), d(i)))
            i += 1
          }
          val bad = badb.result
          if (bad.nonEmpty) {
            println(f"because testing ${c.getClass.getName} vs ${d.getClass.getName}")
            for ((i,x,y) <- bad) {
              println(" "*(indent+2) + f"index $i")
              x =?= y
              subtest(x, y, indent+2)
            }
          }
        case _ =>
      }
      case c: Json.Obj => b match {
        case d: Json.Obj =>
          val badb = Array.newBuilder[(String, Option[Int], Option[String], Jast, Jast)]
          val dupie = c.hasDuplicateKeys || d.hasDuplicateKeys
          if (dupie) {
            var i = 0
            val ci = c.iterator
            val di = d.iterator
            while (ci.hasNext && di.hasNext) {
              val xi = ci.next
              val yi = di.next
              if (xi != yi) badb += ((xi._1, Some(i), if (yi._1 != xi._1) Some(yi._1) else None, xi._2, yi._2))
              i += 1
            }
            if (ci.hasNext || di.hasNext) badb += ((if (ci.hasNext) ci.next._1 else di.next._1, Some(i), Some("NOT EVEN THERE!"), Json.Null, Json.Null))
          }
          else {
            c.foreach{ (ki,vi) =>
              val yi = d(ki)
              if (vi != yi) badb += ((ki, None, None, vi, yi))
            }
            if (c.size != d.size) badb += (("NOT EVEN THE SAME SIZE", None, None, Json.Null, Json.Null))
          }
          val bad = badb.result
          if (bad.nonEmpty) {
            println(f"because testing ${c.getClass.getName} vs ${d.getClass.getName}${if (dupie) " (duplicate keys)" else ""}")
            for ((k,opi,opk2,x,y) <- bad) {
              println(" "*(indent+2) + f"key $k${opi.map(i => " index "+i).getOrElse("")}${opk2.map(s => " other key "+s).getOrElse("")}")
              x =?= y
              subtest(x, y, indent+2)
            }
          }
        case _ =>
      }
      case _ =>
    }
  }

  def test_random_round_trip_String: Boolean = {
    val r = new scala.util.Random
    (0 to 1024).forall{ n =>
      val i = mkVal(r, 6, N(1024))
      val j = i.toString
      val k = Json.parse(j)
      val k2 = Json.relaxed.parse(j)
      k.right.exists{ kk =>
        if (i != kk) {
          i =?= kk
          subtest(i, kk)
          false
        }
        else (("string "+j) =?= ("string " + kk.toString))
      } &&
      k2.right.exists{ kk2 =>
        val l = Json.relaxed.parse(kk2.toString)
        l.right.exists{ l2 => 
          val ans = l2 == kk2
          if (!ans) {
            println("relaxed")
            l2 =?= kk2
          }
          ans
        }
      } &&
      (Json.parse(java.nio.CharBuffer.wrap(j.toCharArray)) match {
        case Right(ll) =>
          if (i != ll) {
            i =?= ll
            subtest(i, ll)
            false
          }
          else true
        case Left(e) => println(j); println(e); false
      }) &&
      (Json.relaxed.parse(java.nio.CharBuffer.wrap(j.toCharArray)) match { 
        case Right(ll) =>
          k2.right.exists{ kk2 =>
            val ans = kk2 == ll
            if (!ans) {
              println("relaxed CharBuffer")
              ll =?= kk2
            }
            ans
          }
        case Left(e) => println(j); println(e); false 
      }) &&
      (Json.parse(java.nio.ByteBuffer.wrap(j.getBytes("UTF-8"))) match {
        case Right(mm) =>
          if (i != mm) {
            i =?= mm
            subtest(i, mm)
            false
          }
          else true
        case Left(e) => println(j); println("ByteBuffer"); println(e); false
      }) &&
      (Json.relaxed.parse(java.nio.ByteBuffer.wrap(j.getBytes("UTF-8"))) match {
        case Right(mm) =>
          k2.right.exists{ kk2 =>
            val ans = kk2 == mm
            if (!ans) {
              println("relaxed ByteBuffer")
              mm =?= kk2
            }
            ans
          }
        case Left(e) => println(j); println("ByteBuffer"); println(e); false
      })      
    }
  }

  def test_specifics_String: Boolean = {
    import JsonConverters._

    Json.Null =?= Json.parse("null").right.get &&
    Json.Bool.True =?= Json.parse("true").right.get &&
    Json.Bool.False =?= Json.parse("false").right.get &&
    Json.Str("fish") =?= Json.parse("\"fish\"").right.get &&
    Seq("nul", "ull", "tru", "True", "rue", "als", "fals", "\"fish", "fish\"").map(Json.parse).collect{
      case Right(js) => js
    } =?= Seq[Json]() &&
    Json.Num(Double.NaN) =?= Json.Null &&
    Json.Num(Double.PositiveInfinity) =?= Json.Null &&
    Json.Num(Double.NegativeInfinity) =?= Json.Null &&
    Json.Num(18014398509481985L) =?= Json.parse("18014398509481985").right.get &&
    Json.Num(1.9125881e13) =?= Json.parse("19.125881e12").right.get &&
    Json.Num(BigDecimal("1234123412341234123412431234123412341234")) =?= Json.parse("1234123412341234123412431234123412341234").right.get &&
    Json.Num(1.234123412341234E39) =?= Json.relaxed.parse("1234123412341234123412431234123412341234").right.get &&
    Json.Arr.All(Array(Json("fish"), Json(2.7))) =?= Json.parse("[\"fish\", 27e-1]").right.get &&
    Json.Arr.All(Array(Json(3.0), Json.Null)) =?= Json.Arr.Dbl(Array(3.0, Double.NaN)) &&
    Json ~ "fish" ~ 2.7 ~ Json =?= Json.Arr ~ "fish" ~ 2.7 ~ Json.Arr &&
    Json ~ "fish" ~ 2.7 ~ Json =?= Json.Arr.All ~ "fish" ~ 2.7 ~ Json.Arr.All &&
    Json ~ ("fish", 2.7) ~ Json =?= Json.Obj(Map("fish" -> Json(2.7))) &&
    Json.Obj(Map("fish" -> Json(2.7))) =?= Json.parse("{\"fish\": 0.27e1}").right.get &&
    Json.Obj ~ ("fish", 2.0) ~ ("fish", 3.0) ~ Json.Obj =?= Json.parse("{\"fish\": 2, \"fish\": 3}").right.get &&
    Json ~ ("fish", 2.0) ~ Json =?= Json.Obj ~ ("fish", 2.0) ~ Json.Obj &&
    Json.Obj(Map("fish" -> (Json ~ Json("\n\n\n\n") ~ 2.7 ~ true ~ Json))) =?= Json.parse("{\"fish\":[\"\\n\\n\\n\\n\", 2.7, true]}").right.get &&
    Json ~ ("fish", Array("wish", "dish")) ~ Json =?= Json ~ ("fish", Json ~ "wish" ~ "dish" ~ Json) ~ Json &&
    Json ~ ("fish", Map("wish" -> "dish")) ~ Json =?= Json ~ ("fish", Json ~ ("wish", "dish") ~ Json) ~ Json &&
    Json(false).to[Boolean] =?= Right(false) &&
    Json(2.127515).to[Double] =?= Right(2.127515) &&
    Json("fish").to[String] =?= Right("fish") &&
    (Json ~ "cod" ~ "herring" ~ Json).to[Array[String]].right.map(_.toSeq) =?= Right(Seq("cod", "herring")) &&
    (Json ~ "cod" ~ "herring" ~ Json).to[Vector[String]] =?= Right(Vector("cod", "herring")) &&
    { val now = java.time.Instant.now; Json ~ now ~ Json =?= Json ~ now.toString ~ Json } &&
    { val now = java.time.Instant.now; (Json ~ now ~ Json).to[Array[java.time.Instant]].right.map(_.headOption) =?= Right(Option(now)) }
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Jsonal_from_JUnit {
  @org.junit.Test
  def test() { Test_Jsonal.main(Array()) }
}
