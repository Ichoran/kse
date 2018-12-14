// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr and Calico Life Sciences.

package kse.tests

import java.nio._

import scala.util._

import kse.flow._
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

  def test_random_round_trip: Boolean = {
    val r = new scala.util.Random
    (0 to 1024).forall{ n =>
      val i = mkVal(r, 6, N(1024))
      val j = i.toString
      val k = Json.parse(j)
      val k2 = Json.relaxed.parse(j)
      k.exists{ kk =>
        if (i != kk) {
          i =?= kk
          subtest(i, kk)
          false
        }
        else (("string "+j) =?= ("string " + kk.toString))
      } &&
      k2.exists{ kk2 =>
        val l = Json.relaxed.parse(kk2.toString)
        l.exists{ l2 => 
          val ans = l2 == kk2
          if (!ans) {
            println("relaxed")
            l2 =?= kk2
          }
          ans
        }
      } &&
      (Json.parse(java.nio.CharBuffer.wrap(j.toCharArray)) match {
        case Yes(ll) =>
          if (i != ll) {
            println("CharBuffer")
            i =?= ll
            subtest(i, ll)
            false
          }
          else true
        case No(e) => println(j); println("CharBuffer"); println(e); false
      }) &&
      (Json.relaxed.parse(java.nio.CharBuffer.wrap(j.toCharArray)) match { 
        case Yes(ll) =>
          k2.exists{ kk2 =>
            val ans = kk2 == ll
            if (!ans) {
              println("relaxed CharBuffer")
              ll =?= kk2
            }
            ans
          }
        case No(e) => println(j); println("relaxed CharBuffer"); println(e); false 
      }) &&
      (Json.parse(java.nio.ByteBuffer.wrap(j.getBytes("UTF-8"))) match {
        case Yes(mm) =>
          if (i != mm) {
            println("ByteBuffer")
            i =?= mm
            subtest(i, mm)
            false
          }
          else true
        case No(e) => println(j); println("ByteBuffer"); println(e); false
      }) &&
      (Json.relaxed.parse(java.nio.ByteBuffer.wrap(j.getBytes("UTF-8"))) match {
        case Yes(mm) =>
          k2.exists{ kk2 =>
            val ans = kk2 == mm
            if (!ans) {
              println("relaxed ByteBuffer")
              mm =?= kk2
            }
            ans
          }
        case No(e) => println(j); println("relaxed ByteBuffer"); println(e); false
      }) &&
      (Jast.parse(new java.io.ByteArrayInputStream(j.getBytes("UTF-8"))) match {
        case nn: Json =>
          if (i != nn) {
            println("InputStream")
            i =?= nn
            subtest(i, nn)
            false
          }
          else true
        case e: JastError => println(j); println("InputStream"); println(e); false
      }) &&
      (Jast.parse(new java.io.ByteArrayInputStream(j.getBytes("UTF-8"))) match {
        case nn: Json =>
          k2.exists{ kk2 =>
            val ans = kk2 == nn
            if (!ans) {
              println("relaxed InputStream")
              nn =?= kk2
            }
            ans
          }
        case e: JastError => println(j); println("relaxed InputStream"); println(e); false
      }) &&
      k2.exists{ kk2 =>
        val s = kk2.toString
        val ss = { val sb = new java.lang.StringBuilder; kk2.jsonString(sb); sb.toString }
        val sss = {
          val b = new Array[Byte](s.length + s.length/4 + 128)
          val bb = java.nio.ByteBuffer.wrap(b)
          kk2.jsonBytes(bb, _ => throw new Exception("too long"))
          new String(b, 0, bb.position)
        }
        val ssss = {
          val c = new Array[Char](s.length + s.length/4 + 128)
          val cc = java.nio.CharBuffer.wrap(c)
          kk2.jsonChars(cc, _ => throw new Exception("too long"))
          new String(c, 0, cc.position)
        }
        s =?= ss &&
        s =?= sss &&
        s =?= ssss
      }
    }
  }

  def test_base64: Boolean = {
    import JsonConverters._

    val r = new scala.util.Random
    (0 to 1024).forall{ _ =>
      val a = Array.fill(math.max(-1, r.nextInt(20)-4) + (1 << r.nextInt(10)))(r.nextInt.toByte)
      val j = Json(a)
      j.to[Array[Byte]] match {
        case Yes(b) =>
          var same = a.length == b.length
          var i = 0
          while (i < a.length && same) { same = a(i) == b(i); i += 1 }
          if (!same) println(f"${a.length}:${a.take(i+1).mkString(",")} != ${b.length}:${b.take(i+1).mkString(",")}")
          same
        case No(je) => println(je); println(j); false
      }
    }
  }

  def test_specifics_Direct: Boolean = {
    import JsonConverters._

    Json.Num(Double.NaN) =?= Json.Null &&
    Json.Num(Double.PositiveInfinity) =?= Json.Null &&
    Json.Num(Double.NegativeInfinity) =?= Json.Null &&
    Json ~ ("fish", Array("wish", "dish")) ~ Json =?= Json ~ ("fish", Json ~ "wish" ~ "dish" ~ Json) ~ Json &&
    Json ~ ("fish", Map("wish" -> "dish")) ~ Json =?= Json ~ ("fish", Json ~ ("wish", "dish") ~ Json) ~ Json &&
    Json(false).to[Boolean] =?= Yes(false) &&
    Json(2.127515).to[Double] =?= Yes(2.127515) &&
    Json("fish").to[String] =?= Yes("fish") &&
    Json.Arr.All(Array(Json(3.0), Json.Null)) =?= Json.Arr.Dbl(Array(3.0, Double.NaN)) &&
    Json ~ "fish" ~ 2.7 ~ Json =?= Json.Arr ~ "fish" ~ 2.7 ~ Json.Arr &&
    Json ~ "fish" ~ 2.7 ~ Json =?= Json.Arr.All ~ "fish" ~ 2.7 ~ Json.Arr.All &&
    Json ~ ("fish", 2.7) ~ Json =?= Json.Obj(Map("fish" -> Json(2.7))) &&
    Json ~ ("fish", 2.0) ~ Json =?= Json.Obj ~ ("fish", 2.0) ~ Json.Obj &&
    Json ~? ("fish", Json()) ~ Json =?= Json.Obj.empty &&
    Json ~? ("fish", "") ~ Json =?= Json.Obj.empty &&
    Json ~? ("fish", Option[String](null)) ~ Json =?= Json.Obj.empty &&
    Json ~? ("fish", Json(Vector.empty[String])) ~ Json =?= Json.Obj.empty &&
    Json ~? ("fish", Json ~? ("wish", Json(Double.NaN)) ~ Json) ~ Json =?= Json.Obj.empty &&
    Json ~~ (Json.Obj ~ ("fish", "herring") ~ Json.Obj) ~ Json =?= Json ~ ("fish", "herring") ~ Json &&
    (Json ~ "cod" ~ "herring" ~ Json).to[Array[String]].map(_.toSeq) =?= Yes(Seq("cod", "herring")) &&
    (Json ~ "cod" ~ "herring" ~ Json).to[Vector[String]] =?= Yes(Vector("cod", "herring")) &&
    (Json.Arr.All ~ "cod" ~ true ~ Json.Arr.All).filter(_.string.isDefined) =?= (Json ~ "cod" ~ Json) &&
    (Json.Arr.Dbl ~ -2.7 ~ 3.7 ~ Json.Arr.Dbl).filter((x: Double) => x > 0) =?= (Json ~ 3.7 ~ Json) &&
    (Json.Obj ~ ("apple", 0) ~ ("b", 1) ~ ("c", -1) ~ Json.Obj).filter((k,v) => k.length < 2 && v.double >= 0) =?= Json ~ ("b", 1) ~ Json &&
    (Json.Obj ~ ("cod", true) ~ ("herring", false) ~ ("herring", 7) ~ Json.Obj).transformValues{ (k,v) =>
      if (k == "herring") v match {
        case Json.Bool(b) => Json.Bool(!b)
        case _ => v
      }
      else v
    } =?= (Json.Obj ~ ("cod", true) ~ ("herring", true) ~ ("herring", 7) ~ Json.Obj) &&
    { val dt = java.time.Duration.parse("PT5M2.7S"); Json ~ dt ~ Json =?= Json ~ dt.toString ~ Json } &&
    { val dt = java.time.Duration.parse("PT5M2.7S"); (Json ~ dt ~ Json).to[Array[java.time.Duration]].map(_.headOption) =?= Yes(Option(dt)) } &&
    { val now = java.time.Instant.now; Json ~ now ~ Json =?= Json ~ now.toString ~ Json } &&
    { val now = java.time.Instant.now; (Json ~ now ~ Json).to[Array[java.time.Instant]].map(_.headOption) =?= Yes(Option(now)) } &&
    { val now = java.time.LocalDateTime.now; Json ~ now ~ Json =?= Json ~ now.toString ~ Json } &&
    { val now = java.time.LocalDateTime.now; (Json ~ now ~ Json).to[Array[java.time.LocalDateTime]].map(_.headOption) =?= Yes(Option(now)) } &&
    { val now = java.time.OffsetDateTime.now; Json ~ now ~ Json =?= Json ~ now.toString ~ Json } &&
    { val now = java.time.OffsetDateTime.now; (Json ~ now ~ Json).to[Array[java.time.OffsetDateTime]].map(_.headOption) =?= Yes(Option(now)) } &&
    { val now = java.time.ZonedDateTime.now; Json ~ now ~ Json =?= Json ~ now.toString ~ Json } &&
    { val now = java.time.ZonedDateTime.now; (Json ~ now ~ Json).to[Array[java.time.ZonedDateTime]].map(_.headOption) =?= Yes(Option(now)) } &&
    { val x = ("one", 2); Json(x).to[(String, Int)] =?= Yes(x) } &&
    { val x = ("one", 2, 3); Json(x).to[(String, Int, Double)] =?= Yes(x) } &&
    { val x = (1, 2, 3, 4); Json(x).to[(Int, Long, Double, Int)] =?= Yes(x) } &&
    { val x = ("one", 2, 3, "four", 5); Json(x).to[(String, Int, Int, String, Int)] =?= Yes(x) } &&
    { val x = ("one", 2, 3, "four", 5, "six"); Json(x).to[(String, Int, Int, String, Int, String)] =?= Yes(x) } &&
    { val x = ("one", 2, 3, "four", 5, "six", 7); Json(x).to[(String, Int, Int, String, Int, String, Int)] =?= Yes(x) } &&
    { val x = ("one", 2, 3, "four", 5, "six", 7, None); Json(x).to[(String, Int, Int, String, Int, String, Int, Option[String])] =?= Yes(x) } &&
    { val x = ("one", 2, 3, "four", 5, "six", 7, None, (0, 9)); Json(x).to[(String, Int, Int, String, Int, String, Int, Option[String], (Int, Double))] =?= Yes(x) }
  }

  def test_specifics_String: Boolean = {
    import JsonConverters._

    Yes(Json.Null) =?= Json.Null.parse("null") &&
    Yes(Json.Bool.True) =?= Json.Bool.parse("true") &&
    Yes(Json.Bool.False) =?= Json.Bool.parse("false") &&
    Yes(Json.Str("fish")) =?= Json.Str.parse("\"fish\"") &&
    Yes(Json.Num(1.7)) =?= Json.Num.parse("1.7") &&
    Yes(Json.Arr(Array(1.7, 1.8))) =?= Json.Arr.parse("[1.7, 1.8]") &&
    Yes(Json.Arr(Array(Json("fish")))) =?= Json.Arr.parse("[\"fish\"]") &&
    Yes(Json.Obj(Map("fish" -> Json(true)))) =?= Json.Obj.parse("{\"fish\": true}") &&
    Json.Null =?= Json.parse("null").yes &&
    Json.Bool.True =?= Json.parse("true").yes &&
    Json.Bool.False =?= Json.parse("false").yes &&
    Json.Str("fish") =?= Json.parse("\"fish\"").yes &&
    Seq("nul", "ull", "tru", "True", "rue", "als", "fals", "\"fish", "fish\"").map(Json.parse).collect{
      case Yes(js) => js
    } =?= Seq[Json]() &&
    Json.Num(18014398509481985L) =?= Json.parse("18014398509481985").yes &&
    Json.Num(1.9125881e13) =?= Json.parse("19.125881e12").yes &&
    Json.Num(BigDecimal("1234123412341234123412431234123412341234")) =?= Json.parse("1234123412341234123412431234123412341234").yes &&
    Json.Num(1.234123412341234E39) =?= Json.relaxed.parse("1234123412341234123412431234123412341234").yes &&
    Json.Arr.All(Array(Json("fish"), Json(2.7))) =?= Json.parse("[\"fish\", 27e-1]").yes &&
    Json.Obj(Map("fish" -> Json(2.7))) =?= Json.parse("{\"fish\": 0.27e1}").yes &&
    Json.Obj ~ ("fish", 2.0) ~ ("fish", 3.0) ~ Json.Obj =?= Json.parse("{\"fish\": 2, \"fish\": 3}").yes &&
    Json.Obj(Map("fish" -> (Json ~ Json("\n\n\n\n") ~ 2.7 ~ true ~ Json))) =?= Json.parse("{\"fish\":[\"\\n\\n\\n\\n\", 2.7, true]}").yes &&
    classOf[Json.Arr.Dbl] =?= Json.parse("[-0.18873, -0.17394, null, -0.14907, -0.0905366, -0.009166, 0.0772224]").yes.getClass 
  }

  def test_specifics_CharBuffer: Boolean = {
    import JsonConverters._
    implicit class StringToCharBuffer(private val string: String) { def cb: CharBuffer = CharBuffer.wrap(string.toCharArray) }

    Yes(Json.Null) =?= Json.Null.parse("null".cb) &&
    Yes(Json.Bool.True) =?= Json.Bool.parse("true".cb) &&
    Yes(Json.Bool.False) =?= Json.Bool.parse("false".cb) &&
    Yes(Json.Str("fish")) =?= Json.Str.parse("\"fish\"".cb) &&
    Yes(Json.Num(1.7)) =?= Json.Num.parse("1.7".cb) &&
    Yes(Json.Arr(Array(1.7, 1.8))) =?= Json.Arr.parse("[1.7, 1.8]".cb) &&
    Yes(Json.Arr(Array(Json("fish")))) =?= Json.Arr.parse("[\"fish\"]".cb) &&
    Yes(Json.Obj(Map("fish" -> Json(true)))) =?= Json.Obj.parse("{\"fish\": true}".cb) &&
    Json.Null =?= Json.parse("null".cb).yes &&
    Json.Bool.True =?= Json.parse("true".cb).yes &&
    Json.Bool.False =?= Json.parse("false".cb).yes &&
    Json.Str("fish") =?= Json.parse("\"fish\"".cb).yes &&
    Seq("nul", "ull", "tru", "True", "rue", "als", "fals", "\"fish", "fish\"").map(_.cb).map(Json.parse).collect{
      case Yes(js) => js
    } =?= Seq[Json]() &&
    Json.Num(18014398509481985L) =?= Json.parse("18014398509481985".cb).yes &&
    Json.Num(1.9125881e13) =?= Json.parse("19.125881e12".cb).yes &&
    Json.Num(BigDecimal("1234123412341234123412431234123412341234")) =?= Json.parse("1234123412341234123412431234123412341234".cb).yes &&
    Json.Num(1.234123412341234E39) =?= Json.relaxed.parse("1234123412341234123412431234123412341234".cb).yes &&
    Json.Arr.All(Array(Json("fish"), Json(2.7))) =?= Json.parse("[\"fish\", 27e-1]".cb).yes &&
    Json.Obj(Map("fish" -> Json(2.7))) =?= Json.parse("{\"fish\": 0.27e1}".cb).yes &&
    Json.Obj ~ ("fish", 2.0) ~ ("fish", 3.0) ~ Json.Obj =?= Json.parse("{\"fish\": 2, \"fish\": 3}".cb).yes &&
    Json.Obj(Map("fish" -> (Json ~ Json("\n\n\n\n") ~ 2.7 ~ true ~ Json))) =?= Json.parse("{\"fish\":[\"\\n\\n\\n\\n\", 2.7, true]}".cb).yes &&
    classOf[Json.Arr.Dbl] =?= Json.parse("[-0.18873, -0.17394, null, -0.14907, -0.0905366, -0.009166, 0.0772224]".cb).yes.getClass
  }

  def test_specifics_ByteBuffer: Boolean = {
    import JsonConverters._
    implicit class StringToCharBuffer(private val string: String) { def bb: ByteBuffer = ByteBuffer.wrap(string.getBytes) }

    Yes(Json.Null) =?= Json.Null.parse("null".bb) &&
    Yes(Json.Bool.True) =?= Json.Bool.parse("true".bb) &&
    Yes(Json.Bool.False) =?= Json.Bool.parse("false".bb) &&
    Yes(Json.Str("fish")) =?= Json.Str.parse("\"fish\"".bb) &&
    Yes(Json.Num(1.7)) =?= Json.Num.parse("1.7".bb) &&
    Yes(Json.Arr(Array(1.7, 1.8))) =?= Json.Arr.parse("[1.7, 1.8]".bb) &&
    Yes(Json.Arr(Array(Json("fish")))) =?= Json.Arr.parse("[\"fish\"]".bb) &&
    Yes(Json.Obj(Map("fish" -> Json(true)))) =?= Json.Obj.parse("{\"fish\": true}".bb) &&
    Json.Null =?= Json.parse("null".bb).yes &&
    Json.Bool.True =?= Json.parse("true".bb).yes &&
    Json.Bool.False =?= Json.parse("false".bb).yes &&
    Json.Str("fish") =?= Json.parse("\"fish\"".bb).yes &&
    Seq("nul", "ull", "tru", "True", "rue", "als", "fals", "\"fish", "fish\"").map(_.bb).map(Json.parse).collect{
      case Yes(js) => js
    } =?= Seq[Json]() &&
    Json.Num(18014398509481985L) =?= Json.parse("18014398509481985".bb).yes &&
    Json.Num(1.9125881e13) =?= Json.parse("19.125881e12".bb).yes &&
    Json.Num(BigDecimal("1234123412341234123412431234123412341234")) =?= Json.parse("1234123412341234123412431234123412341234".bb).yes &&
    Json.Num(1.234123412341234E39) =?= Json.relaxed.parse("1234123412341234123412431234123412341234".bb).yes &&
    Json.Arr.All(Array(Json("fish"), Json(2.7))) =?= Json.parse("[\"fish\", 27e-1]".bb).yes &&
    Json.Obj(Map("fish" -> Json(2.7))) =?= Json.parse("{\"fish\": 0.27e1}".bb).yes &&
    Json.Obj ~ ("fish", 2.0) ~ ("fish", 3.0) ~ Json.Obj =?= Json.parse("{\"fish\": 2, \"fish\": 3}".bb).yes &&
    Json.Obj(Map("fish" -> (Json ~ Json("\n\n\n\n") ~ 2.7 ~ true ~ Json))) =?= Json.parse("{\"fish\":[\"\\n\\n\\n\\n\", 2.7, true]}".bb).yes &&
    classOf[Json.Arr.Dbl] =?= Json.parse("[-0.18873, -0.17394, null, -0.14907, -0.0905366, -0.009166, 0.0772224]".bb).yes.getClass
  }

  def test_specifics_InputStream: Boolean = {
    import JsonConverters._
    implicit class StringToInputStream(private val string: String) {
      def is: java.io.InputStream = new java.io.ByteArrayInputStream(string.getBytes)
    }

    Yes(Json.Null) =?= Json.Null.parse("null".is) &&
    Yes(Json.Bool.True) =?= Json.Bool.parse("true".is) &&
    Yes(Json.Bool.False) =?= Json.Bool.parse("false".is) &&
    Yes(Json.Str("fish")) =?= Json.Str.parse("\"fish\"".is) &&
    Yes(Json.Num(1.7)) =?= Json.Num.parse("1.7".is) &&
    Yes(Json.Arr(Array(1.7, 1.8))) =?= Json.Arr.parse("[1.7, 1.8]".is) &&
    Yes(Json.Arr(Array(Json("fish")))) =?= Json.Arr.parse("[\"fish\"]".is) &&
    Yes(Json.Obj(Map("fish" -> Json(true)))) =?= Json.Obj.parse("{\"fish\": true}".is) &&
    Json.Null =?= Json.parse("null".is).yes &&
    Json.Bool.True =?= Json.parse("true".is).yes &&
    Json.Bool.False =?= Json.parse("false".is).yes &&
    Json.Str("fish") =?= Json.parse("\"fish\"".is).yes &&
    Seq("nul", "ull", "tru", "True", "rue", "als", "fals", "\"fish", "fish\"").map(_.is).map(Json.parse).collect{
      case Yes(js) => js
    } =?= Seq[Json]() &&
    Json.Num(18014398509481985L) =?= Json.parse("18014398509481985".is).yes &&
    Json.Num(1.9125881e13) =?= Json.parse("19.125881e12".is).yes &&
    Json.Num(BigDecimal("1234123412341234123412431234123412341234")) =?= Json.parse("1234123412341234123412431234123412341234".is).yes &&
    Json.Num(1.234123412341234E39) =?= Json.relaxed.parse("1234123412341234123412431234123412341234".is).yes &&
    Json.Arr.All(Array(Json("fish"), Json(2.7))) =?= Json.parse("[\"fish\", 27e-1]".is).yes &&
    Json.Obj(Map("fish" -> Json(2.7))) =?= Json.parse("{\"fish\": 0.27e1}".is).yes &&
    Json.Obj ~ ("fish", 2.0) ~ ("fish", 3.0) ~ Json.Obj =?= Json.parse("{\"fish\": 2, \"fish\": 3}".is).yes &&
    Json.Obj(Map("fish" -> (Json ~ Json("\n\n\n\n") ~ 2.7 ~ true ~ Json))) =?= Json.parse("{\"fish\":[\"\\n\\n\\n\\n\", 2.7, true]}".is).yes &&
    classOf[Json.Arr.Dbl] =?= Json.parse("[-0.18873, -0.17394, null, -0.14907, -0.0905366, -0.009166, 0.0772224]".is).yes.getClass
  }

  def test_numericStringEquals: Boolean = {
    def nsc(a: String, b: String, expect: Boolean = true) = {
      val ans = Json.Num.numericStringEquals(a, b)
      if (ans != expect) println(f"$a ${if (expect) "!" else "="}= $b")
      ans == expect
    }
    val zeros = Seq("0", "-0", "0.0000000", "-0.000000", "0e123", "-0e123", "0.0000000e-321", "-0.00000000e-321")
    val nonzeros = Seq("1", "0.0000000001", "-1", "-0.00000000001", "0.00000125e-129385718957", "-0.000151e9123751871")
    zeros.forall(z => zeros.forall(y => nsc(z, y))) &&
    nsc("1e-123456789123456789", "10.0000e-123456789123456790") &&
    nsc("0E-277906189","0E-277906189") /* issue 5 */ &&
    zeros.forall(z => nonzeros.forall(n => nsc(z,n,false) && nsc(n,z,false)))
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Jsonal_from_JUnit {
  @org.junit.Test
  def test() { Test_Jsonal.main(Array()) }
}
