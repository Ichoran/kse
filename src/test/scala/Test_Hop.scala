package kse.tests

import scala.util._

import kse.flow._

object Test_Hop extends Test_Kse {
  def test_oopsThrowingRealException = Try{ OOPS(oopsThrowingRealException) } match { case Failure(t: OopsException) => true; case _ => false }

  def test_OOPS = {
    implicit val o = oopsThrowingRealException
    Try { OOPS } match { case Failure(t: OopsException) => true; case _ => false }
  }

  def test_tapOops = {
    var count = 0
    var effect = false
    var affect = false
    implicit val o = {
      var temp: Oops = null
      probably{ oops => temp = oops }  // Never do this!  For testing only!
      temp
    }
    try { tapOops{ count += 1 }{ affect = true } } catch { case t if t eq o => count += 2 }
    try { tapOops{ count += 4; OOPS }{ effect = true } } catch { case t if t eq o => count += 8 }
    count == 13 && effect && !affect
  }

  private val myHop = UnboundHopSupplier.of[String]

  def test_Hop: Boolean = {
    var p = try { myHop(""); true } catch { case t if myHop is t => false }
    val test0 = p =?= false
    def f: Hop[String] => Int = h => if (!p) 1 else h("frog")
    val test1 = myHop.hopless{ f(myHop) } =?= Yes(1)
    p = !p
    val test2 = myHop.hopless{ f(myHop) } =?= No("frog")
    test0 && test1 && test2
  }
  
  def test_HopKey: Boolean = {
    sealed trait Mark
    final class A extends Mark {}
    final class B extends Mark {}
    var p = false
    implicit val hop = UnboundHopSupplier.keyed[String, A]
    implicit val hip = UnboundHopSupplier.keyed[String, B]
    def set(implicit h: HopKey[String, A]) { if (h eq hop) p = true }
    set
    p
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
    probably{ implicit o =>
      oopsless{ "fish" } == Some("fish") && oopsless[String]{ OOPS } == None
    } == Some(true)
  }

  def test_oopslessOr = {
    probably{ implicit o =>
      oopslessOr("dog"){ "fish" } == "fish" && oopslessOr("dog"){ OOPS } == "dog"
    } == Some(true)
  }

  def test_oopslessOrNull = {
    probably{ implicit o =>
      oopslessOrNull{ "fish" } == "fish" && oopslessOrNull[String]{ OOPS } == null
    } == Some(true)
  }

  def test_oopslessTry = {
    probably{ implicit o =>
      var effect = false
      var affect = false
      !oopslessTry{ effect = true; OOPS; affect = true } && effect && !affect
    } == Some(true)
  }

  def test_probably = probably{ implicit oops => "fish" } == Some("fish") && probably[String]{ implicit oops => OOPS } == None

  def test_probablyOr = probablyOr("dog"){ implicit oops => "fish" } == "fish" && probablyOr("dog"){ implicit oops => OOPS } == "dog"

  def test_probablyOrNull = probablyOrNull{ implicit oops => "fish" } == "fish" && probablyOrNull[String]{ implicit oops => OOPS } == null

  def test_probablyTry = {
    var effect = false
    var affect = false
    !probablyTry{ implicit oops => effect = true; OOPS; affect = true } && effect && !affect
  }

  def test_probablyOne = {
    var effect = false
    var affect = false
    val answer: Unit = probablyOne(Seq[Oops => Unit](implicit oops => effect = true, implicit oops => OOPS, implicit oops => affect = true))(())
    effect && !affect
  }

  def test_hopOut =
    Seq("fish", "dog").map( x => hopOut[Int]( hop => if (x == "fish") 0 else hop(x.length) ) ) =?= Seq(0, 3) &&
    Seq("fish", "dog").map( x => hopOut[Long]( hop => if (x == "fish") -1L else hop(x.length.toLong << 33L) ) ) =?= Seq(-1, 3L << 33L) &&
    Seq("fish", "dog").map( x => hopOut[String]( hop => if (x == "fish") x else hop("dogfish") ) ) =?= Seq("fish", "dogfish")
  
  def test_hopOn =
    Seq("fish", "dog").map(x => hopOut[Int]{ implicit hop =>
      hopOn((s: String) => s.length)(hip => if (x=="fish") 0 else hip("dogfish"))
    }) =?= Seq(0, 7) &&
    Seq("fish", "dog").map(x => hopOut[Int]{ implicit hop =>
      hopOn((l: Long) => (l-1).toInt)(hip => if (x=="fish") 0 else hip(7L))
    }) =?= Seq(0, 6) &&
    Seq("fish", "dog").map(x => hopOut[Int]{ implicit hop =>
      hopOn((i: Int) => -i)(hip => if (x=="fish") 0 else hip(1))
    }) =?= Seq(0, -1) &&
    Seq("fish", "dog").map(x => hopOut[Long]{ implicit hop =>
      hopOn((s: String) => 2L+s.length)(hip => if (x=="fish") 0L else hip("dogfish"))
    }) =?= Seq(0L, 9L) &&
    Seq("fish", "dog").map(x => hopOut[Long]{ implicit hop =>
      hopOn((l: Long) => l-2)(hip => if (x=="fish") 0L else hip(7L))
    }) =?= Seq(0L, 5L) &&
    Seq("fish", "dog").map(x => hopOut[Long]{ implicit hop =>
      hopOn((i: Int) => -i-1L)(hip => if (x=="fish") 0L else hip(1))
    }) =?= Seq(0L, -2L) &&
    Seq("fish", "dog").map(x => hopOut[String]{ implicit hop =>
      hopOn((s: String) => s + "fish")(hip => if (x=="fish") x else hip("dog"))
    }) =?= Seq("fish", "dogfish") &&
    Seq("fish", "dog").map(x => hopOut[String]{ implicit hop =>
      hopOn((l: Long) => "dogfish".substring((l >>> 32).toInt, (l & 0xFFFFFFFFL).toInt))(hip => if (x=="fish") x else hip(5L))
    }) =?= Seq("fish", "dogfi") &&
    Seq("fish", "dog").map(x => hopOut[String]{ implicit hop =>
      hopOn((i: Int) => "fish"*i)(hip => if (x=="fish") x else hip(2))
    }) =?= Seq("fish", "fishfish")
  
  def test_hopKeyOnOut = {
    trait A {}
    trait B {}
    def hai(implicit hop: HopKey[Int, A]) { hop(1) }
    def hbi(implicit hop: HopKey[Int, B]) { hop(2) }
    def hal(implicit hop: HopKey[Long, A]) { hop(-1L) }
    def hbl(implicit hop: HopKey[Long, B]) { hop(1L << 33L) }
    def hao(implicit hop: HopKey[Object, A]) { hop('x) }
    def hbo(implicit hop: HopKey[Object, B]) { hop('yy) }
    Seq("fish", "dog").map(x => hopKeyOut[Int, A]{ implicit hop => hopKeyOut[Int, B]{ implicit hip => if (x=="fish") hai else hbi; 0 } }) =?= Seq(1,2) &&
    Seq("fish", "dog").map(x => hopKeyOut[Long, A]{ implicit hop => hopKeyOut[Long, B]{ implicit hip => if (x=="fish") hal else hbl; 0L } }) =?= Seq(-1L, 1L << 33L) &&
    Seq("fish", "dog").map(x => hopKeyOut[Object, A]{ implicit hop => hopKeyOut[Object, B]{ implicit hip => if (x=="fish") hao else hbo; "" } }) =?= Seq('x, 'yy) &&
    Seq("fish", "dog").map(x => hopKeyOut[Int, A]{ implicit hop => hopKeyOut[Int, B]{ implicit hup =>
      hopKeyOn[A]((o: Object) => o.toString.length){ implicit hip => hopKeyOn[B]((o: Object) => o.toString.length*2){ implicit hep =>
        if (x == "fish") hao else hbo; 0
      }}
    }}) =?= Seq(2, 6) &&
    Seq("fish", "dog").map(x => hopKeyOut[Int, A]{ implicit hop => hopKeyOut[Int, B]{ implicit hup =>
      hopKeyOn[A]((l: Long) => (l & 0xFFFFFFFFL).toInt){ implicit hip => hopKeyOn[B]((l: Long) => (l >>> 32).toInt){ implicit hep =>
        if (x == "fish") hal else hbl; 0
      }}
    }}) =?= Seq(-1, 2) &&
    Seq("fish", "dog").map(x => hopKeyOut[Int, A]{ implicit hop => hopKeyOut[Int, B]{ implicit hup =>
      hopKeyOn[A]((i: Int) => i-3){ implicit hop => hopKeyOn[B]((i: Int) => -i-2){ implicit hup =>
        if (x == "fish") hai else hbi; 0
      }}
    }}) =?= Seq(-2, -4) &&
    Seq("fish", "dog").map(x => hopKeyOut[Long, A]{ implicit hop => hopKeyOut[Long, B]{ implicit hup =>
      hopKeyOn[A]((o: Object) => o.toString.length.toLong << 2L){ implicit hip => hopKeyOn[B]((o: Object) => o.toString.length*2L){ implicit hep =>
        if (x == "fish") hao else hbo; 0L
      }}
    }}) =?= Seq(8L, 6L) &&
    Seq("fish", "dog").map(x => hopKeyOut[Long, A]{ implicit hop => hopKeyOut[Long, B]{ implicit hup =>
      hopKeyOn[A]((l: Long) => l+1){ implicit hop => hopKeyOn[B]((l: Long) => l >>> 31){ implicit hup =>
        if (x == "fish") hal else hbl; 0L
      }}
    }}) =?= Seq(0L, 4L) &&
    Seq("fish", "dog").map(x => hopKeyOut[Long, A]{ implicit hop => hopKeyOut[Long, B]{ implicit hup =>
      hopKeyOn[A]((i: Int) => i-3L){ implicit hip => hopKeyOn[B]((i: Int) => -i-2L){ implicit hep =>
        if (x == "fish") hai else hbi; 0L
      }}
    }}) =?= Seq(-2L, -4L) &&
    Seq("fish", "dog").map(x => hopKeyOut[Object, A]{ implicit hop => hopKeyOut[Object, B]{ implicit hup =>
      hopKeyOn[A]((o: Object) => (o.toString + "fish"): Object){ implicit hop => hopKeyOn[B]((o: Object) => o){ implicit hup =>
        if (x == "fish") hao else hbo; ""
      }}
    }}) =?= Seq("'xfish", 'yy) &&
    Seq("fish", "dog").map(x => hopKeyOut[Object, A]{ implicit hop => hopKeyOut[Object, B]{ implicit hup =>
      hopKeyOn[A]((l: Long) => l.toString: Object){ implicit hip => hopKeyOn[B]((l: Long) => (if (l > 0) 'x else 'z): Object){ implicit hep =>
        if (x == "fish") hal else hbl; ""
      }}
    }}) =?= Seq("-1", 'x) &&
    Seq("fish", "dog").map(x => hopKeyOut[Object, A]{ implicit hop => hopKeyOut[Object, B]{ implicit hup =>
      hopKeyOn[A]((i: Int) => "fish".charAt(i).toString: Object){ implicit hip => hopKeyOn[B]((i: Int) => Seq('a, 'b, 'c, 'd).apply(i): Object){ implicit hep =>
        if (x == "fish") hai else hbi; ""
      }}
    }}) =?= Seq("i", 'c)
  }
  
  def test_okay = okay[String]{ _ => "frog" } == Yes("frog") && okay[String]{ hop => hop("dog"); "frog" } == No("dog")
  
  def test_okayKey = {
    trait A {}
    trait B {}
    def has(implicit hop: HopKey[String, A]) = hop("frog")
    def hbs(implicit hop: HopKey[String, B]) = hop("dog")
    Seq(-1, 0, 1).map(i =>
      okayKey[String, A]{ implicit hop => okayKey[String, B]{ implicit hip => if (i < 0) has; if (i > 0) hbs; math.Pi }}.flatten
    ) =?= Seq(No("frog"), Yes(math.Pi), No("dog"))
  } 
  
  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Hop_from_JUnit {
  @org.junit.Test
  def test() { Test_Hop.main(Array()) }
}
