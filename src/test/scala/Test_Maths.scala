// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr and Calico Life Sciences

package kse.tests

import java.nio._

import kse.flow._
import kse.coll._
import kse.maths._

object Test_Maths extends Test_Kse {
  def test_finiteness: Boolean = {
    val r = new scala.util.Random(957815)
    val fs = Array.fill(16384){ r.nextInt(4) match {
      case 0 => r.nextInt(4) match { case 0 => Float.PositiveInfinity; case 1 => Float.NegativeInfinity; case _ => Float.NaN }
      case 1 => java.lang.Float.intBitsToFloat(0x7F800000 | r.nextInt)
      case 2 => r.nextDouble.toFloat
      case _ => java.lang.Float.intBitsToFloat(r.nextInt)
    }}
    val ds = Array.fill(fs.length){ r.nextInt(4) match {
      case 0 => r.nextInt(4) match { case 0 => Double.PositiveInfinity; case 1 => Double.NegativeInfinity; case _ => Double.NaN }
      case 1 => java.lang.Double.longBitsToDouble(0x7FF0000000000000L | r.nextInt)
      case 2 => r.nextDouble
      case _ => java.lang.Double.longBitsToDouble(r.nextLong)
    }}
    var i = 0
    while (i < fs.length) {
      val f = fs(i)
      if (f.finite != !(f.isNaN || f.isInfinite)) { println(f + " finiteness " + f.bits.hex); return false }
      if (f.nan != f.isNaN || f.inf != f.isInfinite) { println(f + " naninf " + f.bits.hex); return false }
      val d = ds(i)
      if (d.finite != !(d.isNaN || d.isInfinite)) { println(d + " finiteness " + d.bits.hex); return false }
      if (d.nan != d.isNaN || d.inf != d.isInfinite) { println(d + " naninf " + d.bits.hex); return false }
      i += 1
    }
    true
  }

  def test_error_estimate: Boolean = {
    val r = new scala.util.Random(8715351)
    val x = Array.fill(24){ r.nextDouble }
    val xm = x.sum / x.length
    val xs = x.map(_ - xm).map(_.sq).sum
    val xhi = x.max
    val xlo = x.min
    (1 until 24).forall{ k =>
      def is(a: Double, b: Double, what: String = ""): Boolean = {
        val diff = (a-b).abs
        if (diff < 1e-12) true else { println(f"$k $a $b ${x.mkString(",")} $what"); false } 
      }
      val a = x.take(k)
      val b = x.drop(k)
      val ex = stats.Est from x
      val ea = stats.Est from a
      val eb = stats.Est from b
      val e1 = stats.EstM(); x.foreach{ e1 += _ }
      val e2 = ea.mutable; b.foreach{ e2 += _ }
      val e3 = ea.mutable; e3 ++= eb
      val e4 = eb.mutable; e4 ++= ea
      val fx = stats.EstX from x
      val f1 = stats.EstXM(); x.foreach{ f1 += _ }
      val f2 = stats.EstXM(); f2 ++= (stats.EstX from a); f2 ++= (stats.EstX from b) 
      e1.n =?= e2.n && e1.n =?= e3.n && e1.n =?= e4.n && e1.n =?= ex.n && e1.n =?= x.length &&
      is(e1.mean, e2.mean, "m12") && is(e1.mean, e3.mean, "m13") && is(e1.mean, e4.mean, "m14") && is(e1.mean, ex.mean) && is(e1.mean, xm, "m") &&
      is(e1.sse, e2.sse, "s12") && is(e1.sse, e3.sse, "s13") && is(e1.sse, e4.sse, "s14") && is(e1.sse, ex.sse) && is(e1.sse, xs, "s") &&
      e1.n =?= fx.n && e1.n =?= f1.n && e1.n =?= f2.n &&
      is(e1.mean, fx.mean, "fxm") && is(e1.mean, f1.mean, "f1m") && is(e1.mean, f2.mean, "f2m") &&
      is(e1.sse, fx.sse, "fxe") && is(e1.sse, f1.sse, "f1e") && is(e1.sse, f2.sse, "f2e") &&
      is(xhi, fx.max, "fx^") && is(fx.max, f1.max, "f1^") && is(fx.max, f2.max, "f2^") &&
      is(xlo, fx.min, "fx_") && is(fx.min, f1.min, "f1_") && is(fx.min, f2.min, "f2_")
    }
  }

  def test_histm: Boolean = {
    val h = new stats.HistM(Array(0, 1, 2, 3, 4))
    val data = Array(0.5, 1.3, 1.7, 2.1, 2.4, 2.5, 3.8, 3.9)
    val e = stats.Est from data
    data.foreach(h += _)
    h.n == e.n && (h.mean - e.mean).abs < 1e-12 && (h.error - e.error).abs < 1e-12 && (h.sd - e.sd).abs < 1e-12 &&
    h.valueAt(0.5) == h.median && h.median.in(2.1, 2.9) &&
    h.rankOf(1).in(1.0/8, 2.0/8) && h.slot(1) == 2 && h.binOf(3.1) == 3
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Maths_from_JUnit {
  @org.junit.Test
  def test() { Test_Maths.main(Array()) }
}
