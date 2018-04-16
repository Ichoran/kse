// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2018 Rex Kerr and Calico Labs.

package kse.maths

import kse.coll.packed._

package stats {
  object Parametric {
    def tTestPackedPD(ea: Est, eb: Est): Floatx2 = {
      val xa = ea.variance/ea.n
      val xb = eb.variance/eb.n
      val ss = xa + xb
      val t = (ea.mean - eb.mean)/math.sqrt(ss)
      val df = (ss*ss/(xa*xa/(ea.n - 1) + xb*xb/(eb.n - 1))).rint.toLong
      Floats((2*cdfStudentT(df, t)).toFloat, (ea.mean - eb.mean).toFloat)
    }
    def tTestP(ea: Est, eb: Est): Double = tTestPackedPD(ea, eb).f0.toDouble
    def tTestPD(ea: Est, eb: Est): (Double, Double) = { val pt = tTestPackedPD(ea, eb); (pt.f0.toDouble, pt.f1.toDouble) }

    def tTestPackedPD(a: Array[Double], b: Array[Double]): Floatx2 = tTestPackedPD(Est from a, Est from b)
    def tTestP(a: Array[Double], b: Array[Double]): Double = tTestP(Est from a, Est from b)
    def tTestPD(a: Array[Double], b: Array[Double]): (Double, Double) = tTestPD(Est from a, Est from b)
    
    def tTestPackedPD(a: Array[Float], b: Array[Float]): Floatx2 = tTestPackedPD(Est from a, Est from b)
    def tTestP(a: Array[Float], b: Array[Float]): Double = tTestP(Est from a, Est from b)
    def tTestPD(a: Array[Float], b: Array[Float]): (Double, Double) = tTestPD(Est from a, Est from b)
  }

  object Nonparametric {
    /** Returns both an estimate of the p-value and the signed rank biserial correlation
      * (with negative values indicating that the first data set is lower than the second)
      */
    private def rankTestEncoded(ab: Array[Double], na: Int): Floatx2 = {
      val nb = ab.length - na
      java.util.Arrays.sort(ab)
      var ra, rb = 0.0
      var i = 0
      var t3_t = 0.0
      while (i < ab.length) {
        val x = ab(i).bits
        var j = i+1
        var same = true
        var n = 1 - (x & 1).toInt
        var m = j
        while (same && j < ab.length) {
          val y = ab(j).bits
          same = ((x ^ y) | 0x1L) == 0x1L
          if (same) {
            n += 1 - (y & 1).toInt
            j += 1
            m += j
          }
        }
        if (j == i+1) {
          if (n != 0) ra += j
          else        rb += j
        }
        else {
          val tied = j - i
          t3_t += tied*(tied*tied.toDouble - 1)
          val avg = m.toDouble/tied
          ra += avg * n
          rb += avg * (tied - n)
        }
        i = j
      }
      val u = na*(nb + (na+1).toDouble/2) - ra
      val mid = na*0.5*nb
      val correction = if (t3_t > 0) t3_t/((na+nb)*(na+nb-1)) else 0
      val dev = (mid*(na + nb + 1 - correction)/6).sqrt
      val z = math.abs(u - mid)/dev
      val p = 2*cdfNormal(-z)
      val rbc = {
        // Kind of silly to remove signs and then add them back again, but
        // it's easier to follow textbook/Wikipedia math that way
        val unsign = 1 - (if (u > mid) 2*mid - u else u)/mid
        if (ra < rb)     -unsign
        else if (ra > rb) unsign
        else              0
      }
      Floats(p.toFloat, rbc.toFloat)
    }
    def rankTestPackedPR(a: Array[Double], b: Array[Double]): Floatx2 = {
      val c = new Array[Double](a.length + b.length)
      var i = 0
      while (i < a.length) {
        c(i) = (a(i).bits & 0xFFFFFFFFFFFFFFFEL).binary64
        i += 1
      }
      while (i < c.length) {
        c(i) = (b(i - a.length).bits | 0x1).binary64
        i += 1
      }
      rankTestEncoded(c, a.length)
    }
    def rankTestPackedPR(a: Array[Float], b: Array[Float]): Floatx2 = {
      val c = new Array[Double](a.length + b.length)
      var i = 0
      while (i < a.length) {
        c(i) = (a(i).toDouble.bits & 0xFFFFFFFFFFFFFFFEL).binary64
        i += 1
      }
      while (i < c.length) {
        c(i) = (b(i - a.length).toDouble.bits | 0x1).binary64
        i += 1
      }
      rankTestEncoded(c, a.length)
    }
    def rankTestP(a: Array[Double], b: Array[Double]): Double = rankTestPackedPR(a, b).f0.toDouble
    def rankTestP(a: Array[Float], b: Array[Float]): Double = rankTestPackedPR(a, b).f0.toDouble
    def rankTestR(a: Array[Double], b: Array[Double]): Double = rankTestPackedPR(a, b).f1.toDouble
    def rankTestR(a: Array[Float], b: Array[Float]): Double = rankTestPackedPR(a, b).f1.toDouble
    def rankTestPR(a: Array[Double], b: Array[Double]): (Double, Double) = { val pr = rankTestPackedPR(a, b); (pr.f0.toDouble, pr.f1.toDouble) }
    def rankTestPR(a: Array[Float], b: Array[Float]): (Double, Double) = { val pr = rankTestPackedPR(a, b); (pr.f0.toDouble, pr.f1.toDouble) }
  }
}
package object stats {
  implicit class DoubleStatisticalTesting(private val underlying: Array[Double]) extends AnyVal {
    def tTestP(that: Array[Double]): Double = Parametric.tTestP(underlying, that)
    def rankTestP(that: Array[Double]): Double = Nonparametric.rankTestP(underlying, that)
    def rankTestR(that: Array[Double]): Double = Nonparametric.rankTestR(underlying, that)
    def rankTestPR(that: Array[Double]): (Double, Double) = Nonparametric.rankTestPR(underlying, that)
    def rankTestPackedPR(that: Array[Double]): Floatx2 = Nonparametric.rankTestPackedPR(underlying, that)
    def est = Est from underlying
  }
  implicit class FloatStatisticalTesting(private val underlying: Array[Float]) extends AnyVal {
    def tTestP(that: Array[Float]): Double = Parametric.tTestP(underlying, that)
    def rankTestP(that: Array[Float]): Double = Nonparametric.rankTestP(underlying, that)
    def rankTestR(that: Array[Float]): Double = Nonparametric.rankTestR(underlying, that)
    def rankTestPR(that: Array[Float]): (Double, Double) = Nonparametric.rankTestPR(underlying, that)
    def rankTestPackedPR(that: Array[Float]): Floatx2 = Nonparametric.rankTestPackedPR(underlying, that)
    def est = Est from underlying
  }
}
