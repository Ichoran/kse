// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2018 Rex Kerr and Calico Labs.

package kse.maths

import kse.coll.packed._

package stats {
  object Parametric {
    def tTestP(ea: Est, eb: Est): Double = {
      val xa = ea.variance/ea.n
      val xb = eb.variance/eb.n
      val ss = xa + xb
      val t = (ea.mean - eb.mean)/math.sqrt(ss)
      val df = (ss*ss/(xa*xa/(ea.n - 1) + xb*xb/(eb.n - 1))).rint.toLong
      2*cdfStudentT(df, t)
    }
    def tTestP(a: Array[Double], b: Array[Double]): Double = tTestP(Est from a, Est from b)
    def tTestP(a: Array[Float], b: Array[Float]): Double = tTestP(Est from a, Est from b)
  }

  object Nonparametric {
    private def rankTestEncoded(ab: Array[Double], na: Int): Double = {
      val nb = ab.length - na
      java.util.Arrays.sort(ab)
      var ra = 0.0
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
            n += 1 - (x & 1).toInt
            j += 1
            m += j
          }
        }
        if (j == i+1) {
          if (n != 0) ra += j
        }
        else {
          val tied = j - i
          t3_t = tied*(tied*tied.toDouble - 1)
          ra += n*m.toDouble/tied
        }
        i = j
      }
      val u = ra - na*(na-1).toDouble/2
      val mid = na*0.5*nb
      val correction = if (t3_t > 0) t3_t/((na+nb)*(na+nb-1)) else 0
      val dev = (mid*(na + nb + 1 - correction)/6).sqrt
      val z = math.abs(u - mid)/dev
      2*cdfNormal(-z)
    }
    def rankTestP(a: Array[Double], b: Array[Double]): Double = {
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
    def rankTestP(a: Array[Float], b: Array[Float]): Double = {
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
  }
}
package object stats {
  implicit class DoubleStatisticalTesting(private val underlying: Array[Double]) extends AnyVal {
    def tTestP(that: Array[Double]): Double = Parametric.tTestP(underlying, that)
    def rankTestP(that: Array[Double]): Double = Nonparametric.rankTestP(underlying, that)
    def est = Est from underlying
  }
  implicit class FloatStatisticalTesting(private val underlying: Array[Float]) extends AnyVal {
    def tTestP(that: Array[Float]): Double = Parametric.tTestP(underlying, that)
    def rankTestP(that: Array[Float]): Double = Nonparametric.rankTestP(underlying, that)
    def est = Est from underlying
  }
}
