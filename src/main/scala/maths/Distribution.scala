// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-16 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse.maths
package stats

import scala.math._

import kse.flow._

trait Accumulates[@specialized(Float, Double, Int) A, @specialized(Double) B] {
  def reset(): this.type
  def +=(a: A): this.type
  def result: B
}

final class Mean(var n: Int, var mean: Double) extends Accumulates[Double, Double] {
  def reset(): this.type = { n = 0; mean = 0; this }
  def +=(x: Double): this.type = { mean = (n*mean + x)/(n+1); n += 1; this }
  def result: Double = mean
}

trait Deviable {
  def value: Double
  def error: Double
  def errorSq: Double
  def coev: Double
  def neg: Dev = new Dev(-value, error)
  def inv: Dev = { val r = 1.0/value; new Dev(r, r*coev) }
  def +(that: Deviable): Dev = new Dev(value + that.value, (errorSq + that.errorSq).sqrt)
  def -(that: Deviable): Dev = new Dev(value - that.value, (errorSq + that.errorSq).sqrt)
  def *(that: Deviable): Dev = new Dev(value * that.value, value * that.value * (coev.sq + that.coev.sq).sqrt)
  def /(that: Deviable): Dev = { val f = value / that.value; new Dev(f, f * (coev.sq + that.coev.sq).sqrt) }
  def abs: Dev = new Dev(math.abs(value), error)
  def sqrt: Dev = { val r = math.sqrt(value); new Dev(r, r*coev*0.5) }
  def sq: Dev = new Dev(value * value, value * error)
}
object Deviable {
  def zero: Deviable = Dev.zero
}

case class Dev(value: Double, error: Double) extends Deviable {
  def errorSq = error*error
  def coev = if (value != 0 || error != 0) error/value else 0.0
}
object Dev {
  val zero = new Dev(0, 0)
}


trait Estimate extends Deviable {
  def n: Int
  def mean: Double
  def sse: Double
  final def value = mean
  def =~=(that: Estimate): Boolean = (n == that.n) && (mean closeTo that.mean) && (sse closeTo that.sse)
  def variance = sse / (if (n > 2) n-1 else 1)
  def sd = math.sqrt(variance)
  def errorSq = sse / (if (n > 1) n.toDouble*(n-1) else 1)
  def error = math.sqrt(errorSq)
  def coev = error / mean
}

abstract class MutableEstimate(var n: Int, var mean: Double, var sse: Double) extends Estimate {
  final protected def incorporate(x: Double) {
    val mold = mean
    mean = (n*mean + x)/(n+1)
    sse += (x - mean)*(x - mold)
    n += 1
  }
  final protected def incorporate(that: Estimate) {
    val nold = n
    n += that.n
    val mold = mean
    mean = (nold*mean + that.mean*that.n)/n
    sse += that.sse + (mold - that.mean).sq*nold*that.n.toDouble / n    
  }
  final protected def disincorporate(x: Double) {
    val mold = mean
    mean = if (n == 1) mean - x else (n*mean - x)/(n-1)
    sse -= (x - mold)*(x - mean)
    n -= 1
  }
}

final class EstM(n0: Int, mean0: Double, sse0: Double) extends MutableEstimate(n0, mean0, sse0) with Accumulates[Double, Double] {
  def this() = this(0, 0, 0)
  override def clone: EstM = new EstM(n, mean, sse)
  def reset(): this.type = { n = 0; mean = 0; sse = 0; this }
  def result: Double = mean
  def immutable = new Est(n, mean, sse)
  def sdToErrorInPlace: this.type = { if (n > 1) sse *= n; this }
  def errorToSDInPlace: this.type = { if (n > 1) sse /= n; this }
  def ++(that: Estimate): EstM = clone ++= that
  def +:(x: Double): EstM = if (!x.nan) this else clone += x
  def :+(x: Double): EstM = if (!x.nan) this else clone += x
  def +=(x: Double): this.type = { if (!x.nan) incorporate(x); this }
  def ++=(that: Estimate): this.type = { incorporate(that); this }
  def ++=(xs: Array[Double]): this.type = {
    var i = 0
    while (i < xs.length) { this += xs(i); i += 1 }
    this
  }
  def ++=(xs: Array[Float]): this.type = {
    var i = 0
    while (i < xs.length) { this += xs(i); i += 1 }
    this
  }
  def -=(x: Double): this.type = { if (!x.nan) disincorporate(x); this }
  override def toString = f"$mean +- $error (n=$n)"
}
object EstM {
  def empty = new EstM(0, 0, 0)
  def apply(): EstM = new EstM(0, 0, 0)
  def apply(n: Int, mean: Double, sse: Double): EstM = new EstM(n, mean, sse)
}

final case class Est(n: Int, mean: Double, sse: Double) extends Estimate {
  def sdToError = if (n <= 1) this else new Est(n, mean, sse*n)
  def errorToSD = if (n <= 1) this else new Est(n, mean, sse/n)
  def mutable = new EstM(n, mean, sse)
  def +:(x: Double): Est = (mutable += x).immutable
  def :+(x: Double): Est = (mutable += x).immutable
  def ++(that: Estimate): Est = (mutable ++= that).immutable
  override val sd = math.sqrt(variance)
  override val error = math.sqrt(errorSq)
}
object Est {
  val empty = new Est(0, 0, 0)

  def from(data: Array[Float], i0: Int, iN: Int): Est = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    var n = 0
    while (i < iB) { val x = data(i); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    cuml = 0.0
    i = iA
    while (i < iB) { val x = data(i); if (!x.nan) { cuml += (x - mean).sq }; i += 1 }
    new Est(n, mean, cuml)
  }
  def from(data: Array[Float]): Est = from(data, 0, data.length)

  def from(data: Array[Double], i0: Int, iN: Int): Est = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    var n = 0
    while (i < iB) { val x = data(i); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    cuml = 0.0
    i = iA
    while (i < iB) { val x = data(i); if (!x.nan) { cuml += (x - mean).sq }; i += 1 }
    new Est(n, mean, cuml)
  }
  def from(data: Array[Double]): Est = from(data, 0, data.length)

  def boxed(data: TraversableOnce[Double]): Est = {
    val em = new EstM()
    data.foreach{ d => em += d }
    em.immutable
  }
  def boxed[A](data: TraversableOnce[Float])(implicit ev: A =:= Float): Est = {
    val em = new EstM()
    data.foreach{ f => em += f }
    em.immutable
  }

  def bayes(i: Int, j: Int) = {
    val n = i+j
    new Est(n, (i+1.0)/(n+2.0), ((i+1.0)*(j+1.0)/(n+3.0))*(math.max(1, n-1)/(n+2.0).sq))
  }
  def unbayes(e: Est): Option[(Int, Int)] =
    if (e.mean < 0 || e.mean > 1) None
    else if (e.n <= 0) Some((0, 0))
    else {
      val i = ((e.mean*(e.n+2.0))-1).rint.toInt
      Some((i, e.n-i))
    }
}

trait Extremal extends Estimate {
  def max: Double
  def min: Double
  def central: Estimate
}

final class EstXM(n0: Int, mean0: Double, sse0: Double, var min: Double, var max: Double)
extends MutableEstimate(n0, mean0, sse0) with Extremal {
  def this() = this(0, 0, 0, 0 , 0)
  override def clone: EstXM = new EstXM(n, mean, sse, min, max)
  def immutable = new EstX(n, mean, sse, min, max)
  def central = new EstM(n, mean, sse)
  def ++(that: Estimate): EstM = central ++= that
  def ++(that: Extremal): EstXM = clone ++= that
  def +:(x: Double): EstXM = if (!x.nan) this else clone += x
  def :+(x: Double): EstXM = if (!x.nan) this else clone += x
  def +=(x: Double): this.type = {
    if (!x.nan) {
      if (n < 1) { min = x; max = x }
      else if (x > max) max = x
      else if (x < min) min = x
      incorporate(x)
    }
    this
  }
  def ++=(that: Estimate): EstM = central ++= that
  def ++=(that: Extremal): this.type = {
    if (n < 1) { min = that.min; max = that.max }
    else if (that.n > 0) { min = math.min(min, that.min); max = math.max(max, that.max) }
    incorporate(that)
    this
  }
  override def toString = f"$mean +- $error (min=$min, max=$max, n=$n)"
}
object EstXM {
  def apply(): EstXM = new EstXM(0, 0, 0, 0, 0)
  def apply(n: Int, mean: Double, sse: Double, min: Double, max: Double): EstXM = new EstXM(n, mean, sse, min, max)
}

final case class EstX(n: Int, mean: Double, sse: Double, min: Double, max: Double) extends Extremal {
  def mutable = new EstXM(n, mean, sse, min, max)
  def central = new Est(n, mean, sse)
  def +:(x: Double): EstX = (mutable += x).immutable
  def :+(x: Double): EstX = (mutable += x).immutable
  def ++(that: Estimate): Est = (mutable ++= that).immutable
  def ++(that: Extremal): EstX = (mutable ++= that).immutable
  override val sd = math.sqrt(variance)
  override val error = math.sqrt(errorSq)
}
object EstX {
  def from(data: Array[Float], i0: Int, iN: Int): EstX = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    var n = 0
    while (i < iB) { val x = data(i); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    if (n < 2) return EstX(n, mean, 0, mean, mean)
    cuml = 0.0
    i = iA
    var min, max = mean
    while (i < iB) {
      val x = data(i)
      if (!x.nan) { 
        cuml += (x - mean).sq
        if (x > max) max = x
        else if (x < min) min = x
      }
      i += 1
    }
    new EstX(n, mean, cuml, min, max)
  }
  def from(data: Array[Float]): EstX = from(data, 0, data.length)
  def from(data: Array[Double], i0: Int, iN: Int): EstX = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    var n = 0
    while (i < iB) { val x = data(i); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    if (n < 2) return EstX(n, mean, 0, mean, mean)
    cuml = 0.0
    i = iA
    var min, max = mean
    while (i < iB) {
      val x = data(i)
      if (!x.nan) { 
        cuml += (x - mean).sq
        if (x > max) max = x
        else if (x < min) min = x
      }
      i += 1
    }
    new EstX(n, mean, cuml, min, max)
  }
  def from(data: Array[Double]): EstX = from(data, 0, data.length)
}

trait Quantile extends Estimate {
  // The estimated value of something at fractional rank `p` (p ranges from 0 to 1)
  def valueAt(p: Double): Double

  // The estimated fractional rank (0-1) of x if added to this Quantile.
  def rankOf(x: Double): Double

  def median: Double = valueAt(0.5)
  def fwhm: Double = valueAt(0.75) - valueAt(0.25)
}

trait Histographic extends Quantile with Extremal {
  def borders: Array[Double]
  def binOf(x: Double): Int = {
    if (borders.length == 0) return -1
    var lo = 0
    var hi = borders.length - 1
    while (hi-lo > 1) {
      val m = (hi+lo)/2
      if (borders(m) < x) lo = m
      else hi = m
    }
    if (x < lo) lo-1
    else if (x < hi) lo
    else hi
  }
}

class HistM(val borders: Array[Double]) extends Histographic with Extremal {
  private val gauss = new EstXM()
  private val tooLow, tooHigh = new EstM()
  private val counts = new Array[Int](math.max(0, borders.length - 1))
  private var cumuls: Array[Int] = null
  private var cumulThrough = -1
  private var myNans = 0
  def max = gauss.max
  def min = gauss.min
  def central = gauss.central
  def n = gauss.n
  def mean = gauss.mean
  def sse = gauss.sse
  def nans = myNans
  def +=(x: Double): this.type = {
    if (x.nan) myNans += 1
    else {
      gauss += x
      val i = binOf(x)
      cumulThrough = math.min(cumulThrough, i)
      if (i < 0) tooLow += x
      else if (i > counts.length) tooHigh += x
      else counts(i) += 1
    }
    this
  }
  def lobin: Est = tooLow.immutable
  def hibin: Est = tooHigh.immutable
  def slot(i: Int): Int = 
    if (i < 0) tooLow.n
    else if (i >= counts.length) tooHigh.n
    else counts(i)
  def rankOf(x: Double): Double = {
    val i = binOf(x)
    if (i < 0 || i > counts.length-1) Double.NaN
    else {
      if (i-1 > cumulThrough) {
        if (cumuls eq null) cumuls = new Array[Int](counts.length)
        if (cumulThrough < 0) {
          cumuls(0) = tooLow.n + counts(0)
          cumulThrough = 0
        }
        var j = cumulThrough+1
        while (j < i) {
          cumuls(j) = cumuls(j-1) + counts(j)
          j += 1
        }
        cumulThrough = i-1
      }
      val below = cumuls(i-1)
      val partial = counts(i) * (if (borders(i+1) > borders(i)) (x - borders(i))/(borders(i+1) - borders(i)) else 1.0)
      (below + partial + 0.5)/(1+gauss.n)
    }
  }
  def valueAt(p: Double): Double = {
    if (borders.length < 1 || gauss.n == 0 || p*gauss.n < tooLow.n || (1-p)*gauss.n < tooHigh.n) Double.NaN
    else {
      val np = p*gauss.n
      val i = {
        if (cumulThrough < 0 || cumuls(cumulThrough) < np) {
          if (cumuls eq null) cumuls = new Array[Int](counts.length)
          if (cumulThrough < 0) {
            cumuls(0) = tooLow.n + counts(0)
            cumulThrough = 0
          }
          while (cumuls(cumulThrough) < np && cumulThrough+1 < counts.length) {
            cumuls(cumulThrough+1) = cumuls(cumulThrough) + counts(cumulThrough+1)
            cumulThrough += 1
          }
          cumulThrough
        }
        else {
          var lo = 0
          var hi = cumulThrough
          while (hi - lo > 1) {
            val m = (hi + lo)/2
            if (cumuls(m) >= np) hi = m
            else lo = m
          }
          if (cumuls(lo) >= np) lo else hi
        }
      }

      val frac = (cumuls(i) - np) / math.max(1,counts(i))
      val a = borders(i)
      val b = borders(i+1)
      (frac*a + (1-frac)*b).clip(a, b)
    }
  }
}
