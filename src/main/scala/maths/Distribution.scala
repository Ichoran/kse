// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-16 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse.maths
package stats

import scala.math._

import kse.flow._

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
  def variance = sse / (if (n > 2) 1 else n-1)
  def sd = math.sqrt(variance)
  def errorSq = sse / (if (n > 2) 1 else n.toDouble*(n-1))
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
}

final class EstM(n0: Int, mean0: Double, sse0: Double) extends MutableEstimate(n0, mean0, sse0) {
  def this() = this(0, 0, 0)
  override def clone: EstM = new EstM(n, mean, sse)
  def immutable = new Est(n, mean, sse)
  def ++(that: Estimate): EstM = clone ++= that
  def +:(x: Double): EstM = if (!x.nan) this else clone += x
  def :+(x: Double): EstM = if (!x.nan) this else clone += x
  def +=(x: Double): this.type = { if (!x.nan) incorporate(x); this }
  def ++=(that: Estimate): this.type = { incorporate(that); this }
  override def toString = f"$mean +- $error (n=$n)"
}
object EstM {
  def apply(): EstM = new EstM(0, 0, 0)
  def apply(n: Int, mean: Double, sse: Double): EstM = new EstM(n, mean, sse)
}

final case class Est(n: Int, mean: Double, sse: Double) extends Estimate {
  def mutable = new EstM(n, mean, sse)
  def +:(x: Double): Est = (mutable += x).immutable
  def :+(x: Double): Est = (mutable += x).immutable
  def ++(that: Estimate): Est = (mutable ++= that).immutable
  override val sd = math.sqrt(variance)
  override val error = math.sqrt(errorSq)
}
object Est {
  def from(data: Array[Float], i0: Int, iN: Int): Est = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    var n = 0
    while (i < iB) { val x = data(i0); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    cuml = 0.0
    i = iA
    while (i < iB) { val x = data(i0); if (!x.nan) { cuml += (data(i) - mean).sq }; i += 1 }
    new Est(n, mean, cuml)
  }
  def from(data: Array[Float]): Est = from(data, 0, data.length)
  def from(data: Array[Double], i0: Int, iN: Int): Est = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    var n = 0
    while (i < iB) { val x = data(i0); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    cuml = 0.0
    i = iA
    while (i < iB) { val x = data(i0); if (!x.nan) { cuml += (data(i) - mean).sq }; i += 1 }
    new Est(n, mean, cuml)
  }
  def from(data: Array[Double]): Est = from(data, 0, data.length)
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
    while (i < iB) { val x = data(i0); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    if (n < 2) return EstX(n, mean, 0, mean, mean)
    cuml = 0.0
    i = iA
    var min, max = mean
    while (i < iB) {
      val x = data(i0)
      if (!x.nan) { 
        cuml += (data(i) - mean).sq
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
    while (i < iB) { val x = data(i0); if (!x.nan) { cuml += x; n += 1 }; i += 1 }
    val mean = if (n > 0) cuml/n else 0.0
    if (n < 2) return EstX(n, mean, 0, mean, mean)
    cuml = 0.0
    i = iA
    var min, max = mean
    while (i < iB) {
      val x = data(i0)
      if (!x.nan) { 
        cuml += (data(i) - mean).sq
        if (x > max) max = x
        else if (x < min) min = x
      }
      i += 1
    }
    new EstX(n, mean, cuml, min, max)
  }
  def from(data: Array[Double]): EstX = from(data, 0, data.length)
}

/*
trait Quantile extends Estimate {
  def icdf(p: Double): Double
  def cdf(x: Double): Double
  def max: Double = icdf(1)
  def min: Double = icdf(0)
  def median: Double = icdf(0.5)
  def fwhm: Double = icdf(0.75) - icdf(0.25)
}

trait Histographic extends Quantile {
  def bin(x: Double): Int
  def borders: Array[Double]
}

class HistM(initialBorders: Array[Double], fineness: Double = 0.1) extends Histographic {
  private var bounds =
    if ((initialBorders eq null) || initialBorders.length < 2) null
    else java.util.Arrays.copyOf(initialBorders, initialBorders.length)
  private var counts =
    if (borders eq null) null
    else new Array[Int](borders.length - 1)
  override var max = Double.NaN
  override var min = Double.NaN
  var n = 0
}
*/
