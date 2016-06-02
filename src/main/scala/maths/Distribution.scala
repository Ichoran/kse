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
  def mutable = new EstM(n, mean, sse)
  def immutable = this match {
    case e: Est => e
    case _ => new Est(n, mean, sse)
  }
}

class EstM(var n: Int, var mean: Double, var sse: Double) extends Estimate {
  def this() = this(0, 0, 0)
  override def clone: EstM = new EstM(n, mean, sse)
  def ++(that: Estimate): EstM = clone ++= that
  def +:(x: Double): EstM = clone += x
  def :+(x: Double): EstM = clone += x
  def +=(x: Double): this.type = {
    val mold = mean
    mean = (n*mean + x)/(n+1)
    sse += (x - mean)*(x - mold)
    n += 1
    this
  }
  def ++=(that: Estimate): this.type = {
    val nold = n
    n += that.n
    val mold = mean
    mean = (nold*mean + that.mean*that.n)/n
    sse += that.sse + (mold - that.mean).sq*nold*that.n.toDouble / n
    this
  }
  override def toString = f"$mean +- $error (n=$n)"
}
object EstM {
  def apply() = new EstM(0, 0, 0)
  def apply(n: Int, mean: Double, sse: Double) = new EstM(n, mean, sse)
}

case class Est(n: Int, mean: Double, sse: Double) extends Estimate {
  override val sd = math.sqrt(variance)
  override val error = math.sqrt(errorSq)
}
object Est {
  def from(data: Array[Float], i0: Int, iN: Int): Est = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    while (i < iB) { cuml += data(i); i += 1 }
    val mean = if (iB > iA) cuml/i else 0.0
    cuml = 0.0
    i = iA
    while (i < iB) { cuml += (data(i) - mean).sq; i += 1 }
    new Est(iB - iA, mean, cuml)
  }
  def from(data: Array[Float]): Est = from(data, 0, data.length)
  def from(data: Array[Double], i0: Int, iN: Int): Est = {
    val iA = math.max(0, i0)
    val iB = math.max(iA, math.min(data.length, iN))
    var cuml = 0.0
    var i = iA
    while (i < iB) { cuml += data(i); i += 1 }
    val mean = if (iB > iA) cuml/i else 0.0
    cuml = 0.0
    i = iA
    while (i < iB) { cuml += (data(i) - mean).sq; i += 1 }
    new Est(iB - iA, mean, cuml)
  }
  def from(data: Array[Double]): Est = from(data, 0, data.length)
}
