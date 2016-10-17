// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse.maths
package fits

import scala.math._

import kse.flow._

trait Interpolate {
  def inputDimension: Int
  def size: Int
  def parameters: Array[Double]
}

trait Interpolate1D extends Interpolate {
  final def inputDimension = 1
  def lower: Double
  def upper: Double
}

final class LinearInterp(in: Array[Double], out: Array[Double]) extends Interpolate1D {
  val size = min(in.length, out.length)
  val parameters = new Array[Double](2*size)
  System.arraycopy(in, 0, parameters, 0, size)
  System.arraycopy(out, 0, parameters, size, size)
  val lower = if (parameters.length > 0) parameters(0) else Double.NaN
  val upper = if (parameters.length > 0) parameters(parameters.length - 1) else Double.NaN
  def apply(a: Double): Double = {
    var i0 = 0
    var i1 = size - 1
    while (i0 + 1 < i1) {
      val i = (i0 + i1) >>> 1   // Avoids overflow, but we don't actually need to
      if (parameters(i) < a) i0 = i
      else i1 = i
    }
    val a0 = parameters(i0)
    val a1 = parameters(i1)
    if (a <= a0) parameters(size + i0)
    else if (a >= a1) parameters(size + i1)
    else {
      val z0 = parameters(size + i0)
      val z1 = parameters(size + i1)
      z0 + (z1 - z0)*(a - a0)/(a1 - a0)
    }
  }
}
object LinearInterp {
  def apply(in: Array[Double], out: Array[Double]) = new LinearInterp(in, out)
}

/** Marker trait so we know what fits we could have */
sealed trait Fit[F <: Fit[F]] extends scala.Cloneable {
  def samples: Long
  def clear: this.type
  override def clone(): F = ???
}   


/** FitTX performs a ordinary least squares fit of a parameter t against a readout x.
  * We assume t is accurate; this method is not precise if both t and x can vary.
  */
final class FitTX extends Fit[FitTX] {
  private[this] var Ot = 0.0
  private[this] var Ox = 0.0

  private[this] var n = 0L
  private[this] var St = 0.0
  private[this] var Stt = 0.0
  private[this] var Sx = 0.0
  private[this] var Sxx = 0.0
  private[this] var Stx = 0.0

  private[this] var cached = false
  private[this] var myAx = 0.0
  private[this] var myBx = 0.0
  private[this] var myE = 0.0


  private def setEverything(_Ot: Double, _Ox: Double, _n: Long, _St: Double, _Stt: Double, _Sx: Double, _Sxx: Double, _Stx: Double): this.type = {
    Ot = _Ot; Ox = _Ox;
    n = _n;
    St = _St; Stt = _Stt;
    Sx = _Sx; Sxx = _Sxx;
    Stx = _Stx;
    this
  }

  override def clone(): FitTX = {
    val that = new FitTX
    that.setEverything(Ot, Ox, n, St, Stt, Sx, Sxx, Stx)
  }


  private def compute() {
    if (n < 2) {
      myAx = Double.NaN
      myBx = Double.NaN
      myE = Double.NaN
    }
    else {
      val den = n*Stt - St*St
      myBx = if (den != 0) (n*Stx - St*Sx)/den else Double.NaN
      myAx = (Sx - myBx*St) / n
      myE = (n*Sxx - Sx*Sx - myBx*myBx*den) / n
    }
    cached = true
  }

  def alphaX = { if (!cached) compute(); myAx }
  def betaX = { if (!cached) compute(); myBx }
  def error = { if (!cached) compute(); myE }
  def onlyBetaX = if (n < 2) Double.NaN else Stx / Stt
  def onlyBetaError = if (n < 2) Double.NaN else Sxx - Stx*Stx/Stt
  def samples = n
  def meanT = if (n < 1) Double.NaN else St / n
  def meanX = if (n < 1) Double.NaN else Sx / n

  def apply(t: Double): Double = alphaX + betaX*(t - Ot) + Ox
  def xt(t: Double): Double = apply(t)

  /** An estimate of the probability that a particular x at a value t comes from the linear relationship
    * fit by this fitter.  If the true (or a superior) estimate of the variance is known, it can
    * substantially improve the accuracy of the estimate especially for small n
    */
  def p(t: Double, x: Double, knownSigmaSq: Double = Double.NaN) = if (n <= 2) Double.NaN else {
    if (knownSigmaSq.nan) cdfStudentT(n, (x - xt(t))/sqrt((error/(n-2))*(1.0/n + (n*t - St).sq/(n*(n*Stt - St*St)))))
    else cdfNormal((x - xt(t))/sqrt(knownSigmaSq*(1.0/n + (n*t - St).sq/(n*(n*Stt - St*St)))))
  }

  def xform(in: Array[Double], i0: Int = 0, iN: Int = Int.MaxValue) {
    val iM = math.min(in.length, iN)
    var i = i0
    val aX = alphaX
    val bX = betaX
    while (i < iM) {
      in(i) = aX + bX*(in(i) - Ot) + Ox
      i += 1
    }
  }

  def clear(): this.type = {
    cached = false
    n = 0
    St = 0
    Stt = 0
    Sx = 0
    Sxx = 0
    Stx = 0
    this    
  }

  def origin(ot: Double, ox: Double): this.type = {
    if (n > 0) {
      cached = false
      val dt = Ot - ot
      val dx = Ox - ox
      val ndt = n*dt
      val ndx = n*dx
      Stt += dt*(2*St + ndt)
      Sxx += dx*(2*Sx + ndx)
      Stx += dt*Sx + dx*(St + ndt)
      St += ndt
      Sx += ndx
    }
    Ot = ot
    Ox = ox
    this
  }

  def +=(ti: Double, xi: Double) {
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    n += 1
    St += t
    Stt += t*t
    Sx += x
    Sxx += x*x
    Stx += t*x
  }

  def -=(ti: Double, xi: Double) {
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    n -= 1
    St -= t
    Stt -= t*t
    Sx -= x
    Sxx -= x*x
    Stx -= t*x
  }

  override def toString = s"Fit: x = ${betaX.fmt()} t + ${xt(0).fmt()} (n=$n, rmse=${(sqrt(error)/n).fmt()})"
}
object FitTX {
  def apply(ts: Array[Double], xs: Array[Double], i0: Int, iN: Int): FitTX = {
    val fit = new FitTX
    if (iN > i0) {
      var i = i0
      fit.origin(ts(i), xs(i))
      while (i < iN) {
        fit += (ts(i), xs(i))
        i += 1
      }
      fit.origin(0, 0)
    }
    fit
  }

  def apply(ts: Array[Double], xs: Array[Double]): FitTX = apply(ts, xs, 0, min(ts.length, xs.length))

  def apply(tf: Int => Double, xf: Int => Double, i0: Int, iN: Int): FitTX = {
    val fit = new FitTX
    if (iN > i0) {
      var i = i0
      fit.origin(tf(i), xf(i))
      while (i < iN) {
        fit += (tf(i), xf(i))
        i += 1
      }
      fit.origin(0, 0)
    }
    fit
  }

  def apply(ts: Array[Float], xs: Array[Float], i0: Int, iN: Int): FitTX = apply(i => ts(i), i => xs(i), i0, iN)

  def apply(ts: Array[Float], xs: Array[Float]): FitTX = apply(ts, xs, 0, min(ts.length, xs.length))
}


/** FitTXY performs a ordinary least squares fit of a parameter t against two readouts x and y.
  * We assume t is accurate; this method is not precise if t has error as well as x and y.
  */
final class FitTXY extends Fit[FitTXY] {
  private[this] var Ot = 0.0
  private[this] var Ox = 0.0
  private[this] var Oy = 0.0

  private[this] var n = 0L
  private[this] var St = 0.0
  private[this] var Stt = 0.0
  private[this] var Sx = 0.0
  private[this] var Sxx = 0.0
  private[this] var Sy = 0.0
  private[this] var Syy = 0.0
  private[this] var Stx = 0.0
  private[this] var Sty = 0.0

  private[this] var cached = false
  private[this] var myAx = 0.0
  private[this] var myAy = 0.0
  private[this] var myBx = 0.0
  private[this] var myBy = 0.0
  private[this] var myE = 0.0


  private def setEverything(
    _Ot: Double, _Ox: Double, _n: Long,
    _St: Double, _Stt: Double,
    _Sx: Double, _Sxx: Double,
    _Sy: Double, _Syy: Double,
    _Stx: Double, _Sty: Double
  ): this.type = {
    Ot = _Ot; Ox = _Ox;
    n = _n;
    St = _St; Stt = _Stt;
    Sx = _Sx; Sxx = _Sxx;
    Sy = _Sy; Syy = _Syy;
    Stx = _Stx; Sty = _Sty;
    this
  }

  override def clone(): FitTXY = {
    val that = new FitTXY
    that.setEverything(Ot, Ox, n, St, Stt, Sx, Sxx, Sy, Syy, Stx, Sty)
  }


  private def compute() {
    if (n < 2) {
      myAx = Double.NaN
      myAy = Double.NaN
      myBx = Double.NaN
      myBy = Double.NaN
      myE = Double.NaN
    }
    else {
      val den = n*Stt - St*St
      myBx = if (den != 0) (n*Stx - St*Sx)/den else Double.NaN
      myBy = if (den != 0) (n*Sty - St*Sy)/den else Double.NaN
      myAx = (Sx - myBx*St) / n
      myAy = (Sy - myBy*St) / n
      myE = (n*(Sxx + Syy) - Sx*Sx - Sy*Sy - (myBx*myBx + myBy*myBy)*den) / n
    }
    cached = true
  }

  private def computeA() {
    if (n < 2) {
      myAx = Double.NaN
      myBx = Double.NaN
    }
    else {
      if (!cached) compute()
      myAx = (Sx - myBx*St) / n
      myAy 
    }
  }

  def alphaX = { if (!cached) compute(); myAx }
  def alphaY = { if (!cached) compute(); myAy }
  def betaX = { if (!cached) compute(); myBx }
  def betaY = { if (!cached) compute(); myBy }
  def error = { if (!cached) compute(); myE }
  def onlyBetaX = if (n < 2) Double.NaN else Stx / Stt
  def onlyBetaY = if (n < 2) Double.NaN else Sty / Stt
  def onlyBetaError = if (n < 2) Double.NaN else Sxx + Syy - (Stx*Stx + Sty*Sty)/Stt
  def samples = n
  def meanT = if (n < 1) Double.NaN else St / n
  def meanX = if (n < 1) Double.NaN else Sx / n
  def meanY = if (n < 1) Double.NaN else Sy / n

  def apply(t: Double): (Double, Double) = (alphaX + betaX*(t - Ot) + Ox, alphaY + betaY*(t - Ot) + Oy)
  def xt(t: Double): Double = alphaX + betaX*(t - Ot) + Ox
  def yt(t: Double): Double = alphaY + betaY*(t - Ot) + Oy
  def xform(in: Array[Double], i0: Int = 0, iN: Int = Int.MaxValue) {
    val iM = math.min(in.length, iN)
    var i = i0
    val aX = alphaX
    val bX = betaX
    while (i < iM) {
      in(i) = aX + bX*(in(i) - Ot) + Ox
      i += 1
    }
  }
  def yform(in: Array[Double], i0: Int = 0, iN: Int = Int.MaxValue) {
    val iM = math.min(in.length, iN)
    var i = i0
    val aY = alphaY
    val bY = betaY
    while (i < iM) {
      in(i) = aY + bY*(in(i) - Ot) + Ox
      i += 1
    }
  }

  def clear(): this.type = {
    cached = false
    n = 0
    St = 0
    Stt = 0
    Sx = 0
    Sxx = 0
    Sy = 0
    Syy = 0
    Stx = 0
    Sty = 0
    this
  }

  def origin(ot: Double, ox: Double, oy:Double): this.type = {
    if (n > 0) {
      cached = false
      val dt = Ot - ot
      val dx = Ox - ox
      val dy = Oy - oy
      val ndt = n*dt
      val ndx = n*dx
      val ndy = n*dy
      Stt += dt*(2*St + ndt)
      Sxx += dx*(2*Sx + ndx)
      Syy += dy*(2*Sy + ndy)
      Stx += dt*Sx + dx*(St + ndt)
      Sty += dt*Sy + dy*(St + ndt)
      St += ndt
      Sx += ndx
      Sy += ndy
    }
    Ot = ot
    Ox = ox
    this
  }

  def +=(ti: Double, xi: Double, yi: Double) {
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    val y = yi - Oy
    n += 1
    St += t
    Stt += t*t
    Sx += x
    Sxx += x*x
    Sy += y
    Syy += y*y
    Stx += t*x
    Sty += t*y
  }

  def -=(ti: Double, xi: Double, yi: Double) {
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    val y = yi - Oy
    n -= 1
    St -= t
    Stt -= t*t
    Sx -= x
    Sxx -= x*x
    Sy -= y
    Syy -= y*y
    Stx -= t*x
    Sty -= t*y
  }

  override def toString = s"Fit: x = ${betaX.fmt()} t + ${xt(0).fmt()}; y = ${betaY.fmt()} t + ${yt(0).fmt()} (n=$n, rmse=${(sqrt(error)/n).fmt()}"
}
object FitTXY {
  def apply(ts: Array[Double], xs: Array[Double], ys: Array[Double], i0: Int, iN: Int): FitTXY = {
    val fit = new FitTXY
    if (iN > i0) {
      var i = i0
      fit.origin(ts(i), xs(i), ys(i))
      while (i < iN) {
        fit += (ts(i), xs(i), ys(i))
        i += 1
      }
      fit.origin(0, 0, 0)
    }
    fit
  }

  def apply(ts: Array[Double], xs: Array[Double], ys: Array[Double]): FitTXY = apply(ts, xs, ys, 0, min(ts.length, min(xs.length, ys.length)))

  def apply(tf: Int => Double, xf: Int => Double, yf: Int => Double, i0: Int, iN: Int): FitTXY = {
    val fit = new FitTXY
    if (iN > i0) {
      var i = i0
      fit.origin(tf(i), xf(i), yf(i))
      while (i < iN) {
        fit += (tf(i), xf(i), yf(i))
        i += 1
      }
      fit.origin(0, 0, 0)
    }
    fit
  }

  def apply(ts: Array[Float], xs: Array[Float], ys: Array[Float], i0: Int, iN: Int): FitTXY = apply(i => ts(i), i => xs(i), i => ys(i), i0, iN)

  def apply(ts: Array[Float], xs: Array[Float], ys: Array[Float]): FitTXY = apply(ts, xs, ys, 0, min(ts.length, xs.length))
}

/** FitOLS performs a ordinary least squares fit of a parameter against dim-1 other values.
  * We assume the first parameter is accurate; this method does not minimize Euclidean
  * distance to nearest line point.
  */
final class FitOLS(dims: Int) extends Fit[FitOLS] {
  private[this] var n = 0L
  private[this] val m = max(1, dims)
  private[this] val O = new Array[Double](m)
  private[this] val S = new Array[Double](3*m)
  private[this] var cached = false
  private[this] val C = new Array[Double](m)

  private def setEverything(_O: Array[Double], _n: Long, _S: Array[Double]): this.type = {
    var i = 0
    while (i < O.length) { O(i) = _O(i); i += 1 }
    i = 0
    while (i < S.length) { S(i) = _S(i); i += 1 }
    n = _n
    this
  }

  override def clone(): FitOLS = {
    val that = new FitOLS(dims)
    that.setEverything(O, n, S)
  }


  def compute() {
    if (n < 2) {
      var i = 0; while (i < m) { C(i) = Double.NaN; i += 1 }
    }
    else {
      val St = S(0)
      val den = n*S(m) - St*St
      var i = 1; while (i < m) { C(i) = if (den != 0) (n*S(2*m+i) - St*S(i)) / den else Double.NaN; i += 1 }
      var E = 0.0
      i = 1
      while (i < m) { E += n*S(m+i) - S(i).sq - den*C(i).sq; i += 1 }
      C(0) = E / n
    }
    cached = true
  }

  def alpha(i: Int) = if (i == 0) 0 else { if (!cached) compute(); (S(i) - S(0)*C(i)) / n }
  def beta(i: Int) = if (i == 0) 1 else { if (!cached) compute(); C(i) }
  def error = { if (!cached) compute(); C(0) }
  def onlyBeta(i: Int) = if (i == 0) 1 else if (n < 2 || i >= m) Double.NaN else S(i)/S(0)
  def onlyBetaError = if (n < 2) Double.NaN else {
    var i = 1
    var Se = 0.0
    val iStt = 1.0/S(m)
    while (i < m) {
      Se += S(m+i) - S(2*m+i).sq*iStt
      i += 1
    }
    Se
  }
  def dimensions = m
  def samples = n
  def mean(i: Int) = if (n < 1 || i >= m) Double.NaN else S(i) / n

  def apply(t: Double): Array[Double] = {
    val a = new Array[Double](m-1)
    var i = 0
    while (i < a.length) { a(i) = xit(i+1, t); i += 1 }
    a
  }
  def xit(index: Int, t: Double): Double = alpha(index) + beta(index)*(t - O(0)) + O(index)
  def xiform(index: Int, in: Array[Double], i0: Int = 0, iN: Int = Int.MaxValue) {
    val iM = math.min(in.length, iN)
    var i = i0
    val a = alpha(index)
    val b = beta(index)
    while (i < iM) {
      in(i) = a + b*(in(i) - O(0)) + O(index)
      i += 1
    }
  }

  def clear: this.type = {
    n = 0
    var i = 0
    cached = false
    while (i < m) {
      O(i) = 0
      i += 1
    }
    i = 0
    while (i < 3*m) {
      S(i) = 0
      i += 1
    }
    this
  }

  def origin(ori: Array[Double], offset: Int = 0): this.type = {
    if (n > 0) {
      cached = false
      val St = S(0)
      val dt = O(0) - ori(offset)
      val ndt = n*dt
      var j = 1
      while (j < m) {
        val dxj = O(j) - ori(j + offset)
        val ndxj = n*dxj
        S(m+j) += dxj*(2*S(j) + ndxj)
        S(2*m+j) += dt*S(j) + dxj*(St + ndt)
        S(j) += ndxj
        j += 1
      }
      S(m) += dt*(2*St + ndt)
      S(0) += ndt
    }
    var i = 0
    while (i < m) {
      O(i) = ori(i + offset)
      i += 1
    }
    this
  }

  def +=(data: Array[Double], offset: Int = 0) {
    cached = false
    n += 1
    var j = 0
    var i = offset
    val t = data(offset) - O(0)
    while (j < m) {
      val xi = data(i) - O(j)
      S(2*m+j) += t*xi
      S(m+j) += xi*xi
      S(j) += xi
      j += 1
      i += 1
    }
  }

  def -=(data: Array[Double], offset: Int = 0) {
    cached = false
    n -= 1
    var j = 0
    var i = offset
    val t = data(offset) - O(0)
    while (j < m) {
      val xi = data(i) - O(j)
      S(2*m+j) -= t*xi
      S(m+j) -= xi*xi
      S(j) -= xi
      j += 1
      i += 1
    }
  }

  override def toString = (1 until m).map(i => s"x($i) = ${beta(i).fmt()} t + ${xit(i,0).fmt()}").mkString("Fit: ", "; ", s" (n=$n, err=${sqrt(error/n).fmt()})")
}
object FitOLS {
  def apply(dims: Int, xs: Array[Double], i0: Int, iN: Int): FitOLS = {
    val m = max(1, dims)
    val fit = new FitOLS(m)
    if (iN > i0) {
      var i = i0
      fit.origin(xs, i*m)
      while (i < iN) {
        fit += (xs, i*m)
        i += 1
      }
      fit.origin(new Array[Double](m))
    }
    fit
  }

  def apply(dims: Int, xs: Array[Double]): FitOLS = apply(dims, xs, 0, xs.length / max(1, dims))

  def apply(dims: Int, xf: (Int, Int) => Double, i0: Int, iN: Int): FitOLS = {
    val m = max(1, dims)
    val fit = new FitOLS(m)
    if (iN > i0) {
      val arr = Array.tabulate(m)(j => xf(i0, j))
      var i = i0
      fit.origin(arr, 0)
      while (i < iN) {
        var j = 0; while (j < m) { arr(j) = xf(i, j); j += 1 }
        fit += (arr, 0)
        i += 1
      }
      var j = 0; while (j < m) { arr(j) = 0.0; j += 1 }
      fit.origin(arr, 0)
    }
    fit
  }

  def apply(dims: Int, xs: Array[Float], i0: Int, iN: Int): FitOLS = {
    val m = max(1, dims)
  apply(dims, (i, j) => xs(i*m + j), i0, iN)
}

def apply(dims: Int, xs: Array[Float]): FitOLS = apply(dims, xs, 0, xs.length / max(1, dims))
}


trait Poly[P <: Poly[P]] {
  def t0: Double
  def t1: Double
  def dims: Int
  def apply(t: Double, d: Int): Double
  def apply(t: Double, a: Array[Double], i0: Int): Unit
  def apply(t: Array[Double], d: Int, a: Array[Double], i0: Int): Unit
  def apply(t: Array[Double], a: Array[Double], i0: Int): Unit
}

class PolyTX(val points: Array[Double]) extends Poly[PolyTX] {
  def dims = 1
  private val npts = max(0, points.length / 2)
  val t0 = points(0)
  val t1 = points(2*(npts-1))
  private val dt = (t1 - t0)/(npts-1)
  private val equallySpaced = {
    var ixs = new Array[Int](npts-1)
    var i, j = 0
    while (i < ixs.length) {
      val t = points(2*j)
      ixs(i) = 2*j
      while (j+1 < npts && t+dt+1e-9 > points(j*2+2)) j += 1
      i += 1
    }
    ixs
  }
  def x(t: Double) = if (t < t0 || t > t1) Double.NaN else {
    var i = equallySpaced(((t-t0)/dt).floor.toInt.clip(0, equallySpaced.length-1))
    while (i+4 < points.length && t > points(i+2)) i += 2
    val tL = points(i)
    val tR = points(i+2)
    (points(i+1)*(tR - t) + points(i+3)*(t - tL))/(tR - tL)
  }
  def apply(t: Double, d: Int) = if (d != 0) throw new NoSuchElementException("Only one dimension fit.") else x(t)
  def apply(t: Double, a: Array[Double], i0: Int) { a(i0) = x(t) }
  def apply(t: Array[Double], d: Int, a: Array[Double], i0: Int) {
    if (d != 0) throw new NoSuchElementException("Only one dimension fit.")
    apply(t, a, i0)
  }
  def apply(t: Array[Double], a: Array[Double], i0: Int) {
    var i = i0
    var j = 0
    while (j < t.length) {
      a(i) = x(t(j))
      i += 1
      j += 1
    }
  }
  def stride(t: Array[Double], a: Array[Double], i0: Int, step: Int) {
    var i = i0
    var j = 0
    while (j < t.length) {
      a(i) = x(t(j))
      i += step
      j += 1
    }
  }
}
object PolyTX {
  def apply(tnodes: Array[Double], ts: Array[Double], xs: Array[Double], i0: Int, iN: Int): PolyTX = {
    /* Note -- derivation of fit goes something like this.
     * Each block of points has error E|j = sum(xi - x'i)|j where x'i = pi*xL + qi*xR where xL and xR are
     * the left and rightmost endpoint, and qi and pi are the fraction of the way way from the right and
     * left ends respectively.  By differentiating the total error we end up with relations that look like
     * sum(xi*pi)|j + sum(xi*qi)|(j-1) = x|j-1 * sum(pi*qi)|j-1 + x|j * (sum(pi*pi)|j + sum(qi*qi)|j-1) + x|(j+1)*sum(pi*qi)
     * where you just use 0 if the value of j is not defined.
     */
    val nodes = new Array[Double](tnodes.length*2)
    val tridi = new Array[Double](tnodes.length*4)
    var j = 1
    var i = i0
    while (j < tnodes.length) {
      var ppS, pqS, qqS, pxS, qxS = 0.0
      val tL = tnodes(j-1)
      val tR = tnodes(j)
      val idt = 1.0/(tR - tL)
      while (i < iN && tR >= ts(i)) {
        val p = (tR - ts(i))*idt
        val q = (ts(i) - tL)*idt
        ppS += p*p
        pqS += p*q
        qqS += q*q
        pxS += p*xs(i)
        qxS += q*xs(i)

        i += 1
      }
      nodes(2*j) = tnodes(j)
      tridi(4*j-3) += ppS       
      tridi(4*j-2) += pqS      
      tridi(4*j-1) += pxS        
      tridi(4*j)   += pqS    
      tridi(4*j+1) += qqS  
      tridi(4*j+3) += qxS   
      j += 1
    }
    val xnodes = LinAlg.solveTriDiagonal(tridi)
    j = 0
    while (j < tnodes.length) {
      nodes(j*2+1) = xnodes(j)
      j += 1
    }
    new PolyTX(nodes)
  }
  def apply(inodes: Array[Int], ts: Array[Double], xs: Array[Double]): PolyTX = {
    val tnodes = new Array[Double](inodes.length)
    var i = 0
    while (i < inodes.length) { tnodes(i) = ts(inodes(i)); i += 1 }
    apply(tnodes, ts, xs, inodes(0), inodes(inodes.length-1)+1)
  }
}

class PolyTXY(val points: Array[Double]) extends Poly[PolyTXY] {
  def dims = 2
  private val npts = max(0, points.length / 3)
  val t0 = points(0)
  val t1 = points(3*(npts-1))
  private val dt = (t1 - t0)/(npts-1)
  private val equallySpaced = {
    var ixs = new Array[Int](npts-1)
    var i, j = 0
    while (i < ixs.length) {
      val t = points(3*j)
      ixs(i) = 3*j
      while (j+1 < npts && t+dt+1e-9 > points(j*3+3)) j += 1
      i += 1
    }
    ixs
  }
  def x(t: Double) = if (t < t0 || t > t1) Double.NaN else {
    var i = equallySpaced(((t-t0)/dt).floor.toInt.clip(0, equallySpaced.length-1))
    while (i+6 < points.length && t > points(i+3)) i += 3
    val tL = points(i)
    val tR = points(i+3)
    (points(i+1)*(tR - t) + points(i+4)*(t - tL))/(tR - tL)
  }
  def y(t: Double) = if (t < t0 || t > t1) Double.NaN else {
    var i = equallySpaced(((t-t0)/dt).floor.toInt.clip(0, equallySpaced.length-1))
    while (i+6 < points.length && t > points(i+3)) i += 3
    val tL = points(i)
    val tR = points(i+3)
    (points(i+2)*(tR - t) + points(i+5)*(t - tL))/(tR - tL)
  }
  def apply(t: Double, d: Int) = d match {
    case 0 => x(t)
    case 1 => y(t)
    case _ => throw new NoSuchElementException("Only two dimensions fit.")
  }
  def apply(t: Double, a: Array[Double], i0: Int) { 
    if (t < t0 || t > t1) {
      a(i0) = Double.NaN
      a(i0+1) = Double.NaN
    }
    else {
      var i = equallySpaced(((t-t0)/dt).floor.toInt.clip(0, equallySpaced.length-1))
      while (i+6 < points.length && t > points(i+3)) i += 3
      val tL = points(i)
      val tR = points(i+3)
      val p = (tR - t)/(tR - tL)
      a(i0) = points(i+1)*p + points(i+4)*(1-p)
      a(i0+1) = points(i+2)*p + points(i+5)*(1-p)
    }
  }
  def apply(t: Array[Double], d: Int, a: Array[Double], i0: Int) { 
    var i = i0
    var j = 0
    d match {
      case 0 => while (j < t.length) { a(i) = x(t(j)); i += 1; j += 1 }
      case 1 => while (j < t.length) { a(i) = y(t(j)); i += 1; j += 1 }
      case _ => throw new NoSuchElementException("Only two dimensions fit.")
    }
  }
  def apply(t: Array[Double], a: Array[Double], i0: Int) {
    var i = i0
    var j = 0
    while (j < t.length) {
      a(i) = x(t(j))
      a(i+1) = y(t(j))
      i += 2
      j += 1
    }
  }
}
object PolyTXY {
  def apply(tnodes: Array[Double], ts: Array[Double], xs: Array[Double], ys: Array[Double], i0: Int, iN: Int): PolyTXY = {
    // Note -- fit for x and y are independent, so just (conceptually) use single solution!
    val nodes = new Array[Double](tnodes.length*3)
    val tridix, tridiy = new Array[Double](tnodes.length*4)
    var j = 1
    var i = i0
    while (j < tnodes.length) {
      var ppS, pqS, qqS, pxS, qxS, pyS, qyS = 0.0
      val tL = tnodes(j-1)
      val tR = tnodes(j)
      val idt = 1.0/(tR - tL)
      while (i < iN && tR >= ts(i)) {
        val p = (tR - ts(i))*idt
        val q = (ts(i) - tL)*idt
        ppS += p*p
        pqS += p*q
        qqS += q*q
        pxS += p*xs(i)
        qxS += q*xs(i)
        pyS += p*ys(i)
        qyS += q*ys(i)

        i += 1
      }
      nodes(3*j) = tnodes(j)
      tridix(4*j-3) += ppS       
      tridix(4*j-2) += pqS      
      tridix(4*j-1) += pxS        
      tridix(4*j)   += pqS    
      tridix(4*j+1) += qqS  
      tridix(4*j+3) += qxS
      tridiy(4*j-3) += ppS
      tridiy(4*j-2) += pqS
      tridiy(4*j-1) += pyS
      tridiy(4*j)   += pqS
      tridiy(4*j+1) += qqS
      tridiy(4*j+3) += qyS
      j += 1
    }
    val xnodes = LinAlg.solveTriDiagonal(tridix)
    val ynodes = LinAlg.solveTriDiagonal(tridiy)
    j = 0
    while (j < tnodes.length) {
      nodes(j*3+1) = xnodes(j)
      nodes(j*3+2) = ynodes(j)
      j += 1
    }
    new PolyTXY(nodes)
  }
  def apply(inodes: Array[Int], ts: Array[Double], xs: Array[Double], ys: Array[Double]): PolyTXY = {
    val tnodes = new Array[Double](inodes.length)
    var i = 0
    while (i < inodes.length) { tnodes(i) = ts(inodes(i)); i += 1 }
    apply(tnodes, ts, xs, ys, inodes(0), inodes(inodes.length-1)+1)
  }
}

class PolyOLS(val polys: Array[PolyTX]) extends Poly[PolyOLS] {
  def dims = polys.length+1
  def t0 = polys(0).t0
  def t1 = polys(0).t1
  def apply(t: Double, d: Int): Double = if (d == 0) t else polys(d).x(t)
  def apply(t: Double, a: Array[Double], i0: Int) {
    var i = 0
    a(i0) = t
    while (i < polys.length) { a(i0+i+1) = polys(i).x(t); i += 1 }
  }
  def apply(t: Array[Double], d: Int, a: Array[Double], i0: Int) {
    if (d == 0) System.arraycopy(t, 0, a, i0, t.length)
    else if (d < 0 || d > polys.length) throw new NoSuchElementException("Dimension out of bounds")
    polys(d)(t, a, i0)
  }
  def apply(t: Array[Double], a: Array[Double], i0: Int) {
    var i = 0
    while (i < t.length) {
      a(i0+i*dims) = t(i)
      i += 1
    }
    i = 0
    while (i < polys.length) {
      polys(i).stride(t, a, i0+i+1, dims)
      i += 1
    }
  }
}
object PolyOLS {
  def apply(tnodes: Array[Double], dims: Int, datas: Array[Double], i0: Int, iN: Int): PolyOLS = {
    val xs = new Array[Double](iN - i0)
    val ts = {
      val ans = new Array[Double](iN - i0)
      var i = i0
      var j = 0
      while (j < ans.length) {
        ans(j) = datas(i)
        j += 1
        i += dims
      }
      ans
    }
    val polys = (1 until dims).map{ d =>
      var i = i0+d
      var j = 0
      while (j < xs.length) {
        xs(j) = datas(i)
        i += dims
        j += 1
      }
      PolyTX(tnodes, ts, xs, 0, xs.length)
    }
    new PolyOLS(polys.toArray)
  }
  def apply(inodes: Array[Int], dims: Int, datas: Array[Double]): PolyOLS = {
    val tnodes = new Array[Double](inodes.length)
    var i = 0
    while (i < inodes.length) { tnodes(i) = datas(inodes(i)*dims); i += 1 }
    apply(tnodes, dims, datas, inodes(0), inodes(inodes.length-1)+1)
  }
}
