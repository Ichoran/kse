// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse.maths

import scala.math._


/** FitTX performs a ordinary least squares fit of a parameter t against a readout x.
  * We assume t is accurate; this method is not precise if both t and x can vary.
  */
final class FitTX {
  private[this] var Ot = 0.0
  private[this] var Ox = 0.0

  private[this] var n = 0L
  private[this] var St = 0.0
  private[this] var Stt = 0.0
  private[this] var Sx = 0.0
  private[this] var Sxx = 0.0
  private[this] var Stx = 0.0

  private[this] var cached = false
  private[this] var myBx = 0.0
  private[this] var myE = 0.0

  private def compute() {
    if (n < 2) {
      myBx = Double.NaN
      myE = Double.NaN
    }
    else {
      val den = n*Stt - St*St
      myBx = if (den != 0) (n*Stx - St*Sx)/den else Double.NaN
      myE = (n*Sxx - Sx*Sx - myBx*myBx*den) / n
    }
    cached = true
  }

  def alphaX = { if (!cached) compute(); (Sx - myBx*St ) / n }
  def betaX = { if (!cached) compute(); myBx }
  def error = { if (!cached) compute(); myE }
  def onlyBetaX = if (n < 2) Double.NaN else Stx / Stt
  def onlyBetaError = if (n < 2) Double.NaN else Sxx - Stx*Stx/Stt
  def samples = n
  def meanT = if (n < 1) Double.NaN else St / n
  def meanX = if (n < 1) Double.NaN else Sx / n

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

  override def toString = s"Fit: x = $betaX t + $alphaX (n=$n, rmse=${sqrt(error)/n}"
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
final class FitTXY {
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
  private[this] var myBx = 0.0
  private[this] var myBy = 0.0
  private[this] var myE = 0.0

  private def compute() {
    if (n < 2) {
      myBx = Double.NaN
      myBy = Double.NaN
      myE = Double.NaN
    }
    else {
      val den = n*Stt - St*St
      myBx = if (den != 0) (n*Stx - St*Sx)/den else Double.NaN
      myBy = if (den != 0) (n*Sty - St*Sy)/den else Double.NaN
      myE = (n*(Sxx + Syy) - Sx*Sx - Sy*Sy - (myBx*myBx + myBy*myBy)*den) / n
    }
    cached = true
  }

  def alphaX = { if (!cached) compute(); (Sx - myBx*St) / n }
  def alphaY = { if (!cached) compute(); (Sy - myBy*St) / n }
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

  override def toString = s"Fit: x = $betaX t + $alphaX; y = $betaY t + $alphaY (n=$n, rmse=${sqrt(error)/n}"
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
final class FitOLS(dims: Int) {
  private[this] var n = 0L
  private[this] val m = max(1, dims)
  private[this] val O = new Array[Double](m)
  private[this] val S = new Array[Double](3*m)
  private[this] var cached = false
  private[this] val C = new Array[Double](m)

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
  def samples = n
  def mean(i: Int) = if (n < 1 || i >= m) Double.NaN else S(i) / n

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

  override def toString = (1 until m).map(i => s"x($i) = ${beta(i)} t + ${alpha(i)}").mkString("Fit: ", "; ", s" (n=$n, err=$error)")
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
