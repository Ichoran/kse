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
      myBx = Float.NaN
      myE = Float.NaN
    }
    else {
      val den = n*Stt - St*St
      myBx = if (den != 0) (n*Stx - St*Sx)/den else Float.NaN
      myE = (n*Sxx - Sx*Sx - myBx*myBx*den) / n
    }
    cached = true
  }

  def alphaX = { if (!cached) compute(); (Sx - myBx*St ) / n }
  def betaX = { if (!cached) compute(); myBx }
  def error = { if (!cached) compute(); myE }
  def samples = n

  def clear(): this.type = {
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
      val dt = ot - Ot
      val dx = ox - Ox
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
  def samples = n

  def clear(): this.type = {
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
      val dt = ot - Ot
      val dx = ox - Ox
      val dy = oy - Oy
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
}

/** FitOLS performs a ordinary least squares fit of a parameter against dim-1 other values.
  * We assume the first parameter is accurate; this method does not minimize Euclidean
  * distance to nearest line point.
  */
final class FitOLS(dim: Int) {
  private[this] var n = 0L
  private[this] val m = max(1, dim)
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
  def samples = n

  def clear: this.type = {
    n = 0
    var i = 0
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

  def origin(ori: Array[Double]): this.type = {
    if (n > 0) {
      val St = S(0)
      val dt = ori(0) - O(0)
      val ndt = n*dt
      var j = 1
      while (j < m) {
        val dxj = ori(j) - O(j)
        val ndxj = n*dxj
        S(m+j) += dxj*(2*S(j) + ndxj)
        S(2*m+j) += dt*S(j) + dxj*(St + ndt)
        S(j) += ndxj
        j += 1
      }
      S(m) += dt*(2*St + ndt)
      S(0) + ndt
    }
    var i = 0
    while (i < m) {
      O(i) = ori(i)
      i += 1
    }
    this
  }

  def +=(data: Array[Double], i0: Int = 0) {
    cached = false
    n += 1
    var j = 0
    var i = i0
    val t = data(i0)
    while (j < m) {
      val xi = data(i)
      S(2*m+j) += t*xi
      S(m+j) += xi*xi
      S(j) += xi
      j += 1
      i += 1
    }
  }

  def -=(data: Array[Double], i0: Int = 0) {
    cached = false
    n -= 1
    var j = 0
    var i = i0
    val t = data(i0)
    while (j < m) {
      val xi = data(i)
      S(2*m+j) -= t*xi
      S(m+j) -= xi*xi
      S(j) -= xi
      j += 1
      i += 1
    }
  }
}
