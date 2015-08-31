// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse.maths

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
      myE = if (n == 2) 0 else (n*Sxx - Sx*Sx - myBx*myBx*den)/(n.toDouble*(n-2))
    }
  }

  def localAlphaX = { if (!cached) compute(); (Sx - myBx*St)/ n }
  def alphaX = { localAlphaX + Ot*myBx + Ox }   // TODO -- wrong signs, fix
  def betaX = { if (!cached) compute(); myBx }
  def errorX = { if (!cached) compute(); myE }

  def clear(): this.type = clearWithOrigin(0, 0)
  def clearWithOrigin(ot: Double, ox: Double): this.type = {
    Ot = ot
    Ox = ox
    n = 0
    St = 0
    Stt = 0
    Sx = 0
    Sxx = 0
    Stx = 0
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
      myE = if (n == 2) 0 else (n*(Sxx + Syy) - Sx*Sx - Sy*Sy - (myBx*myBx + myBy*myBy)*den)/(n.toDouble*(n-2))
    }
  }

  def localAlphaX = { if (!cached) compute(); (Sx - myBx*St) / n }
  def localAlphaY = { if (!cached) compute(); (Sy - myBy*St) / n }
  def alphaX = { localAlphaX + Ot*myBx + Ox }   // TODO -- wrong signs, fix
  def alphaY = { localAlphaY + Ot*myBy + Oy }   // TODO -- wrong signs, fix
  def betaX = { if (!cached) compute(); myBx }
  def betaY = { if (!cached) compute(); myBy }
  def errorX = { if (!cached) compute(); myE }

  def clear(): this.type = clearWithOrigin(0, 0, 0)
  def clearWithOrigin(ot: Double, ox: Double, oy: Double): this.type = {
    Ot = ot
    Ox = ox
    Oy = oy
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
