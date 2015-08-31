// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse.coll.packed

final class RichFloatToVc(private val underlying: Float) extends AnyVal {
  import Vc._
  @inline final def vc(f: Float) = Vc(underlying, f)
  @inline final def vcX = Vc.x(underlying)
  @inline final def vcY = Vc.y(underlying)
  
  def +(v: Vc) = Vc(underlying + v.x, underlying + v.y)
  def -(v: Vc) = Vc(underlying - v.x, underlying - v.y)
  def *(v: Vc) = Vc(underlying * v.x, underlying * v.y)

  def degrees = (underlying * 0.017453292519943295).toFloat
  def revolutions = (underlying * 6.283185307179586).toFloat
}

final class Vc(val underlying: Long) extends AnyVal {
  import Vc._
  def x = java.lang.Float.intBitsToFloat((underlying & 0xFFFFFFFFL).toInt)
  def xTo(f: Float) = new Vc((underlying & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL))
  def xFn(fn: Float => Float) = xTo(fn(x))

  def y = java.lang.Float.intBitsToFloat((underlying >>> 32).toInt)
  def yTo(f: Float) = new Vc((underlying & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f).toLong << 32))
  def yFn(fn: Float => Float) = yTo(fn(y))

  def isNaN = (java.lang.Float.isNaN(x) || java.lang.Float.isNaN(y))
  def isInf = (java.lang.Float.isInfinite(x) || java.lang.Float.isInfinite(y))
  def isFinite = { val a = x; val b = y; !(java.lang.Float.isNaN(a) || java.lang.Float.isInfinite(a) || java.lang.Float.isNaN(b) || java.lang.Float.isInfinite(b)) }
  def isZero = (underlying & 0x7FFFFFFF7FFFFFFFL) == 0

  def lenSq: Double = { val a = x.toDouble; val b = y.toDouble; a*a + b*b }
  def len: Double = math.sqrt(lenSq)

  def theta: Double = math.atan2(y,x)

  def swap = new Vc((underlying >>> 32) | (underlying << 32))
  def cw = new Vc(((underlying >>> 32) | (underlying << 32)) ^ 0x8000000000000000L)
  def ccw = new Vc(((underlying >>> 32) | (underlying << 32)) ^ 0x80000000L)
  def rotate(angle: Float) = { val ca = math.cos(angle); val sa = math.sin(angle); val nx = x*ca - y*sa; val ny = y*ca + x*sa; Vc(nx.toFloat, ny.toFloat) }

  def +(f: Float) = Vc(x+f, y+f)
  def +(f: Float, g: Float) = Vc(x+f, y+g)
  def +(v: Vc) = Vc(x+v.x, y+v.y)

  def unary_- = new Vc(underlying ^ 0x8000000080000000L)
  def -(f: Float) = Vc(x-f, y-f)
  def -(f: Float, g: Float) = Vc(x-f, y-g)
  def -(v: Vc) = Vc(x-v.x, y-v.y)

  def *(f: Float) = Vc(x*f, y*f)
  def *(f: Float, g: Float) = x*f + y*g
  def *(v: Vc) = x*v.x + y*v.y
  def X(f: Float, g: Float) = x*g - y*f
  def X(v: Vc) = x*v.y - y*v.x

  def proj(f: Float, g: Float) = { val a = x; val b = y; val e = (a*f + b*g)/(f*f + g*g); Vc(f*e, g*e) }
  def proj(v: Vc) = { val a = x; val b = y; val c = v.x; val d = v.y; val e = (a*c + b*d)/(c*c + d*d); Vc(c*e, d*e) }

  def orth(f: Float, g: Float) = { val a = x; val b = y; val e = (a*f + b*g)/(f*f + g*g); Vc(a-f*e, b-g*e) }
  def orth(v: Vc) = { val a = x; val b = y; val c = v.x; val d = v.y; val e = (a*c + b*d)/(c*c + d*d); Vc(a-c*e, b-d*e) }

  def hat = { val a = x.toDouble; val b = y.toDouble; val l2 = a*a + b*b; if (math.abs(l2-1) < 3e-7f) this else if (l2 == 0) Vc.zero else { val il = 1.0/math.sqrt(l2); Vc((a*il).toFloat, (b*il).toFloat) } }
  def dotHat(f: Float, g: Float): Double =
    { val a = x; val b = y; ((a*f + b*g)/math.sqrt((a*a + b*b)*(f*f + g*g))) }
  def dotHat(v: Vc): Double =
    { val a = x.toDouble; val b = y.toDouble; val c = v.x.toDouble; val d = v.y.toDouble; ((a*c + b*d)/math.sqrt((a*a + b*b)*(c*c + d*d))) }

  def distSq(f: Float, g: Float): Double = { val a = (x-f).toDouble; val b = (y-g).toDouble; a*a + b*b }
  def distSq(v: Vc): Double = { val a = (x-v.x).toDouble; val b = (y-v.y).toDouble; a*a + b*b }
  def dist(f: Float, g: Float): Double = math.sqrt(distSq(f,g))
  def dist(v: Vc): Double = math.sqrt(distSq(v))

  def angle(f: Float, g: Float): Double =
    { val a = x.toDouble; val b = y.toDouble; (math.acos(math.max(-1,math.min(1,((a*f+b*g)/math.sqrt((a*a + b*b)*(f*f + g*g))))))*math.signum(a*g-b*f)) }
  def angle(v: Vc): Double =
    { val a = x.toDouble; val b = y.toDouble; val c = v.x.toDouble; val d = v.y.toDouble; (math.acos(math.max(-1,math.min(1,((a*c+b*d)/math.sqrt((a*a + b*b)*(c*c + d*d))))))*math.signum(a*d-b*c)) }

  def ===(v: Vc) = (x == v.x) && (y == v.y)

  override def toString = "["+x+", "+y+"]"

  def toTuple = (x, y)
  def toPoint = new java.awt.Point(math.round(x), math.round(y))
  def toPoint2D = new java.awt.geom.Point2D.Float(x, y)
  def toDimension = new java.awt.Dimension(math.round(x), math.round(y))
}

object Vc {
  val NaN = apply(Float.NaN, Float.NaN)
  val zero = new Vc(0L)

  @inline final def apply(f: Float, g: Float) = new Vc((java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(g).toLong << 32))

  @inline final def x(f: Float) = new Vc(java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL)
  @inline final def y(f: Float) = new Vc(java.lang.Float.floatToRawIntBits(f).toLong << 32)

  def from(l: Long) = new Vc(l)
  def from(d: Double, e: Double) = apply(d.toFloat, e.toFloat)
  def from[D >: Double <: Double](t: (D, D))(implicit ev: D =:= Double) = apply(t._1.toFloat, t._2.toFloat)
  def from(t: (Float, Float)) = apply(t._1, t._2)

  def from(p: java.awt.geom.Point2D) = apply(p.getX.toFloat, p.getY.toFloat)
  def from(d: java.awt.geom.Dimension2D) = apply(d.getWidth.toFloat, d.getHeight.toFloat)
  
  /** Find point of intersection between lines p0,v0 and p1,v1.  If the lines are coincident, p0 will be chosen.  If they are
    * paralel but not coincident, NaN will be returned.  If v1 is zero, NaN will be returned unless p0 == p1.  If v0 is zero,
    * NaN will be returned unless p1,v1 goes through p0.
    */
  def intersectLines(p0: Vc, v0: Vc, p1: Vc, v1: Vc): Vc = {
    if (v1.isZero) {
      if (math.abs(p0.x - p1.x) + math.abs(p0.y - p1.y) < 10*(java.lang.Math.ulp(p0.x) + java.lang.Math.ulp(p1.x))) p0
      else NaN
    }
    else if (v0.isZero) {
      val delta = p1-p0
      val coef = delta dotHat v1
      if (math.abs(math.abs(coef)-1) < 1e-6) p0
      else NaN
    }
    else {
      // Use determinant formula 
      val a0x = p0.x.toDouble
      val a0y = p0.y.toDouble
      val a1x = a0x + v0.x.toDouble
      val a1y = a0y + v0.y.toDouble
      val b0x = p1.x.toDouble
      val b0y = p1.y.toDouble
      val b1x = b0x + v1.x.toDouble
      val b1y = b0y + v1.y.toDouble
      val den = (a0x - a1x)*(b0y - b1y) - (a0y - a1y)*(b0x - b1x)
      if (math.abs(den) < 1e-6) intersectLines(p0, zero, p1, v1)
      else {
        val aMix = (a0x*a1y - a0y*a1x)
        val bMix = (b0x*b1y - b1x*b0y)
        from((aMix*(b0x-b1x) - (a0x-a1x)*bMix)/den, (aMix*(b0y-b1y) - (a0y-a1y)*bMix)/den)
      }
    }
  }
}
