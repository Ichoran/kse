// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and UCSF

package kse.coll

import scala.math._

final case class IVec3F(x: Float, y: Float, z: Float) {
  def xTo(xf: Float) = new IVec3F(xf, y, z)
  def xFn(f: Float => Float) = new IVec3F(f(x), y, z)
  
  def yTo(yf: Float) = new IVec3F(x, yf, z)
  def yFn(f: Float => Float) = new IVec3F(x, f(y), z)
  
  def zTo(zf: Float) = new IVec3F(x, y, zf)
  def zFn(f: Float => Float) = new IVec3F(x, y, f(z))
  
  def isNaN = (java.lang.Float.isNaN(x) || java.lang.Float.isNaN(y) || java.lang.Float.isNaN(z))
  def isInf = (java.lang.Float.isInfinite(x) || java.lang.Float.isInfinite(y) || java.lang.Float.isInfinite(z))
  def isFinite = !isNaN && !isInf
  def isZero = (x == 0 && y == 0 && z == 0)
  
  def lenSq = x*x + y*y + z*z
  def len = sqrt(lenSq)
  
  def +(c: Float) = new IVec3F(x+c, y+c, z+c)
  def +(xf: Float, yf: Float, zf: Float) = new IVec3F(x+xf, y+yf, z+zf)
  def +(v: IVec3F) = new IVec3F(x + v.x, y + v.y, z + v.z)
  
  def unary_- = new IVec3F(-x, -y, -z)
  def -(c: Float) = new IVec3F(x-c, y-c, z-c)
  def -(xf: Float, yf: Float, zf: Float) = new IVec3F(x-xf, y-yf, z-zf)
  def -(v: IVec3F) = new IVec3F(x - v.x, y - v.y, z - v.z)
  
  def *(c: Float) = new IVec3F(x*c, y*c, z*c)
  def *(xf: Float, yf: Float, zf: Float) = x*xf + y*yf + z*zf
  def *(v: IVec3F) = x*v.x + y*v.y + z*v.z
  
  def X(xf: Float, yf: Float, zf: Float) = new IVec3F(y*zf - z*yf, xf*z - zf*x, x*yf - y*xf)
  def X(v: IVec3F) = new IVec3F(y*v.z - z*v.y, v.x*z - v.z*x, x*v.y - y*v.x)
  
  def hat = { val l2 = lenSq; if ((l2-1) < 5e-7) this else if (l2 == 0) IVec3F.zero else { val il = 1.0/sqrt(l2); new IVec3F((x*il).toFloat, (y*il).toFloat, (z*il).toFloat) } }
  def dotHat(xf: Float, yf: Float, zf: Float) = (x*xf + y*yf + z*zf)/sqrt((x*x + y*y + z*z)*(xf*xf + yf*yf + zf*zf))
  def dotHat(v: IVec3F) = (x*v.x + y*v.y + z*v.z)/sqrt(lenSq * v.lenSq)
  
  def proj(xf: Float, yf: Float, zf: Float): IVec3F = { val e = (x*xf + y*yf + z*zf)/(xf*xf + yf*yf + zf*zf); new IVec3F(xf*e, yf*e, zf*e) }
  def proj(v: IVec3F): IVec3F = proj(v.x, v.y, v.z)
  
  def orth(xf: Float, yf: Float, zf: Float): IVec3F = { val e = (x*xf + y*yf + z*zf)/(xf*xf + yf*yf + zf*zf); new IVec3F(x - xf*e, y - yf*e, z - zf*e) }
  def orth(v: IVec3F): IVec3F = orth(v.x, v.y, v.z)
  
  def distSq(xf: Float, yf: Float, zf: Float) = { val dx = x-xf.toDouble; val dy = y-yf.toDouble; val dz = z-zf.toDouble; dx*dx + dy*dy + dz*dz }
  def distSq(v: IVec3F) = { val dx = x - v.x.toDouble; val dy = y - v.y.toDouble; val dz = z - v.z.toDouble; dx*dx + dy*dy + dz*dz }
  def dist(xf: Float, yf: Float, zf: Float) = sqrt(distSq(xf, yf, zf))
  def dist(v: IVec3F) = sqrt(distSq(v))
  
  def angle(xf: Float, yf: Float, zf: Float) = acos(max(-1, min(1, dotHat(xf, yf, zf))))
  def angle(v: IVec3F) = acos(max(-1, min(1, dotHat(v))))
  
  def interpolate(v: IVec3F, fraction: Float): IVec3F = 
    new IVec3F((1-fraction)*x + fraction*v.x, (1-fraction)*y + fraction*v.y, (1-fraction)*z + fraction*v.z)
  
  def interangle(v: IVec3F, fraction: Float): IVec3F = {
    val a2 = lenSq
    val b2 = v.lenSq
    if (a2 == 0 || b2 == 0 || !(fraction > 0 && fraction < 1)) return interpolate(v, fraction)
    val abct = this * v
    val costheta = max(-1, min(1, abct/sqrt(a2*b2)))
    if (costheta+1 < 1e-3) return interpolate(v, fraction)
    val cosphi = cos(fraction*acos(costheta))
    val A = 1 - cosphi*cosphi
    val B = costheta*costheta - cosphi*cosphi
    val C = A*a2*a2
    val D = A*a2*2*abct
    val E = B*a2*b2
    val F = 2*E-D
    val G = 2*(C-D+E)
    val H = sqrt(F*F - 2*E*G)
    val t =
      if (G < 0) { if (F+H < 0) (F+H)/G else (F-H)/G }
      else {       if (F-H > 0) (F-H)/G else (F+H)/G }
    val xt = t*x + (1-t)*v.x
    val yt = t*y + (1-t)*v.y
    val zt = t*z + (1-t)*v.z
    val fix = sqrt((t*a2 + (1-t)*b2)/(xt*xt + yt*yt + zt*zt))
    new IVec3F((fix*xt).toFloat, (fix*yt).toFloat, (fix*zt).toFloat)
  }
  
  def ===(v: IVec3F) = (x == v.x && y == v.y && z == v.z)
  
  override def toString = "[" + x + ", " + y + ", " + z + "]"
}
object IVec3F {
  val zero = new IVec3F(0, 0, 0)
}
