// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr, UCSF, and Calico Labs.

package kse.visual

import scala.math._
import scala.util._

import kse.coll._
import kse.maths._
import kse.flow._
import kse.eio._

class Rgba private (_r: Float, _g: Float, _b: Float, _a: Float) {
  import Rgba._

  def r = _r
  def g = _g
  def b = _b
  def a = _a

  def rTo(red: Float) = new Rgba(clip(red), g, b, a)
  def gTo(green: Float) = new Rgba(r, clip(green), b, a)
  def bTo(blue: Float) = new Rgba(r, g, clip(blue), a)
  def aTo(alpha: Float) = new Rgba(r, g, b, if (alpha.nan) alpha else clip(alpha))

  def rFn(red: Float => Float) = new Rgba(clip(red(r)), g, b, a)
  def gFn(green: Float => Float) = new Rgba(r, clip(green(g)), b, a)
  def bFn(blue: Float => Float) = new Rgba(r, g, clip(blue(b)), a)
  def aFn(alpha: Float => Float) = new Rgba(r, g, b, { val aph = alpha(a); if (aph.nan) aph else clip(aph) })

  def grayLevel = clip(0.1f*r + 0.7f*g + 0.2f*b)
  def gray = { val k = grayLevel; new Rgba(k, k, k, a) }
  
  def saturate(fraction: Float) = if (fraction == 1) this else {
    val k = grayLevel;
    var f = fraction
    if (f > 1) {
      // Reduce fraction if any dimension would go out of 0,1 bounds
      if (k < r) f = min(f, 1+(1-r)/(r-k)) else if (k > r) f = min(f, 1 + r/(k-r))
      if (k < g) f = min(f, 1+(1-g)/(g-k)) else if (k > g) f = min(f, 1 + g/(k-g))
      if (k < b) f = min(f, 1+(1-b)/(b-k)) else if (k > b) f = min(f, 1 + b/(k-b))
    }
    val x = f
    val yk = (1-f)*k
    def mix(c: Float) = clip(x*c + yk)
    new Rgba(mix(r), mix(g), mix(b), a)
  }
  
  def ghost(fraction: Float) = if (fraction == 1 || a.nan) this else new Rgba(r, g, b, (fraction*a).clip(0f, 1f))

  def opaque = if (a < 1) new Rgba(r, g, b, 1f) else this
  
  def blend(rgba: Rgba, frac: Float, babble: Boolean = false): Rgba = {
    require(0 <= frac && frac <= 1, s"Mixing fraction should be in [0, 1] but is $frac")
    // Common gray level
    val p = frac.toDouble
    val q = (1.0 - frac)
    val k = grayLevel
    val kk = rgba.grayLevel
    val zk = dclip(q*k + p*kk)
    // Our vector from our gray
    val u = new IVec3F(r-k, g-k, b-k)
    // Their vector from their gray
    val v = new IVec3F(rgba.r - kk, rgba.g - kk, rgba.b - kk)
    // New vector
    val w =
      if ((u dotHat v) > -0.9) u interangle (v, frac)
      else {
        val targetLength = sqrt(u.lenSq*q + v.lenSq*p)
        val greeny = greenvector * (targetLength/greenvector.len).toFloat
        val cyany = cyanvector * (targetLength/cyanvector.len).toFloat
        val waypoint = if (abs(u dotHat greeny) < abs(u dotHat cyany)) greeny else cyany
        val angle1 = u angle waypoint
        val angle2 = v angle waypoint
        val fangle = angle1 / (angle1+angle2)
        if (frac <= fangle) u.interangle(waypoint, (frac/fangle).toFloat)
        else v.interangle(waypoint, ((1-frac)/(1-fangle)).toFloat)
      }
    val s = IVec3F(r, g, b) interangle ( IVec3F(rgba.r, rgba.g, rgba.b) , frac )  // Need w at all?  Maybe just use s?
    val aph = if (a.nan) { if (rgba.a.nan) a else rgba.a } else if (rgba.a.nan) a else dclip(q*a + p*rgba.a)
    new Rgba( dclip((w.x + zk)*0.5 + s.x*0.5), dclip((w.y + zk)*0.5 + s.y*0.5), dclip((w.z + zk)*0.5 + s.z*0.5), aph )
  }
  
  def ~(rgba: Rgba): Rgba = blend(rgba, 0.5f)
  
  def rgbText = "%02X%02X%02X".format(rint(r*255.0).toInt,rint(g*255.0).toInt,rint(b*255.0).toInt)
  
  override def toString = "#%s%02X".format(rgbText, if (a.nan) 255 else rint(a*255.0).toInt)

  override def hashCode = {
    var h = -1106268609
    h = scala.util.hashing.MurmurHash3.mix(h, java.lang.Float.floatToRawIntBits(r) match { case Int.MinValue => 0; case x => x })
    h = scala.util.hashing.MurmurHash3.mix(h, java.lang.Float.floatToRawIntBits(g) match { case Int.MinValue => 0; case x => x })
    h = scala.util.hashing.MurmurHash3.mix(h, java.lang.Float.floatToRawIntBits(b) match { case Int.MinValue => 0; case x => x })
    if (a.nan) scala.util.hashing.MurmurHash3.finalizeHash(h, 3)
    else {
      h = scala.util.hashing.MurmurHash3.mix(h, java.lang.Float.floatToRawIntBits(a) match { case Int.MinValue => 0; case x => x })
      scala.util.hashing.MurmurHash3.finalizeHash(h, 4)
    }
  }

  override def equals(o: Any) = o match {
    case rgba: Rgba => r == rgba.r && g == rgba.g && b == rgba.b && (a == rgba.a || (a.nan && rgba.a.nan))
    case _ => false
  }
}
object Rgba {
  def apply(r: Float, g: Float, b: Float, a: Float): Rgba = new Rgba(clip(r), clip(g), clip(b), clip(a))
  def apply(r: Float, g: Float, b: Float): Rgba = new Rgba(clip(r), clip(g), clip(b), Float.NaN)

  def web(r: Int, g: Int, b: Int, a: Int) = new Rgba(iclip(r), iclip(g), iclip(b), iclip(a))
  def web(r: Int, g: Int, b: Int): Rgba = new Rgba(iclip(r), iclip(g), iclip(b), Float.NaN)

  def rgba(i: Int): Rgba = new Rgba(iclip(i & 0xFF), iclip((i >>> 8) & 0xFF), iclip((i >>> 16) & 0xFF), iclip(i >>> 24))
  def rgb(i: Int): Rgba = new Rgba(iclip(i & 0xFF), iclip((i >>> 8) & 0xFF), iclip((i >>> 16) & 0xFF), Float.NaN)

  val White = apply(1, 1, 1, 1)
  val Black = apply(0, 0, 0, 1)

  private val greenvector = IVec3F(-0.7f, 0.3f, -0.7f)
  private val cyanvector = IVec3F(-0.9f, 0.1f, 0.1f)

  def dclip(d: Double): Float = max(0.0, min(1.0, d)).toFloat
  def clip(f: Float): Float = max(0.0f, min(1.0f, f))
  def iclip(i: Int): Float = max(0.0f, min(1.0f, (i/255.0).toFloat))

  def parse(s: String) = {
    val g = Grok(s).delimit(true)
    val i = (if (g.peek == '#') 1 else 0)
    g{ implicit fail =>
      val r0 = g.input(s,i,i+2).xI
      val g0 = g.input(s,i+2,i+4).xI
      val b0 = g.input(s,i+4,i+6).xI
      if (s.length < i + 6) web(r0, g0, b0)
      else web(r0, g0, b0, g.input(s, i+6, s.length).delimit(true).xI)
    }.mapNo(n => n.toString)
  }
}

