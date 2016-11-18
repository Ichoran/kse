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

  def exists: Boolean = a > 0
  def unicorn: Boolean = this == Rgba.InvisiblePink

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
    if (u.lenSq == 0 || v.lenSq == 0 || (u dotHat v).abs > 0.95) {
      new Rgba(dclip(q*r + p*rgba.r), dclip(q*g + p*rgba.g), dclip(q*b + p*rgba.b), dclip(q*a + p*rgba.a))
    }
    else {
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

  def rgbaInt: Int = math.rint(r*255).toInt | (math.rint(g*255).toInt << 8) | (math.rint(b*255).toInt << 16) | (math.rint(a*255).toInt << 24)
  def bgraInt: Int = math.rint(b*255).toInt | (math.rint(g*255).toInt << 8) | (math.rint(r*255).toInt << 16) | (math.rint(a*255).toInt << 24)
}
object Rgba {
  def apply(r: Float, g: Float, b: Float, a: Float): Rgba = new Rgba(clip(r), clip(g), clip(b), clip(a))
  def apply(r: Float, g: Float, b: Float): Rgba = new Rgba(clip(r), clip(g), clip(b), 1)

  def web(r: Int, g: Int, b: Int, a: Int) = new Rgba(iclip(r), iclip(g), iclip(b), iclip(a))
  def web(r: Int, g: Int, b: Int): Rgba = new Rgba(iclip(r), iclip(g), iclip(b), 1)

  def rgba(i: Int): Rgba = new Rgba(iclip(i & 0xFF), iclip((i >>> 8) & 0xFF), iclip((i >>> 16) & 0xFF), iclip(i >>> 24))
  def rgb(i: Int): Rgba = new Rgba(iclip(i & 0xFF), iclip((i >>> 8) & 0xFF), iclip((i >>> 16) & 0xFF), 1)

  def abgr(i: Int): Rgba = new Rgba(iclip(i >>> 24), iclip((i >>> 16) & 0xFF), iclip((i >>> 8) & 0xFF), iclip(i & 0xFF))
  def bgr(i: Int): Rgba = new Rgba(iclip((i >>> 16) & 0xFF), iclip((i >>> 8) & 0xFF), iclip(i & 0xFF), 1)
  def bgra(i: Int): Rgba = new Rgba(iclip((i >>> 16) & 0xFF), iclip((i >>> 8) & 0xFF), iclip(i & 0xFF), iclip(i >>> 24))

  /////////////////////////////
  // Named colors begin here //
  /////////////////////////////

  // These are not visible colors.
  val Empty         = apply(0, 0, 0, 0)
  val InvisiblePink = apply(1, 0.7529412f, 0.79607844f, 0)   // Some unicorns are this color

  // Standard monochromatic colors
  val White   = apply(1, 1, 1)
  val Silver  = bgr(0xC0C0C0)
  val Gray    = bgr(0x808080);   @inline def Grey = Gray
  val Black   = apply(0, 0, 0)

  // Standard chromatic colors (standard 16)
  val Red     = bgr(0xFF0000)
  val Maroon  = bgr(0x800000)
  val Yellow  = bgr(0xFFFF00)
  val Olive   = bgr(0x808000)
  val Lime    = bgr(0x00FF00)
  val Green   = bgr(0x008000)
  val Cyan    = bgr(0x00FFFF);   @inline def Aqua = Cyan
  val Teal    = bgr(0x008080)
  val Blue    = bgr(0x0000FF)
  val Navy    = bgr(0x000080)
  val Magenta = bgr(0xFF00FF);   @inline def Fuchsia = Magenta
  val Purple  = bgr(0x800080)

  // SVG / X11 colors.  Names and values from Wikipedia Web Colors page on 2016-08-27T14:36:00.000-07:00

  // Pinks
  val Pink            = bgr(0xFFC0CB)
  val LightPink       = bgr(0xFFB6C1)
  val HotPink         = bgr(0xFF69B4)
  val DeepPink        = bgr(0xFF1493)
  val PaleVioletRed   = bgr(0xDB7093)
  val MediumVioletRed = bgr(0xC71585)

  // Reds
  val LightSalmon = bgr(0xFFA07A)
  val Salmon      = bgr(0xFA8072)
  val DarkSalmon  = bgr(0xE9967A)
  val LightCoral  = bgr(0xF08080)
  val IndianRed   = bgr(0xCD5C5C)
  val Crimson     = bgr(0xDC143C)
  val FireBrick   = bgr(0xB22222)
  val DarkRed     = bgr(0x8B0000)

  // Oranges
  val OrangeRed  = bgr(0xFF4500)
  val Tomato     = bgr(0xFF6347)
  val Coral      = bgr(0xFF7F50)
  val DarkOrange = bgr(0xFF8C00)
  val Orange     = bgr(0xFFA500)

  // Yellows
  val LightYellow           = bgr(0xFFFFE0)
  val LemonChiffon          = bgr(0xFFFACD)
  val LightGoldenrodYellow  = bgr(0xFAFAD2)
  val PapayaWhip            = bgr(0xFFEFD5)
  val Moccasin              = bgr(0xFFE4B5)
  val PeachPuff             = bgr(0xFFDAB9)
  val PaleGoldenrod         = bgr(0xEEE8AA)
  val Khaki                 = bgr(0xF0E68C)
  val DarkKhaki             = bgr(0xBDB76B)
  val Gold                  = bgr(0xFFD700)

  // Browns
  val Cornsilk       = bgr(0xFFF8DC)
  val BlanchedAlmond = bgr(0xFFEBCD)
  val Bisque         = bgr(0xFFE4C4)
  val NavajoWhite    = bgr(0xFFDEAD)
  val Wheat          = bgr(0xF5DEB3)
  val BurlyWood      = bgr(0xDEB887)
  val Tan            = bgr(0xD2B48C)
  val RosyBrown      = bgr(0xBC8F8F)
  val SandyBrown     = bgr(0xF4A460)
  val Goldenrod      = bgr(0xDAA520)
  val DarkGoldenrod  = bgr(0xB8860B)
  val Peru           = bgr(0xCD853F)
  val Chocolate      = bgr(0xD2691E)
  val SaddleBrown    = bgr(0x8B4513)
  val Sienna         = bgr(0xA0522D)
  val Brown          = bgr(0xA52A2A)

  // Greens
  val DarkOliveGreen    = bgr(0x556B2F)
  val OliveDrab         = bgr(0x6B8E23)
  val YellowGreen       = bgr(0x9ACD32)
  val LimeGreen         = bgr(0x32CD32)
  val LawnGreen         = bgr(0x7CFC00)
  val Chartreuse        = bgr(0x7FFF00)
  val GreenYellow       = bgr(0xADFF2F)
  val SpringGreen       = bgr(0x00FF7F)
  val MediumSpringGreen = bgr(0x00FA9A)
  val LightGreen        = bgr(0x90EE90)
  val PaleGreen         = bgr(0x98FB98)
  val DarkSeaGreen      = bgr(0x8FBC8F)
  val MediumAquamarine  = bgr(0x66CDAA)
  val MediumSeaGreen    = bgr(0x3CB371)
  val SeaGreen          = bgr(0x2E8B57)
  val ForestGreen       = bgr(0x228B22)
  val DarkGreen         = bgr(0x006400)

  // Cyans
  val LightCyan       = bgr(0xE0FFFF)
  val PaleTurquoise   = bgr(0xAFEEEE)
  val Aquamarine      = bgr(0x7FFFD4)
  val Turquoise       = bgr(0x40E0D0)
  val MediumTurquoise = bgr(0x48D1CC)
  val DarkTurquoise   = bgr(0x00CED1)
  val LightSeaGreen   = bgr(0x20B2AA)
  val CadetBlue       = bgr(0x5F9EA0)
  val DarkCyan        = bgr(0x008B8B)

  // Blues
  val LightSteelBlue = bgr(0xB0C4DE)
  val PowderBlue     = bgr(0xB0E0E6)
  val LightBlue      = bgr(0xADD8E6)
  val SkyBlue        = bgr(0x87CEEB)
  val LightSkyBlue   = bgr(0x87CEFA)
  val DeepSkyBlue    = bgr(0x00BFFF)
  val DodgerBlue     = bgr(0x1E90FF)
  val CornflowerBlue = bgr(0x6495ED)
  val SteelBlue      = bgr(0x4682B4)
  val RoyalBlue      = bgr(0x4169E1)
  val MediumBlue     = bgr(0x0000CD)
  val DarkBlue       = bgr(0x00008B)
  val MidnightBlue   = bgr(0x191970)

  // Purples
  val Lavender        = bgr(0xE6E6FA)
  val Thistle         = bgr(0xD8BFD8)
  val Plum            = bgr(0xDDA0DD)
  val Violet          = bgr(0xEE82EE)
  val Orchid          = bgr(0xDA70D6)
  val MediumOrchid    = bgr(0xBA55D3)
  val MediumPurple    = bgr(0x9370DB)
  val BlueViolet      = bgr(0x8A2BE2)
  val DarkViolet      = bgr(0x9400D3)
  val DarkOrchid      = bgr(0x9932CC)
  val DarkMagenta     = bgr(0x8B008B)
  val Indigo          = bgr(0x4B0082)
  val DarkSlateBlue   = bgr(0x483D8B)
  val SlateBlue       = bgr(0x6A5ACD)
  val MediumSlateBlue = bgr(0x7B68EE)

  // Whites
  val Snow          = bgr(0xFFFAFA)
  val Honeydew      = bgr(0xF0FFF0)
  val MintCream     = bgr(0xF5FFFA)
  val Azure         = bgr(0xF0FFFF)
  val AliceBlue     = bgr(0xF0F8FF)
  val GhostWhite    = bgr(0xF8F8FF)
  val WhiteSmoke    = bgr(0xF5F5F5)
  val Seashell      = bgr(0xFFF5EE)
  val Beige         = bgr(0xF5F5DC)
  val OldLace       = bgr(0xFDF5E6)
  val FloralWhite   = bgr(0xFFFAF0)
  val Ivory         = bgr(0xFFFFF0)
  val AntiqueWhite  = bgr(0xFAEBD7)
  val Linen         = bgr(0xFAF0E6)
  val LavenderBlush = bgr(0xFFF0F5)
  val MistyRose     = bgr(0xFFE4E1)

  // Grays
  val Gainsboro      = bgr(0xDCDCDC)
  val LightGray      = bgr(0xD3D3D3);  @inline def LightGrey      = LightGray   
  val DarkGray       = bgr(0xA9A9A9);  @inline def DarkGrey       = DarkGray
  val DimGray        = bgr(0x696969);  @inline def DimGrey        = DimGray
  val LightSlateGray = bgr(0x778899);  @inline def LightSlateGrey = LightSlateGray
  val SlateGray      = bgr(0x708090);  @inline def SlateGrey      = SlateGray
  val DarkSlateGray  = bgr(0x2F4F4F);  @inline def DarkSlateGrey  = DarkSlateGray

  ///////////////////////////
  // Named colors end here //
  ///////////////////////////

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
      if (s.length <= i + 6) web(r0, g0, b0)
      else web(r0, g0, b0, g.input(s, i+6, s.length).delimit(true).xI)
    }.mapNo(n => n.toString)
  }
}

case class Spectrum(colors: Array[Rgba], low: Option[Rgba], high: Option[Rgba]) {
  val extremeLo = low.getOrElse(colors(0))
  val extremeHi = high.getOrElse(colors(colors.length-1))
  def apply(f: Float) = {
    if (f closeTo 0) colors(0)
    else if (f closeTo 1) colors(colors.length - 1)
    else if (f < 0) extremeLo
    else if (f > 1) extremeHi
    else if (colors.length < 2) colors(0)
    else {
      val i = math.floor((colors.length - 1) * f).toInt
      val cA = colors(i)
      val cB = colors(i + 1)
      val g = (colors.length - 1)*f - i
      if (g closeTo 0) cA
      else if (g closeTo 1) cB
      else cA.blend(cB, g)
    }
  }
}
object Spectrum {
  import Rgba._
  val Grays = new Spectrum(Array(Black, White), None, None)
  val Greys = Grays
  val Rainbow = new Spectrum(Array(MediumVioletRed, Red, Orange, PaleGoldenrod, MediumSeaGreen, Blue, Thistle), Some(Magenta), Some(LavenderBlush))
  val Fire = new Spectrum(Array(Black, Red, Orange, Yellow, White), Some(Navy), Some(Lavender))
  val Flag = new Spectrum(Array(Blue, White, Red), Some(Indigo), Some(DarkRed))
  val HotCold = new Spectrum(Array(Blue, Black, Red), Some(CornflowerBlue), Some(Salmon))
  val Candy = new Spectrum(Array(Magenta, White, Rgba(0, 0.7f, 0, 1)), Some(DarkOrange), Some(DarkCyan))
  val Fluoresce = new Spectrum(Array(Magenta, Black, Rgba(0, 0.7f, 0, 1)), Some(Orange), Some(LightSeaGreen))
  val Sunset = new Spectrum(Array(CornflowerBlue, Orchid, Coral), Some(Rgba(0, 0.7f, 0, 1)), Some(DarkOrange))
}
