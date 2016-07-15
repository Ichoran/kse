// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr, UCSF, and Calico Labs.

package kse.visual
package chart

import scala.math._
import scala.util._
import scala.collection.mutable.{ AnyRefMap => RMap }

import kse.coll._
import kse.maths._
import kse.maths.stats._
import kse.flow._
import kse.eio._


class Formatter {
  import SvgSelect._

  def apply(x: Float): String =
    if (x.toInt == x) x.toInt.toString
    else if (x*10 == (x*10).toInt) "%.1f".format(x)
    else if (x*100 == (x*100).toInt) "%.2f".format(x)
    else "%.3f".format(x)

  def apply(v: Vc, c: Char): String =
    { val sb = new StringBuilder; sb ++= apply(v.x); sb += c; sb ++= apply(v.y); sb.result }
  def apply(v: Vc): String = apply(v, ' ')
  def comma(v: Vc): String = apply(v, ',')
  def vquote(v: Vc, xname: String, yname: String): String = f" $xname=$q${apply(v.x)}$q $yname=$q${apply(v.y)}$q"

  def attribute(tag: String, text: String) = if (text.isEmpty) "" else f" $tag=$q$text$q"
  def attribute(tag: String, value: Float) = if (value.finite) f" $tag=$q${apply(value)}$q" else ""

  def apply(title: String, color: Rgba): String = f" $title=$q#${color.rgbText}$q"
  def apply(title: String, color: Rgba, opacity: String => String): String =
    f" $title=$q#${color.rgbText}$q ${opacity(title)}=$q${apply(color.a)}$q"

  def opaquely(value: Float, mask: Masked) = if (mask has Opaqued) attribute("opacity",value) else ""
  def apply(opaque: Opaque, mask: Masked): String = opaquely(opaque.opacity, mask)
  final def apply(opaque: Opaque): String = apply(opaque, opaque.mask)

  def filling(color: Rgba, mask: Masked) =
    if (mask has Unfilled) attribute("fill", "none")
    else if (mask hasnt Filled) ""
    else if (mask has FillColor+FillOpacity) apply("fill", color, _ + "-opacity")
    else if (mask has FillOpacity) attribute("fill-opacity", color.a)
    else if (mask has FillColor) apply("fill", color)
    else ""
  def apply(fill: Fill, mask: Masked): String = filling(fill.fillcolor, mask)
  final def apply(fill: Fill): String = apply(fill, fill.mask)

  def stroking(width: Float, color: Rgba, join: StrokeJoin, cap: StrokeCap, mask: Masked): String =
    if ((mask hasnt Stroked) || (color.a == 0)) ""
    else {
      ( if (mask has StrokeWidth) attribute("stroke-width", width) else "" ) +
      ( if (mask has StrokeColor) apply("stroke", color) else "" ) +
      ( if (mask has StrokeOpacity) attribute("stroke-opacity", color.a) else "" ) +
      ( if (mask has StrokeJoined) join match {
          case StrokeJoin.Miter(limit) => attribute("stroke-miterlimit", limit)
          case x if x.text.nonEmpty         => attribute("stroke-linejoin", join.text)
          case _                            => ""
        }
        else ""
      ) +
      ( if ((mask has StrokeCapped) && cap.text.nonEmpty) attribute("stroke-linecap", cap.text) else "" )
    }
  def apply(stroke: Stroke, mask: Masked): String = stroking(stroke.width, stroke.strokecolor, stroke.join, stroke.cap, mask)
  final def apply(stroke: Stroke): String = apply(stroke, stroke.mask)

  def fonting(face: String, size: Float, color: Rgba, valign: Font.Vertical, halign: Font.Horizontal, outline: Option[Stroke], mask: Masked): String = {
    if (mask hasnt Fonted) ""
    else {
      ( if ((mask has FontFace) && face.nonEmpty) attribute("font-family", face) else "" ) +
      ( if (mask has FontSize) attribute("font-size", size) else "" ) +
      ( if (mask has FontColor) apply("fill", color) else "" ) +
      ( if ((mask has FontVAlign) && valign.text.nonEmpty) attribute("dominant-baseline", valign.text) else "" ) +
      ( if ((mask has FontHAlign) && halign.text.nonEmpty) attribute("text-anchor", halign.text) else "") +
      ( if (mask has FontOpacity) attribute("opacity", color.a) else "" ) +
      ( if ((mask has FontOutlined) && outline.isDefined) apply(outline.get, mask & (Stroked + StrokeWidth + StrokeColor)) else "" )
    }
  }
  def apply(font: Font, mask: Masked): String = fonting(font.face, font.size, font.color, font.valign, font.halign, font.outline, mask)
  final def apply (font: Font): String = apply(font, font.mask)

  def styling(font: Font, stroke: Stroke, fill: Fill, opacity: Opaque, mask: Masked): String =
    apply(
      font, 
      mask.
        epistatic(Opaqued, FontOpacity + FontOutlineOpacity).
        epistatic(Filled + FillColor, FontColor).
        epistatic(Stroked+StrokeWidth, FontOutlined)
    ) +
    apply(stroke, mask.epistatic(Opaqued, StrokeOpacity)) +
    apply(fill, mask.epistatic(Opaqued, FillOpacity)) +
    apply(opacity, mask)
  def apply(style: Style, mask: Masked): String = styling(style, style, style, style, mask)
  final def apply(style: Style): String = apply(style, style.mask)
}
class ProxyFormatter(original: Formatter) extends Formatter {
  import SvgSelect._
  override def apply(x: Float) = original(x)
  override def apply(v: Vc, c: Char) = original(v, c)
  override def apply(v: Vc) = original(v)
  override def comma(v: Vc) = original comma v
  override def vquote(v: Vc, xname: String, yname: String) = original.vquote(v, xname, yname)

  override def attribute(tag: String, text: String) = original.attribute(tag, text)
  override def attribute(tag: String, value: Float) = original.attribute(tag, value)

  override def apply(title: String, color: Rgba): String = original.apply(title, color)
  override def apply(title: String, color: Rgba, opacity: String => String) = original.apply(title, color, opacity)

  override def opaquely(value: Float, mask: Masked) = original.opaquely(value, mask)
  override def apply(opaque: Opaque, mask: Masked) = original(opaque, mask)

  override def filling(color: Rgba, mask: Masked) = original.filling(color, mask)
  override def apply(fill: Fill, mask: Masked) = original(fill, mask)

  override def stroking(width: Float, color: Rgba, join: StrokeJoin, cap: StrokeCap, mask: Masked) =
    original.stroking(width, color, join, cap, mask)
  override def apply(stroke: Stroke, mask: Masked) = original(stroke, mask)

  override def fonting(face: String, size: Float, color: Rgba, valign: Font.Vertical, halign: Font.Horizontal, outline: Option[Stroke], mask: Masked) =
    original.fonting(face, size, color, valign, halign, outline, mask)
  override def apply(font: Font, mask: Masked) = original(font, mask)

  override def styling(font: Font, stroke: Stroke, fill: Fill, opacity: Opaque, mask: Masked) =
    original.styling(font, stroke, fill, opacity, mask)
  override def apply(style: Style, mask: Masked) = original(style, mask)
}
object DefaultFormatter extends Formatter {}

trait SvgSelect {
  def mask: SvgSelect.Masked
}
object SvgSelect {
  case class Masked(val bits: Long) extends AnyVal {
    def has(bit: Long) = (bits & bit) == bit
    def hasnt(bit: Long) = (bits | bit) != bits
    def +(bit: Long) = new Masked(bits | bit)
    def -(bit: Long) = new Masked(bits - (bits & bit))
    def &(thoseBits: Long): Masked = new Masked(bits & thoseBits)
    def &[L](those: Masked)(implicit ev: L =:= Long): Masked = new Masked(bits & those.bits)
    def |(thoseBits: Long): Masked = new Masked(bits | thoseBits)
    def |[L](those: Masked)(implicit ev: L =:= Long): Masked = new Masked(bits | those.bits)

    def epistatic(a: Long, b: Long): Masked = if (has(a) && !hasnt(b)) this - (b - (b&a)) else this

    override def toString = "0x" + bits.toHexString
  }
  object Masked {
    val empty = new Masked(0L)
    val fonty = new Masked(
      Fonted + FontFace + FontSize + FontColor + FontVAlign + FontHAlign + FontOpacity +
      FontOutlined + FontOutlineWidth + FontOutlineColor + FontOutlineOpacity
    )
    val stroky = new Masked(Stroked + StrokeWidth + StrokeColor + StrokeOpacity + StrokeJoined + StrokeCapped)
    val filly = new Masked(Filled + FillColor + FillOpacity)
    val opaquey = new Masked(Opaqued)
    val full = new Masked(
      Fonted + FontFace + FontSize + FontColor + FontVAlign + FontHAlign + FontOpacity +
      FontOutlined + FontOutlineWidth + FontOutlineColor + FontOutlineOpacity +
      Stroked + StrokeWidth + StrokeColor + StrokeOpacity + StrokeJoined + StrokeCapped +
      Filled + FillColor + FillOpacity + Opaqued
    )
    val undetermined = new Masked(-1L | Undetermined)
  }

  val Fonted =       0x100000000L
  val FontFace =             0x1L
  val FontSize =             0x2L
  val FontColor =            0x4L
  val FontVAlign =           0x8L
  val FontHAlign =          0x10L
  val FontOpacity =         0x20L
  val FontOutlined =   0x1000000L
  val FontOutlineWidth =    0x40L
  val FontOutlineColor =    0x80L
  val FontOutlineOpacity = 0x100L
  val Stroked =      0x200000000L
  val StrokeWidth =        0x100L
  val StrokeColor =        0x200L
  val StrokeOpacity =      0x400L
  val StrokeJoined =       0x800L
  val StrokeCapped =      0x1000L
  val Filled =       0x400000000L
  val Unfilled =       0x2000000L
  val FillColor =         0x2000L
  val FillOpacity =       0x4000L
  val Opaqued =      0x800000000L
  val Undetermined =  0x10000000L   // Indicates that you shouldn't really trust this mask
}


trait Opaque extends SvgSelect { 
  def mask = Opaque.DefaultMask
  def opacity: Float = 1f
  def opaqueCase: OpaqueValue = new OpaqueValue(opacity, mask)
  def fixed: Opaque = opaqueCase
}
object Opaque {
  import SvgSelect._
  val DefaultMask = Masked(Opaqued)

  val empty: Opaque = new Opaque { override def mask = Masked.empty }
  def apply(opaqueness: Float): Opaque = new Opaque { override def opacity = opaqueness }
}
class OpaqueProxy(original: Opaque) extends Opaque {
  override def mask = original.mask
  override def opacity = original.opacity
}
final case class OpaqueValue(override val opacity: Float = 1f, override val mask: SvgSelect.Masked = Opaque.DefaultMask)
extends Opaque {
  override def opaqueCase = this
}


trait Fill extends SvgSelect {
  def mask = if (fillcolor.a == 1) Fill.DefaultMask else Fill.TranslucentMask
  def fillcolor: Rgba = Rgba.Black
  def fillCase: FillValue = new FillValue(fillcolor, mask)
  def fixed: Fill = fillCase
}
object Fill {
  import SvgSelect._
  val DefaultMask = Masked(Filled + FillColor)
  val TranslucentMask = Masked(Filled + FillColor + FillOpacity)
  val UnfilledMask = Masked(Unfilled)

  val empty: Fill = new Fill { override def mask = Masked.empty }
  val off: Fill = new Fill { override def mask = UnfilledMask }
  def apply(fillColor: Rgba): Fill = new Fill { override def fillcolor = fillColor }
}
class FillProxy(original: Fill) extends Fill {
  override def mask = original.mask
  override def fillcolor = original.fillcolor
}
final case class FillValue(override val fillcolor: Rgba = Rgba.Black, override val mask: SvgSelect.Masked = Fill.DefaultMask)
extends Fill {
  override def fillCase = this
}


sealed trait StrokeJoin { def text: String }
object StrokeJoin {
  final case class Miter(limit: Float = 4.0f) extends StrokeJoin { def text = "miter" }
  final case object Bevel extends StrokeJoin { def text = "bevel" }
  final case object Round extends StrokeJoin { def text = "round" }
  final case object Whatever extends StrokeJoin { def text = "" }
}

sealed trait StrokeCap { def text: String }
object StrokeCap {
  final case object Butt extends StrokeCap { def text = "butt" }
  final case object Round extends StrokeCap { def text = "round" }
  final case object Square extends StrokeCap { def text = "square" }
  final case object Whatever extends StrokeCap { def text = "" }
}

trait Stroke extends SvgSelect {
  import SvgSelect._
  def mask = Masked(
    Stroked + 
    (if (width.finite && width > 0) StrokeWidth else 0) +
    (if (strokecolor.a > 0) StrokeColor else 0) +
    (if (strokecolor.a > 0 && strokecolor.a < 1) StrokeOpacity else 0) +
    (if (join != StrokeJoin.Whatever) StrokeJoined else 0) +
    (if (cap != StrokeCap.Whatever) StrokeCapped else 0)
  )
  def width: Float = 0f
  def strokecolor: Rgba = Rgba.Black
  def join: StrokeJoin = StrokeJoin.Whatever
  def cap: StrokeCap = StrokeCap.Whatever

  def strokeCase: StrokeValue = new StrokeValue(width, strokecolor, join, cap, mask)
  def fixed: Stroke = strokeCase
  def scaled(scale: Float): Stroke = 
    if (scale closeTo 1) this else new StrokeProxy(this) { override def width = scale * super.width }
}
object Stroke {
  import SvgSelect._

  val empty: Stroke = new Stroke { override def mask = Masked.empty }

  def apply(width: Float) =                                                StrokeValue(width)
  def apply(width: Float, color: Rgba) =                                   StrokeValue(width, color)
  def apply(width: Float, color: Rgba, miterJoin: Float) =                 StrokeValue(width, color, StrokeJoin.Miter(miterJoin))
  def apply(width: Float, join: StrokeJoin) =                              StrokeValue(width, join = join)
  def apply(width: Float, color: Rgba, join: StrokeJoin) =                 StrokeValue(width, color, join)
  def apply(width: Float, cap: StrokeCap) =                                StrokeValue(width, cap = cap)
  def apply(width: Float, color: Rgba, cap: StrokeCap) =                   StrokeValue(width, color, cap = cap)
  def apply(width: Float, join: StrokeJoin, cap: StrokeCap) =              StrokeValue(width, join = join, cap = cap)
  def apply(width: Float, color: Rgba, join: StrokeJoin, cap: StrokeCap) = StrokeValue(width, color, join, cap)
}
class StrokeProxy(original: Stroke) extends Stroke {
  override def mask = original.mask
  override def width = original.width
  override def strokecolor = original.strokecolor
  override def join = original.join
  override def cap = original.cap
}
final case class StrokeValue(
  override val width: Float,
  override val strokecolor: Rgba = Rgba.Black,
  override val join: StrokeJoin = StrokeJoin.Whatever,
  override val cap: StrokeCap = StrokeCap.Whatever,
  val theMask: SvgSelect.Masked = SvgSelect.Masked.undetermined
) extends Stroke {
  override def mask = if (theMask has SvgSelect.Undetermined) SvgSelect.Masked.stroky else theMask
  override def strokeCase = this
}


trait Font extends SvgSelect {
  def mask = Font.DefaultMask
  def size: Float = 0f
  def face: String = ""
  def color: Rgba = Rgba.Black
  def valign: Font.Vertical = Font.WHEREVER
  def halign: Font.Horizontal = Font.WHEREVER
  def outline: Option[Stroke] = None

  def fontCase: FontValue = new FontValue(size, face, color, valign, halign, outline, mask)
  def fixed: Font = fontCase
  def scaled(scale: Float) = 
    if (scale closeTo 1) this
    else new FontProxy(this) {
      override def size = super.size * scale
      override def outline = super.outline.map(_.scaled(scale))
    }
}
object Font {
  import SvgSelect._

  sealed trait Vertical { def text: String; override def toString = text }
  object HANG extends Vertical { def text = "hanging" }
  object VMID extends Vertical { def text = "middle" }
  object BASE extends Vertical { def text = "baseline"}

  sealed trait Horizontal { def text: String; override def toString = text }
  object LEFT extends Horizontal { def text = "start" }
  object HMID extends Horizontal { def text = "middle" }
  object RIGHT extends Horizontal { def text = "end" }

  object WHEREVER extends Vertical with Horizontal { def text = "" }

  val DefaultMask = Masked.fonty

  val empty: Font = new Font { override def mask = Masked.empty }

  def apply(
    size: Float, face: String = "", color: Rgba = Rgba.Black,
    valign: Vertical = WHEREVER, halign: Horizontal = WHEREVER, outline: Option[Stroke] = None
  ): Font = FontValue(size, face, color, valign, halign, outline)
  def apply(size: Float, color: Rgba): Font = FontValue(size, color = color)
}
class FontProxy(original: Font) extends Font {
  override def face = original.face
  override def size = original.size
  override def color = original.color
  override def valign = original.valign
  override def halign = original.halign
  override def outline = original.outline
  override def mask = original.mask
}
final case class FontValue(
  override val size: Float,
  override val face: String = "",
  override val color: Rgba = Rgba.Black,
  override val valign: Font.Vertical = Font.WHEREVER,
  override val halign: Font.Horizontal = Font.WHEREVER,
  override val outline: Option[Stroke] = None,
  theMask: SvgSelect.Masked = SvgSelect.Masked.undetermined
) extends Font {
  override def mask = if (theMask has SvgSelect.Undetermined) Font.DefaultMask else theMask
  override def fontCase = this
}


trait Style extends Font with Stroke with Fill with Opaque { self =>
  override def mask = Style.DefaultMask

  def cased: StyleValue = StyleValue(
    size, face, color, valign, halign, outline,
    width, strokecolor, join, cap,
    fillcolor, opacity, mask
  )
  override def fixed: Style = cased

  def maskedOn(overmask: SvgSelect.Masked): Style = new StyleProxy(this) {
    override def mask = super.mask | overmask
  }
  def maskedOnly(overmask: SvgSelect.Masked): Style = new StyleProxy(this) {
    override def mask = super.mask & overmask
  }
  def maskedOff(overmask: SvgSelect.Masked): Style = new StyleProxy(this) {
    override def mask = { val m = super.mask; m - (m.bits & overmask.bits) }
  }
  def colored(oneColor: Rgba): Style = new StyleProxy(this) {
    private[this] val theColor = if (oneColor.a == 1) oneColor else oneColor.aTo(1)
    override def color = theColor
    override def outline = super.outline.map{ x => new StrokeProxy(x) { override def strokecolor = theColor } }
    override def strokecolor = theColor
    override def fillcolor = theColor
    override def opacity = oneColor.a
  }
  override def scaled(scale: Float): Style = 
    if (scale closeTo 1) this
    else new StyleProxy(this) {
      override def size = scale * super.size
      override def width = scale * super.width
      override def outline = super.outline.map(_.scaled(scale))
    }
  def promoted: Style = new StylePromotion(this)
  def demoted: Style = new StyleDemotion(this)
}
object Style {
  import SvgSelect._

  val DefaultMask = Masked.full

  val empty: Style = new Style {}

  def apply(font: Font): Style = new Stylish(font, Stroke.empty, Fill.empty, Opaque.empty)
  def apply(stroke: Stroke): Style = new Stylish(Font.empty, stroke, Fill.off, Opaque.empty)
  def apply(fill: Fill): Style = new Stylish(Font.empty, Stroke.empty, fill, Opaque.empty)
  def apply(opaque: Opaque): Style = new Stylish(Font.empty, Stroke.empty, Fill.empty, opaque)
  def apply(font: Font, stroke: Stroke): Style = new Stylish(font, stroke, Fill.empty, Opaque.empty)
  def apply(font: Font, fill: Fill): Style = new Stylish(font, Stroke.empty, fill, Opaque.empty)
  def apply(font: Font, opaque: Opaque): Style = new Stylish(font, Stroke.empty, Fill.empty, opaque)
  def apply(stroke: Stroke, fill: Fill): Style = new Stylish(Font.empty, stroke, fill, Opaque.empty)
  def apply(stroke: Stroke, opaque: Opaque): Style = new Stylish(Font.empty, stroke, Fill.off, opaque)
  def apply(font: Font, stroke: Stroke, fill: Fill): Style = new Stylish(font, stroke, fill, Opaque.empty)
  def apply(font: Font, stroke: Stroke, opaque: Opaque): Style = new Stylish(font, stroke, Fill.empty, opaque)
  def apply(font: Font, fill: Fill, opaque: Opaque): Style = new Stylish(font, Stroke.empty, fill, opaque)
  def apply(stroke: Stroke, fill: Fill, opaque: Opaque): Style = new Stylish(Font.empty, stroke, fill, opaque)
  def apply(font: Font, stroke: Stroke, fill: Fill, opaque: Opaque): Style = new Stylish(font, stroke, fill, opaque)
}
class StyleProxy(original: Style) extends FontProxy(original) with Style {
  override def mask = original.mask
  override def width = original.width
  override def strokecolor = original.strokecolor
  override def join = original.join
  override def cap = original.cap
  override def fillcolor = original.fillcolor
  override def opacity = original.opacity
}
class Stylish(val font: Font, val stroke: Stroke, val fill: Fill, val opaque: Opaque) extends FontProxy(font) with Style {
  override def mask = {
    import SvgSelect._
    var bits = font.mask.bits | stroke.mask.bits | fill.mask.bits | opaque.mask.bits
    if ((bits & Opaqued) != 0) bits -= (bits & (FontOutlineOpacity | StrokeOpacity | FillOpacity))
    if ((bits & (Filled | Unfilled)) == (Filled | Unfilled)) bits -= Unfilled
    Masked(bits)
  }
  override def width = stroke.width
  override def strokecolor = stroke.strokecolor
  override def join = stroke.join
  override def cap = stroke.cap
  override def fillcolor = fill.fillcolor
  override def opacity = opaque.opacity
}
final case class StyleValue(
  override val size: Float = 0f,
  override val face: String = "",
  override val color: Rgba = Rgba.Black,
  override val valign: Font.Vertical = Font.WHEREVER,
  override val halign: Font.Horizontal = Font.WHEREVER,
  override val outline: Option[Stroke] = None,
  override val width: Float = 0f,
  override val strokecolor: Rgba = Rgba.Black,
  override val join: StrokeJoin = StrokeJoin.Whatever,
  override val cap: StrokeCap = StrokeCap.Whatever,
  override val fillcolor: Rgba = Rgba.Black,
  override val opacity: Float = 1f,
  theMask: SvgSelect.Masked = SvgSelect.Masked.undetermined
) extends Style {
  override def mask = if (theMask has SvgSelect.Undetermined) Font.DefaultMask else theMask
  override def cased = this
}
final class StylePromotion(original: Style) extends StyleProxy(original) {
  import SvgSelect._
  private[this] def transparency = {
    val o = if (original.mask has Opaqued) original.opacity else 1f
    val s = if (original.mask has Stroked + StrokeOpacity) original.strokecolor.a else 1f
    val f = if (original.mask has Filled + FillOpacity) original.fillcolor.a else 1f
    val c = if (original.mask has Fonted + FontOpacity) original.color.a else 1f
    val x = if (original.mask has Fonted + FontOutlined + FontOutlineOpacity) original.outline.map(_.strokecolor.a).getOrElse(1f) else 1f
    val sf = if (s > 0 && f > 0) math.min(s, f) else math.max(s, f)
    val cx = if (c > 0 && x > 0) math.min(c, x) else math.max(c, x)
    o * (if (sf > 0 && cx > 0) math.min(sf, cx) else if (sf <= 0 && cx <= 0) 1f else math.max(sf, cx))
  }
  override def mask = {
    val t = transparency
    if (t < 1) (original.mask - (FontSize + StrokeWidth)) + Opaqued
    else original.mask - (FontSize + StrokeWidth)
  }
  override def strokecolor =
    if (mask has Stroked + StrokeOpacity) {
      val t = transparency;
      val s = original.strokecolor.a
      if (s < t && !(s closeTo t) && t > 0) original.strokecolor.aTo(s/t)
      else original.strokecolor.aTo(1)
    }
    else original.strokecolor
  override def fillcolor =
    if (mask has Filled + FillOpacity) {
      val t = transparency
      val f = original.fillcolor.a
      if (f < t && !(f closeTo t) && t > 0) original.fillcolor.aTo(f/t)
      else original.fillcolor.aTo(1)
    }
    else original.fillcolor
  override def color =
    if (mask has Fonted + FontOpacity) {
      val t = transparency
      val c = original.color.a
      if (c < t && !(c closeTo t) && t > 0) original.color.aTo(c/t)
      else original.color.aTo(1)
    }
    else original.color
  override def outline =
    if ((mask has FontOutlined) && original.outline.isDefined) {
      val t = transparency
      val oo = original.outline.get
      val x = oo.strokecolor.a
      Some(new StrokeProxy(oo) {
        override def strokecolor = oo.strokecolor.aTo(if (x < t && !(x closeTo t) && t > 0) x/t else 1f)
      })
    }
    else original.outline
}
final class StyleDemotion(original: Style) extends StyleProxy(original) {
  import SvgSelect._
  override def mask = original.mask & Masked(Fonted + FontSize + Stroked + StrokeWidth)
}


final case class Indent(text: String, level: Int = 0) {
  def in = new Indent(text, level+1)
  override def toString = if (level <= 0) text else " "*(2*level) + text
}
object Indent {
  def V(text: String*) = {
    val N = text.length
    if (N == 0) Vector.empty[Indent]
    else {
      val vb = Vector.newBuilder[Indent]
      var i = 0
      text.foreach{ t => i += 1; vb += new Indent(t, if (i==1 || i==N) 0 else 1) }
      vb.result
    }
  }
}


case class Magnification(value: Float) {
  def scale(factor: Float) = new Magnification(value*factor)
}
object Magnification {
  val one = new Magnification(1f)
  def from(mag: Option[Float], xf: Xform, v: Vc) =
    mag match {
      case None => one
      case Some(x) => 
        if (x.finite) new Magnification(x)
        else new Magnification(xf mag v)
    }
  def from(mag: Option[Float], xf: Xform, va: Vc, vb: Vc) =
    mag match {
      case None => one
      case Some(x) =>
        if (x.finite) new Magnification(x)
        else new Magnification(0.5f*((xf mag va) + (xf mag vb)))
    }
  def from(mag: Option[Float], xf: Xform, vs: Array[Long]) =
    mag match {
      case None => one
      case Some(x) =>
        if (x.finite) new Magnification(x)
        else {
          var m = 0.0
          var i = 0
          while (i < vs.length) { m += xf.mag(Vc from vs(i)); i += 1 }
          new Magnification((m / math.max(1, vs.length)).toFloat)
        }
    }
  def from(mag: Option[Float], x0: Float, x1: Float) =
    mag match {
      case None => one
      case Some(x) =>
        if (x.finite) new Magnification(x)
        else new Magnification((x1/x0.toDouble).toFloat)
    }
  def from(mag: Option[Float], x0a: Float, x0b: Float, x1a: Float, x1b: Float) =
    mag match {
      case None => one
      case Some(x) =>
        if (x.finite) new Magnification(x)
        else new Magnification(((x1a + x1b)/(x0a + x0b).toDouble).toFloat)
    }
}


trait InSvg { 
  def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent]
}

trait SvgStyled extends InSvg with SvgSelect {
  def mask: SvgSelect.Masked = SvgSelect.Masked.undetermined
  def style: Style
  def masked(s: Style) = if (mask has SvgSelect.Undetermined) s else s.maskedOnly(mask)
  def showing(s: Style)(implicit fm: Formatter, scale: Magnification = Magnification.one): String =
    fm(if (scale.value closeTo 1) masked(s) else masked(s).scaled(scale.value))
  def show(implicit fm: Formatter, scale: Magnification = Magnification.one): String = showing(style)(fm, scale)
}
