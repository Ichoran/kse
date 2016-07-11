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
  def attribute(tag: String, value: Float) = if (value.finite) " " + tag + " " + apply(value) else ""

  def apply(title: String, color: Rgba, opacity: String => String = _ + "-opacity"): String =
    if (color.a >= 0.9995) f" $title=$q#${color.rgbText}$q"
    else if (color.a > 0.5e-3) f" $title=$q${color.rgbText}$q ${opacity(title)}=$q${apply(color.a)}$q"
    else ""

  def apply(opaque: Opaque): String = if (opaque.opacity in (0, 1)) f" opacity=$q${apply(opaque.opacity)}$q" else ""
  def apply(fill: Fill): String =
    if (fill.unfilled) attribute("fill", "none")
    else apply("fill", fill.fillcolor)
  def apply(stroke: Stroke): String =
    if (stroke.width > 0) 
      f" stroke-width=$q${apply(stroke.width)}$q" +
      apply("stroke", stroke.strokecolor) +
      (stroke.join match {
        case None => ""
        case Some(None) => " stroke-linejoin=\"rounded\""
        case Some(Some(x)) =>
          if (x.isNaN) " stroke-linejoin=\"miter\""
          else attribute("stroke-miterlimit", x)
      })
    else ""
  def apply(font: Font): String =
    if (font.size > 0) {
      val sb = new StringBuilder
      if (font.face.nonEmpty) sb ++= f" font-family=$q${font.face}$q"
      sb ++= apply("fill", font.color)
      font.outline.foreach(o => sb ++= apply(o))
      sb ++= attribute("text-anchor",font.halign.text)
      sb ++= attribute("dominant-baseline", font.valign.text)
      sb.result
    }
    else ""

  def apply(style: Stylish): String = 
    apply(style: Opaque) + 
    apply(style: Fill) + 
    apply(style: Stroke) + 
    apply(style: Font)
}
class ProxyFormatter(original: Formatter) extends Formatter {
  override def apply(x: Float) = original(x)
  override def apply(v: Vc, c: Char) = original(v, c)
  override def apply(v: Vc) = original(v)
  override def comma(v: Vc) = original comma v
  override def vquote(v: Vc, xname: String, yname: String) = original.vquote(v, xname, yname)
  override def attribute(tag: String, text: String) = original.attribute(tag, text)
  override def attribute(tag: String, value: Float) = original.attribute(tag, value)
  override def apply(title: String, color: Rgba, opacity: String => String = _ + "-opacity") = original.apply(title, color, opacity)
  override def apply(opaque: Opaque) = original(opaque)
  override def apply(fill: Fill) = original(fill)
  override def apply(stroke: Stroke) = original(stroke)
  override def apply(font: Font) = original(font)
  override def apply(style: Stylish) = original(style)
}
object DefaultFormatter extends Formatter {}
final case class TransformFormatter(fm: Formatter, st: StyleTransformer) extends ProxyFormatter(fm) {
  override def apply(opaque: Opaque) = super.apply(st(Stylish(opaque)): Opaque)
  override def apply(fill: Fill) = super.apply(st(Stylish(fill)): Fill)
  override def apply(stroke: Stroke) = super.apply(st(Stylish(stroke)): Stroke)
  override def apply(font: Font) = super.apply(st(Stylish(font)): Font)
  override def apply(style: Stylish) = super.apply(st(style))
}

trait Font {
  def face: String = ""
  def size: Float = 0f
  def color: Rgba = Rgba(0, 0, 0, 0)
  def outline: Option[Stroke] = None
  def valign: Font.Vertical = Font.WHEREVER
  def halign: Font.Horizontal = Font.WHEREVER

  def fixed: Font = new FontFixed(this)
  def scaled(scale: Float) = 
    if (scale closeTo 1) this
    else new FontProxy(this) {
      override def size = super.size * scale
      override def outline = super.outline.map(_.scaled(scale))
    }
}
object Font {
  sealed trait Vertical { def text: String; override def toString = text }
  object HANG extends Vertical { def text = "hanging" }
  object VMID extends Vertical { def text = "middle" }
  object BASE extends Vertical { def text = "baseline"}

  sealed trait Horizontal { def text: String; override def toString = text }
  object LEFT extends Horizontal { def text = "start" }
  object HMID extends Horizontal { def text = "middle" }
  object RIGHT extends Horizontal { def text = "end" }

  object WHEREVER extends Vertical with Horizontal { def text = "" }

  val empty: Font = new Font {}

  def apply(fontSize: Float): Font = new Font { override def size = fontSize }
  def apply(fontFace: String, fontSize: Float, fontColor: Rgba): Font = new Font {
    override def face = fontFace
    override def size = fontSize
    override def color = fontColor
  }
  def apply(fontFace: String, fontSize: Float, fontColor: Rgba, fontV: Vertical, fontH: Horizontal): Font = new Font {
    override def face = fontFace
    override def size = fontSize
    override def color = fontColor
    override def valign = fontV
    override def halign = fontH
  }
}
class FontProxy(original: Font) extends Font {
  override def face = original.face
  override def size = original.size
  override def color = original.color
  override def outline = original.outline
  override def valign = original.valign
  override def halign = original.halign
}
final class FontFixed(original: Font) extends Font {
  override val face = original.face
  override val size = original.size
  override val color = original.color
  override val outline = original.outline
  override val valign = original.valign
  override val halign = original.halign
  override def fixed = this
}

trait Stroke {
  def width: Float = 0f
  def strokecolor: Rgba = Rgba(0, 0, 0, 0)
  def join: Option[Option[Float]] = None

  def fixed: Stroke = new StrokeFixed(this)
  def scaled(scale: Float): Stroke = 
    if (scale closeTo 1) this
    else new StrokeProxy(this) { override def width = scale * super.width }
}
object Stroke {
  val empty: Stroke = new Stroke {}
  def apply(strokeWidth: Float) = new Stroke { override def width = strokeWidth }
  def apply(strokeWidth: Float, strokeColor: Rgba) = new Stroke {
    override def width = strokeWidth
    override def strokecolor = strokeColor
  }
  def apply(strokeWidth: Float, strokeColor: Rgba, miterJoin: Option[Float]) = new Stroke {
    override def width = strokeWidth
    override def strokecolor = strokeColor
    override def join = Option(miterJoin)
  }
}
class StrokeProxy(original: Stroke) extends Stroke {
  override def width = original.width
  override def strokecolor = original.strokecolor
  override def join = original.join
}
final class StrokeFixed(original: Stroke) extends Stroke {
  override val width = original.width
  override val strokecolor = original.strokecolor
  override val join = original.join
  override def fixed = this
}

trait Fill {
  def fillcolor: Rgba = Rgba(0, 0, 0, 0)
  def unfilled: Boolean = false
  def fixed: Fill = new FillFixed(this)
}
object Fill {
  val empty: Fill = new Fill {}
  val off: Fill = new Fill { override def unfilled = true }
  def apply(fillColor: Rgba): Fill = new Fill { override def fillcolor = fillColor }
}
class FillProxy(original: Fill) extends Fill {
  override def fillcolor = original.fillcolor
  override def unfilled = original.unfilled
}
final class FillFixed(original: Fill) extends Fill {
  override val fillcolor = original.fillcolor
  override val unfilled = original.unfilled
  override def fixed = this
}

trait Opaque { 
  def opacity: Float = Float.PositiveInfinity
  def fixed: Opaque = new OpaqueFixed(this)
}
object Opaque {
  val empty: Opaque = new Opaque {}
  def apply(opaqueness: Float): Opaque = new Opaque { override def opacity = opaqueness }
}
class OpaqueProxy(original: Opaque) extends Opaque {
  override def opacity = original.opacity
}
final class OpaqueFixed(original: Opaque) extends Opaque {
  override val opacity = original.opacity
  override def fixed = this
}

trait Stylish extends Font with Stroke with Fill with Opaque {
  override def fixed: Stylish = this
  def colored(oneColor: Rgba): Stylish = new StylishProxy(this, this, this, this) {
    private[this] val theColor = if (oneColor.a == 1) oneColor else oneColor.aTo(1)
    override def color = theColor
    override def outline = super.outline.map{ x => new StrokeProxy(x) { override def strokecolor = theColor } }
    override def strokecolor = theColor
    override def fillcolor = theColor
    override def opacity = oneColor.a
  }
  override def scaled(scale: Float): Stylish = 
    if (scale closeTo 1) this
    else new StylishProxy(this, this, this, this) {
      override def size = scale * super.size
      override def width = scale * super.width
      override def outline = super.outline.map(_.scaled(scale))
    }
}
object Stylish {
  def apply(font: Font): Stylish = new StylishProxy(font, Stroke.empty, Fill.empty, Opaque.empty)
  def apply(stroke: Stroke): Stylish = new StylishProxy(Font.empty, stroke, Fill.empty, Opaque.empty)
  def apply(fill: Fill): Stylish = new StylishProxy(Font.empty, Stroke.empty, fill, Opaque.empty)
  def apply(opaque: Opaque): Stylish = new StylishProxy(Font.empty, Stroke.empty, Fill.empty, opaque)
  def apply(font: Font, stroke: Stroke): Stylish = new StylishProxy(font, stroke, Fill.empty, Opaque.empty)
  def apply(font: Font, fill: Fill): Stylish = new StylishProxy(font, Stroke.empty, fill, Opaque.empty)
  def apply(font: Font, opaque: Opaque): Stylish = new StylishProxy(font, Stroke.empty, Fill.empty, opaque)
  def apply(stroke: Stroke, fill: Fill): Stylish = new StylishProxy(Font.empty, stroke, fill, Opaque.empty)
  def apply(stroke: Stroke, opaque: Opaque): Stylish = new StylishProxy(Font.empty, stroke, Fill.empty, opaque)
  def apply(font: Font, stroke: Stroke, fill: Fill): Stylish = new StylishProxy(font, stroke, fill, Opaque.empty)
  def apply(font: Font, stroke: Stroke, opaque: Opaque): Stylish = new StylishProxy(font, stroke, Fill.empty, opaque)
  def apply(font: Font, fill: Fill, opaque: Opaque): Stylish = new StylishProxy(font, Stroke.empty, fill, opaque)
  def apply(stroke: Stroke, fill: Fill, opaque: Opaque): Stylish = new StylishProxy(Font.empty, stroke, fill, opaque)
  def apply(font: Font, stroke: Stroke, fill: Fill, opaque: Opaque): Stylish = new StylishProxy(font, stroke, fill, opaque)
  val empty: Stylish = new Stylish {}
}
class StylishProxy(font: Font, stroke: Stroke, fill: Fill, opaque: Opaque) extends FontProxy(font) with Stylish {
  override def width = stroke.width
  override def strokecolor = stroke.strokecolor
  override def join = stroke.join
  override def fillcolor = fill.fillcolor
  override def unfilled = fill.unfilled
  override def opacity = opaque.opacity
}
final class StylishFixed(original: Stylish) extends Stylish {
  override val face = original.face
  override val size = original.size
  override val color = original.color
  override val outline = original.outline
  override val valign = original.valign
  override val halign = original.halign
  override val width = original.width
  override val strokecolor = original.strokecolor
  override val join = original.join
  override val fillcolor = original.fillcolor
  override val unfilled = original.unfilled
  override val opacity = original.opacity
  override def fixed = this
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

trait InSvg { def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] }

trait StyleTransformer {
  def off: Set[StyleTransformer.Fields]
  def transform: (Stylish => Stylish) = (x => x)
  def apply(s: Stylish): Stylish = {
    val pre = transform(s)
    new StylishProxy(pre, pre, pre ,pre) {
      override def face = if (off contains StyleTransformer.Face) Font.empty.face else super.face
      override def size = if (off contains StyleTransformer.Size) Font.empty.size else super.size
      override def color = if (off contains StyleTransformer.Color) Font.empty.color else super.color
      override def outline = if (off contains StyleTransformer.Outline) Font.empty.outline else super.outline
      override def valign = if (off contains StyleTransformer.VAlign) Font.empty.valign else super.valign
      override def halign = if (off contains StyleTransformer.HAlign) Font.empty.halign else super.halign
      override def width = if (off contains StyleTransformer.Width) Stroke.empty.width else super.width
      override def strokecolor = if (off contains StyleTransformer.StrokeColor) Stroke.empty.strokecolor else super.strokecolor
      override def fillcolor = if (off contains StyleTransformer.FillColor) Fill.empty.fillcolor else super.fillcolor      
      override def opacity = if (off contains StyleTransformer.Opacity) Opaque.empty.opacity else super.opacity
    }
  }
}
object StyleTransformer {
  sealed trait Fields { def text: String }
  final case object Face extends Fields { def text = "font-face" }
  final case object Size extends Fields { def text = "font-size" }
  final case object Color extends Fields { def text = "font-color" }
  final case object Outline extends Fields { def text = "font-outline" }
  final case object VAlign extends Fields { def text = "font-vertical-align" }
  final case object HAlign extends Fields { def text = "font-horizontal-align" }
  final case object Width extends Fields { def text = "stroke-width" }
  final case object StrokeColor extends Fields { def text = "stroke-color" }
  final case object Join extends Fields { def text = "stroke-join" }
  final case object FillColor extends Fields { def text = "fill-color" }
  final case object Opacity extends Fields { def text = "opacity" }
  object Fields {
    val all: Set[Fields] = Set(Face, Size, Color, Outline, VAlign, HAlign, Width, StrokeColor, Join, FillColor, Opacity)
    val font: Set[Fields] = Set(Face, Size, Color, Outline, VAlign, HAlign)
    val stroke: Set[Fields] = Set(Width, StrokeColor, Join)
    val fill: Set[Fields] = Set(FillColor)
    val opacity: Set[Fields] = Set(Opacity)
    val none: Set[Fields] = Set.empty[Fields]  
  }

  val all = new StyleTransformer { def off = Fields.none; override def apply(s: Stylish): Stylish = s }
  val nofont = new StyleTransformer { def off = Fields.font }
  val nostroke = new StyleTransformer { def off = Fields.stroke }
  val nofill = new StyleTransformer { def off = Fields.fill }
  val noopacity = new StyleTransformer { def off = Fields.opacity }
  val noshape = new StyleTransformer { val off = Fields.stroke | Fields.fill }
  val noscale = new StyleTransformer {
    val off = Fields.none
    override def transform: (Stylish => Stylish) = s => new StylishProxy(s, s, s, s) {
      override def width = Stroke.empty.width
      override def size = Font.empty.size
      override def outline = super.outline.map(o => new StrokeProxy(o) { override def width = Stroke.empty.width })
    }
  }
  val onlyscale = new StyleTransformer {
    val off = Fields.opacity | Fields.fill
    override def transform: (Stylish => Stylish) = s => new StylishProxy(
      new FontProxy(Font.empty) {
        override def size = s.size
        override def outline = s.outline.map(o => new StrokeProxy(Stroke.empty) { override def width = o.width })
      },
      new StrokeProxy(Stroke.empty) { override def width = s.width },
      Fill.empty,
      Opaque.empty
    )
  }
  val onlyshape = new StyleTransformer { val off = Fields.font | Fields.opacity }
  val onlyfont = new StyleTransformer { val off = Fields.all -- Fields.font }
  val onlystroke = new StyleTransformer { val off = Fields.all -- Fields.stroke }
  val onlyfill = new StyleTransformer { val off = Fields.all -- Fields.fill }
  val onlyopacity = new StyleTransformer { val off = Fields.all -- Fields.opacity }
  val purestroke = new StyleTransformer { 
    val off = Fields.all -- Fields.stroke
    override def transform = (s: Stylish) => new StylishProxy(s, s, s, s) { override def unfilled = true }
  }
  val none = new StyleTransformer { val off = Fields.all; override def apply(s: Stylish): Stylish = Stylish.empty }
}

