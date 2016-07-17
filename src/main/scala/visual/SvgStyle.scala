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

  def apply(color: Rgba) = "#" + color.rgbText

  def apply(tag: String, text: String): String = if (text.isEmpty) "" else f" $tag=$q$text$q"
  def apply(tag: String, value: Float): String = if (value.finite) f" $tag=$q${apply(value)}$q" else ""
  def apply(tag: String, color: Rgba): String = f" $tag=$q${apply(color)}$q"

  def apply(tx: Textual): String = if (tx.text.isEmpty) "" else f" ${tx.label}=$q${tx.text}$q"

  def apply(stylish: Stylish): String =
    if (stylish.off) ""
    else stylish match {
      case Opaque(o, _)         => apply("opacity", o)
      case FillColor(c, _)      => apply("fill", c)
      case FillOpacity(o, _)    => apply("fill-opacity", o)
      case FillNone(_)          => apply("fill", "none")
      case StrokeColor(c, _)    => apply("stroke", c)
      case StrokeOpacity(o, _)  => apply("stroke-opacity", o)
      case StrokeWidth(w, _)    => apply("stroke-width", w)
      case StrokeJoin(j, _)     => apply(j)
      case StrokeMiter(m, _)    => apply("stroke-miterlimit", m)
      case StrokeCap(c, _)      => apply(c)
      case FontFace(f, _)       => apply("font-family", f)
      case FontSize(s, _)       => apply("font-size", s)
      case FontVertical(v, _)   => apply(v)
      case FontHorizontal(h, _) => apply(h)
      case Style(es, _)         => es.map(e => apply(e)).mkString
    }
}
class ProxyFormatter(original: Formatter) extends Formatter {
  override def apply(x: Float) = original(x)

  override def apply(v: Vc, c: Char) = original(v, c)
  override def apply(v: Vc) = original(v)
  override def comma(v: Vc) = original comma v
  override def vquote(v: Vc, xname: String, yname: String) = original.vquote(v, xname, yname)

  override def apply(color: Rgba) = original(color)

  override def apply(tag: String, text: String) = original(tag, text)
  override def apply(tag: String, value: Float) = original(tag, value)
  override def apply(tag: String, color: Rgba) = original(tag, color)

  override def apply(tx: Textual) = original(tx)

  override def apply(stylish: Stylish): String = original(stylish)
}
object DefaultFormatter extends Formatter {}


trait Textual { 
  def text: String
  def label: String
}

sealed trait Join extends Textual { def label = "stroke-linejoin" }
object Join {
  final case object Miter extends Join { def text = "miter" }
  final case object Bevel extends Join { def text = "bevel" }
  final case object Round extends Join { def text = "round" }
}

sealed trait Cap extends Textual { def label = "stroke-linecap" }
object Cap {
  final case object Butt extends Cap { def text = "butt" }
  final case object Round extends Cap { def text = "round" }
  final case object Square extends Cap { def text = "square" }
}

sealed trait Vertical extends Textual { def label = "dominant-baseline" }
object Vertical {
  final case object Top extends Vertical { def text = "hanging" }
  final case object Middle extends Vertical { def text = "middle" }
  final case object Bottom extends Vertical { def text = "baseline" }
}

sealed trait Horizontal extends Textual { def label = "text-anchor"}
object Horizontal {
  final case object Left extends Horizontal { def text = "start" }
  final case object Middle extends Horizontal { def text = "middle" }
  final case object Right extends Horizontal { def text = "end" }
}

final case object Whatever extends Join with Cap with Vertical with Horizontal { 
  override def label = ""
  def text = ""
}

sealed trait Stylish {
  def category: Int
  def off: Boolean
  def toggle: Stylish
  def toOff: Stylish
  def toOn: Stylish
}
sealed abstract class Stylized[+A <: Stylized[_]](val category: Int) extends Stylish {
  def me: A
  override def toggle: A
  override def toOff: A = if (off) me else toggle
  override def toOn: A = if (off) toggle else me
}
sealed trait Colorful[A <: Colorful[_]] extends Stylish { def rgb: Rgba; def colorize(f: Rgba => Rgba): A }
sealed trait Ghostly[A <: Ghostly[_]] extends Stylish { def opacity: Float; def luminize(f: Float => Float): A }
sealed trait Scalable[A <: Scalable[_]] extends Stylish { def scale(factor: Float): A }
sealed abstract class Fillish[A <: Stylized[_]](category: Int) extends Stylized[A](category) {}
sealed abstract class Strokish[A <: Stylized[_]](category: Int) extends Stylized[A](category) {}

final case class Opaque(opacity: Float, off: Boolean = false)                 extends Stylized[Opaque](1)        with Ghostly[Opaque]        { def me = this; def toggle = new Opaque(opacity, !off);        def luminize(f: Float => Float) = new Opaque(f(opacity), off) }
final case class FillColor(rgb: Rgba, off: Boolean = false)                   extends Fillish[FillColor](2)      with Colorful[FillColor]    { def me = this; def toggle = new FillColor(rgb, !off);         def colorize(f: Rgba => Rgba) = new FillColor(f(rgb), off) }
final case class FillOpacity(opacity: Float, off: Boolean = false)            extends Fillish[FillOpacity](3)    with Ghostly[FillOpacity]   { def me = this; def toggle = new FillOpacity(opacity, !off);   def luminize(f: Float => Float) = new FillOpacity(f(opacity), off) }
final case class FillNone(off: Boolean = false)                               extends Fillish[FillNone](2)                                   { def me = this; def toggle = new FillNone(!off) }
final case class StrokeColor(rgb: Rgba, off: Boolean = false)                 extends Strokish[StrokeColor](4)   with Colorful[StrokeColor]  { def me = this; def toggle = new StrokeColor(rgb, !off);       def colorize(f: Rgba => Rgba) = new StrokeColor(f(rgb), off) }
final case class StrokeOpacity(opacity: Float, off: Boolean = false)          extends Strokish[StrokeOpacity](5) with Ghostly[StrokeOpacity] { def me = this; def toggle = new StrokeOpacity(opacity, !off); def luminize(f: Float => Float) = new StrokeOpacity(f(opacity), off) }
final case class StrokeWidth(width: Float, off: Boolean = false)              extends Strokish[StrokeWidth](6)   with Scalable[StrokeWidth]  { def me = this; def toggle = new StrokeWidth(width, !off);     def scale(factor: Float) = new StrokeWidth(width * factor, off) }
final case class StrokeJoin(join: Join, off: Boolean = false)                 extends Strokish[StrokeJoin](7)                                { def me = this; def toggle = new StrokeJoin(join, !off) }
final case class StrokeMiter(miter: Float, off: Boolean = false)              extends Strokish[StrokeMiter](8)                               { def me = this; def toggle = new StrokeMiter(miter, !off) }
final case class StrokeCap(cap: Cap, off: Boolean = false)                    extends Strokish[StrokeCap](9)                                 { def me = this; def toggle = new StrokeCap(cap, !off) }
final case class FontFace(face: String, off: Boolean = false)                 extends Stylized[FontFace](10)                                 { def me = this; def toggle = new FontFace(face, !off) }
final case class FontSize(size: Float, off: Boolean = false)                  extends Stylized[FontSize](11)      with Scalable[FontSize]    { def me = this; def toggle = new FontSize(size, !off);         def scale(factor: Float) = new FontSize(factor * size, off) }
final case class FontVertical(vertical: Vertical, off: Boolean = false)       extends Stylized[FontVertical](12)                             { def me = this; def toggle = new FontVertical(vertical, !off) }
final case class FontHorizontal(horizontal: Horizontal, off: Boolean = false) extends Stylized[FontHorizontal](13)                           { def me = this; def toggle = new FontHorizontal(horizontal, !off) }

object Opacity {
  def apply(opacity: Float) = Style(Set(Opaque(opacity)))
}

object Fill {
  val off = Style(Set(FillNone()))

  def apply(color: Rgba) = Style(Set(FillColor(color)))
  def alpha(color: Rgba) = Style(Set(FillColor(color), FillOpacity(color.a)))
}

object Stroke {
  def off = Style(Set(StrokeWidth(0)))

  def apply(color: Rgba) = Style(Set(StrokeColor(color)))
  def apply(width: Float) = Style(Set(StrokeWidth(width)))
  def apply(join: Join) = Style(Set(StrokeJoin(join)))
  def apply(cap: Cap) = Style(Set(StrokeCap(cap)))

  def apply(color: Rgba, width: Float) = Style(Set(StrokeColor(color), StrokeWidth(width)))
  def apply(color: Rgba, join: Join) = Style(Set(StrokeColor(color), StrokeJoin(join)))
  def apply(color: Rgba, cap: Cap) = Style(Set(StrokeColor(color), StrokeCap(cap)))
  def apply(width: Float, join: Join) = Style(Set(StrokeWidth(width), StrokeJoin(join)))
  def apply(width: Float, cap: Cap) = Style(Set(StrokeWidth(width), StrokeCap(cap)))
  def apply(join: Join, cap: Cap) = Style(Set(StrokeJoin(join), StrokeCap(cap)))

  def apply(color: Rgba, width: Float, join: Join) = Style(Set(StrokeColor(color), StrokeWidth(width), StrokeJoin(join)))
  def apply(color: Rgba, width: Float, cap: Cap) = Style(Set(StrokeColor(color), StrokeWidth(width), StrokeCap(cap)))
  def apply(color: Rgba, join: Join, cap: Cap) = Style(Set(StrokeColor(color), StrokeJoin(join), StrokeCap(cap)))
  def apply(width: Float, join: Join, cap: Cap) = Style(Set(StrokeWidth(width), StrokeJoin(join), StrokeCap(cap)))

  def apply(color: Rgba, width: Float, join: Join, cap: Cap) =
    Style(Set(StrokeColor(color), StrokeWidth(width), StrokeJoin(join), StrokeCap(cap)))

  def alpha(color: Rgba) = Style(Set(StrokeColor(color), StrokeOpacity(color.a)))
  def alpha(color: Rgba, width: Float) = Style(Set(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width)))
  def alpha(color: Rgba, join: Join) = Style(Set(StrokeColor(color), StrokeOpacity(color.a), StrokeJoin(join)))
  def alpha(color: Rgba, cap: Cap) = Style(Set(StrokeColor(color), StrokeOpacity(color.a), StrokeCap(cap)))
  def alpha(color: Rgba, width: Float, join: Join) =
    Style(Set(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width), StrokeJoin(join)))
  def alpha(color: Rgba, width: Float, cap: Cap) =
    Style(Set(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width), StrokeCap(cap)))
  def alpha(color: Rgba, join: Join, cap: Cap) =
    Style(Set(StrokeColor(color), StrokeOpacity(color.a), StrokeJoin(join), StrokeCap(cap)))
  def alpha(color: Rgba, width: Float, join: Join, cap: Cap) =
    Style(Set(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width), StrokeJoin(join), StrokeCap(cap)))

  def miter(limit: Float) = Style(Set(StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(color: Rgba, limit: Float) = Style(Set(StrokeColor(color), StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(width: Float, limit: Float) = Style(Set(StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(limit: Float, cap: Cap) = Style(Set(StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
  def miter(color: Rgba, width: Float, limit: Float) =
    Style(Set(StrokeColor(color), StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(color: Rgba, limit: Float, cap: Cap) =
    Style(Set(StrokeColor(color), StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
  def miter(width: Float, limit: Float, cap: Cap) =
    Style(Set(StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
  def miter(color: Rgba, width: Float, limit: Float, cap: Cap) =
    Style(Set(StrokeColor(color), StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
}

object Font {
  def apply(face: String) = Style(Set(FontFace(face)))
  def apply(size: Float) = Style(Set(FontSize(size)))
  def apply(vertical: Vertical) = Style(Set(FontVertical(vertical)))
  def apply(horizontal: Horizontal) = Style(Set(FontHorizontal(horizontal)))

  def apply(face: String, size: Float) = Style(Set(FontFace(face), FontSize(size)))
  def apply(face: String, vertical: Vertical) = Style(Set(FontFace(face), FontVertical(vertical)))
  def apply(face: String, horizontal: Horizontal) = Style(Set(FontFace(face), FontHorizontal(horizontal)))
  def apply(size: Float, vertical: Vertical) = Style(Set(FontSize(size), FontVertical(vertical)))
  def apply(size: Float, horizontal: Horizontal) = Style(Set(FontSize(size), FontHorizontal(horizontal)))
  def apply(vertical: Vertical, horizontal: Horizontal) = Style(Set(FontVertical(vertical), FontHorizontal(horizontal)))

  def apply(face: String, size: Float, vertical: Vertical) =
    Style(Set(FontFace(face), FontSize(size), FontVertical(vertical)))
  def apply(face: String, size: Float, horizontal: Horizontal) =
    Style(Set(FontFace(face), FontSize(size), FontHorizontal(horizontal)))
  def apply(face: String, vertical: Vertical, horizontal: Horizontal) =
    Style(Set(FontFace(face), FontVertical(vertical), FontHorizontal(horizontal)))
  def apply(size: Float, vertical: Vertical, horizontal: Horizontal) =
    Style(Set(FontSize(size), FontVertical(vertical), FontHorizontal(horizontal)))

  def apply(face: String, size: Float, vertical: Vertical, horizontal: Horizontal) =
    Style(Set(FontFace(face), FontSize(size), FontVertical(vertical), FontHorizontal(horizontal)))
}

final case class Style(elements: Set[Stylish], off: Boolean = false) extends Scalable[Style] with Colorful[Style] with Ghostly[Style] {
  def listed = elements.toList

  def opacity = {
    val relevant = elements.collect{ case e: Ghostly[_] if !e.off => e }
    if (relevant.isEmpty) 1f
    else if (relevant.size == 1) relevant.head.opacity
    else {
      val overall = relevant.collectFirst{ case o: Opaque => o.opacity } getOrElse 1f
      val max = (0f /: relevant){ (x, r) => r match { case o: Opaque => x; case _ => math.max(x, r.opacity) } }
      overall * max
    }
  }
  def rgb = {
    val relevant = elements.collect{ case c: Colorful[_] if !c.off => c.rgb }
    if (relevant.isEmpty) Rgba.Black
    else if (relevant.size == 1) relevant.head
    else relevant.toArray.map(c => Array(c.r, c.g, c.b)).transpose.map(_.sum / relevant.size) match {
      case Array(r, g, b) => Rgba(r, g, b, 1)
    }
  }
  def category = 0
  def toOff = if (off) this else new Style(elements, true)
  def toOn = if (off) new Style(elements, false) else this
  def toggle = if (off) toOn else toOff

  def scale(factor: Float): Style = new Style(
    elements.map{
      case sc: Scalable[_] => sc.scale(factor)
      case e               => e
    },
    off
  )
  def colorize(f: Rgba => Rgba): Style = new Style(
    elements.map{
      case cf: Colorful[_] => cf.colorize(f)
      case e               => e
    },
    off
  )
  def luminize(f: Float => Float): Style = new Style(
    elements.map{
      case gh: Ghostly[_] => gh.luminize(f)
      case e              => e
    },
    off
  )

  def +(s: Stylish) = 
    new Style((if (elements.exists(_.category == s.category)) elements.filter(_.category != s.category) else elements) + s, off)
  def -(s: Stylish) =
    if (!elements.exists(_.category == s.category)) this
    else new Style(elements.filter(_.category != s.category), off)
  def ++(style: Style) = 
    if (off != style.off) { if (off) style else this }
    else new Style((listed ++ style.listed).groupBy(_.category).map(_._2.last).toSet, off)
  def map(f: Stylish => Stylish) = {
    val es = elements.map(f)
    if (es.map(_.category).size == es.size) new Style(es, off)
    else new Style(es.toList.groupBy(_.category).map{ case (_, vs) => vs.last }.toSet, off)
  }
  def flatMap(f: Stylish => Style) = {
    val es = elements.flatMap{ e => val x = f(e); if (x.off) Set.empty[Stylish] else x.elements }
    if (es.map(_.category).size == es.size) new Style(es, off)
    else new Style(es.toList.groupBy(_.category).map{ case (_, vs) => vs.last }.toSet, off)
  }
  def filter(p: Stylish => Boolean) = {
    val es = elements.filter(p)
    if (es == elements) this else new Style(es, off)
  }
  def withFilter(p: Stylish => Boolean) = filter(p)
  def foreach[U](f: Stylish => U) { elements.foreach(f) }
  def keep(p: Stylish => Boolean) = new Style(elements.map(e => if (p(e)) e else e.toOff), off)
  def enable(p: Stylish => Boolean) = new Style(elements.map(e => if (p(e)) e.toOn else e), off)

  def shapely: Style =
    new Style(elements.filter{ e => e match { case _: Strokish[_] | _: Fillish[_] | _: Opaque => !e.off; case _ => false } }, off)

  def filly: Style =
    new Style(elements.filter{ e => e match { case _: Fillish[_] | _: Opaque => !e.off; case _ => false } }, off)

  def stroky: Style =
    new Style(elements.filter{ e => e match { case _: Strokish[_] | _: Opaque => !e.off; case _ => false } } + FillNone(), off)

  def common(that: Style): Style =
    if (off != that.off) Style(Set())
    else new Style((listed ++ that.listed).groupBy(_.category).collect{ case (_, x :: y :: Nil) if (x == y) => x }.toSet, off)

  def unique(that: Style): (Style, Style) =
    if (off != that.off) (this, that)
    else {
      val thiscat = elements.map(_.category)
      val thatcat = that.elements.map(_.category)
      val shared = thiscat & thatcat
      val sharethis = elements.filter(e => shared(e.category)).map(e => e.category -> e).toMap
      val sharethat = that.elements.filter(e => shared(e.category)).map(e => e.category -> e).toMap
      ( new Style(elements.filterNot(e => shared(e.category) && e == sharethat(e.category))),
        new Style(that.elements.filterNot(e => shared(e.category) && e == sharethis(e.category)))
      )
    }

  def generally: Style = {
    val o = opacity
    val es = elements.collect[Stylish, Set[Stylish]]{
      case fn: FillNone => fn
      case sj: StrokeJoin => sj
      case sm: StrokeMiter => sm
      case sc: StrokeCap => sc
      case ff: FontFace => ff
      case fv: FontVertical => fv
      case fh: FontHorizontal => fh
    }
    new Style(es + Opaque(o), off)
  }

  def specifically: Style = {
    val o = opacity
    val es = elements.collect[Stylish, Set[Stylish]]{
      case fc: FillColor => fc
      case fo: FillOpacity if (fo.opacity < o && !(fo.opacity closeTo o)) => fo.luminize(_ / o)
      case sc: StrokeColor => sc
      case so: StrokeOpacity if (so.opacity < o && !(so.opacity closeTo o)) => so.luminize(_ / o)
      case sw: StrokeWidth => sw
      case fs: FontSize => fs
    }
    new Style(es, off)
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

sealed trait Indent {
  def level: Int
  def in: Indent
}
object Indent {
  def apply(text: String) = InText(text)
  def apply(text: String, level: Int) = InText(text, level)
  def V(text: String*) = {
    val N = text.length
    if (N == 0) Vector.empty[Indent]
    else {
      val vb = Vector.newBuilder[Indent]
      var i = 0
      text.foreach{ t => i += 1; vb += new InText(t, if (i==1 || i==N) 0 else 1) }
      vb.result
    }
  }
}
final case class InText(text: String, level: Int = 0) extends Indent {
  def in = new InText(text, level+1)
  override def toString = if (level <= 0) text else " "*(2*level) + text
}

trait InSvg {
  def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent]
}
