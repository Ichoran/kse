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

  def apply(stylish: Stylish): String = stylish match {
    case Opaque(o)         => apply("opacity", o)
    case FillColor(c)      => apply("fill", c)
    case FillOpacity(o)    => apply("fill-opacity", o)
    case FillNone          => apply("fill", "none")
    case StrokeColor(c)    => apply("stroke", c)
    case StrokeOpacity(o)  => apply("stroke-opacity", o)
    case StrokeWidth(w)    => apply("stroke-width", w)
    case StrokeJoin(j)     => apply(j)
    case StrokeMiter(m)    => apply("stroke-miterlimit", m)
    case StrokeCap(c)      => apply(c)
    case FontFace(f)       => apply("font-family", f)
    case FontSize(s)       => apply("font-size", s)
    case FontVertical(v)   => apply(v)
    case FontHorizontal(h) => apply(h)
    case Style(es)         => es.map(e => apply(e)).mkString
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


trait Textual extends Serializable with Product { 
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

sealed trait Stylish {}
object Stylish {
  def unique(elements: Iterable[Stylish], more: Iterable[Stylish] = List.empty[Stylish]): List[Stylish] = {
    var o, f, fo, sc, so, sw, sj, sm, sp, tf, ts, tv, th, s = true
    val eb = List.newBuilder[Stylish]
    val i = if (more.nonEmpty) elements.iterator ++ more.iterator else elements.iterator
    while (i.hasNext) i.next match {
      case x: Opaque         => if (o ) { o  = false; eb += x }
      case x: FillIndicator  => if (f ) { f  = false; eb += x }
      case x: FillOpacity    => if (fo) { fo = false; eb += x }
      case x: StrokeColor    => if (sc) { sc = false; eb += x }
      case x: StrokeOpacity  => if (so) { so = false; eb += x }
      case x: StrokeWidth    => if (sw) { sw = false; eb += x }
      case x: StrokeJoin     => if (sj) { sj = false; eb += x }
      case x: StrokeMiter    => if (sm) { sm = false; eb += x }
      case x: StrokeCap      => if (sp) { sp = false; eb += x }
      case x: FontFace       => if (tf) { tf = false; eb += x }
      case x: FontSize       => if (ts) { ts = false; eb += x }
      case x: FontVertical   => if (tv) { tv = false; eb += x }
      case x: FontHorizontal => if (th) { th = false; eb += x }
      case x: Style          => if (s ) {  s = false; eb += x }
    }
    eb.result
  }
}

sealed trait Colorful[A <: Colorful[A]] extends Stylish { def rgb: Rgba; def color(f: Rgba => Rgba): A }
sealed trait Ghostly[A <: Ghostly[A]] extends Stylish { def opacity: Float; def solidify(f: Float => Float): A }
sealed trait Scalable[A <: Scalable[A]] extends Stylish { def scale(factor: Float): A }
sealed trait Fillish extends Stylish {}
sealed trait Strokish extends Stylish {}
sealed trait FillIndicator extends Fillish {}

final case class Opaque(opacity: Float)                 extends                    Ghostly[Opaque]        { def solidify(f: Float => Float) = new Opaque(f(opacity)) }
final case class FillColor(rgb: Rgba)                   extends FillIndicator with Colorful[FillColor]    { def color(f: Rgba => Rgba) = new FillColor(f(rgb)) }
final case class FillOpacity(opacity: Float)            extends Fillish       with Ghostly[FillOpacity]   { def solidify(f: Float => Float) = new FillOpacity(f(opacity)) }
final case object FillNone                              extends FillIndicator                             {}      
final case class StrokeColor(rgb: Rgba)                 extends Strokish      with Colorful[StrokeColor]  { def color(f: Rgba => Rgba) = new StrokeColor(f(rgb)) }
final case class StrokeOpacity(opacity: Float)          extends Strokish      with Ghostly[StrokeOpacity] { def solidify(f: Float => Float) = new StrokeOpacity(f(opacity)) }
final case class StrokeWidth(width: Float)              extends Strokish      with Scalable[StrokeWidth]  { def scale(factor: Float) = new StrokeWidth(width * factor) }
final case class StrokeJoin(join: Join)                 extends Strokish                                  {}
final case class StrokeMiter(miter: Float)              extends Strokish                                  {}
final case class StrokeCap(cap: Cap)                    extends Strokish                                  {}
final case class FontFace(face: String)                 extends Stylish                                   {}
final case class FontSize(size: Float)                  extends                    Scalable[FontSize]     { def scale(factor: Float) = new FontSize(factor * size) }
final case class FontVertical(vertical: Vertical)       extends Stylish                                   {}
final case class FontHorizontal(horizontal: Horizontal) extends Stylish                                   {}

object Opacity {
  def apply(opacity: Float) = Style(List(Opaque(opacity)))
}

object Fill {
  val off = Style(List(FillNone))

  def apply(color: Rgba) = Style(List(FillColor(color)))
  def alpha(color: Rgba) = Style(List(FillColor(color), FillOpacity(color.a)))
}

object Stroke {
  def off = Style(List(StrokeWidth(0)))

  def apply(color: Rgba) = Style(List(StrokeColor(color)))
  def apply(width: Float) = Style(List(StrokeWidth(width)))
  def apply(join: Join) = Style(List(StrokeJoin(join)))
  def apply(cap: Cap) = Style(List(StrokeCap(cap)))

  def apply(color: Rgba, width: Float) = Style(List(StrokeColor(color), StrokeWidth(width)))
  def apply(color: Rgba, join: Join) = Style(List(StrokeColor(color), StrokeJoin(join)))
  def apply(color: Rgba, cap: Cap) = Style(List(StrokeColor(color), StrokeCap(cap)))
  def apply(width: Float, join: Join) = Style(List(StrokeWidth(width), StrokeJoin(join)))
  def apply(width: Float, cap: Cap) = Style(List(StrokeWidth(width), StrokeCap(cap)))
  def apply(join: Join, cap: Cap) = Style(List(StrokeJoin(join), StrokeCap(cap)))

  def apply(color: Rgba, width: Float, join: Join) = Style(List(StrokeColor(color), StrokeWidth(width), StrokeJoin(join)))
  def apply(color: Rgba, width: Float, cap: Cap) = Style(List(StrokeColor(color), StrokeWidth(width), StrokeCap(cap)))
  def apply(color: Rgba, join: Join, cap: Cap) = Style(List(StrokeColor(color), StrokeJoin(join), StrokeCap(cap)))
  def apply(width: Float, join: Join, cap: Cap) = Style(List(StrokeWidth(width), StrokeJoin(join), StrokeCap(cap)))

  def apply(color: Rgba, width: Float, join: Join, cap: Cap) =
    Style(List(StrokeColor(color), StrokeWidth(width), StrokeJoin(join), StrokeCap(cap)))

  def alpha(color: Rgba) = Style(List(StrokeColor(color), StrokeOpacity(color.a)))
  def alpha(color: Rgba, width: Float) = Style(List(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width)))
  def alpha(color: Rgba, join: Join) = Style(List(StrokeColor(color), StrokeOpacity(color.a), StrokeJoin(join)))
  def alpha(color: Rgba, cap: Cap) = Style(List(StrokeColor(color), StrokeOpacity(color.a), StrokeCap(cap)))
  def alpha(color: Rgba, width: Float, join: Join) =
    Style(List(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width), StrokeJoin(join)))
  def alpha(color: Rgba, width: Float, cap: Cap) =
    Style(List(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width), StrokeCap(cap)))
  def alpha(color: Rgba, join: Join, cap: Cap) =
    Style(List(StrokeColor(color), StrokeOpacity(color.a), StrokeJoin(join), StrokeCap(cap)))
  def alpha(color: Rgba, width: Float, join: Join, cap: Cap) =
    Style(List(StrokeColor(color), StrokeOpacity(color.a), StrokeWidth(width), StrokeJoin(join), StrokeCap(cap)))

  def miter(limit: Float) = Style(List(StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(color: Rgba, limit: Float) = Style(List(StrokeColor(color), StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(width: Float, limit: Float) = Style(List(StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(limit: Float, cap: Cap) = Style(List(StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
  def miter(color: Rgba, width: Float, limit: Float) =
    Style(List(StrokeColor(color), StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit)))
  def miter(color: Rgba, limit: Float, cap: Cap) =
    Style(List(StrokeColor(color), StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
  def miter(width: Float, limit: Float, cap: Cap) =
    Style(List(StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
  def miter(color: Rgba, width: Float, limit: Float, cap: Cap) =
    Style(List(StrokeColor(color), StrokeWidth(width), StrokeJoin(Join.Miter), StrokeMiter(limit), StrokeCap(cap)))
}

object Font {
  def apply(face: String) = Style(List(FontFace(face)))
  def apply(size: Float) = Style(List(FontSize(size)))
  def apply(horizontal: Horizontal) = Style(List(FontHorizontal(horizontal)))
  def apply(vertical: Vertical) = Style(List(FontVertical(vertical)))

  def apply(face: String, size: Float) = Style(List(FontFace(face), FontSize(size)))
  def apply(face: String, horizontal: Horizontal) = Style(List(FontFace(face), FontHorizontal(horizontal)))
  def apply(face: String, vertical: Vertical) = Style(List(FontFace(face), FontVertical(vertical)))
  def apply(size: Float, horizontal: Horizontal) = Style(List(FontSize(size), FontHorizontal(horizontal)))
  def apply(size: Float, vertical: Vertical) = Style(List(FontSize(size), FontVertical(vertical)))
  def apply(horizontal: Horizontal, vertical: Vertical) = Style(List(FontHorizontal(horizontal), FontVertical(vertical)))

  def apply(face: String, size: Float, horizontal: Horizontal) =
    Style(List(FontFace(face), FontSize(size), FontHorizontal(horizontal)))
  def apply(face: String, size: Float, vertical: Vertical) =
    Style(List(FontFace(face), FontSize(size), FontVertical(vertical)))
  def apply(face: String, horizontal: Horizontal, vertical: Vertical) =
    Style(List(FontFace(face), FontHorizontal(horizontal), FontVertical(vertical)))
  def apply(size: Float, horizontal: Horizontal, vertical: Vertical) =
    Style(List(FontSize(size), FontHorizontal(horizontal), FontVertical(vertical)))

  def apply(face: String, size: Float, horizontal: Horizontal, vertical: Vertical) =
    Style(List(FontFace(face), FontSize(size), FontHorizontal(horizontal), FontVertical(vertical)))
}

final case class Style(elements: List[Stylish]) extends Scalable[Style] with Colorful[Style] with Ghostly[Style] {
  def opacity = {
    val relevant = elements.collect{ case e: Ghostly[_] => e }
    if (relevant.isEmpty) 1f
    else if (relevant.size == 1) relevant.head.opacity
    else {
      val overall = relevant.collectFirst{ case o: Opaque => o.opacity } getOrElse 1f
      val max = (0f /: relevant){ (x, r) => r match { case o: Opaque => x; case _ => math.max(x, r.opacity) } }
      overall * max
    }
  }
  def rgb = {
    val relevant = elements.collect{ case c: Colorful[_] => c.rgb }
    if (relevant.isEmpty) Rgba.Black
    else if (relevant.size == 1) relevant.head
    else relevant.toArray.map(c => Array(c.r, c.g, c.b)).transpose.map(_.sum / relevant.size) match {
      case Array(r, g, b) => Rgba(r, g, b, 1)
    }
  }

  def scale(factor: Float): Style = new Style(elements.map{ case sc: Scalable[_] => sc.scale(factor); case e => e })

  def color(f: Rgba => Rgba): Style = new Style(elements.map{ case cf: Colorful[_] => cf.color(f); case e => e })

  def solidify(f: Float => Float): Style = new Style(elements.map{ case gh: Ghostly[_] => gh.solidify(f); case e => e })

  def fade(f: Float => Float): Style =
    if (elements.count{ case g: Ghostly[_] => true; case _ => false } == 1) solidify(f)
    else this + elements.collectFirst{ case o: Opaque => o.solidify(f) }.getOrElse(Opaque(f(1)))

  def +(s: Stylish) = 
    new Style(Stylish.unique(s :: elements))

  def ++(style: Style) = new Style(Stylish.unique(style.elements, elements))

  def map(f: Stylish => Stylish) = new Style(Stylish.unique(elements.map(f)))

  def flatMap(f: Stylish => Style) = new Style(Stylish.unique(elements.flatMap{ e => f(e).elements }))

  def collect(pf: PartialFunction[Stylish, Stylish]) = new Style(Stylish.unique(elements.collect(pf)))

  def filter(p: Stylish => Boolean) = {
    val es = elements.filter(p)
    if (es == elements) this else new Style(es)
  }

  def withFilter(p: Stylish => Boolean) = filter(p)

  def foreach[U](f: Stylish => U) { elements.foreach(f) }

  def shapely: Style =
    new Style(elements.filter{ e => e match { case _: Strokish | _: Fillish | _: Opaque => true; case _ => false } })

  def filly: Style =
    new Style(elements.filter{ e => e match { case _: Fillish | _: Opaque => true; case _ => false } })

  def stroky: Style =
    new Style(Stylish.unique(FillNone :: elements.filter{ e => e match { case _: Strokish | _: Opaque => true; case _ => false } }))

  def unfilled: Style = this + FillNone

  def explicitFillOnly = 
    if (elements.exists{ case f: Fillish => true; case _ => false }) this
    else this + FillNone

  def unstroked: Style =
    new Style(elements.filter{ e => e match { case _: Strokish => false; case _ => true } })

  def generally: Style = {
    val o = opacity
    val es = elements.collect[Stylish, List[Stylish]]{
      case FillNone => FillNone
      case sj: StrokeJoin => sj
      case sm: StrokeMiter => sm
      case sc: StrokeCap => sc
      case ff: FontFace => ff
      case fv: FontVertical => fv
      case fh: FontHorizontal => fh
    }
    new Style(Stylish.unique(Opaque(o) :: es))
  }

  def specifically: Style = {
    val o = opacity
    val es = elements.collect[Stylish, List[Stylish]]{
      case fc: FillColor => fc
      case fo: FillOpacity if (fo.opacity < o && !(fo.opacity closeTo o)) => fo.solidify(_ / o)
      case sc: StrokeColor => sc
      case so: StrokeOpacity if (so.opacity < o && !(so.opacity closeTo o)) => so.solidify(_ / o)
      case sw: StrokeWidth => sw
      case fs: FontSize => fs
    }
    new Style(es)
  }
}
object Style {
  val empty = new Style(Nil)
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
