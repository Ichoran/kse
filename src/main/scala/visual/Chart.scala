// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr, UCSF, and Calico Labs.

package kse.visual

import scala.math._
import scala.util._
import scala.collection.mutable.{ AnyRefMap => RMap }

import kse.coll._
import kse.maths._
import kse.maths.stats._
import kse.flow._
import kse.eio._

object Chart {
  private val q = "\""

  trait NumberFormatter { def fmt(x: Float): String }
  trait AppearanceFormatter { def fmt(a: Appearance): String }

  val defaultNumberFormatter = new NumberFormatter { 
    def fmt(x: Float) = if (x.toInt == x) x.toInt.toString else if (x*10 == (x*10).toInt) "%.1f".format(x) else "%.2f".format(x)
  }
  val emptyAppearanceFormatter = new AppearanceFormatter { def fmt(a: Appearance) = "" }
  val defaultAppearanceFormatter = new AppearanceFormatter {
    def fmt(a: Appearance) =
      a.strokeColor.get.map(c => f" stroke=$q#${c.rgbText}$q").getOrElse("") +
      a.stroke.get.filter(x => x.finite && x > 0).map(w => f" stroke-width=$q$w%.2f$q").getOrElse("") +
      a.fillColor.get.map(c => f" fill=$q#${c.rgbText}$q").getOrElse("") +
      a.opacity.get.filter(_ < 0.9995).map(o => f" opacity=$q$o%.3f$q").getOrElse("") +
      ( if (!a.opacity.alive && a.stroke.alive && a.strokeColor.alive)
          a.strokeColor.value.a match { case x if x < 0.9995 => f" stroke-opacity=$q$x%.3f$x"; case _ => "" }
        else ""
      ) +
      ( if (!a.opacity.alive && a.fillColor.alive)
          a.fillColor.value.a match { case x if x < 0.9995 => f" fill-opacity=$q$x%.3f$x"; case _ => "" }
        else ""
      )
  }

  sealed trait Appearance {
    def opacity: Q[Float]
    def stroke: Q[Float]
    def strokeColor: Q[Rgba]
    def fillColor: Q[Rgba]
  }
  sealed trait ProxyAppear {
    def appear: Q[Appearance]
    val opacity = appear.flatMap(_.opacity)
    val stroke = appear.flatMap(_.stroke)
    val strokeColor = appear.flatMap(_.strokeColor)
    val fillColor = appear.flatMap(_.fillColor)
  }
  final class AppearanceOf(that: Appearance) extends Appearance {
    val opacity = that.opacity.map(identity)
    val stroke = that.stroke.map(identity)
    val strokeColor = that.strokeColor.map(identity)
    val fillColor = that.fillColor.map(identity)
  }
  object Plain extends Appearance {
    val opacity = Q empty 1f
    val stroke = Q empty 0f
    val color = Q empty Rgba(0, 0, 0, 1)
    def fillColor = color
    def strokeColor = color
  }
  final class Filled(theColor: Rgba) extends Appearance {
    val opacity = Q(theColor.a match { case x if x.finite && x >= 0 && x < 1 => x; case _ => 1f })
    val stroke = Q empty 0f
    val strokeColor = Q empty Rgba(0, 0, 0, 0)
    val fillColor = Q(theColor)
  }
  object Filled {
    def apply(theColor: Rgba) = new Filled(theColor)
  }
  final class Translucent(theOpacity: Float) extends Appearance {
    val opacity = Q(theOpacity match { case x if x.finite && x >= 0 && x < 1 => x; case _ => 1f })
    val stroke = Q empty 0f
    val strokeColor = Plain.color
    val fillColor = Plain.color
  }
  object Translucent {
    def apply(theOpacity: Float) = new Translucent(theOpacity)
  }

  final case class IndentedSvg(text: String, level: Int = 0) {
    override def toString = if (level <= 0) text else " "*(2*level) + text
  }
  trait InSvg { def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] }

  final case class Circ(c: Q[Vc], r: Q[Float], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vc = c.value
      val vr = r.value
      if (!vr.finite || vr == 0 || !vc.finite) return Vector.empty
      val circ = kse.visual.Circle(vc, vr) into xform
      if (!circ.radius.finite || circ.radius == 0 || !circ.center.finite) return Vector.empty
      Vector(IndentedSvg(
        f"<circle cx=$q${nf fmt circ.center.x}$q cy=$q${nf fmt circ.center.y}$q r=$q${nf fmt circ.radius}$q${af fmt appear.value}/>"
      ))
    }
  }

  final case class Bar(c: Q[Vc], r: Q[Vc], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vc = c.value
      val vr = r.value
      if (!vr.finite || vr.x == 0 || vr.y == 0) return Vector.empty
      val rect0 = 
        if (vr.x < vr.y) kse.visual.Rect(vc, Vc(0, vr.y), vr.x/vr.y)
        else kse.visual.Rect(vc, Vc(vr.x, 0), vr.y/vr.x)
      val rect = rect0 into xform
      if (!rect.major.finite || ((rect.major.y closeTo 0) && (rect.major.x closeTo 0)) || !rect.aspect.finite || rect.aspect == 0) return Vector.empty
      Vector(IndentedSvg({
        if ((rect.major.x closeTo 0) || (rect.major.y closeTo 0)) {
          val rw = rect.major.x.abs + (rect.aspect*rect.major.y).abs
          val rh = rect.major.y.abs + (rect.aspect*rect.major.x).abs
          val x = rect.center.x - rw;
          val y = rect.center.y - rh;
          f"<rect x=$q${nf fmt x}$q y=$q${nf fmt y}$q width=$q${nf fmt 2*rw}$q height=$q${nf fmt 2*rh}$q${af fmt appear.value}/>"
        }
        else {
          f"<polygon points=$q${rect.corners.map{ l => val v = Vc from l; (nf fmt v.x) + "," + (nf fmt v.y)}.mkString(" ")}$q${af fmt appear.value}/>"
        }
      }))
    }
  }

  final case class Letters(pt: Q[Vc], text: Q[String], height: Q[Float], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val p = pt.value
      val vl = xform(p - Vc(0, 0.5f*height.value))
      val vh = xform(p + Vc(0, 0.5f*height.value))
      if (vl.x closeTo vh.x) {
        val size = (vh.y - vl.y).abs
        Vector(IndentedSvg(
          f"<text x=$q${nf fmt vl.x}$q y=$q${nf fmt vl.y}$q font-size=$q${nf fmt size}$q text-anchor=${"\"middle\""}${af fmt appear.value}>${text.value}</text>"
        ))        
      }
      else ???
    }
  }

  sealed class MuGroup(var elements: Vector[InSvg]) extends InSvg {
    def +=(i: InSvg): this.type = { elements = elements :+ i; this }
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter) = {
      elements.flatMap(i => i.inSvg(xform)(nf, af)) match {
        case vs => IndentedSvg("<g>") +: vs.map(x => x.copy(level = x.level + 1)) :+ IndentedSvg("</g>")
      }
    }
  }
  object MuGroup {
    def apply(es: InSvg*) = new MuGroup(es.toVector)
  }

  final class Origin(at: Q[Vc], theElements: Vector[InSvg]) extends MuGroup(theElements) {
    private[this] def unoriginate: Xform = Xform.natural(Frame(at.value, Vc(1,0)), Frame.natural)
    override def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter) =
      super.inSvg(if (at.alive) unoriginate andThen xform else xform)(nf, af)
  }
  object Origin {
    def apply(at: Q[Vc], theElements: Vector[InSvg]) = new Origin(at, theElements)
  }

  final class Axes(scaling: Q[Vc], theScaled: Vector[InSvg]) extends MuGroup(theScaled) {
    private[this] def unscale: Xform = Xform.scaly(scaling.value).inverted
    override def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter) =
      super.inSvg(if (scaling.alive) unscale andThen xform else xform)(nf, af)
  }
  object Axes {
    def apply(scaling: Q[Vc], theScaled: Vector[InSvg]) = new Axes(scaling, theScaled)
  }

  def quick(i: InSvg, offset: Vc = Vc(0,0)) {
    val fr = if (offset == Vc(0,0)) Frame.natural else Frame.natural.copy(origin = offset)
    val svg = 
      Vector("<html>", "<body>", """<svg width="640" height="480">""").map(x => IndentedSvg(x)) ++
      i.inSvg(Xform.flipy(fr, Frame(0 vc -480, 1 vc 0)))(defaultNumberFormatter, defaultAppearanceFormatter).map(x => x.copy(level = x.level+1)) ++
      Vector("</svg>", "</body>", "</html>").map(x => IndentedSvg(x))
    println(svg.mkString("\n"))
    svg.map(_.toString).toFile("test.html".file)
  }



  trait Svgable {
    def toSvg: Vector[String]
  }

  trait SvgLine extends Svgable {
    def svgLine: String
    def toSvg = Vector[String](svgLine)
  }

  trait SvgBasicStyled extends Svgable {
    def style: Style
    def restyle(f: Style => Style): SvgBasicStyled
    def styleTag = style.tag.map(x => "style=\""+x+"\" ") match { case Some(s) => s; case None => "" }    
  }

  trait SvgStyled[S <: SvgStyled[S]] extends SvgBasicStyled {
    override def restyle(f: Style => Style): S
  }

  trait SvgLineStyled[S <: SvgStyled[S]] extends SvgLine with SvgStyled[S] {}

  case class Style(
    name: Option[String] = None,
    fillc: Option[Ok[String, Rgba]] = None,
    linec: Option[Ok[String, Rgba]] = None,
    linew: Option[Float] = None,
    alpha: Option[Float] = None
  ) {
    val mask =
      (if (fillc.isEmpty) 0 else 1) |
      (if (linec.isEmpty) 0 else 2) |
      (if (linew.isEmpty) 0 else 4) |
      (if (alpha.isEmpty) 0 else 8)

    def orElse(s: Style) = 
      if ((s.mask & ~mask & 0xF) == 0) this
      else new Style(name, fillc orElse s.fillc, linec orElse s.linec, linew orElse s.linew, alpha orElse s.alpha)

    def diff(s: Style) = 
      if ((mask & s.mask) == 0) this
      else new Style(
        name,
        if (fillc == s.fillc) None else fillc,
        if (linec == s.linec) None else linec,
        if (linew == s.linew) None else linew,
        if (alpha == s.alpha) None else alpha
      )

    def tag: Option[String] = (
      fillc.map{ _.fold(s => "fill:" + s)(c => "fill:#"+c.rgbText+";fill-opacity:"+c.a.fmt) }.toList ++
      linec.map{ _.fold(s => "stroke:" + s)(c => "stroke:#"+c.rgbText+";stroke-opacity:"+c.a.fmt) }.toList ++
      linew.map{ w => "stroke-width:"+w.fmt } ++
      alpha.map{ a => "opacity:"+a.fmt }
    ).reduceLeftOption(_ + ";" + _)
  }
  object Style {
    val empty = Style()
  }

  trait Grouped extends Svgable {
    def group: Group
    override def toSvg = group.toSvg
  }

  class Group private (val global: Style, val specific: Seq[SvgBasicStyled]) extends SvgStyled[Group] {
    def style = global
    def restyle(f: Style => Style) = {
      val nglob = f(global)
      Group.apply(nglob, specific.map(x => x.restyle(y => f(y orElse global))): _*)
    }
    def toSvg = (if (style == Style.empty) "<g>" else s"<g $styleTag>") +: (specific.flatMap(_.toSvg).map("  " + _).toVector :+ "</g>")
  }
  
  object Group {
    private def canonize[A, B](xs: Seq[A])(f: A => Option[B]): Option[(B, A)] = 
      xs.
      groupBy(x => f(x) match { case None => return None; case Some(b) => b }).
      filter(_._2.length > min(1, xs.length-1)).
      toList.sortBy(- _._2.length).
      headOption.map{ x => x._1 -> x._2.head }

    def apply(style: Style, elts: SvgBasicStyled*): Group = {
      val styles = elts.map(_.style)

      val canonicalFillC = canonize(styles)(x => x.fillc)
      val canonicalLineC = canonize(styles)(x => x.linec)
      val canonicalLineW = canonize(styles)(x => x.linew)
      val canonicalAlpha = canonize(styles)(x => x.alpha)

      val mine = style.copy(
        fillc = style.fillc orElse canonicalFillC.map(_._1),
        linec = style.linec orElse canonicalLineC.map(_._1),
        linew = style.linew orElse canonicalLineW.map(_._1),
        alpha = style.alpha orElse canonicalAlpha.map(_._1)
      )

      new Group(mine, elts.map(_.restyle(s => s diff mine)))
    }

    def apply(elts: SvgBasicStyled*): Group = apply(Style.empty, elts: _*)
  }

  case class Circle(c: Vc, r: Float, style: Style) extends SvgLineStyled[Circle] {
    def restyle(f: Style => Style) = copy(style = f(style))
    def svgLine = f"<circle cx=$q${c.x.fmt()}$q cy=$q${c.y.fmt()}$q r=$q${r.fmt()}$q $styleTag/>"
  }
  object Circle {
    def apply(c: Vc, r: Float, color: Rgba): Circle =
      new Circle(c, r, Style(fillc = Some(Yes(color))))

    def ring(c: Vc, r: Float, color: Rgba, width: Float = Float.NaN): Circle =
      new Circle(c, r, Style(fillc = Some(No("none")), linec = Some(Yes(color)), linew = Some(if (width.nan) 0.1f*r else width)))

    def lots(cs: Array[Long], r: Float, style: Style): Group =
      Group(style, cs.map{ l => new Circle(Vc from l, r, Style.empty) }: _*)

    def lots(cs: Array[Long], r: Float, color: Rgba): Group = lots(cs, r, Style(fillc = Some(Yes(color))))

    def rings(cs: Array[Long], r: Float, color: Rgba, width: Float = Float.NaN): Group =
      lots(cs, r, Style(fillc = Some(No("none")), linec = Some(Yes(color)), linew = Some(if (width.nan) 0.1f*r else width)))
  }

  case class Line(ps: collection.mutable.WrappedArray[Long], style: Style) extends SvgLineStyled[Line] {
    def restyle(f: Style => Style) = copy(style = f(style))
    def svgLine = f"<polyline points=$q${ if (ps.length > 0) ps.map{ l => val v = Vc from l; v.x.fmt + "," + v.y.fmt }.mkString(" ") else "" }$q $styleTag/>"
  }

  object ErrorLineStyle {
    def reshuffle(style: Style, riser: Style): (Style, Style) = {
      val bs = 
        if (style.name.nonEmpty) style
        else Style(
          fillc = Some(No("none")),
          linec = style.linec.map(_.map(_.opaque)),
          linew = style.linew,
          alpha = style.alpha.map(_ * style.linec.flatMap(_.map(_.a).toOption).getOrElse(1f)) orElse style.linec.flatMap(_.map(_.a).toOption)
        )
      val rs =
        if (riser.name.nonEmpty) riser
        else Style(
          fillc = riser.fillc orElse bs.fillc,
          linec = riser.linec.map(_.map(_.opaque)) orElse bs.linec,
          linew = riser.linew orElse bs.linew.map(_ * 0.4f),
          alpha = riser.alpha.map(_ * riser.linec.flatMap(_.map(_.a).toOption).getOrElse(1f)) orElse riser.linec.flatMap(_.map(_.a).toOption) orElse bs.alpha
        )
      (bs, rs)      
    }
  }

  case class ErrorBarY(c: Vc, r: Float, r2: Float, eyp: Float, eyn: Float, style: Style, riser: Style = Style.empty) extends SvgStyled[ErrorBarY] with Grouped {
    def restyle(f: Style => Style) = new ErrorBarY(c, r, r2, eyp, eyn, f(style), f(riser))
    lazy val group: Group = {
      val cL = c.xFn(_ - r)
      val cR = c.xFn(_ + r)
      val bU = c.yFn(_ + eyp)
      val bD = c.yFn(_ - eyn)
      val bUL = bU.xFn(_ - r2)
      val bUR = bU.xFn(_ + r2)
      val bDL = bD.xFn(_ - r2)
      val bDR = bD.xFn(_ + r2)
      val (bs, rs) = ErrorLineStyle.reshuffle(style, riser)
      Group(
        Style.empty,
        Line(Array(cL.underlying, cR.underlying), bs),
        Line(Array(bU.underlying, bD.underlying), rs),
        Line(Array(bUL.underlying, bUR.underlying), rs),
        Line(Array(bDL.underlying, bDR.underlying), rs)
      )
    }
  }
  object ErrorBarY {
    def apply(c: Vc, r: Float, r2: Float, ey: Float, style: Style, riser: Style): ErrorBarY =
      new ErrorBarY(c, r, r2, ey, ey, style, riser)

    def apply(c: Vc, r: Float, r2: Float, ey: Float, style: Style): ErrorBarY =
      new ErrorBarY(c, r, r2, ey, ey, style)

    def apply(c: Vc, r: Float, r2: Float, eyp: Float, eyn: Float, color: Rgba, width: Float): ErrorBarY =
      new ErrorBarY(c, r, r2, eyp, eyn, Style(linec = Some(Yes(color.opaque)), linew = Some(width), alpha = if (color.a.nan) None else Some(color.a)))

    def apply(c: Vc, r: Float, r2: Float, ey: Float, color: Rgba, width: Float): ErrorBarY = apply(c, r, r2, ey, ey, color, width)
  }

  case class ErrorBracketY(c: Vc, r: Float, r2: Float, eyp: Float, eyn: Float, style: Style, riser: Style = Style.empty) extends SvgStyled[ErrorBracketY] with Grouped {
    def restyle(f: Style => Style) = new ErrorBracketY(c, r, r2, eyp, eyn, f(style), f(riser))
    lazy val group: Group = {
      val cL = c.xFn(_ - r)
      val cR = c.xFn(_ + r)
      val uL = cL.yFn(_ + eyp)
      val vL = uL.xFn(_ + r2)
      val lL = cL.yFn(_ - eyn)
      val kL = lL.xFn(_ + r2)
      val uR = cR.yFn(_ + eyp)
      val vR = uR.xFn(_ - r2)
      val lR = cR.yFn(_ - eyn)
      val kR = lR.xFn(_ - r2)
      val (bs, rs) = ErrorLineStyle.reshuffle(style, riser)
      Group(
        Style.empty,
        Line(Array(cL.underlying, cR.underlying), bs),
        Line(Array(vL.underlying, uL.underlying, lL.underlying, kL.underlying), rs),
        Line(Array(vR.underlying, uR.underlying, lR.underlying, kR.underlying), rs)
      )
    }
  }
  object ErrorBracketY {
    def apply(c: Vc, r: Float, r2: Float, ey: Float, style: Style, riser: Style): ErrorBracketY =
      new ErrorBracketY(c, r, r2, ey, ey, style, riser)

    def apply(c: Vc, r: Float, r2: Float, ey: Float, style: Style): ErrorBracketY =
      new ErrorBracketY(c, r, r2, ey, ey, style)

    def apply(c: Vc, r: Float, r2: Float, eyp: Float, eyn: Float, color: Rgba, width: Float): ErrorBracketY =
      new ErrorBracketY(c, r, r2, eyp, eyn, Style(linec = Some(Yes(color.opaque)), linew = Some(width), alpha = if (color.a.nan) None else Some(color.a)))

    def apply(c: Vc, r: Float, r2: Float, ey: Float, color: Rgba, width: Float): ErrorBracketY =
      apply(c, r, r2, ey, ey, color, width)
  }

  case class AxisLine(o: Vc, e: Vc, style: Style, arrow: Option[Float] = Some(3.5f)) extends SvgLineStyled[AxisLine] {
    def restyle(f: Style => Style) = copy(style = f(style))
    def svgLine = f"<path d=${q}M ${o.x.fmt} ${o.y.fmt} L ${(o+e).x.fmt} ${(o+e).y.fmt}$q $styleTag/>"
  }

  case class Canvas(sz: Vc) extends Svgable {
    private val content = new collection.mutable.ArrayBuffer[SvgStyled[_]]
    def +=(ss: SvgStyled[_]): this.type = { content += ss; this }
    def toSvg = f"<svg width=$q${sz.x}$q height=$q${sz.y}$q>" +: (content.flatMap(_.toSvg.map("  " + _)).toVector :+ "</svg>")
    def toHtml = ("<html>" +: "<body>" +: toSvg) :+ "</body>" :+ "</html>"
  }
}
