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

  val defaultNumberFormatter = new NumberFormatter { def fmt(x: Float) = x.toString }
  val emptyAppearanceFormatter = new AppearanceFormatter { def fmt(a: Appearance) = "" }

  sealed trait Quantity[X] {
    def isDefault: Boolean = false
    def value: X
    def defaultValue(x: X): Quantity[X] = new Defaulted(x)
    def valueTo(x: X): Quantity[X] = new Absolute(x)
    def map[Y](f: X => Y): Quantity[Y] = new RelQuant(this, f)
    def flatMap[Y](f: X => Quantity[Y]): Quantity[Y] = new RelQuant(this, f andThen (_.value))
  }
  object Quantity {
    def apply[X](x: X): Quantity[X] = Absolute(x)
  }
  final case class Defaulted[X](value: X) extends Quantity[X] { override def isDefault = true }
  final case class Absolute[X](value: X) extends Quantity[X] {}
  final class Relative[Y, X](val that: Y, val view: Y => X) extends Quantity[X] { def value = view(that) }
  final class RelQuant[Y, X](val that: Quantity[Y], val view: Y => X) extends Quantity[X] { def value = view(that.value) }


  sealed trait Appearance {
    def stroke: Quantity[Float]
    def color: Quantity[Rgba]
    def strokeColor: Quantity[Rgba]
    def fillColor: Quantity[Rgba]
  }
  sealed trait ProxyAppear {
    def appear: Quantity[Appearance]
    val stroke = appear.flatMap(_.stroke)
    val color = appear.flatMap(_.color)
    val strokeColor = appear.flatMap(_.strokeColor)
    val fillColor = appear.flatMap(_.fillColor)
  }
  final class AppearanceOf(that: Appearance) extends Appearance {
    val stroke = that.stroke.map(identity)
    val color = that.color.map(identity)
    val strokeColor = that.strokeColor.map(identity)
    val fillColor = that.fillColor.map(identity)
  }
  sealed trait OneColor extends Appearance {
    val strokeColor = color.map(identity)
    val fillColor = color.map(identity)
  }

  final case class IndentedSvg(text: String, level: Int = 0) {
    override def toString = if (level <= 0) text else " "*(2*level) + text
  }
  trait InSvg { def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] }

  sealed trait Primitive {}
  final case class C(c: Quantity[Vc], r: Quantity[Float], appear: Quantity[Appearance])
  extends ProxyAppear with InSvg {
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter) = {
      val circ = kse.visual.Circle(c.value, r.value) into xform
      Vector(IndentedSvg(
        f"<circle cx=$q${nf fmt circ.center.x}$q cy=$q${nf fmt circ.center.y}$q r=$q${nf fmt circ.radius}$q${af fmt appear.value}/>"
      ))
    }
  }

  final case class Listed(list: List[InSvg]) extends InSvg {
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter) = {
      val b = Vector.newBuilder[IndentedSvg]
      list.foreach{ l => b ++= l.inSvg(xform) }
      b.result
    }
  }

  def quick(i: InSvg) {
    val svg = 
      Vector("<html>", "<body>", """<svg width="640" height="480">""").map(x => IndentedSvg(x)) ++
      i.inSvg(Xform.flipy(Frame.natural, Frame(0 vc -480, 1 vc 0)))(defaultNumberFormatter, emptyAppearanceFormatter).map(x => x.copy(level = 1)) ++
      Vector("</svg>", "</body>", "</html>").map(x => IndentedSvg(x))
    println(svg.mkString("\n"))
    svg.map(_.toString).toFile("test.html".file)
  }


  sealed trait Stroked { def stroke: Float; def strokeColor: Rgba }
  sealed trait Filled { def fillColor: Rgba }
  sealed trait StrokeProxy { def proxy: Stroked; def stroke = proxy.stroke; def strokeColor = proxy.strokeColor }
  sealed trait FillProxy { def proxy: Filled; def fillColor = proxy.fillColor }
  final case class Dot(shape: Circle, proxy: Filled) extends FillProxy {}
  final case class Circ(shape: Circle, proxy: Stroked with Filled) extends StrokeProxy with FillProxy {}
  final case class Horz(center: Vc, length: Float, proxy: Stroked) extends StrokeProxy {}
  final case class Vert(center: Vc, length: Float, proxy: Stroked) extends StrokeProxy {}
  final case class Bar(shape: Rect, proxy: Stroked with Filled) extends StrokeProxy with FillProxy {}
  final case class Arrowhead(tip: Vc, direction: Vc, angle: Float, fatness: Float, proxy: Stroked with Filled)
  extends StrokeProxy with FillProxy {}

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
