// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr, UCSF, and Calico Labs.

package kse.see

import scala.math._
import scala.util._
import scala.collection.mutable.{ AnyRefMap => RMap }
import kse.coll._
import kse.maths._
import kse.flow._
import kse.eio._

object Chart {
  private val q = "\""

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

  case class Line(ps: collection.mutable.WrappedArray[Long], style: Style) extends SvgLineStyled[Line] {
    def restyle(f: Style => Style) = copy(style = f(style))
    def svgLine = f"<polyline points=$q${ if (ps.length > 0) ps.map{ l => val v = Vc from l; v.x.fmt + "," + v.y.fmt }.mkString(" ") else "" }$q $styleTag/>"
  }

  case class ErrorBarY(c: Vc, r: Float, r2: Float, ey: Float, style: Style, riser: Style = Style.empty) extends SvgStyled[ErrorBarY] {
    def restyle(f: Style => Style) = new ErrorBarY(c, r, r2, ey, f(style), f(riser))
    lazy val g: Group = {
      val cL = c.xFn(_ - r)
      val cR = c.xFn(_ + r)
      val bU = c.yFn(_ + ey)
      val bD = c.yFn(_ - ey)
      val bUL = bU.xFn(_ - r2)
      val bUR = bU.xFn(_ + r2)
      val bDL = bD.xFn(_ - r2)
      val bDR = bD.xFn(_ + r2)
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
      Group(
        Style.empty,
        Line(Array(cL.underlying, cR.underlying), bs),
        Line(Array(bU.underlying, bD.underlying), rs),
        Line(Array(bUL.underlying, bUR.underlying), rs),
        Line(Array(bDL.underlying, bDR.underlying), rs)
      )
    }
    def toSvg = g.toSvg
  }
  case class ErrorBracketY(c: Vc, r: Float, r2: Float, ey: Float, style: Style, riser: Style = Style.empty) extends SvgStyled[ErrorBracketY] {
    def restyle(f: Style => Style) = copy(style = f(style))
    def toSvg = {
      val cL = c.xFn(_ - r)
      val cR = c.xFn(_ + r)
      val uL = cL.yFn(_ + ey)
      val vL = uL.xFn(_ + r2)
      val lL = cL.yFn(_ - ey)
      val kL = lL.xFn(_ + r2)
      val uR = cR.yFn(_ + ey)
      val vR = uR.xFn(_ - r2)
      val lR = cR.yFn(_ - ey)
      val kR = lR.xFn(_ - r2)
      val rs = Style(fillc = None, linec = riser.linec orElse style.linec, linew = riser.linew orElse style.linew.map(_ * 0.5f), alpha = riser.alpha orElse style.alpha)
      Vector(
        "<g>",
        "  " + Line(Array(cL.underlying, cR.underlying), style).svgLine,
        "  " + Line(Array(vL.underlying, uL.underlying, lL.underlying, kL.underlying), rs).svgLine,
        "  " + Line(Array(vR.underlying, uR.underlying, lR.underlying, kR.underlying), rs).svgLine,
        "</g>"
      )
    }
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
