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

trait Svgable {
  def toSvg: Vector[String]
}

trait SvgLine extends Svgable {
  def svgLine: String
  def toSvg = Vector[String](svgLine)
}

trait SvgStyled[S <: SvgStyled[S]] extends Svgable {
  def style: Chart.Style
  def restyle(f: Chart.Style => Chart.Style): S
  def styleTag = style.tag.map(x => "style=\""+x+"\" ") match { case Some(s) => s; case None => "" }
}

trait SvgLineStyled[S <: SvgStyled[S]] extends SvgLine with SvgStyled[S] {}

object Chart {
  private val q = "\""

  case class Style(
    name: Option[String] = None,
    fillc: Option[Ok[String, Rgba]] = None,
    linec: Option[Ok[String, Rgba]] = None,
    linew: Option[Float] = None,
    alpha: Option[Float] = None
  ) {
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

  class Group private (val others: Seq[SvgStyled[_]], val style: Style) extends SvgStyled[Group] {
    def restyle(f: Style => Style) = new Group(others, f(style))
    def toSvg = (if (style == Style.empty) "<g>" else s"<g $styleTag>") +: (others.flatMap(_.toSvg).map("  " + _).toVector :+ "</g>")
  }
  /*
  object Group {
    private def degen(x: Option[Ok[String, Rgba]]): AnyRef = x match {
      case None => None
      case Some(No(s)) => s
      case Some(Yes(c)) => c
    }

    private def canonize[A, B](xs: Seq[A])(f: A => B): Option[A] = 
      xs.groupBy(f).filter(_._2.length > 1 && _._1 != None).toList.sortBy(- _._2.length).headOption.map(_._2.head)

    def apply(style: Style, elts: SvgStyled[_]*): Group = {
      val styles = elts.map(_.style)
      var mine = style
      val canonicalFillC = canonize(styles)(x => degen(x.fillc)).filter(x => (style.fillc == x) || { if (style.fillc.isEmpty) {} else false }
      val canonicalLineC = canonize(styles)(x => degen(x.linec))
      val canonicalLineW = canonize(styles)(x => x.linew)
      val canonicalAlpha = canonize(styles)(x => x.alpha)

      val registryC = RMap.empty[Option[Ok[String, Rgba]], Option[Ok[String, Rgba]]]
      val registryF = RMap.empty[Option[Float], Option[Float]]



      stylish(elts.map(_.style)) match {
        case Some((gst, ests)) => new Group((elts zip ests).map{ case (x,y) => x.restyle(y) }, gst)
        case None => new Group(ests, style)
      }
    }

    def apply(elts: SvgStyled[_]*): Group = apply(Style.empty, elts)
  }
  */

  case class Circle(c: Vc, r: Float, style: Style) extends SvgLineStyled[Circle] {
    def restyle(f: Style => Style) = copy(style = f(style))
    def svgLine = f"<circle cx=$q${c.x.fmt()}$q cy=$q${c.y.fmt()}$q r=$q${r.fmt()}$q $styleTag/>"
  }

  case class Line(ps: collection.mutable.WrappedArray[Long], style: Style) extends SvgLineStyled[Line] {
    def restyle(f: Style => Style) = copy(style = f(style))
    def svgLine = f"<polyline points=$q${if (ps.length > 0) ps.map{l => val v = Vc from l; v.x.fmt + "," + v.y.fmt }.mkString(" ") else ""}$q $styleTag/>"
  }


  case class ErrorBarY(c: Vc, r: Float, r2: Float, ey: Float, style: Style, riser: Style = Style.empty) extends SvgStyled[ErrorBarY] {
    def restyle(f: Style => Style) = new ErrorBarY(c, r, r2, ey, f(style), f(riser))
    lazy val g: Group = ???/*{
      val cL = c.xFn(_ - r)
      val cR = c.xFn(_ + r)
      val bU = c.yFn(_ + ey)
      val bD = c.yFn(_ - ey)
      val bUL = bU.xFn(_ - r2)
      val bUR = bU.xFn(_ + r2)
      val bDL = bD.xFn(_ - r2)
      val bDR = bD.xFn(_ + r2)
      val (cs, bs, rs) = GroupStyle(bs, rs) match {
        case (c, Array(b,r)) => (c, b, r)
        case _ => (Style(fillc = Some(No("none"))), style, riser.copy(linec = riser.linec orElse style.linec, linew = riser.linew orElse style.linew.map(_ * 0.5f)))
      }
      Group(
        cs,
        Line(Array(cL.underlying, cR.underlying), bs),
        Line(Array(bU.underlying, bD.underlying), rs),
        Line(Array(bUL.underlying, bUR.underlying), rs),
        Line(Array(bDL.underlying, bDR.underlying), rs)
      )
    }*/
    def toSvg = ???
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
