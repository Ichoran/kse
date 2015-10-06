// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr, UCSF, and Calico Labs.

package kse.see

import scala.math._
import scala.util._
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

trait SvgStyled extends Svgable {
  def style: Chart.Style
  def styleTag = style.tag.map(x => "style=\""+x+"\" ") match { case Some(s) => s; case None => "" }
}

trait SvgLineStyled extends SvgLine with SvgStyled {}

object Chart {
  private val q = "\""

  case class Style(name: Option[String], fillc: Ok[String, Rgba] = No(""), linec: Ok[String, Rgba] = No(""), linew: Option[Float] = None) {
    def tag: Option[String] = (
      fillc.map{ c => "fill:#"+c.rgbText+";fill-opacity:"+c.a.fmt() }.accept{ case s if s.length > 0 => "fill:"+s }.toOption.toList ++
      linec.map{ c => "stroke:#"+c.rgbText+";stroke-opacity:"+c.a.fmt() }.accept{ case s if s.length > 0 => "stroke:"+s }.toOption.toList ++
      linew.map{ w => "stroke-width:"+w.fmt }
    ).reduceLeftOption(_ + ";" + _)
  }
  object Style {
    val empty = Style(None)
  }
  case class Circle(c: Vc, r: Float, style: Style) extends SvgLineStyled {
    def svgLine = f"<circle cx=$q${c.x.fmt()}$q cy=$q${c.y.fmt()}$q r=$q${r.fmt()}$q $styleTag/>"
  }
  case class Line(ps: collection.mutable.WrappedArray[Long], style: Style) extends SvgLineStyled {
    def svgLine = f"<polyline points=$q${if (ps.length > 0) ps.map{l => val v = Vc from l; v.x.fmt + "," + v.y.fmt }.mkString(" ") else ""}$q $styleTag/>"
  }
  case class ErrorBarY(c: Vc, r: Float, r2: Float, ey: Float, style: Style, riser: Style = Style.empty) extends SvgStyled {
    def toSvg = {
      val cL = c.xFn(_ - r)
      val cR = c.xFn(_ + r)
      val bU = c.yFn(_ + ey)
      val bD = c.yFn(_ - ey)
      val bUL = bU.xFn(_ - r2)
      val bUR = bU.xFn(_ + r2)
      val bDL = bD.xFn(_ - r2)
      val bDR = bD.xFn(_ + r2)
      val rs = Style(None, No("none"), if (riser.linec == No("")) style.linec else riser.linec, riser.linew orElse style.linew.map(_ * 0.5f))
      Vector(
        "<g>",
        "  " + Line(Array(cL.underlying, cR.underlying), style).svgLine,
        "  " + Line(Array(bU.underlying, bD.underlying), rs).svgLine,
        "  " + Line(Array(bUL.underlying, bUR.underlying), rs).svgLine,
        "  " + Line(Array(bDL.underlying, bDR.underlying), rs).svgLine,
        "</g>"
      )
    }
  }
  case class ErrorBracketY(c: Vc, r: Float, r2: Float, ey: Float, style: Style, riser: Style = Style.empty) extends SvgStyled {
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
      val rs = Style(None, No("none"), if (riser.linec == No("")) style.linec else riser.linec, riser.linew orElse style.linew.map(_ * 0.5f))
      Vector(
        "<g>",
        "  " + Line(Array(cL.underlying, cR.underlying), style).svgLine,
        "  " + Line(Array(vL.underlying, uL.underlying, lL.underlying, kL.underlying), rs).svgLine,
        "  " + Line(Array(vR.underlying, uR.underlying, lR.underlying, kR.underlying), rs).svgLine,
        "</g>"
      )
    }
  }
  case class Canvas(sz: Vc) extends Svgable {
    private val content = new collection.mutable.ArrayBuffer[SvgStyled]
    def +=(ss: SvgStyled): this.type = { content += ss; this }
    def toSvg = f"<svg width=$q${sz.x}$q height=$q${sz.y}$q>" +: (content.flatMap(_.toSvg.map("  " + _)).toVector :+ "</svg>")
    def toHtml = ("<html>" +: "<body>" +: toSvg) :+ "</body>" :+ "</html>"
  }
}
