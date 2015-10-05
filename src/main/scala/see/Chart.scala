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
}

trait SvgLineStyled extends SvgLine with SvgStyled {}

object Chart {
  private val q = "\""

  case class Style(name: Option[String], fillc: Option[Rgba], linec: Option[Rgba]) {
    def tag: Option[String] = (
      fillc.map{ c => "fill:#"+c.rgbText+";fill-opacity:"+c.a.fmt() }.toList ++
      linec.map{ c => "stroke:#"+c.rgbText+";stroke-opacity:"+c.a.fmt() }.toList
    ).reduceLeftOption(_ + ";" + _)
  }
  object Style {
    val empty = Style(None, None, None)
  }
  case class Circle(c: Vc, r: Float, color: Rgba, style: Style) extends SvgLineStyled {
    def svgLine = f"<circle cx=$q${c.x.fmt()}$q cy=$q${c.y.fmt()}$q r=$q${r.fmt()}$q ${style.tag.map(x => "style="+q+x+q+" ").getOrElse("")}/>"
  }
  case class Canvas(sz: Vc) extends Svgable {
    private val content = new collection.mutable.ArrayBuffer[SvgStyled]
    def +=(ss: SvgStyled): this.type = { content += ss; this }
    def toSvg = f"<svg width=$q${sz.x}$q height=$q${sz.y}$q>" +: (content.flatMap(_.toSvg.map("  " + _)).toVector :+ "</svg>")
    def toHtml = ("<html>" +: "<body>" +: toSvg) :+ "</body>" :+ "</html>"
  }
}
