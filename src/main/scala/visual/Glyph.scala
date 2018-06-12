// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015-2018 Rex Kerr, UCSF, and Calico Labs.

// Herein lies a collection of fundamental elements from which to build graphs and charts.
// They're not literally glyphs.

package kse.visual.chart

import scala.math._
import scala.util._
import scala.collection.mutable.{ AnyRefMap => RMap }

import kse.coll._
import kse.coll.packed._
import kse.maths._
import kse.maths.stats._
import kse.flow._
import kse.eio._
import kse.visual._


////////////////////////////////////////////////
// Utility objects to control and group stuff //
////////////////////////////////////////////////

object q {
  override def toString = "\""
}

trait Shown extends InSvg {
  def style: Style
  def styled: Style = style
  def show(implicit fm: Formatter, mag: Magnification) = 
    fm(if (mag.value closeTo 1) styled else styled.scale(mag.value))
  def showWith(f: Style => Style)(implicit fm: Formatter, mag: Magnification) =
    fm(f(if (mag.value closeTo 1) styled else styled.scale(mag.value)))
}

final case class Dynamic[S <: Shown, T <: Shown](static: S, react: (S, Xform, Option[Float => Float], Formatter) => T) extends Shown {
  def style = static.style
  def inSvg(xform:Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] =
    react(static, xform, mag, fm).inSvg(xform, mag)(fm)
}
object Dynamic {
  def apply[S <: Shown, T <: Shown](static: S)(react: (S, Xform, Option[Float => Float], Formatter) => T)(implicit ev: S <:< T) = new Dynamic(static, react)
}

final case class Grouping(stuff: Seq[InSvg], opacity: Float = 1f) extends Shown {
  def style = Style.empty
  def this(thing: InSvg, morestuff: InSvg*) = this(thing +: morestuff)
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    Indent.V(if (opacity.in(0, 0.9994999f)) f"<g opacity=$q$opacity%.3f$q>" else "<g>") ++
    stuff.toVector.flatMap(_.inSvg(xform, mag).map(_.in)) ++
    Indent.V("</g>")
  }
}
object Grouping {
  def apply(thing: InSvg, morestuff: InSvg*) = new Grouping(thing +: morestuff)
  def faded(opacity: Float)(thing: InSvg, morestuff: InSvg*) = new Grouping(thing +: morestuff, opacity)
}

final case class Literal(literal: String, inverted: Boolean = true, rooted: Vc = Vc(0, 0)) extends Shown {
  def style = Style.empty
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    val offset = xform(Vc(0, 0))
    val xcol   = xform(Vc(1, 0)) - offset
    val ycol   = xform(Vc(0, 1)) - offset
    val ocol   = offset - (if (inverted) Vc(rooted.x, -rooted.y) else rooted)
    val unscaled =
      (xcol.x-1).abs < 1e-4 && xcol.y.abs < 1e-6 &&
      ycol.x.abs < 1e-6 && (ycol.y-1).abs < 1e-4
    val boring = ocol.x.abs < 1e-6 && ocol.y.abs < 1e-6 && unscaled

    val indent = literal.lines.map(line => if (boring && !inverted) Indent(line) else Indent(line, 1))
    (
      if (boring && !inverted) indent
      else {
        Iterator(Indent(
          """<g transform="""" + {
            if (unscaled && !inverted) f"translate(${ocol.x}%.6f ${ocol.y}%.6f)"
            else {
              val inv = if (inverted) -1 else 1
              "matrix(%.6f %.6f %.6f %.6f %.6f %.6f)".format(xcol.x, xcol.y, ycol.x*inv, ycol.y*inv, ocol.x, ocol.y)
            }
          } +
          """">"""
        )) ++
        indent ++
        Iterator(Indent("</g>"))
      }
    ).toVector
  }
}


////////////////////////////
// Basic geometric shapes //
////////////////////////////

object Marker {
  final case class C(c: Vc, r: Float, style: Style) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
      val ctr = xform(c)
      implicit val myMag = Magnification.from(mag, r, xform.radius(c, Vc(r, 0)))
      Indent.V(f"<circle${fm.vquote(ctr, "cx", "cy")}${fm("r", r * myMag.value)}$show/>")
    }
  }
  final case class E(c: Vc, r: Vc, style: Style) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
      val ctr = xform(c)
      implicit val myMag = Magnification.from(mag, xform, c)
      Indent.V(f"<ellipse${fm.vquote(ctr, "cx", "cy")}${fm.vquote(r* myMag.value, "rx", "ry")}$show/>")
    }      
  }
  final case class R(c: Vc, r: Vc, style: Style) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
      val ctr = xform(c)
      implicit val myMag = Magnification.from(mag, xform, c)
      Indent.V(f"<rect${fm.vquote(ctr, "x", "y")}${fm.vquote(r * (2*myMag.value), "width", "height")}$show/>")
    }
  }
  final case class ManyC(cs: Array[Long], r: Float, tiled: Boolean, style: Style, count: Option[Int] = None, relsize: Option[Float] = None) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
      var umx, umy, uMx, uMy = 0f
      val us = {
        val result = new Array[Long](cs.length)
        var i = 0
        while (i < cs.length) { 
          val ui = xform(Vc from cs(i))
          if (i == 0 || ui.x < umx) umx = ui.x
          if (i == 0 || ui.x > uMx) uMx = ui.x
          if (i == 0 || ui.y < umy) umy = ui.y
          if (i == 0 || ui.y > uMy) uMy = ui.y
          result(i) = ui.underlying
          i += 1
        }
        result
      }
      implicit val myMag = Magnification.from(mag, xform, cs)
      val ur = r * myMag.value
      val showSpec = showWith(_.specificallyInsubstantial)
      val fmR = fm("r", ur)
      if (cs.length < 100) {
        Indent(f"<g ${showWith(_.generallySolid)}>") +: (
          us.flatMap{ lu => 
            val u = Vc from lu
            if (u.finite) Some(Indent(f"<circle${fm.vquote(u, "cx", "cy")}$fmR$showSpec/>", 1))
            else None
          }.toVector
        ) :+ Indent("</g>")
      }
      else {
        val alphic = style.opacity
        val sz = max(relsize.getOrElse(4f), 1f)
        val bsp = new BspTree[Long](sz*ur, sz*ur, max(2, count.getOrElse(math.ceil(4/alphic.toFloat).toInt)), Vc from _)
        bsp.suggestBounds(umx vc umy, uMx vc uMy)
        var i = 0
        while (i < us.length) { bsp += us(i); i += 1 }
        val b = Vector.newBuilder[Indent]
        val top = Vector.newBuilder[Vc]
        b += Indent(f"<g ${showWith(_.generallySolid)}>")
        bsp.forleaves{ (usi, u0, u1) =>
          val td = u1 - u0
          var j = 0
          val lim = 
            if (tiled) 1 + ((td.x * td.y)/(math.Pi*ur*ur))/sqrt(alphic)
            else (1 + 0.5*td.x/ur)*(1 + 0.5*td.y/ur)/sqrt(alphic)
          val J = 
            if (tiled) (if (usi.length <= 3*lim/2) 0 else usi.length - math.max(1, lim))
            else (if (usi.length <= lim) 0 else usi.length - math.max(1, lim/2))
          val ex, ey = new EstXM
          while (j < J) {
            val uij = Vc from usi(j)
            ex += uij.x
            ey += uij.y
            j += 1
          }
          val ec = Vc.from((ex.max + ex.min)/2, (ey.max + ey.min)/2)
          val er = Vc.from((ex.max - ex.min)/2, (ey.max - ey.min)/2)
          val areaRatio = 
            if (tiled) (td.x*td.y)/(math.Pi*ur*ur)
            else (1 + er.x/ur)*(1 + er.y/ur)
          val densityRatio = J / areaRatio
          if (J > 0) {
            val restyle =
              if ((densityRatio - 1).abs < 0.05 || !(alphic < 1)) style.specificallyInsubstantial
              else style.specificallyInsubstantial.solidify(f => if (f <= 0 || f >= 1) f else (1-math.exp(math.log(1-f)*densityRatio)).clip(0,1).toFloat)
            if (tiled) b += Indent(f"<rect${fm.vquote(u0, "x", "y")}${fm.vquote(td, "width", "height")}${showWith(_ => restyle)}/>",1)
            else       b += Indent(f"<ellipse${fm.vquote(ec, "cx", "cy")}${fm.vquote(er + ur, "rx", "ry")}${showWith(_ => restyle)}/>", 1)
          }
          while (j < usi.length) {
            top += Vc from usi(j)
            j += 1
          }
        }
        top.result.foreach(uc => if (uc.finite) b += Indent(f"<circle${fm.vquote(uc, "cx", "cy")}$fmR$showSpec/>", 1))
        b += Indent("</g>")
        b.result
      }
    }
  }
}


final case class Circ(c: Vc, r: Float, style: Style) extends Shown {
  override def styled = style.shapely
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
    if (!r.finite || r <= 0 || !c.finite) return Vector.empty
    val ctr = xform(c)
    val rad = xform.radius(c, Vc(r, 0))
    implicit val myMag = Magnification.from(mag, r, rad)
    if (!rad.finite || rad == 0 || !ctr.finite) return Vector.empty
    Indent.V(
      f"<circle${fm.vquote(ctr, "cx", "cy")}${fm("r", rad)}$show/>"
    )
  }
}

final case class Oval(c: Vc, ab: Vc, style: Style) extends Shown {
  override def styled = style.shapely
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
    if (!ab.finite || !c.finite) return Vector.empty
    val ctr = xform(c)
    val rad = Vc((xform(c + Vc(ab.x, 0))-ctr).len.toFloat, (xform(c + Vc(0, ab.y))-ctr).len.toFloat)
    implicit val myMag = Magnification.from(mag, xform, c)
    if (!rad.finite || (rad.x == 0 && rad.y == 0) || !ctr.finite)
      Vector.empty
    else if (rad.x == 0 || rad.y == 0)
      Indent.V(f"<line${fm.vquote(ctr-rad, "x1", "y1")}${fm.vquote(ctr+rad, "x2", "y2")}$show/>")
    else
      Indent.V(f"<ellipse${fm.vquote(ctr, "cx", "cy")}${fm.vquote(rad, "rx", "ry")}$show/>")
  }
}

final case class Bar(c: Vc, r: Vc, style: Style) extends Shown {
  override def styled = style.shapely
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
    if (!r.finite || !c.finite) return Vector.empty
    val s = Vc(r.x, -r.y)
    val ld = xform(c - r)
    val lu = xform(c - s)
    val rd = xform(c + s)
    val ru = xform(c + r)
    Indent.V({
      if ((ld.x closeTo lu.x) && (rd.x closeTo ru.x) && (ld.y closeTo rd.y) && (lu.y closeTo ru.y)) {
        val qc = xform(c)
        val rw = 0.25f*(rd.x - ld.x + ru.x - lu.x).abs
        val rh = 0.25f*(ru.y - rd.y + lu.y - ld.y).abs
        val qr = Vc(rw, rh)
        implicit val myMag = Magnification.from(mag, r.x, r.y, rw, rh)
        f"<rect ${fm.vquote(qc - qr, "x", "y")} ${fm.vquote(qr*2, "width", "height")}$show/>"
      }
      else {
        implicit val myMag = Magnification.from(mag, xform, c)
        f"<polygon points=$q${fm comma ld} ${fm comma rd} ${fm comma ru} ${fm comma lu}$q$show/>"
      }
    })
  }
}


final case class Shape(corners: Array[Long], marker: Option[Shown], style: Style) extends Shown {
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    implicit val myMag = Magnification.from(mag, xform, corners)
    val sb = new StringBuilder
    sb ++= "<path d=\""
    var i = 0
    if (i < corners.length) { sb ++= "M "; sb ++= fm(xform(Vc from corners(0))); i += 1 }
    if (corners.length > 1) sb ++= " L"
    while (i < corners.length) { sb += ' '; sb ++= fm(xform(Vc from corners(i))); i += 1 }
    if (i > 0) sb ++= " Z"
    marker match {
      case Some(m) =>
        sb ++= f"$q${showWith(_.specifically)}/>"
        Vector(
          Indent(f"<g ${showWith(_.generally)}>"),
          Indent(sb.result, 1)
        ) ++
        corners.flatMap{ c =>
          m.inSvg(Xform.origin(-(Vc from c)) andThen xform, mag.map(f => (x: Float) => f(x)*myMag.value))
        } ++
        Vector(Indent("</g>"))
      case None =>
        sb ++= f"$q$show/>"
        Indent.V(sb.result)
    }
  }
}

//////////////////////
// Lines and arrows //
//////////////////////

final case class DataLine(pts: Array[Long], style: Style) extends Shown {
  override def styled =
    if (style.elements.exists{ case sj: StrokeJoin => true; case _ => false }) style.stroky
    else style.stroky + StrokeJoin(Join.Round)
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    val v = new Array[Long](pts.length)
    var i = 0;
    while (i < v.length) { v(i) = xform(Vc from pts(i)).underlying; i += 1 }
    val sb = new StringBuilder
    sb ++= "<path d=\""
    i = 0;
    var i0 = 0
    var first = true
    while (i < v.length) {
      val vi = Vc from v(i)
      if (!vi.y.finite) { i0 = i + 1 }
      else {
        if (first) { sb ++= "M "; first = false }
        else if (i == i0) sb ++= " M "
        else if (i == i0+1) sb ++= " L "
        else sb += ' '
        sb ++= fm(vi)
      }
      i += 1
    }
    implicit val myMag = Magnification.from(mag, xform, pts)
    sb ++= f"$q$show/>"
    Indent.V(sb.result)
  }
}
object DataLine {
  def from(xs: Array[Float], ys: Array[Float], style: Style): DataLine = {
    val n = math.min(xs.length, ys.length)
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (xs(i) <> ys(i)).L
      i += 1
    }
    new DataLine(a, style)
  }
  def from(xs: Array[Double], ys: Array[Double], style: Style): DataLine = {
    val n = math.min(xs.length, ys.length)
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (xs(i).toFloat <> ys(i).toFloat).L
      i += 1
    }
    new DataLine(a, style)
  }
  def from[A, B](xs: Array[A], ys: Array[B], style: Style)(f: A => Float, g: B => Float): DataLine = {
    val n = math.min(xs.length, ys.length)
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (f(xs(i)) <> g(ys(i))).L
      i += 1
    }
    new DataLine(a, style)
  }
  def from[A](xys: Array[A], style: Style)(f: A => Float, g: A => Float): DataLine = from[A, A](xys, xys, style)(f, g)
  def from(n: Int, x: Int => Float, y: Int => Float, style: Style): DataLine = {
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (x(i) <> y(i)).L
      i += 1
    }
    new DataLine(a, style)
  }

  def from(pts: Array[Long], color: Rgba, size: Float): DataLine = new DataLine(pts, Stroke.alpha(color, size))
  def from(xs: Array[Float], ys: Array[Float], color: Rgba, size: Float): DataLine = from(xs, ys, Stroke.alpha(color, size))
  def from(xs: Array[Double], ys: Array[Double], color: Rgba, size: Float): DataLine = from(xs, ys, Stroke.alpha(color, size))
  def from[A, B](xs: Array[A], ys: Array[B], color: Rgba, size: Float)(f: A => Float, g: B => Float): DataLine =
    from[A, B](xs, ys, Stroke.alpha(color, size))(f, g)
  def from[A](xys: Array[A], color: Rgba, size: Float)(f: A => Float, g: A => Float)(): DataLine =
    from[A](xys, Stroke.alpha(color, size))(f, g)
  def from(n: Int, x: Int => Float, y: Int => Float, color: Rgba, size: Float): DataLine =
    from(n, x, y, Stroke.alpha(color, size))

  def dotted(pts: Array[Long], r: Float, lstyle: Style, dstyle: Style): Grouping = {
    val line = new DataLine(pts, lstyle)
    val dots = Marker.ManyC(pts, r, false, dstyle)
    Grouping(line, dots)
  }
  def dotted(pts: Array[Long], rs: Array[Float], lstyle: Style, dstyle: Style): Grouping = {
    val all = Array.newBuilder[InSvg]
    all += new DataLine(pts, lstyle)
    var i = 0
    while (i < pts.length && i < rs.length) {
      val vi = new Vc(pts(i))
      val r = rs(i)
      if (r.finite && vi.y.finite) {
        all += Marker.C(vi, r, dstyle)
      }
      i += 1
    }
    Grouping(all.result())
  }
  def dotted(xs: Array[Float], ys: Array[Float], r: Float, lstyle: Style, dstyle: Style): Grouping = {
    val n = math.min(xs.length, ys.length)
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (xs(i) <> ys(i)).L
      i += 1
    }
    dotted(a, r, lstyle, dstyle)
  }
  def dotted(xs: Array[Float], ys: Array[Float], rs: Array[Float], lstyle: Style, dstyle: Style): Grouping = {
    val n = math.min(xs.length, ys.length)
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (xs(i) <> ys(i)).L
      i += 1
    }
    dotted(a, rs, lstyle, dstyle)
  }
  def dotted[A, B](xs: Array[A], ys: Array[B], r: Float, lstyle: Style, dstyle: Style)(f: A => Float, g: B => Float): Grouping = {
    val n = math.min(xs.length, ys.length)
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (f(xs(i)) <> g(ys(i))).L
      i += 1
    }
    dotted(a, r, lstyle, dstyle)
  }
  def dotted[A, B, C](xs: Array[A], ys: Array[B], rs: Array[C], lstyle: Style, dstyle: Style)(f: A => Float, g: B => Float, h: C => Float): Grouping = {
    val n = math.min(xs.length, ys.length)
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (f(xs(i)) <> g(ys(i))).L
      i += 1
    }
    val all = Array.newBuilder[InSvg]
    all += new DataLine(a, lstyle)
    i = 0
    while (i < a.length && i < rs.length) {
      val vi = new Vc(a(i))
      val r = h(rs(i))
      if (r.finite && vi.y.finite) {
        all += Marker.C(vi, r, dstyle)
      }
      i += 1
    }
    Grouping(all.result())
  }
  def dotted[A](xys: Array[A], r: Float, lstyle: Style, dstyle: Style)(f: A => Float, g: A => Float): Grouping =
    dotted[A, A](xys, xys, r, lstyle, dstyle)(f, g)
  def dotted[A](xyrs: Array[A], lstyle: Style, dstyle: Style)(f: A => Float, g: A => Float, h: A => Float): Grouping =
    dotted[A, A, A](xyrs, xyrs, xyrs, lstyle, dstyle)(f, g, h)
  def dotted(n: Int, x: Int => Float, y: Int => Float, r: Float, lstyle: Style, dstyle: Style): Grouping = {
      val a = new Array[Long](n)
      var i = 0
      while (i < n) {
        a(i) = (x(i) <> y(i)).L
        i += 1
      }
      dotted(a, r, lstyle, dstyle)    
  }
  def dotted(n: Int, x: Int => Float, y: Int => Float, r: Int => Float, lstyle: Style, dstyle: Style): Grouping = {
    val a = new Array[Long](n)
    var i = 0
    while (i < n) {
      a(i) = (x(i) <> y(i)).L
      i += 1
    }
    val all = Array.newBuilder[InSvg]
    all += new DataLine(a, lstyle)
    i = 0
    while (i < a.length) {
      val vi = new Vc(a(i))
      val ri = r(i)
      if (ri.finite && vi.y.finite) {
        all += Marker.C(vi, ri, dstyle)
      }
      i += 1
    }
    Grouping(all.result())
  }


  def dotted(pts: Array[Long], r: Float, style: Style): Grouping = dotted(pts, r, style, style)
  def dotted(pts: Array[Long], rs: Array[Float], style: Style): Grouping = dotted(pts, rs, style, style)
  def dotted(xs: Array[Float], ys: Array[Float], r: Float, style: Style): Grouping = dotted(xs, ys, r, style, style)
  def dotted(xs: Array[Float], ys: Array[Float], rs: Array[Float], style: Style): Grouping = dotted(xs, ys, rs, style, style)
  def dotted[A, B](xs: Array[A], ys: Array[B], r: Float, style: Style)(f: A => Float, g: B => Float): Grouping = dotted(xs, ys, r, style, style)(f, g)
  def dotted[A, B, C](xs: Array[A], ys: Array[B], rs: Array[C], style: Style)(f: A => Float, g: B => Float, h: C => Float): Grouping = dotted(xs, ys, rs, style, style)(f, g, h)
  def dotted[A](xys: Array[A], r: Float, style: Style)(f: A => Float, g: A => Float): Grouping = dotted(xys, r, style, style)(f, g)
  def dotted[A](xyrs: Array[A], style: Style)(f: A => Float, g: A => Float, h: A => Float): Grouping = dotted(xyrs, style, style)(f, g, h)
  def dotted(n: Int, x: Int => Float, y: Int => Float, r: Float, style: Style): Grouping = dotted(n, x, y, r, style, style)
  def dotted(n: Int, x: Int => Float, y: Int => Float, r: Int => Float, style: Style): Grouping = dotted(n, x, y, r, style, style)

  def dotted(pts: Array[Long], r: Float, color: Rgba, size: Float): Grouping = dotted(pts, r, Stroke.alpha(color, size), Fill.alpha(color))
  def dotted(pts: Array[Long], rs: Array[Float], color: Rgba, size: Float): Grouping = dotted(pts, rs, Stroke.alpha(color, size), Fill.alpha(color))
  def dotted(xs: Array[Float], ys: Array[Float], r: Float, color: Rgba, size: Float): Grouping = dotted(xs, ys, r, Stroke.alpha(color, size), Fill.alpha(color))
  def dotted(xs: Array[Float], ys: Array[Float], rs: Array[Float], color: Rgba, size: Float): Grouping = dotted(xs, ys, rs, Stroke.alpha(color, size), Fill.alpha(color))
  def dotted[A, B](xs: Array[A], ys: Array[B], r: Float, color: Rgba, size: Float)(f: A => Float, g: B => Float): Grouping = dotted(xs, ys, r, Stroke.alpha(color, size), Fill.alpha(color))(f, g)
  def dotted[A, B, C](xs: Array[A], ys: Array[B], rs: Array[C], color: Rgba, size: Float)(f: A => Float, g: B => Float, h: C => Float): Grouping = dotted(xs, ys, rs, Stroke.alpha(color, size), Fill.alpha(color))(f, g, h)
  def dotted[A](xys: Array[A], r: Float, color: Rgba, size: Float)(f: A => Float, g: A => Float): Grouping = dotted(xys, r, Stroke.alpha(color, size), Fill.alpha(color))(f, g)
  def dotted[A](xyrs: Array[A], color: Rgba, size: Float)(f: A => Float, g: A => Float, h: A => Float): Grouping = dotted(xyrs, Stroke.alpha(color, size), Fill.alpha(color))(f, g, h)
  def dotted(n: Int, x: Int => Float, y: Int => Float, r: Float, color: Rgba, size: Float): Grouping = dotted(n, x, y, r, Stroke.alpha(color, size), Fill.alpha(color))
  def dotted(n: Int, x: Int => Float, y: Int => Float, r: Int => Float, color: Rgba, size: Float): Grouping = dotted(n, x, y, r, Stroke.alpha(color, size), Fill.alpha(color))
}
object Line {
  def apply(v0: Vc, v1: Vc, style: Style): DataLine =
    new DataLine(Array(v0.underlying, v1.underlying), style)
  def apply(v0: Vc, v1: Vc, v2: Vc, style: Style): DataLine =
    new DataLine(Array(v0.underlying, v1.underlying, v2.underlying), style)
  def apply(v0: Vc, v1: Vc, v2: Vc, v3: Vc, style: Style): DataLine =
    new DataLine(Array(v0.underlying, v1.underlying, v2.underlying, v3.underlying), style)
  def apply(v0: Vc, v1: Vc, v2: Vc, v3: Vc, v4: Vc, style: Style): DataLine =
    new DataLine(Array(v0.underlying, v1.underlying, v2.underlying, v3.underlying, v4.underlying), style)
  def apply(v0: Vc, v1: Vc, v2: Vc, v3: Vc, v4: Vc, v5: Vc, style: Style): DataLine =
    new DataLine(Array(v0.underlying, v1.underlying, v2.underlying, v3.underlying, v4.underlying, v5.underlying), style)
}

final case class DataRange(xs: Array[Float], ylos: Array[Float], yhis: Array[Float], style: Style) extends Shown {
  override def styled = style.filly
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    val n = math.min(xs.length, math.min(ylos.length, yhis.length))
    val vs = new Array[Long](2*n)
    var i = 0
    while (i < n) {
      vs(i) = Vc(xs(i), ylos(i)).underlying
      vs(vs.length-1-i) = Vc(xs(i), yhis(i)).underlying
      i += 1
    }
    implicit val myMag = Magnification.from(mag, xform, vs)
    i = 0
    while (i < vs.length) {
      vs(i) = xform(Vc from vs(i)).underlying
      i += 1
    }
    val sb = new StringBuilder
    sb ++= "<path d=\""
    i = 0
    if (i < vs.length) { sb ++= "M "; sb ++= fm(Vc from vs(0)); i += 1 }
    if (vs.length > 1) sb ++= " L"
    while (i < vs.length) { sb += ' '; sb ++= fm(Vc from vs(i)); i += 1 }
    if (n > 0) sb ++= " Z"
    sb ++= f"$q$show/>"
    Indent.V(sb.result)
  }
}

trait Arrowhead {
  def setback: Float
  def stroked(tip: Vc, direction: Vc)(xform: Xform, style: Style)(implicit fm: Formatter): (Float, String)
}
final case class LineArrow(angle: Float, length: Float, thickness: Float) extends Arrowhead {
  val phi = angle.abs
  val theta = (math.Pi - phi).toFloat
  val cosx = if (theta < phi) math.cos(theta).toFloat else math.cos(phi).toFloat
  val sinx = if (theta < phi) math.sin(theta).toFloat else math.sin(phi).toFloat
  val flat = phi closeTo (math.Pi/2).toFloat
  val underfilled = 2*thickness < cosx
  val setback = 
    if (flat) 0f
    else if (phi < theta) (cosx/(2*sinx)).toFloat
    else if (underfilled) length*cosx + thickness*sinx
    else length*cosx
  val pointx =
    if (flat) Float.NaN
    else if (phi < theta) thickness/(2*sinx)
    else if (underfilled) setback + (cosx - thickness)/(2*sinx)
    else setback + thickness/(2*sinx)
  val barbx =
    if (flat) thickness/2
    else if (phi < theta) setback + 2*pointx + length*cosx - thickness*sinx*0.5f
    else thickness*sinx/2
  val barby =
    if (flat) 0.5f+length
    else if (phi < theta) 0.5f + length*sinx + thickness*cosx*0.5f
    else if (underfilled) 0.5f + length*sinx - thickness*cosx*0.5f
    else length*sinx + thickness*cosx*0.5f
  def stroked(tip: Vc, direction: Vc)(xform: Xform, style: Style)(implicit fm: Formatter): (Float, String) = {
    val qt = xform(tip)
    val deltadir = if (direction.lenSq < 0.1f*tip.lenSq) direction else direction*(1f/(50*math.max(1e-3f, tip.len.toFloat)))
    val dirx = (xform(tip + deltadir) - qt).hat
    val diry = dirx.ccw
    val w = style.elements.collectFirst{ case StrokeWidth(x) => x } getOrElse 1f
    val sizedStyle = if (thickness closeTo 1) style else style.scale(thickness)
    val s = w * setback
    val px = w * pointx
    val bx = w * barbx
    val by = w * barby
    val qA = qt - dirx*bx + diry*by
    val qB = qt - dirx*bx - diry*by
    val qC = qt - dirx*px
    val miterStyle = {
      val miter = math.ceil(1/sinx+1e-3).toInt
      if (miter <= 4) sizedStyle
      else if (style.elements.exists{ case StrokeMiter(m) => m >= miter || m.closeTo(miter); case _ => false }) sizedStyle
      else sizedStyle + StrokeMiter(miter.toFloat)
    }
    val joinStyle =
      if (flat || !style.elements.exists{ case StrokeJoin(j) => j != Join.Miter; case _ => false }) miterStyle
      else miterStyle + StrokeJoin(Join.Miter)
    val ans = 
      if (flat) f"<path d=${q}M ${fm(qA)} L ${fm(qB)}${q}${fm(joinStyle)}/>"
      else f"<path d=${q}M ${fm(qA)} L ${fm(qC)} ${fm(qB)}${q}${fm(joinStyle)}/>"
    (s, ans)
  }
}

final case class Arrow(from: Vc, to: Vc, indirection: Float, head: Option[Arrowhead], style: Style) extends Shown {
  override def styled = style.stroky.promoteStrokeOpacity
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    val vi = if (indirection.finite && !(indirection closeTo 0)) indirection else 0
    val v = to - from;
    val ip = from + v*0.5f - v.ccw*(2f*vi)
    val uf = xform(from)
    val ut = xform(to)
    val iq = xform(ip)
    implicit val myMag = Magnification.from(mag, xform, from, to)
    if (head.nonEmpty) {
      val ah = head.get
      val (setback, arrowline) = ah.stroked(to, (to - ip).hat)(xform, styled.specifically.scale(myMag.value))
      val wt = ut - setback*(ut - iq).hat
      val mainline =
        if (indirection.finite && !(indirection closeTo 0))
          f"<path d=${q}M ${fm(uf)} Q ${fm(iq)} ${fm(wt)}${q}${showWith(_.specifically)}/>"      
        else
          f"<path d=${q}M ${fm(uf)} L ${fm(wt)}${q}${showWith(_.specifically)}/>"
      Indent.V(
        f"<g${showWith(_.generally)}>",
        mainline,
        arrowline,
        "</g>"
      )
    }
    else Indent.V(
      if (indirection.finite && !(indirection closeTo 0))
        f"<path d=${q}M ${fm(uf)} Q ${fm(iq)} ${fm(ut)}${q}$show/>"      
      else
        f"<path d=${q}M ${fm(uf)} L ${fm(ut)}${q}$show/>"
    )
  }
}


final case class PolyArrow(points: Array[Long], fwdarrow: Option[Arrowhead], bkwarrow: Option[Arrowhead], style: Style) extends Shown {
  override def styled = style.stroky.promoteStrokeOpacity
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    if (points.length < 2) return Vector.empty
    val up = {
      val ans = new Array[Long](points.length)
      var i = 0
      while (i < points.length) { ans(i) = xform(Vc from points(i)).underlying; i += 1 }
      ans
    }
    implicit val myMag = Magnification.from(mag, xform, points)
    val fwd = (Vc.from(points(points.length-1)) - Vc.from(points(points.length-2))).hat
    val bkw = (Vc.from(points(0)) - Vc.from(points(1))).hat
    var arrows = List.empty[String]
    if (bkwarrow.isDefined) {
      val ar = bkwarrow.get
      val (setback, arrowline) = ar.stroked(Vc from points(0), bkw)(xform, styled.specifically.scale(myMag.value))
      up(0) = (Vc.from(up(0)) - setback*(Vc.from(up(0)) - Vc.from(up(1))).hat).underlying
      arrows = arrowline :: arrows
    }
    if (fwdarrow.isDefined) {
      val ar = fwdarrow.get
      val (setback, arrowline) = ar.stroked(Vc from points(points.length-1), fwd)(xform, styled.specifically.scale(myMag.value))
      up(up.length-1) = (Vc.from(up(up.length-1)) - setback*(Vc.from(up(up.length-1)) - Vc.from(up(up.length-2))).hat).underlying
      arrows = arrowline :: arrows
    }
    val line = f"<path d=${q}M ${fm(Vc from up(0))} L ${up.drop(1).map(l => fm(Vc from l)).mkString(" ")}${q}${showWith(_.specifically)}/>"
    Indent.V({
      Seq(f"<g${showWith(_.generally)}>", line) ++
      arrows ++
      Seq("</g>")
    }: _*)
  }
}


///////////////////////
// Text and pictures //
///////////////////////


final case class Letters(anchor: Vc, text: String, rotate: Float, style: Style) extends Shown {
  val safeText = Letters.safeEncode(text)
  override def styled = style.defaultTo(FontSize(12))
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    implicit val myMag = Magnification.from(mag, xform, anchor)
    val rot = (rotate*180/math.Pi).toFloat
    val u = {
      val u0 = xform(anchor)
      if (!fm.verticalTextIsBuggy) u0
      else style.elements.collectFirst{ case FontVertical(valign) =>
        val height = style.elements.collectFirst{ case FontSize(s) => s }.getOrElse(12f)
        val mult = myMag.value * fm.verticalFix(valign)
        if (mult closeTo 0) u0
        else if (rotate closeTo 0) u0 + (0 vc height * mult)
        else u0 + (0 vc height * mult).rotate(rotate)
      }.getOrElse(u0)
    }
    Indent.V(
      if (rotate closeTo 0)
        f"<text${fm.vquote(u,"x","y")}$show>$safeText</text>"
      else 
        f"<text${fm.vquote(u,"x","y")} transform=${q}rotate(${fm(rot)} ${fm comma u})${q}$show>$safeText</text>"
    )
  }
}
object Letters {
  private[this] val safeMapping = {
    val m = new collection.mutable.LongMap[String]
    for (i <- 0 until ' ') { m += (i.toChar, f"&#$i;") }
    m += ('<', "&lt;")
    m += ('>', "&gt;")
    m += ('&', "&amp;")
  }

  def apply(anchor: Vc, text: String, style: Style) = new Letters(anchor, text, 0, style)

  def safeEncode(s: String): String = {
    var i = 0
    var fine = true
    while (i < s.length && fine) {
      val c = s(i)
      if (safeMapping contains c) fine = false
      else i += 1
    }
    if (i >= s.length) return s
    val sb = new java.lang.StringBuilder
    if (i > 0) sb.append(s, 0, i)
    while (i < s.length) {
      val c = s(i)
      val cc = safeMapping.getOrNull(c)
      if (cc eq null) sb.append(c) else sb.append(cc)
      i += 1
    }
    sb.toString
  }
}

final case class Picture(corner: Vc, content: Bitmap, scaled: Option[Vc], style: Style) extends Shown {
  override def styled = style.collect{ case o: Opaque => o }
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    val other = corner + (scaled match { case Some(v) => Vc(v.x * content.w, v.y * content.h); case None => Vc(content.w, content.h) })
    val uc = xform(corner)
    val uo = xform(other)
    val du = uo - uc
    val wh = (uc.x - uo.x).abs vc (uc.y - uo.y).abs
    val invariant = f"${fm.vquote(wh,"width","height")} preserveAspectRatio=${q}none${q} xlink:href=$q${content.toDataURI}$q"
    Indent.V(
      if (du.x < 0 || du.y < 0) {
        val sx = if (du.x < 0) -1 else 1
        val sy = if (du.y < 0) -1 else 1
        val v = Vc(if (du.x < 0) -uc.x else uc.x, if (du.y < 0) -uc.y else uc.y)
        f"<image${fm.vquote(v,"x","y")} transform=${q}scale($sx,$sy)${q} $invariant/>"
      }
      else f"<image${fm.vquote(uc,"x","y")} $invariant/>"
    )
  }
}


/////////////////////////////////////////////
// Common figure elements (error bars etc) //
/////////////////////////////////////////////


/** Note: `hvbias` is (horizontal bar width - vertical bar width)/(horz width + vert width).
  * The wider of the two is drawn at the stroke width; the other, narrower.
  */
final case class ErrorBarYY(x: Float, lo: Float, hi: Float, across: Float, hvbias: Float, style: Style) extends Shown {
  override def styled = style.stroky.promoteStrokeOpacity
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    implicit val myMag = Magnification.from(mag, xform, Vc(x,lo), Vc(x,hi))
    val l = xform(Vc(x, lo))
    val u = xform(Vc(x, hi))
    val ll = xform(Vc(x-across, lo))
    val lr = xform(Vc(x+across, lo))
    val ul = xform(Vc(x-across, hi))
    val ur = xform(Vc(x+across, hi))
    if (hvbias >= 0.9995)
      Indent.V(f"<path d=${q}M ${fm(l)} L ${fm(u)}${q}$show/>") // Entirely vertical
    else if (hvbias <= -0.9995) 
      Indent.V(f"<path d=${q}M ${fm(ll)} L ${fm(lr)} M ${fm(ul)} L ${fm(ur)}${q}$show/>")  // Entirely horizontal
    else if (hvbias in (-0.005f, 0.005f))
      Indent.V(f"<path d=${q}M ${fm(ll)} L ${fm(lr)} M ${fm(ul)} L ${fm(ur)} M ${fm(l)} L ${fm(u)}${q}$show/>")  // All same thickness
    else {
      // Lines of different thickness
      val mcross = if (hvbias >= 0) 1f else (1 + hvbias)/(1 - hvbias)
      val mriser = if (hvbias <= 0) 1f else (1 - hvbias)/(1 + hvbias)
      Indent.V(
        f"<g${showWith(_.generally)}>",
        f"<path d=${q}M ${fm(ll)} L ${fm(lr)} M ${fm(ul)} L ${fm(ur)}${q}${showWith(_.specifically.scale(mcross))}/>",
        f"<path d=${q}M ${fm(l)} L ${fm(u)}${q}${showWith(_.specifically.scale(mriser))}/>",
        f"</g>"
      )
    }
  }
}

/** This is just a rotated version of ErrorBarYY.
  * Note: `vhbias` is (vertical bar width - horizontal bar width)/(horz width + vert width).
  * The wider of the two is drawn at the stroke width; the other, narrower.
  */
final case class ErrorBarXX(lo: Float, hi: Float, y: Float, across: Float, vhbias: Float, style: Style) extends Shown {
  override def styled = style.stroky.promoteStrokeOpacity
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    implicit val myMag = Magnification.from(mag, xform, Vc(lo, y), Vc(hi, y))
    val l = xform(Vc(lo, y))
    val u = xform(Vc(hi, y))
    val ll = xform(Vc(lo, y-across))
    val lr = xform(Vc(lo, y+across))
    val ul = xform(Vc(hi, y-across))
    val ur = xform(Vc(hi, y+across))
    if (vhbias >= 0.9995)
      Indent.V(f"<path d=${q}M ${fm(l)} L ${fm(u)}${q}$show/>") // Entirely horizontal
    else if (vhbias <= -0.9995) 
      Indent.V(f"<path d=${q}M ${fm(ll)} L ${fm(lr)} M ${fm(ul)} L ${fm(ur)}${q}$show/>")  // Entirely vertical
    else if (vhbias in (-0.005f, 0.005f))
      Indent.V(f"<path d=${q}M ${fm(ll)} L ${fm(lr)} M ${fm(ul)} L ${fm(ur)} M ${fm(l)} L ${fm(u)}${q}$show/>")  // All same thickness
    else {
      // Lines of different thickness
      val mcross = if (vhbias >= 0) 1f else (1 + vhbias)/(1 - vhbias)
      val mriser = if (vhbias <= 0) 1f else (1 - vhbias)/(1 + vhbias)
      Indent.V(
        f"<g${showWith(_.generally)}>",
        f"<path d=${q}M ${fm(ll)} L ${fm(lr)} M ${fm(ul)} L ${fm(ur)}${q}${showWith(_.specifically.scale(mcross))}/>",
        f"<path d=${q}M ${fm(l)} L ${fm(u)}${q}${showWith(_.specifically.scale(mriser))}/>",
        f"</g>"
      )
    }
  }
}

final case class ErrorArc(c: Vc, edge: Vc, lo: Vc, hi: Vc, crosswidths: Float, style: Style) extends Shown {
  override def styled = style.stroky.promoteStrokeOpacity
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    implicit val myMag = Magnification.from(mag, xform, c, edge)
    val cu = xform(c)
    val eu = xform(edge)
    val r = (cu dist eu).toFloat
    val lou = (xform(lo) - cu).hat
    val hiu = (xform(hi) - cu).hat
    val arclo = cu + lou*r
    val archi = cu + hiu*r
    val arcpart = f"M ${fm(arclo)} A ${fm comma Vc(r,r)} 0 0,0 ${fm comma archi}"
    if (crosswidths > 0) {
      val w = crosswidths * style.elements.collectFirst{ case StrokeWidth(x) => x }.getOrElse(1f);
      val lobarA = arclo - w*lou
      val lobarB = arclo + w*lou
      val hibarA = archi - w*hiu
      val hibarB = archi + w*hiu
      Indent.V(
        f"<path d=${q}M ${fm(lobarA)} L ${fm(lobarB)} M ${fm(hibarA)} L ${fm(hibarB)} $arcpart${q}$show/>"
      )
    }
    else Indent.V(
      f"<path d=${q}$arcpart${q}$show/>"
    )
  }
}



final case class ColorBar(corner: Vc, size: Vc, spectrum: Spectrum, outbounds: Boolean, ticks: Option[(Int, Float, Float, Float)], style: Style) extends Shown {
  lazy val onlyOpaque = style.collect{ case o: Opaque => o }
  val (picture: Picture, fy0: Float, fy1: Float, lobar: Option[Bar], hibar: Option[Bar]) = {
    val locolor = spectrum.low.filter(_ => outbounds)
    val hicolor = spectrum.high.filter(_ => outbounds)
    val rows =
      Iterator.iterate(spectrum.colors.length * 3)(i => if (i < 1) 30 else if (i < 30) i*2 else i).dropWhile(_ < 30).next +
      (if (locolor.isDefined) 3 else 0) +
      (if (hicolor.isDefined) 3 else 0)
    val cols = math.max(5, rows/10)
    val y0 = (if (locolor.isDefined) 3 else 0)
    val yN = rows - (if (hicolor.isDefined) 3 else 0)
    val img = Bitmap.empty(cols, yN - y0)
    var y = y0
    while (y < yN) {
      val rgba = spectrum((y - y0)/(yN-y0-1).toFloat)
      var x = 0
      while (x < cols) {
        img(x, y - y0) = rgba
        x += 1
      }
      y += 1
    }
    val picstyle = if (ticks.isDefined && onlyOpaque.elements.nonEmpty) style.filter(x => !x.isInstanceOf[Opaque]) else style
    val pic = Picture(corner + Vc(0, size.y*y0/rows), img, Some(Vc(size.x/cols, size.y/rows)), picstyle)
    val lob = locolor.map(c => Bar(corner + Vc(size.x/2, size.y/rows), Vc(size.x/2, size.y/rows), Fill(c)))
    val hib = hicolor.map(c => Bar(corner + Vc(size.x/2, (size.y*(rows-1))/rows), Vc(size.x/2, size.y/rows), Fill(c)))
    (pic, y0/rows.toFloat, yN/rows.toFloat, lob, hib)
  }
  lazy val autoticks = ticks.collect{ case (n, lo, hi, tickl) if n > 1 =>
    val v0 = if (tickl > 0) corner + Vc(size.x, 0) else corner
    val dv = Vc(0, size.y)
    val l = if (tickl < 0) size.x*tickl else 0
    val r = if (tickl < 0) 0 else size.x*tickl
    val tstyle = (Stroke(Rgba.Black, 1.5f) + Titling.defaultFaces) ++ style.filter(x => !x.isInstanceOf[Opaque])
    val raw = AutoTick(0 vc lo, 0 vc hi, n, l, r, -size.x*tickl*0.5f, tstyle)
    raw.theTicks.copy(from = v0 + dv*fy0, to = v0 + dv*fy1)
  }
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = autoticks match {
    case Some(ticked) =>
      Indent(if (onlyOpaque.elements.isEmpty) "<g>" else f"<g${fm(onlyOpaque)}>") +:
      (
        (
          picture.inSvg(xform, mag)(fm) ++
          lobar.toVector.flatMap(_.inSvg(xform, mag)(fm)) ++
          hibar.toVector.flatMap(_.inSvg(xform, mag)(fm)) ++
          ticked.inSvg(xform, mag)(fm)
        ).map(_.in) :+
        Indent("</g>")
      )
    case _ =>
      if (lobar.isDefined || hibar.isDefined) {
        picture.inSvg(xform, mag)(fm) ++
        lobar.toVector.flatMap(_.inSvg(xform, mag)(fm)) ++
        hibar.toVector.flatMap(_.inSvg(xform, mag)(fm))
      }
      else picture.inSvg(xform, mag)(fm)
  }
}

final case class Spread(c: Vc, axis: Vc, major: Deviable, minor: Deviable, dense: Vc, p: Float, style: Style) extends Shown {
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
    val sigmas = pSigma2D(p)
    val a = axis.hat
    val uc = xform(c)
    val uM = xform(c + a * (sigmas * major.error).toFloat) - uc
    val um = xform(c + a.ccw * (sigmas * minor.error).toFloat) - uc
    val uMl = uM.len
    val uml = um.len
    val rectarea = uMl * uml
    val ux = xform(c + Vc(dense.x, 0)) - uc
    val uy = xform(c + Vc(0, dense.y)) - uc
    val darkrect = (ux.lenSq * uy.lenSq).sqrt
    val scaleup = if (rectarea < darkrect) darkrect/rectarea else 1
    val fade = if (rectarea > darkrect) (darkrect/rectarea).toFloat else 1
    val fader = (already: Float) => math.max(math.min(already, 0.01f), already*fade)
    val er = Vc.from(uMl * scaleup, uml * scaleup)
    implicit val myMag = Magnification.from(mag, xform, c)
    if (uM.y closeTo 0)
      Indent.V(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(er, "rx", "ry")}${showWith(_.fade(fader))}/>")
    else if (uM.x closeTo 0)
      Indent.V(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(Vc(er.y, er.x), "rx", "ry")}${showWith(_.fade(fader))}/>")
    else {
      val rotation = f" transform=${q}rotate(${fm((math.atan2(uM.y, uM.x) * 180 / math.Pi).toFloat)} ${fm comma uc})${q}"
      Indent.V(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(er, "rx", "ry")}$rotation${showWith(_.fade(fader))}/>")
    }
  }
}

