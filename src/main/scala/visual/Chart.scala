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

package object chart {
  private[chart] val q = "\""

  def svg(size: Vc, stuff: InSvg*): Vector[Indent] =
    Vector("""<svg xmlns="http://www.w3.org/2000/svg" width="%.2f" height="%.2f">""".format(size.x, size.y), "<g>").map(x => Indent(x)) ++
    stuff.flatMap(_.inSvg(Xform.flipy(size.y), None)(DefaultFormatter)).map(x => x.in) ++
    Vector("</g>", "</svg>").map(x => Indent(x))

  def svgHtml(size: Vc, bg: Rgba, stuff: InSvg*): Vector[String] = (
    Vector(Indent("<html>"), Indent(f"<body${if (!(bg.a closeTo 0)) f" bgcolor=$q${DefaultFormatter(bg)}$q" else ""}>")) ++
    svg(size, stuff: _*).map(x => x.in) ++
    Vector(Indent("</body>"), Indent("</html>"))
  ).map(_.toString)

  def svgHtml(size: Vc, stuff: InSvg*): Vector[String] = svgHtml(size, Rgba.Empty, stuff: _*)

  def quick(i: InSvg*) {
    val svg = 
      Vector("<html>", "<body>", """<svg xmlns="http://www.w3.org/2000/svg" width="640" height="480">""", "<g>").map(x => Indent(x)) ++
      i.flatMap(_.inSvg(Xform.flipy(480), None)(DefaultFormatter)).map(x => x.in) ++
      Vector("</g>", "</svg>", "</body>", "</html>").map(x => Indent(x))
    println(svg.mkString("\n"))
    svg.map(_.toString).toFile("test.html".file)
  }

  private[chart] def pSigma2D(p: Float): Float = (
    if (p < 1e-6) NumericConstants.SqrtTwo * 1e-3
    else if (p > 1-1e-6) 5.2565217697569319786   // Value from Wolfram Alpha (20 digits)
    else (-2*math.log(1-p)).sqrt
  ).toFloat
}

package chart {

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
            us.map{ lu => 
              val u = Vc from lu
              Indent(f"<circle${fm.vquote(u, "cx", "cy")}$fmR$showSpec/>", 1)
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
          top.result.foreach(uc => b += Indent(f"<circle${fm.vquote(uc, "cx", "cy")}$fmR$showSpec/>", 1))
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

  final case class Spider(samples: Array[(Est, Est)], p: Float, style: Style, legs: Option[Style] = None, body: Option[Style] = None) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
      val sigmas = pSigma2D(p)
      val centers = samples.map{ case (ex, ey) => Vc.from(ex.mean, ey.mean).underlying }
      val extents = samples.map{ case (ex, ey) => Vc.from(ex.error, ey.error).underlying }
      implicit val myMag = Magnification.from(mag, xform, centers)
      var tex, tey, mex, mey = EstM()
      var totalA = 0.0
      var qex, qey = 0.0
      var i = 0
      while (i < samples.length) {
        val (ex, ey) = samples(i)
        tex ++= ex
        tey ++= ey
        mex += ex.value
        mey += ey.value
        qex += ex.errorSq
        qey += ey.errorSq
        totalA += ex.error * ey.error
        i += 1
      }
      val totalN = (tex.n + tey.n)/2
      val solidDensity = totalN / (tex.error * tey.error)
      val densities = samples.map{ case (ex, ey) => (((ex.n + ey.n)/(2 * ex.error * ey.error))/solidDensity).clip(0.01, 0.7) }
      val linestyle = legs.getOrElse{
        style.elements.collect{ case sw: StrokeWidth => sw; case sc: StrokeColor => sc }.toVector.fn{ es =>
          if (es.length == 2) style
          else {
            val fc: Option[Stylish] = style.elements.collectFirst{ case FillColor(c) => StrokeColor(c) }
            val fo: Option[Stylish] = style.elements.collectFirst{ case FillOpacity(o) => StrokeOpacity(o) }
            val sw: Option[Stylish] = es.collectFirst{ case x: StrokeWidth => x }
            val sc: Option[Stylish] = es.collectFirst{ case x: StrokeColor => x }
            val so: Option[Stylish] = style.elements.collectFirst{ case x: StrokeOpacity => x }
            Style(Stylish.unique(
              style.elements,
              FillNone :: sw.getOrElse(StrokeWidth(1/myMag.value)) :: sc.orElse(fc).toList ::: so.orElse(fo).toList
            ))
          }
        }
      }
      val centerstyle = body.map(_.explicitFillOnly).getOrElse(linestyle)
      Vector(Indent(s"<g${showWith(_.generally)}>")) ++
      centers.indices.map{ i =>
        val c = Vc from centers(i)
        val e = Vc from extents(i)
        val uc = xform(c)
        val uM = xform(c + (sigmas * Vc(e.x, 0))) - uc
        val um = xform(c + (sigmas * Vc(0, e.y))) - uc
        val er = Vc.from(uM.len, um.len)
        val fader = (already: Float) => math.max(math.min(already, 0.01), already * densities(i)).toFloat
        if (uM.y closeTo 0)
          Indent(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(er, "rx", "ry")}${showWith(_.fade(fader))}/>", 1)
        else if (uM.x closeTo 0)
          Indent(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(Vc(er.y, er.x), "rx", "ry")}${showWith(_.fade(fader))}/>", 1)
        else {
          val rotation = f" transform=${q}rotate(${fm((math.atan2(uM.y, uM.x) * 180 / math.Pi).toFloat)} ${fm comma uc})${q}"
          Indent(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(er, "rx", "ry")}$rotation${showWith(_.fade(fader))}/>", 1)
        }
      }.toVector ++
      {
        val ut = xform(Vc.from(tex.value, tey.value))
        centers.indices.map{ i =>
          val c = Vc from centers(i)
          Indent(f"<path d=${q}M${fm(xform(c))} L${fm(ut)}${q}${showWith(_ => linestyle)}/>", 1)
        }.toVector
      } ++
      Vector(
        Indent(f"<ellipse${fm.vquote(xform(Vc.from(tex.value, tey.value)), "cx", "cy")}${fm.vquote(Vc.from(tex.error, tey.error)*sigmas, "rx", "ry")}${showWith(_ => centerstyle)}/>", 1),
        Indent("</g>")
      )
    }
  }


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
      while (i < math.min(v.length,1)) { sb ++= "M "; sb ++= fm(Vc from v(i)); i += 1 }
      while (i < math.min(v.length,2)) { sb ++= " L "; sb ++= fm(Vc from v(i)); i += 1 }
      while (i < v.length) { sb += ' '; sb ++= fm(Vc from v(i)); i += 1 }
      implicit val myMag = Magnification.from(mag, xform, pts)
      sb ++= f"$q$show/>"
      Indent.V(sb.result)
    }
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


  final case class Ticky(from: Vc, to: Vc, locations: Seq[Float], left: Float, right: Float, style: Style) extends Shown {
    override def styled = style.stroky
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
      val e = (xform(to) - xform(from)).hat
      val uf = xform(from)
      val ud = (xform(to) - xform(from)).len.toFloat
      implicit val myMag = Magnification.from(mag, xform, from, to)
      val strokes = locations.map{ l => 
        val p = uf + e*(ud*l.clip(0,1))
        " M " + fm(p + e.ccw*(left*myMag.value)) + " L " + fm(p + e.ccw*(right*myMag.value))
      }
      Indent.V(f"<path d=${q}${strokes.mkString}$q$show/>")
    }
  }

  final case class Tik(where: Float, what: String) {}

  final case class TickLabels(from: Vc, to: Vc, ticks: Seq[Tik], left: Float, right: Float, anchor: Float, style: Style) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
      val uf = xform(from)
      val ut = xform(to)
      val e = (ut - uf).hat
      val ud = (xform(to) - xform(from)).len.toFloat
      implicit val myMag = Magnification.from(mag, xform, from, to)
      val tkl = e.ccw * (left * myMag.value)
      val tkr = e.ccw * (right * myMag.value)
      val strokes = ticks.map{ case Tik(l,_) => 
        val p = uf + e*(ud*l.clip(0,1))
        " M " + fm(p + tkl) + " L " + fm(p + tkr)
      }
      val xish = e.x.abs >= NumericConstants.OverSqrtTwo
      val (halign, valign) =
        if (xish) (Horizontal.Middle, if (e.x >= 0 == anchor >= 0) Vertical.Top else Vertical.Bottom)
        else      (if (e.y > 0 == anchor > 0) Horizontal.Left else Horizontal.Right, Vertical.Middle)
      val textalign = Font(halign, valign)
      val jump = e.ccw * (myMag.value * ((if (xish == anchor >= 0) right else left) + (if (xish) anchor else -anchor)))
      val specced = (styled.specifically.unstroked + textalign).defaultTo(FontSize(10))
      val spectext = showWith(_ => specced)
      val labels = ticks.map{ case Tik(l,t) =>
        val p = uf + e*(ud*l.clip(0,1))
        val unbug =
          if (!fm.verticalTextIsBuggy || valign == Vertical.Bottom) 0 vc 0
          else {
            val height = specced.elements.collectFirst{ case FontSize(s) => s * myMag.value }.getOrElse(10f)
            0 vc height * fm.verticalFix(valign)
          }
        f"<text${fm.vquote(p + jump + unbug, "x", "y")}$spectext>$t</text>"
      }
      Indent.V({
        Seq(
          f"<g${showWith(_.promoteStrokeOpacity.generally)}>",
          f"<path d=${q}${strokes.mkString}$q${showWith(_.promoteStrokeOpacity.specifically.unfilled)}/>"
        ) ++
        labels ++
        Seq("</g>")
      }: _*)
    }
  }

  final case class AutoTick(from: Vc, to: Vc, number: Int, left: Float, right: Float, anchor: Float, style: Style, sub: Option[(Int,Style)] = None) extends Shown {
    private[this] def firstDifference(a: String, dota: Int, b: String, dotb: Int): Int = {
      if ((a.charAt(0) == '-') != (b.charAt(0) == '-')) {
        var i, j = 0
        if (a.charAt(0) == '-') i += 1
        else j += 1
        while (i < a.length && (a.charAt(i) match { case '0' | '.' => true; case _ => false })) i += 1
        while (j < b.length && (b.charAt(j) match { case '0' | '.' => true; case _ => false })) j += 1
        if (i >= a.length || j >= b.length) i - (dota min dotb)
        else (i - dota) min (i - dotb)
      }
      else if (dota != dotb) -(dota max dotb) + (if (a.charAt(0) == '-') 1 else 0)
      else {
        var i = 0
        while (i < a.length && i < b.length && a.charAt(i) == b.charAt(i)) i += 1
        if (i >= a.length || i >= b.length) i - (dota min dotb)
        else (i - dota) min (i - dotb)
      }
    }
    private[this] def saveDifference(s: String, dot: Int, i0: Int, diff: Int, cs: Array[Char], n: Int) {
      cs(n) = '-'
      var m = 1
      var i = dot + diff
      while (m < 4) {
        if (i == dot) i += 1
        cs(n+m) = if (i < i0 || i >= s.length) '0' else s.charAt(i)
        m += 1
        i += 1
      }
    }
    private[this] def clipDifference(a: String, dota: Int, b: String, dotb: Int, diff: Int): (String, String, String) = {
      val cs = new Array[Char](8)
      val ai0 = if (a.charAt(0) == '-') 1 else 0
      val bi0 = if (b.charAt(0) == '-') 1 else 0
      saveDifference(a, dota, ai0, diff, cs, 0)
      saveDifference(b, dotb, bi0, diff, cs, 4)
      (
        if (diff+dota > ai0) a.substring(ai0, diff+dota) else "",
        new String(cs, 1 - ai0, 3 + ai0),
        new String(cs, 5 - bi0, 3+ bi0)
      )
    }
    private[this] def modScore(n: Int): Int = 
      if ((n % 5) == 0) 2
      else if ((n % 2) == 0) 1
      else 0
    private[this] def findScores(na: Int, nb: Int): Array[Int] = {
      val scores = new Array[Int](1 + (na - nb).abs)
      val inc = if (na > nb) -1 else 1
      var i = 0
      var n = na
      while (i < scores.length) {
        // This is a pretty dumb brute-force way to do this.
        // Even having the array is pretty dumb, but it's easy and this
        // code _probably_ won't be a serious bottleneck.
        scores(i) =
          if ((n % 10) == 0) { 
            val nn = n / 10
            if ((nn % 10) == 0) {
              val nnn = nn / 10
              if (nnn == 0) 9
              else 6 + modScore(nnn)
            }
            else 3 + modScore(nn)
          }
          else modScore(n)
        i += 1
        n += inc
      }
      scores
    }
    private[this] def findBestIndices(scores: Array[Int], goal: Int): (Int, Int, Int) = {
      val alphi = {  // Index with the largest score (first, if there are duplicates)
        var i = 0
        var best = -1
        var besti = 0
        while (i < scores.length) {
          if (best < scores(i)) {
            best = scores(i)
            besti = i
          }
          i += 1
        }
        besti
      }
      val beti = {  // Index with the second-largest score (closest to / after alphi, if there are ties)
        var i = alphi+1
        var best = -1
        var besti = 0
        while (i < scores.length) {
          if (best < scores(i)) {
            best = scores(i)
            besti = i
          }
          i += 1
        }
        i = alphi - 1
        while (i >= 0) {
          if (best < scores(i)) {
            best = scores(i)
            besti = i
          }
          i -= 1
        }
        besti
      }
      var anchor = alphi
      var gap = (alphi - beti).abs // Sane default
      var best = -1.0
      var bestn = 0
      var g = 2*gap                // Twice is probably too much, but it will never hurt given algorithm below
      while (g >= 1 && scores.length/g < 2*goal) {
        var firstpass = true
        var gotbeti = false
        while (!gotbeti) {
          val i0 = if (firstpass) alphi else beti
          gotbeti = i0 == beti
          firstpass = false
          var s = (1 << scores(i0)).toDouble
          var i = i0 + g
          var n = 1
          while (i < scores.length) { s += (1 << scores(i)); i += g; n += 1 }
          i = i0 - g
          while (i >= 0) { s += (1 << scores(i)); i -= g; n += 1 }
          val quality = 1 - (n - goal).abs/goal.toDouble
          s = if (quality < 0) 0 else s * quality * quality
          if (s > best) {
            best = s
            bestn = n
            anchor = i0
            gap = g
          }
        }
        var tenthless = g
        while ((tenthless % 10) == 0 && tenthless > 10) tenthless = tenthless / 10
        g = if ((tenthless % 10) == 5 && (g % 5) == 0) (g/5)*4 else g/2
      }
      (anchor, gap, bestn)
    }
    private[this] def removePointlessZeros(s: String): String = {
      val i0 = if (s.charAt(0) == '-') 1 else 0
      val nLeading = {
        var i = i0
        while (i < s.length && s.charAt(i) == '0') i += 1
        if (i == s.length || s.charAt(i) == '.') i -= 1
        i
      }
      val nTrailing = {
        val ip = s.indexOf('.')
        if (ip < 0) 0
        else {
          var i = s.length-1
          while (i > ip && s.charAt(i) == '0') i -= 1
          if (i == ip) s.length - ip
          else s.length-1-i
        }
      }
      if (nLeading == 0 && nTrailing == 0) s
      else if (nLeading == 0) s.dropRight(nTrailing)
      else if (s.charAt(0) != '-') s.slice(nLeading, s.length-nTrailing)
      else "-" + s.slice(1+nLeading, s.length-nTrailing) 
    }

    private[this] lazy val tickInfo: (TickLabels, Option[Ticky]) = {
      val delta = to - from
      if (delta.lenSq == 0 || !delta.finite) (TickLabels(from, to, Nil, left, right, anchor, style), None)
      else {
        val (fa, fb) = if (delta.x.abs >= delta.y.abs) (from.x, to.x) else (from.y, to.y)
        val f = fa.abs max fb.abs
        var points = 0
        if ("%.0f".format(f).length < 3) {
          points += 10
          while (s"%.${points}f".format(f).drop(points-10).dropWhile(c => c == '0' || c == '.').length < 3 && points < 30) points += 10;
        }
        var stra = s"%.${points}f".format(fa)
        var strb = s"%.${points}f".format(fb)
        var dota = stra.indexOf('.') match { case -1 => stra.length; case x => x }
        var dotb = strb.indexOf('.') match { case -1 => strb.length; case x => x }
        var fdif = firstDifference(stra, dota, strb, dotb)
        while (dota + fdif + 3 >= stra.length && dotb + fdif + 3 >= strb.length && points < 45) {
          points += 5
          stra = s"%.${points}f".format(fa)
          strb = s"%.${points}f".format(fb)
          dota = stra.indexOf('.') match { case -1 => stra.length; case x => x }
          dotb = strb.indexOf('.') match { case -1 => strb.length; case x => x }
          fdif = firstDifference(stra, dota, strb, dotb)
        }
        val (prefix, bita, bitb) = clipDifference(stra, dota, strb, dotb, fdif)
        val na = bita.toInt
        val nb = bitb.toInt
        val scores = findScores(na, nb)
        val (i0, di, len) = findBestIndices(scores, number)
        val labels = (new Array[Tik](len)): collection.mutable.WrappedArray[Tik]
        var i = i0%di
        var j = 0
        while (i < scores.length) {
          val ni = na + i
          val text = removePointlessZeros("%s%s%s" . format(
            if (ni < 0) "-" else "",
            prefix,
            if (fdif == -2)      "%02d.%d".format(ni.abs/10, ni.abs%10)
            else if (fdif == -1) "%d.%02d".format(ni.abs/100, ni.abs%100)
            else if (fdif < -3)
              if (ni != 0)       "%03d%s".format(ni.abs, "0"*(-fdif-3))
              else               "0"
            else                 "%03d".format(ni.abs)
          ))
          val value = text.toDouble
          val frac = (value - fa)/(fb - fa)
          labels(j) = Tik(frac.toFloat, text)
          i += di
          j += 1
        }
        (TickLabels(from, to, labels, left, right, anchor, style), None)
      }
    }
    def theTicks = tickInfo._1
    def subTicks = tickInfo._2


    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = theTicks.inSvg(xform, mag)(fm)
  }


  final case class Letters(anchor: Vc, text: String, rotate: Float, style: Style) extends Shown {
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
          f"<text${fm.vquote(u,"x","y")}$show>$text</text>"
        else 
          f"<text${fm.vquote(u,"x","y")} transform=${q}rotate(${fm(rot)} ${fm comma u})${q}$show>$text</text>"
      )
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

  final case class Assembly(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float => Float], style: Style, stuff: Seq[InSvg]) extends Shown {
    def this(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float => Float], style: Style, thing: InSvg, morestuff: InSvg*) =
      this(oldOrigin, scale, newOrigin, thicken, style, thing +: morestuff)
    def this(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float => Float], stuff: InSvg*) = 
      this(oldOrigin, scale, newOrigin, thicken, Style.empty, stuff)
    def this(oldOrigin: Vc, scale: Vc, newOrigin: Vc, stuff: InSvg*) =
      this(oldOrigin, scale, newOrigin, None, Style.empty, stuff)
    def this(translate: Vc, stuff: InSvg*) =
      this(translate, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
    def this(stuff: InSvg*) =
      this(0 vc 0, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
      implicit val myMag = Magnification.one
      val yform =
        if (oldOrigin == Vc(0,0) && scale == Vc(1,1) && newOrigin == Vc(0,0)) xform
        else if (scale == Vc(1,1)) Xform.origin(oldOrigin - newOrigin) andThen xform
        else Xform.reorigin(oldOrigin, scale, newOrigin) andThen xform
      Indent.V(f"<g$show>") ++
      stuff.toVector.flatMap(_.inSvg(yform, thicken).map(_.in)) ++
      Indent.V("</g>")
    }
  }
  object Assembly{
    def apply(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float => Float], style: Style, thing: InSvg, morestuff: InSvg*) =
      new Assembly(oldOrigin, scale, newOrigin, thicken, style, thing +: morestuff)
    def apply(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float => Float], stuff: InSvg*) =
      new Assembly(oldOrigin, scale, newOrigin, thicken, Style.empty, stuff)
    def apply(oldOrigin: Vc, scale: Vc, newOrigin: Vc, stuff: InSvg*) = 
      new Assembly(oldOrigin, scale, newOrigin, None, Style.empty, stuff)
    def apply(translate: Vc, stuff: InSvg*) =
      new Assembly(translate, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
    def apply(stuff: InSvg*) =
      new Assembly(0 vc 0, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
  }

  final case class Titling(
    title: String, xlegend: String, ylegend: String,
    titler: Style => Style = Titling.defaultTitler, legender: Style => Style = Titling.defaultLegender,
    titleGap: Float = 0f, xlegendGap: Float = 0f, ylegendGap: Float = 0f
  ) {}
  object Titling {
    val defaultTitler = (s: Style) => s.scale(1.59f)
    val defaultLegender = (s: Style) => s.scale(1.26f)
    val defaultFaces = FontFace("Carlito, Callibri, Arial, sans-serif")
  }

  final case class Space(dataOrigin: Vc, dataExtent: Vc, viewOrigin: Vc, viewExtent: Vc, ticknum: Int, ticklen: Float, arrow: Option[Arrowhead], linestyle: Style, stuff: Seq[InSvg], titles: Option[Titling] = None) extends Shown {
    private[this] val extrascale = if (arrow.isDefined) 1.3f else 1.1f;

    lazy val dataAssembly = Assembly(
      dataOrigin,
      Vc(viewExtent.x / dataExtent.x, viewExtent.y / dataExtent.y),
      viewOrigin,
      None,
      Style.empty,
      stuff
    )

    lazy val axisLine = Dynamic(PolyArrow(
      Array(
        (dataOrigin + Vc(0, dataExtent.y * extrascale)).underlying,
        dataOrigin.underlying,
        (dataOrigin + Vc(dataExtent.x * extrascale, 0)).underlying
      ),
      arrow,
      arrow,
      linestyle
    )){ (line, xform, mag, fm) =>
      val m = Magnification.from(mag, xform, line.points)
      line.style.elements.collectFirst{
        case StrokeWidth(w) =>
          line.copy(points = line.points.map(l => (xform.inverse(xform(Vc from l) - Vc(w/2, -w/2)*m.value)).underlying))
      }.getOrElse(line)
    }


    lazy val tickstyle = {
      val w: Float = linestyle.elements.collectFirst{ case StrokeWidth(x) => x }.getOrElse(3)
      linestyle.
        map{ case StrokeWidth(w) => StrokeWidth(w*0.71f); case x => x }.
        defaultTo(FontSize(w*4), Titling.defaultFaces)
    }

    lazy val legendstyle: Style = titles.map(_.legender(tickstyle)).getOrElse(tickstyle).unstroked

    lazy val titlestyle: Style = titles.map(_.titler(tickstyle)).getOrElse(tickstyle).unstroked

    lazy val xTicks = AutoTick(
      dataOrigin,
      dataOrigin + Vc(dataExtent.x, 0),
      ticknum,
      (if (ticklen < 0) ticklen else 0),
      (if (ticklen < 0) 0 else ticklen),
      ticklen.abs / 2,
      tickstyle,
      None
    )

    lazy val yTicks = AutoTick(
      dataOrigin,
      dataOrigin + Vc(0, dataExtent.y),
      ticknum,
      (if (ticklen < 0) 0 else -ticklen),
      (if (ticklen < 0) -ticklen else 0),
      ticklen.abs / 2,
      tickstyle,
      None
    )

    lazy val theTitle = titles.map(_.title).filter(_.nonEmpty).map{ titleText =>
      val textHeight: Float = titlestyle.elements.collectFirst{ case FontSize(fs) => fs }.getOrElse(16f)
      val x = 0.5f * viewExtent.x
      val y = viewExtent.y * (extrascale + 0.05f) + textHeight*0.2f + titles.get.titleGap*textHeight
      Letters(viewOrigin + Vc(x, y), titleText, 0, titlestyle ++ Font(Horizontal.Middle, Vertical.Bottom))
    }.toVector

    lazy val xyLegends = {
      val textHeight: Float = legendstyle.elements.collectFirst{ case FontSize(fs) => fs }.getOrElse(13f)
      val tickTextH: Float = tickstyle.elements.collectFirst{ case FontSize(fs) => fs }.getOrElse(10f)
      titles.map(_.xlegend).filter(_.nonEmpty).map{ xText =>
        val x = 0.5f*viewExtent.x
        val y = -(2*ticklen + tickTextH) - titles.get.xlegendGap*textHeight
        Letters(viewOrigin + Vc(x,y), xText, 0, legendstyle ++ Font(Horizontal.Middle, Vertical.Top))
      }.toVector ++
      titles.map(_.ylegend).filter(_.nonEmpty).map{ yText =>
        val y = 0.5f*viewExtent.y
        val x = -(
          2*ticklen +
          tickTextH * 0.5f * yTicks.theTicks.ticks.map(_.what.length).reduceOption(_ max _).getOrElse(1) +
          -titles.get.ylegendGap*textHeight
        )
        Letters(viewOrigin + Vc(x,y), yText, -math.Pi.toFloat/2, legendstyle ++ Font(Horizontal.Middle, Vertical.Bottom))
      }.toVector
    }

    lazy val lineAssembly = dataAssembly.copy(thicken = None, stuff = Vector(axisLine, xTicks, yTicks))

    lazy val fullAssembly = Assembly((Vector(dataAssembly, lineAssembly) ++ xyLegends ++ theTitle): _*)

    def style = linestyle

    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = fullAssembly.inSvg(xform, mag)(fm)
  }

  final case class Piece(value: Float, legend: String = "", fill: Rgba = Rgba.Empty, stroke: Rgba = Rgba.Empty) {}

  final case class Pie(pieces: Vector[Piece], center: Vc, radius: Float, style: Style, zeroAngle: Option[Float] = None, asideness: Option[(Vc, Float, Float) => Float] = None) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
      val uc = xform(center)
      val ur = xform.radius(center, Vc(radius, 0))
      def myxfr(v: Vc): Vc = (xform(center + v.hat * radius) - uc).hat * ur
      implicit val myMag = Magnification.from(mag, radius, ur)
      val values = pieces.map(_.value)
      val vsum = values.sum
      if (vsum closeTo 0) return Vector.empty
      if (pieces.isEmpty) return Vector.empty
      val zero = zeroAngle.getOrElse(((if (values.length == 2) math.Pi else (math.Pi*5)/6) - math.Pi*values.head/vsum).toFloat).toDouble
      val edges = values.scanLeft(zero)((acc, vi) => acc + 2*math.Pi*vi/vsum)
      val middles = edges.sliding(2).map(x => myxfr(if (x.length > 1) Vc.angle((x(0) + x(1))*0.5) else 1 vc 0)).toArray
      val spec = style.specifically
      val fontsize = style.elements.collectFirst{ case FontSize(x) => x }
      val labelsize = fontsize.getOrElse((ur*10).sqrt.toFloat)
      val strokewidth = style.elements.collectFirst{ case StrokeWidth(x) => x }
      val strokesize = strokewidth.getOrElse(labelsize/10f)
      val asideFn: (Vc, Float) => Float = 
        asideness.getOrElse(Pie.alignText).
        fn(flx => (v: Vc, x: Float) => math.max(flx(v, x, labelsize), math.min(0.3*labelsize, 0.2*ur).toFloat))
      val wedges = edges.indices.dropRight(1).flatMap{ i =>
        val p = pieces(i)
        if (!p.fill.exists) Nil
        else {
          val ua = uc + myxfr(Vc.angle(edges(i)))
          val ub = uc + myxfr(Vc.angle(edges(i+1)))
          val s = showWith{_ => spec.unstroked ++ Fill.alpha(p.fill) }
          val largeangle = (if (values(i) > 0.5*vsum) 1 else 0)
          Indent(f"<path d=${q}M ${fm comma uc} L ${fm comma ua} A $ur,$ur 0 $largeangle,0 ${fm comma ub} L ${fm comma uc}${q}$s/>",1) :: Nil         
        }
      }.toVector
      val arrowpoints = middles.map{ m =>
        val mhat = m.hat
        val e = if (m.x.abs*5 >= m.y.abs) mhat else Vc.from((if (m.x == 0) 1 else m.x.sign)*1, m.y.sign*5).hat
        val point = m + mhat*math.max(ur/10, strokesize * myMag.value)
        val bend = point + e*math.max(ur/3, 8 * strokesize * myMag.value)
        val tail = bend.xFn(x => (if (e.x < 0) -1 else 1) * (x.abs + asideFn(bend, ur)).toFloat)
        (point, bend, tail)
      }.tap{ pts =>
        // Fix up anything that's too close together for text to fit
        val lix = middles.zipWithIndex.
          filter{ case (m, i) => m.x < 0 && pieces(i).fn(p => (p.legend ne null) && p.legend.nonEmpty) }.
          sortBy(_._1.y).map{ case (_, i) => i }
        val rix = middles.zipWithIndex.
          filter{ case (m, i) => m.x >= 0 && pieces(i).fn(p => (p.legend ne null) && p.legend.nonEmpty) }.
          sortBy(_._1.y).map{ case (_, i) => i }
        val lys = Array.tabulate(lix.size)(j => pts(lix(j))._3.y)
        val rys = Array.tabulate(rix.size)(j => pts(rix(j))._3.y)
        val lyok = Pie.separateValues(lys, labelsize*1.05f)
        val ryok = Pie.separateValues(rys, labelsize*1.05f)
        if (lyok ne lys) {
          var j = 0; while (j < lyok.length) { val i = lix(j); pts(i) = pts(i).copy(_3 = pts(i)._3.yTo(lyok(j))); j += 1 }
        }
        if (ryok ne rys) {
          var j = 0; while (j < ryok.length) { val i = rix(j); pts(i) = pts(i).copy(_3 = pts(i)._3.yTo(ryok(j))); j += 1 }
        }
      }
      val arrows = arrowpoints.indices.flatMap{ i =>
        val p = pieces(i)
        val (point, bend, tail) = arrowpoints(i)
        if ((p.legend eq null) || p.legend.isEmpty || p.stroke.unicorn) Nil
        else {
          val theSpec = 
            if (p.stroke.exists) spec ++ Stroke.alpha(p.stroke, strokesize)
            else if (p.fill.exists) spec ++ Stroke.alpha(p.fill, strokesize)
            else if (spec.elements.contains{ (x: Stylish) => x match { case _: StrokeColor => true; case _ => false } })
              spec ++ Stroke(strokesize)
            else if (spec.elements.contains{ (x: Stylish) => x match { case _: FillColor => true; case _ => false } })
              (spec + StrokeWidth(strokesize)).map{
                case FillColor(c) => StrokeColor(c)
                case FillOpacity(o) => StrokeOpacity(o)
                case x => x
              }
            else spec ++ Stroke(strokesize)
          val thePoints =
            if (tail.y == bend.y) Array(point, bend, tail + Vc(labelsize/4, 0)*(if (point.x < 0) -1 else 1))
            else Array(point, bend, tail, tail + Vc(labelsize/4, 0)*(if (point.x < 0) -1 else 1))
          PolyArrow(thePoints.map(u => (uc + u).underlying), None, Some(Pie.defaultArrow), theSpec).
            inSvg(Xform.identity, None)(fm)
        }
      }
      Indent(f"<g${showWith(_.generally.defaultTo(Titling.defaultFaces))}>") +: {
        wedges ++
        arrows.map(_.in) ++
        edges.indices.dropRight(1).flatMap{ i =>
          val p = pieces(i)
          if ((p.legend eq null) || p.legend.isEmpty) Nil
          else {
            val stytxRaw = (
              if (spec.elements.exists{ case _: FillColor => true; case _ => false}) spec.unstroked
              else if (p.fill.exists) spec.unstroked + Fill.alpha(p.fill.aFn(a => if (a < 0.5) (a*0.5).sqrt.toFloat else a))
              else if (p.stroke.exists) spec.unstroked + Fill.alpha(p.stroke.aFn(a => if (a < 0.5) (a*0.5).sqrt.toFloat else a))
              else spec
            ).defaultTo(FontSize(labelsize)) ++ Font(if (arrowpoints(i)._3.x < 0) Horizontal.Right else Horizontal.Left, Vertical.Middle)
            val stytx = showWith{_ => stytxRaw}
            val styln = showWith{_ =>
              if (p.fill.exists) spec.stroky ++ Stroke.alpha(p.fill.aFn(a => if (a < 0.5) (a*0.5).sqrt.toFloat else a), strokesize)
              else if (p.stroke.exists) spec.stroky ++ Stroke.alpha(p.stroke, strokesize)
              else spec.stroky.defaultTo(StrokeWidth(strokesize))
            }
            val u = 
              uc +
              arrowpoints(i)._3 +
              Vc(labelsize/2, 0)*(if (arrowpoints(i)._3.x < 0) -1 else 1) +
              Vc(
                0,
                if (!fm.verticalTextIsBuggy) 0
                else fm.verticalFix(Vertical.Middle) * myMag.value *
                    stytxRaw.elements.collectFirst{ case FontSize(s) => s }.getOrElse(labelsize)                
              )
            Indent(f"<text${fm.vquote(u,"x","y")}$stytx>${p.legend}</text>",1) :: Nil
          }
        }.toVector
      } :+ Indent("</g>")
    }
  }
  object Pie {
    val defaultArrow = LineArrow((math.Pi/8).toFloat, 1.75f, 1)
    val wrapText = (v: Vc, r: Float, lz: Float) => 0f
    val alignText = (v: Vc, r: Float, lz: Float) => 1.67f*r + max(0, lz*0.5f) - v.x.abs
    def separateValues(values: Array[Float], minsep: Float): Array[Float] = {
      if (values.length < 2) return values
      var xs = values
      var fixed = false
      val xi = xs.bisect(0)
      var i0 = xi.floor.toInt
      var i1 = if (i0 < 0) { i0 = 0; 0 } else if (i0 >= xs.length-1) { i0 = xs.length-1; i0 } else i0+1
      if (i0 != i1) {
        if (xs(i1) - xs(i0) < minsep) {
          fixed = true
          xs = values.copy
          if (xs(i0) <= xs(i1)) { xs(i0) = -minsep/2; xs(i1) = xs(i0) + minsep }
          else if (xs(i0) closeTo 0) { xs(i1) = xs(i0) + minsep }
          else if (xs(i1) closeTo minsep) { xs(i0) = xs(i1) - minsep }
          else {
            val sep = xs(i1) - xs(i0)
            xs(i0) = xs(i0)*(minsep/sep)
            xs(i1) = xs(i0) + minsep
          }
        }
      }
      while (i1+1 < xs.length && !(xs(i1+1) - xs(i1) < minsep)) i1 += 1
      if (i1 < xs.length && !fixed) { fixed = true; xs = values.copy }
      while (i1+1 < xs.length) {
        val sep = xs(i1+1) - xs(i1)
        if (sep < minsep) xs(i1+1) = xs(i1) + minsep
        i1 += 1
      }
      while (i0 > 0 && !(xs(i0) - xs(i0-1) < minsep)) i0 -= 1
      if (i1 > 0 && !fixed) { fixed = true; xs = values.copy }
      while (i0 > 0) {
        val sep = xs(i0) - xs(i0-1)
        if (sep < minsep) xs(i0 - 1) = xs(i0) - minsep
        i0 -= 1
      }
      xs
    }
  }
}

package chartTest {
  object Example {
    @deprecated("You're not actually supposed to use this except for testing and/or pasting into the REPL", "0.1")
    def createTestHtml()
    {
      import kse.flow._, kse.coll._, kse.coll.packed._, kse.maths._, kse.maths.stats._, kse.maths.stochastic._, kse.jsonal._, JsonConverters._, kse.eio._, kse.visual._, kse.visual.chart._
      val tga, xga, yga = -0.1f
      val ah = Option(LineArrow((math.Pi/180).toFloat*30, 3, 0.71f))
      val pie = Pie(Vector(Piece(15, "red", Rgba.Red), Piece(5, "green", Rgba.Green, Rgba.DarkGreen), Piece(0.1f, "gold", Rgba.Gold), Piece(0.3f, "logically blue", Rgba.Empty, Rgba.Blue)), 150 vc 350, 20, Style.empty, Some(math.Pi.toFloat/12))
      val c = Circ(100 vc 100, 20, Fill(Rgba(0, 0.8f, 0)))
      val b = Bar(200 vc 200, 10 vc 80, Fill(Rgba(1, 0.3f, 0.3f)))
      val dl = DataLine(Array(Vc(50, 300).underlying, Vc(90, 240).underlying, Vc(130, 280).underlying, Vc(170, 260).underlying), Stroke(Rgba(1, 0, 1), 4))
      val dr = DataRange(Array(90, 130, 170, 210), Array(230, 220, 240, 210), Array(270, 310, 270, 260), Fill alpha Rgba(0, 0, 1, 0.3f))
      val ea = ErrorBarYY(150, 95, 115, 7, 0, Stroke(Rgba(1, 0, 0), 2))
      val eb = ErrorBarYY(150, 395, 445, 10, -0.5f, Stroke.alpha(Rgba(1, 0, 0, 0.5f), 10))
      val aa = Arrow(50 vc 200, 200 vc 100, 0.1f, None, Stroke(Rgba(0.5f, 0, 1), 5))
      val ab = Arrow(50 vc 225, 200 vc 125, 0, ah, Stroke.alpha(Rgba(0.5f, 0, 1, 0.5f), 10))
      val pa = PolyArrow(Array(20 vc 400, 20 vc 20, 400 vc 20).map(_.underlying), ah, ah, Stroke(Rgba(0.7f, 0.7f, 0), 5))
      val tk = Ticky(20 vc 20, 400 vc 20, Seq(0.2f, 0.4f, 0.6f, 0.8f), -20, 0, Stroke(Rgba(0.7f, 0.7f, 0), 2))
      val qbf = Letters(200 vc 200, "Quick brown fox", (10*math.Pi/180).toFloat, Fill(Rgba.Black) ++ Font(40, Horizontal.Middle))
      val tl = TickLabels(Vc(100,100), Vc(200,100), Seq(Tik(0, "0"), Tik(0.4f, "40"), Tik(0.8f, "80")), 0, 20, 5, Font(18) ++ Stroke(Rgba(0f, 0.8f, 0.8f), 4) ++ Fill(Rgba(0f, 0.4f, 0.4f)))
      val at = AutoTick(Vc(0.2f, 100), Vc(1.26f, 100), 5, 0, 20, 5, Font(18) ++ Stroke(Rgba(0f, 0.6f, 1f), 4) ++ Fill(Rgba(0f, 0.2f, 0.5f)))
      val gr = Space(0 vc 0, 200 vc 200, 400 vc 100, 100 vc 100, 4, 8, ah, Stroke(Rgba(1f, 0, 0), 6), Seq(c, Marker.C(50 vc 100, 8, Stroke(Rgba.Black, 2) + FillNone), Marker.C(75 vc 125, 8, Fill(Rgba(0, 0, 1)))), Some(Titling("Fish", "salmon", "perch", titleGap = tga, ylegendGap = yga, xlegendGap = xga)))
      val sh = Shape(Array(Vc(100, 200).underlying, Vc(200, 100).underlying, Vc(300, 300).underlying), Option(c.copy(c = 0 vc 0)), Stroke(Rgba.Blue, 5) ++ Fill.alpha(Rgba.Blue.aTo(0.2f)) ++ Opacity(0.4f))
      val pik = Picture(150 vc 150, Bitmap.fillRgba(2,4){ (x,y) => Rgba.web(100+100*x, 128, 50 + 60*y) }, Some(20 vc 40), Style.empty)
      val cbr = ColorBar(250 vc 150, 10 vc 80, Spectrum.Rainbow, true, Some((5, 3f, 5f, -0.5f)), Style.empty)
      val dbtl = TickLabels(Vc(50, 50), Vc(150, 50), Seq(Tik(0, "0"), Tik(1, "0.5")), -20, 0, -5, Font(18) ++ Stroke(Rgba.Black))
      quick(
        sh, pie, c, b, dl, dr, ea, eb, aa, ab, pa, tk, qbf, tl, pik, Circ(pik.corner, 3, Fill(Rgba.Black)), cbr, dbtl,
        Assembly(0 vc 100, 400f vc 1f, 0 vc 200, None, Opacity(1f), at, at.copy(to = Vc(1.33f, 100))),
        tl.copy(to = 100 vc 200, left = -20, right = 0),
        Assembly(
          0 vc 0, 0.3333f vc 0.3333f, 400 vc 200, Option((x: Float) => x.sqrt.toFloat), Opacity(0.5f),
          c, pa, pie.copy(pieces = pie.pieces.dropRight(1).map(p => p.copy(legend = ""))), cbr.copy(ticks = Some((5, 3f, 5f, 0.5f)))
        ),
        gr
      )
    }
  }
}
