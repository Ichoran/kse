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

  def quick(i: InSvg*) {
    val svg = 
      Vector("<html>", "<body>", """<svg width="640" height="480">""", "<g>").map(x => Indent(x)) ++
      i.flatMap(_.inSvg(Xform.flipy(480), None)(DefaultFormatter)).map(x => x.in) ++
      Vector("</g>", "</svg>", "</body>", "</html>").map(x => Indent(x))
    println(svg.mkString("\n"))
    svg.map(_.toString).toFile("test.html".file)
  }
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


  final case class Circ(c: Vc, r: Float, style: Style) extends Shown {
    override def styled = style.shapely
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
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
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
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


  final case class DataLine(pts: Array[Long], style: Style) extends Shown {
    override def styled =
      if (style.elements.exists{ case sj: StrokeJoin => true; case _ => false }) style.stroky
      else style.stroky + StrokeJoin(Join.Round)
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
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
      sb ++= f"$q$show}/>"
      Indent.V(sb.result)
    }
  }

  final case class DataRange(xs: Array[Float], ylos: Array[Float], yhis: Array[Float], style: Style) extends Shown {
    override def styled = style.filly
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
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


  /** Note: `hvbias` is (horizontal bar width - vertical bar width)/(horz width + vert width).
    * The wider of the two is drawn at the stroke width; the other, narrower.
    */
  final case class ErrorBarYY(x: Float, lo: Float, hi: Float, across: Float, hvbias: Float, style: Style) extends Shown {
    override def styled = style.stroky
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
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
      val w = style.elements.collectFirst{ case StrokeWidth(x, _) => x } getOrElse 1f
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
        else if (style.elements.exists{ case StrokeMiter(m, _) => m >= miter || m.closeTo(miter); case _ => false }) sizedStyle
        else sizedStyle + StrokeMiter(miter.toFloat)
      }
      val joinStyle =
        if (flat || !style.elements.exists{ case StrokeJoin(j, _) => j != Join.Miter; case _ => false }) miterStyle
        else miterStyle + StrokeJoin(Join.Miter)
      val ans = 
        if (flat) f"<path d=${q}M ${fm(qA)} L ${fm(qB)}${q}${fm(joinStyle)}/>"
        else f"<path d=${q}M ${fm(qA)} L ${fm(qC)} ${fm(qB)}${q}${fm(joinStyle)}/>"
      (s, ans)
    }
  }

  final case class Arrow(from: Vc, to: Vc, indirection: Float, head: Option[Arrowhead], style: Style) extends Shown {
    override def styled = style.stroky
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
      val vi = if (indirection.finite && !(indirection closeTo 0)) indirection else 0
      val v = to - from;
      val ip = from + v*0.5f - v.ccw*(2f*vi)
      val uf = xform(from)
      val ut = xform(to)
      val iq = xform(ip)
      implicit val myMag = Magnification.from(mag, xform, from, to)
      if (head.nonEmpty) {
        val ah = head.get
        val (setback, arrowline) = ah.stroked(to, (to - ip).hat)(xform, style.specifically.scale(myMag.value))
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
    override def styled = style.stroky
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
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
        val (setback, arrowline) = ar.stroked(Vc from points(0), bkw)(xform, style.specifically.scale(myMag.value))
        up(0) = (Vc.from(up(0)) - setback*(Vc.from(up(0)) - Vc.from(up(1))).hat).underlying
        arrows = arrowline :: arrows
      }
      if (fwdarrow.isDefined) {
        val ar = fwdarrow.get
        val (setback, arrowline) = ar.stroked(Vc from points(points.length-1), fwd)(xform, style.specifically.scale(myMag.value))
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
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
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
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
      val uf = xform(from)
      val ut = xform(to)
      val e = (ut - uf).hat
      val ud = (xform(to) - xform(from)).len.toFloat
      implicit val myMag = Magnification.from(mag, xform, from, to)
      println(myMag.value)
      val tkl = e.ccw * (left * myMag.value)
      val tkr = e.ccw * (right * myMag.value)
      val strokes = ticks.map{ case Tik(l,_) => 
        val p = uf + e*(ud*l.clip(0,1))
        " M " + fm(p + tkl) + " L " + fm(p + tkr)
      }
      val textalign =
        if (e.x.abs >= NumericConstants.OverSqrtTwo) Font(Horizontal.Middle, if (e.x >= 0 == anchor >= 0) Vertical.Top else Vertical.Bottom)
        else                                         Font(if (e.y > 0 == anchor > 0) Horizontal.Left else Horizontal.Right, Vertical.Middle)
      val jump = e.ccw * (myMag.value * (if (anchor > 0) right + anchor else left - anchor))
      val spectext = showWith(_.specifically.unstroked + textalign)
      val labels = ticks.map{ case Tik(l,t) =>
        val p = uf + e*(ud*l.clip(0,1))
        f"<text${fm.vquote(p + jump, "x", "y")}$spectext>$t</text>"
      }
      Indent.V({
        Seq(
          f"<g${showWith(_.generally)}>",
          f"<path d=${q}${strokes.mkString}$q${showWith(_.specifically.unfilled)}/>"
        ) ++
        labels ++
        Seq("</g>")
      }: _*)
    }
  }


  final case class Letters(anchor: Vc, text: String, style: Style) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
      implicit val myMag = Magnification.from(mag, xform, anchor)
      Indent.V(
        f"<text${fm.vquote(xform(anchor),"x","y")}$show>$text</text>"
      )
    }
  }

  final case class Assembly(origin: Vc, scale: Vc, thicken: Option[Float], style: Style, stuff: InSvg*) extends Shown {
    def this(origin: Vc, scale: Vc, thicken: Option[Float], stuff: InSvg*) = this(origin, scale, thicken, Style.empty, stuff: _*)
    def this(origin: Vc, scale: Vc, stuff: InSvg*) = this(origin, scale, None, Style.empty, stuff: _*)
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
      implicit val myMag = Magnification.one
      val yform = Xform.shiftscale(origin, scale).inverted andThen xform
      Indent.V(f"<g$show>") ++
      stuff.toVector.flatMap(_.inSvg(yform, thicken).map(_.in)) ++
      Indent.V("</g>")
    }
  }

  // This "one-liner" should work in the REPL after: import kse.maths._, kse.visual._, kse.coll._, chart._
  // { val ah = Option(LineArrow((math.Pi/180).toFloat*30, 3, 0.71f)); val c = Circ(100 vc 100, 20, Fill(Rgba(0, 0.8f, 0))); val b = Bar(200 vc 200, 10 vc 80, Fill(Rgba(1, 0.3f, 0.3f))); val dl = DataLine(Array(Vc(50, 300).underlying, Vc(90, 240).underlying, Vc(130, 280).underlying, Vc(170, 260).underlying), Stroke(Rgba(1, 0, 1), 4)); val dr = DataRange(Array(90, 130, 170, 210), Array(230, 220, 240, 210), Array(270, 310, 270, 260), Fill alpha Rgba(0, 0, 1, 0.3f)); val ea = ErrorBarYY(150, 95, 115, 7, 0, Stroke(Rgba(1, 0, 0), 2)); val eb = ErrorBarYY(150, 395, 445, 10, -0.5f, Stroke.alpha(Rgba(1, 0, 0, 0.5f), 10)); val aa = Arrow(50 vc 200, 200 vc 100, 0.1f, None, Stroke(Rgba(0.5f, 0, 1), 5)); val ab = Arrow(50 vc 225, 200 vc 125, 0, ah, Stroke.alpha(Rgba(0.5f, 0, 1, 0.5f), 10)); val pa = PolyArrow(Array(20 vc 400, 20 vc 20, 400 vc 20).map(_.underlying), ah, ah, Stroke(Rgba(0.7f, 0.7f, 0), 5)); val tk = Ticky(20 vc 20, 400 vc 20, Seq(0.2f, 0.4f, 0.6f, 0.8f), -20, 0, Stroke(Rgba(0.7f, 0.7f, 0), 2)); val qbf = Letters(200 vc 200, "Quick brown fox", Fill(Rgba.Black) ++ Font(40, Horizontal.Middle)); quick(c, b, dl, dr, ea, eb, aa, ab, pa, tk, qbf, Assembly(400 vc 100, 3 vc 3, Opacity(0.5f), c, b, dl, dr, ea, eb, aa, ab, pa, tk, qbf)) }
}
