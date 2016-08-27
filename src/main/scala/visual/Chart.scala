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
    Vector("""<svg width="%.2f" height="%.2f">""".format(size.x, size.y), "<g>").map(x => Indent(x)) ++
    stuff.flatMap(_.inSvg(Xform.flipy(480), None)(DefaultFormatter)).map(x => x.in) ++
    Vector("</g>", "</svg>").map(x => Indent(x))

  def svgHtml(size: Vc, stuff: InSvg*): Vector[String] = (
    Vector(Indent("<html>"), Indent("<body>")) ++
    svg(size, stuff: _*).map(x => x.in) ++
    Vector(Indent("</body>"), Indent("</html>"))
  ).map(_.toString)

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

  final case class Dynamic[S <: Shown, T <: Shown](static: S, react: (S, Xform, Option[Float], Formatter) => T) extends Shown {
    def style = static.style
    def inSvg(xform:Xform, mag: Option[Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] =
      react(static, xform, mag, fm).inSvg(xform, mag)(fm)
  }
  object Dynamic {
    def apply[S <: Shown, T <: Shown](static: S)(react: (S, Xform, Option[Float], Formatter) => T)(implicit ev: S <:< T) = new Dynamic(static, react)
  }

  object Marker {
    final case class C(c: Vc, r: Float, style: Style) extends Shown {
      def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
        val ctr = xform(c)
        implicit val myMag = Magnification.from(mag, r, xform.radius(c, Vc(r, 0)))
        Indent.V(f"<circle${fm.vquote(ctr, "cx", "cy")}${fm("r", r * myMag.value)}$show/>")
      }
    }
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


  final case class Spread(c: Vc, axis: Vc, major: Deviable, minor: Deviable, dense: Vc, p: Float, style: Style) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter = DefaultFormatter): Vector[Indent] = {
      val sigmas = 
        if (p < 1e-6) NumericConstants.SqrtTwo * 1e-3
        else if (p > 1-1e-6) 5.2565217697569319786   // Value from Wolfram Alpha (20 digits)
        else (-2*math.log(1-p)).sqrt
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
      val etheta = math.atan2(uM.y, uM.x) * 180 / math.Pi
      implicit val myMag = Magnification.from(mag, xform, c)
      if (uM.y closeTo 0)
        Indent.V(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(er, "rx", "ry")}${showWith(_.fade(fader))}/>")
      else if (uM.x closeTo 0)
        Indent.V(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(Vc(er.y, er.x), "rx", "ry")}${showWith(_.fade(fader))}/>")
      else {
        val rotation = f" transform=${q}rotate(${fm(etheta.toFloat)} ${fm comma uc})${q}"
        Indent.V(f"<ellipse${fm.vquote(uc, "cx", "cy")}${fm.vquote(er, "rx", "ry")}$rotation${showWith(_.fade(fader))}/>")
      }
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
      val tkl = e.ccw * (left * myMag.value)
      val tkr = e.ccw * (right * myMag.value)
      val strokes = ticks.map{ case Tik(l,_) => 
        val p = uf + e*(ud*l.clip(0,1))
        " M " + fm(p + tkl) + " L " + fm(p + tkr)
      }
      val xish = e.x.abs >= NumericConstants.OverSqrtTwo
      val textalign =
        if (xish) Font(Horizontal.Middle, if (e.x >= 0 == anchor >= 0) Vertical.Top else Vertical.Bottom)
        else      Font(if (e.y > 0 == anchor > 0) Horizontal.Left else Horizontal.Right, Vertical.Middle)
      val jump = e.ccw * (myMag.value * (if (xish == anchor >= 0) right + anchor else left - anchor))
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


    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = theTicks.inSvg(xform, mag)(fm)
  }


  final case class Letters(anchor: Vc, text: String, rotate: Float, style: Style) extends Shown {
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
      implicit val myMag = Magnification.from(mag, xform, anchor)
      val u = xform(anchor)
      val rot = (rotate*180/math.Pi).toFloat
      Indent.V(
        if (rotate closeTo 0)
          f"<text${fm.vquote(u,"x","y")}$show>$text</text>"
        else 
          f"<text${fm.vquote(u,"x","y")} transform=${q}rotate(${fm(rot)} ${fm comma u})${q}$show>$text</text>"
      )
    }
  }

  final case class Assembly(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float], style: Style, stuff: Seq[InSvg]) extends Shown {
    def this(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float], style: Style, thing: InSvg, morestuff: InSvg*) =
      this(oldOrigin, scale, newOrigin, thicken, style, thing +: morestuff)
    def this(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float], stuff: InSvg*) = 
      this(oldOrigin, scale, newOrigin, thicken, Style.empty, stuff)
    def this(oldOrigin: Vc, scale: Vc, newOrigin: Vc, stuff: InSvg*) =
      this(oldOrigin, scale, newOrigin, None, Style.empty, stuff)
    def this(translate: Vc, stuff: InSvg*) =
      this(translate, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
    def this(stuff: InSvg*) =
      this(0 vc 0, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = {
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
    def apply(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float], style: Style, thing: InSvg, morestuff: InSvg*) =
      new Assembly(oldOrigin, scale, newOrigin, thicken, style, thing +: morestuff)
    def apply(oldOrigin: Vc, scale: Vc, newOrigin: Vc, thicken: Option[Float], stuff: InSvg*) =
      new Assembly(oldOrigin, scale, newOrigin, thicken, Style.empty, stuff)
    def apply(oldOrigin: Vc, scale: Vc, newOrigin: Vc, stuff: InSvg*) = 
      new Assembly(oldOrigin, scale, newOrigin, None, Style.empty, stuff)
    def apply(translate: Vc, stuff: InSvg*) =
      new Assembly(translate, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
    def apply(stuff: InSvg*) =
      new Assembly(0 vc 0, 1 vc 1, 0 vc 0, None, Style.empty, stuff)
  }

  final case class Space(dataOrigin: Vc, dataExtent: Vc, viewOrigin: Vc, viewExtent: Vc, ticknum: Int, ticklen: Float, arrow: Option[Arrowhead], linestyle: Style, stuff: Seq[InSvg]) extends Shown {
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
        case StrokeWidth(w, false) =>
          line.copy(points = line.points.map(l => (xform.revert(xform(Vc from l) - Vc(w/2, -w/2)*m.value)).underlying))
      }.getOrElse(line)
    }


    lazy val tickstyle = linestyle.map{ case StrokeWidth(w, off) => StrokeWidth(w*0.71f, off); case x => x }

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

    lazy val lineAssembly = dataAssembly.copy(thicken = Some(1f), stuff = Vector(axisLine, xTicks, yTicks))

    lazy val fullAssembly = Assembly(dataAssembly, lineAssembly)

    def style = linestyle

    def inSvg(xform: Xform, mag: Option[Float])(implicit fm: Formatter): Vector[Indent] = fullAssembly.inSvg(xform, mag)(fm)
  }

  // This "one-liner" should work in the REPL after: import kse.flow._, kse.coll._, kse.maths._, kse.maths.stats._, kse.jsonal._, JsonConverters._, kse.eio._, kse.visual._, kse.visual.chart._
  // { val ah = Option(LineArrow((math.Pi/180).toFloat*30, 3, 0.71f)); val c = Circ(100 vc 100, 20, Fill(Rgba(0, 0.8f, 0))); val b = Bar(200 vc 200, 10 vc 80, Fill(Rgba(1, 0.3f, 0.3f))); val dl = DataLine(Array(Vc(50, 300).underlying, Vc(90, 240).underlying, Vc(130, 280).underlying, Vc(170, 260).underlying), Stroke(Rgba(1, 0, 1), 4)); val dr = DataRange(Array(90, 130, 170, 210), Array(230, 220, 240, 210), Array(270, 310, 270, 260), Fill alpha Rgba(0, 0, 1, 0.3f)); val ea = ErrorBarYY(150, 95, 115, 7, 0, Stroke(Rgba(1, 0, 0), 2)); val eb = ErrorBarYY(150, 395, 445, 10, -0.5f, Stroke.alpha(Rgba(1, 0, 0, 0.5f), 10)); val aa = Arrow(50 vc 200, 200 vc 100, 0.1f, None, Stroke(Rgba(0.5f, 0, 1), 5)); val ab = Arrow(50 vc 225, 200 vc 125, 0, ah, Stroke.alpha(Rgba(0.5f, 0, 1, 0.5f), 10)); val pa = PolyArrow(Array(20 vc 400, 20 vc 20, 400 vc 20).map(_.underlying), ah, ah, Stroke(Rgba(0.7f, 0.7f, 0), 5)); val tk = Ticky(20 vc 20, 400 vc 20, Seq(0.2f, 0.4f, 0.6f, 0.8f), -20, 0, Stroke(Rgba(0.7f, 0.7f, 0), 2)); val qbf = Letters(200 vc 200, "Quick brown fox", (10*math.Pi/180).toFloat, Fill(Rgba.Black) ++ Font(40, Horizontal.Middle)); val tl = TickLabels(Vc(100,100), Vc(200,100), Seq(Tik(0, "0"), Tik(0.4f, "40"), Tik(0.8f, "80")), 0, 20, 5, Font(18) ++ Stroke(Rgba(0f, 0.8f, 0.8f), 4) ++ Fill(Rgba(0f, 0.4f, 0.4f))); val at = AutoTick(Vc(0.2f, 100), Vc(1.26f, 100), 5, 0, 20, 5, Font(18) ++ Stroke(Rgba(0f, 0.6f, 1f), 4) ++ Fill(Rgba(0f, 0.2f, 0.5f))); val gr = Space(0 vc 0, 200 vc 200, 400 vc 100, 100 vc 100, 4, 8, ah, Stroke(Rgba(1f, 0, 0), 6), Seq(c, Marker.C(50 vc 100, 8, Stroke(Rgba.Black, 2) + FillNone()), Marker.C(75 vc 125, 8, Fill(Rgba(0, 0, 1))))); quick(c, b, dl, dr, ea, eb, aa, ab, pa, tk, qbf, tl, Assembly(0 vc 100, 400f vc 1f, 0 vc 200, Option(1f), Opacity(1f), at, at.copy(to = Vc(1.33f, 100))), tl.copy(to = 100 vc 200, left = -20, right = 0), Assembly(0 vc 0, 0.3333f vc 0.3333f, 400 vc 200, Option((1/3.sqrt).toFloat), Opacity(0.5f), c, pa), gr) }
}
