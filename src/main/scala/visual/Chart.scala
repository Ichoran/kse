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

  trait NumberFormatter {
    def fmt(x: Float): String
    def vmt(v: Vc, c: Char): String = fmt(v.x) + c.toString + fmt(v.y)
    def comma(v: Vc) = vmt(v, ',')
    def space(v: Vc) = vmt(v, ' ')
  }
  trait AppearanceFormatter {
    def fmt(a: Appearance): String
    def adjust(a: Appearance): Appearance = a
  }
  case class ZoomingFormatter(zoom: Float, original: AppearanceFormatter) extends AppearanceFormatter {
    def fmt(a: Appearance): String = original.fmt(adjust(a))
    override def adjust(a: Appearance): Appearance = if (a.wide.alive) ZoomAppear(zoom, Q(a)) else a
  }

  val defaultNumberFormatter = new NumberFormatter { 
    def fmt(x: Float) = if (x.toInt == x) x.toInt.toString else if (x*10 == (x*10).toInt) "%.1f".format(x) else "%.2f".format(x)
  }
  val emptyAppearanceFormatter = new AppearanceFormatter { def fmt(a: Appearance) = "" }
  val defaultAppearanceFormatter = new AppearanceFormatter {
    def fmt(a: Appearance) =
      a.face.get.map(f => f" font-family=$q$f$q").getOrElse("") +
      a.stroke.get.map(c => f" stroke=$q#${c.rgbText}$q").getOrElse("") +
      a.wide.get.filter(x => x.finite && x > 0).map(w => f" stroke-width=$q$w%.2f$q").getOrElse("") +
      a.fill.get.map(c => f" fill=$q#${c.rgbText}$q").getOrElse("") +
      a.opacity.get.filter(_ < 0.9995).map(o => f" opacity=$q$o%.3f$q").getOrElse("") +
      ( if (!a.opacity.alive && a.wide.alive && a.stroke.alive)
          a.stroke.value.a match { case x if x < 0.9995 => f" stroke-opacity=$q$x%.3f$x"; case _ => "" }
        else ""
      ) +
      ( if (!a.opacity.alive && a.fill.alive)
          a.fill.value.a match { case x if x < 0.9995 => f" fill-opacity=$q$x%.3f$x"; case _ => "" }
        else ""
      )
  }

  sealed trait AppearElement {}
  object FACE extends AppearElement {}
  object OPAC extends AppearElement {}
  object WIDE extends AppearElement {}
  object STRO extends AppearElement {}
  object FILL extends AppearElement {}

  sealed trait Appearance {
    def face: Q[String]
    def opacity: Q[Float]
    def wide: Q[Float]
    def stroke: Q[Rgba]
    def fill: Q[Rgba]
  }
  sealed trait ProxyAppear extends Appearance {
    def appear: Q[Appearance]
    def turnOff: Set[AppearElement] = Set()
    lazy val face: Q[String] = if (turnOff(FACE)) appear.flatMap(_.face.dead) else appear.flatMap(_.face)
    lazy val opacity: Q[Float] =  if (turnOff(OPAC)) appear.flatMap(_.opacity.dead) else appear.flatMap(_.opacity)
    lazy val wide: Q[Float] = if (turnOff(WIDE)) appear.flatMap(_.wide.dead) else appear.flatMap(_.wide)
    lazy val stroke: Q[Rgba] = if (turnOff(STRO)) appear.flatMap(_.stroke.dead) else appear.flatMap(_.stroke)
    lazy val fill: Q[Rgba] = if (turnOff(FILL)) appear.flatMap(_.fill.dead) else appear.flatMap(_.fill)
  }
  final class AppearanceOf(that: Appearance) extends Appearance {
    val face = that.face.map(identity)
    val opacity = that.opacity.map(identity)
    val wide = that.wide.map(identity)
    val stroke = that.stroke.map(identity)
    val fill = that.fill.map(identity)
  }
  final class ZoomAppear(val zoom: Float, val appear: Q[Appearance]) extends ProxyAppear {
    override def turnOff: Set[AppearElement] = appear match {
      case pa: ProxyAppear => pa.turnOff
      case _ => super.turnOff
    }
    override lazy val wide: Q[Float] = appear.flatMap(_.wide.map(_ * zoom))
  }
  object ZoomAppear {
    def apply(zoom: Float, appear: Appearance) = new ZoomAppear(zoom, Q(appear))
    def apply(zoom: Float, appear: Q[Appearance]) = new ZoomAppear(zoom, appear)
  }
  object Plain extends Appearance {
    val face = Q empty "Tahoma, Geneva, sans-serif"
    val opacity = Q empty 1f
    val wide = Q empty 0f
    val color = Q empty Rgba(0, 0, 0, 1)
    def fill = color
    def stroke = color
  }
  abstract class Plainly extends Appearance {
    def face = Plain.face
    def opacity = Plain.opacity
    def wide = Plain.wide
    def fill = Plain.fill
    def stroke = Plain.stroke
  }
  final class Filled(theColor: Rgba) extends Appearance {
    val face = Plain.face
    val opacity = Q(theColor.a match { case x if x.finite && x >= 0 && x < 1 => x; case _ => 1f })
    val wide = Plain.wide
    val stroke = Plain.stroke
    val fill = Q(theColor)
  }
  object Filled {
    def apply(theColor: Rgba) = new Filled(theColor)
  }
  final class Stroked(theWidth: Float, theColor: Rgba) extends Appearance {
    val face = Plain.face
    val opacity = Q(theColor.a match { case x if x.finite && x >= 0 && x < 1 => x; case _ => 1f })
    val wide = Q(theWidth)
    val stroke = Q(theColor)
    val fill = Plain.fill
  }
  object Stroked {
    def apply(theWidth: Float, theColor: Rgba) = new Stroked(theWidth, theColor)
  }
  final class Translucent(theOpacity: Float) extends Appearance {
    val face = Plain.face
    val opacity = Q(theOpacity match { case x if x.finite && x >= 0 && x < 1 => x; case _ => 1f })
    val wide = Plain.wide
    val stroke = Plain.color
    val fill = Plain.color
  }
  object Translucent {
    def apply(theOpacity: Float) = new Translucent(theOpacity)
  }
  final class Ornate(font: Option[String] = None, o: Option[Float] = None, w: Option[Float] = None, s: Option[Rgba] = None, f: Option[Rgba] = None)
  extends Appearance {
    val face = font match { case None => Plain.face; case Some(x) => Q(x) }
    val opacity = o match { case None => Plain.opacity; case Some(x) => Q(x) }
    val wide = w match { case None => Plain.wide; case Some(x) => Q(x) }
    val stroke = s match { case None => Plain.stroke; case Some(x) => Q(x) }
    val fill = f match { case None => Plain.fill; case Some(x) => Q(x) }
  }
  object Ornate {
    def apply(font: String = "", o: Float = Float.NaN, w: Float = Float.NaN, s: Rgba = Rgba(0,0,0,0), f: Rgba = Rgba(0,0,0,0)) =
      new Ornate(
        if (font.isEmpty) None else Some(font),
        if (o.finite) Some(o) else None,
        if (w.finite) Some(w) else None,
        if (s.a > 0 && s.a.finite) Some(s) else None,
        if (f.a > 0 && f.a.finite) Some(f) else None
      )
  }

  final case class IndentedSvg(text: String, level: Int = 0) {
    override def toString = if (level <= 0) text else " "*(2*level) + text
  }
  trait InSvg { def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] }

  final case class Circ(c: Q[Vc], r: Q[Float], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    override def turnOff = Set(FACE)
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vc = c.value
      val vr = r.value
      if (!vr.finite || vr == 0 || !vc.finite) return Vector.empty
      val circ = kse.visual.Circle(vc, vr) into xform
      if (!circ.radius.finite || circ.radius == 0 || !circ.center.finite) return Vector.empty
      Vector(IndentedSvg(
        f"<circle cx=$q${nf fmt circ.center.x}$q cy=$q${nf fmt circ.center.y}$q r=$q${nf fmt circ.radius}$q${af fmt this}/>"
      ))
    }
  }

  final case class Bar(c: Q[Vc], r: Q[Vc], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    override def turnOff = Set(FACE)
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
          f"<rect x=$q${nf fmt x}$q y=$q${nf fmt y}$q width=$q${nf fmt 2*rw}$q height=$q${nf fmt 2*rh}$q${af fmt this}/>"
        }
        else {
          f"<polygon points=$q${rect.corners.map{ l => val v = Vc from l; (nf fmt v.x) + "," + (nf fmt v.y)}.mkString(" ")}$q${af fmt this}/>"
        }
      }))
    }
  }

  final case class DataLine(pts: Q[Array[Long]], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    override def turnOff = Set(FACE, FILL)
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vps = pts.value
      val v = new Array[Long](vps.length)
      var i = 0;
      while (i < v.length) { v(i) = xform(Vc from vps(i)).underlying; i += 1 }
      val sb = new StringBuilder
      sb ++= "<path d=\""
      i = 0;
      while (i < math.min(v.length,1)) { sb ++= "M "; val vi = Vc from v(i); sb ++= nf fmt vi.x; sb += ' '; sb ++= nf fmt vi.y; i += 1 }
      while (i < math.min(v.length,2)) { sb ++= " L "; val vi = Vc from v(i); sb ++= nf fmt vi.x; sb += ' '; sb ++= nf fmt vi.y; i += 1 }
      while (i < v.length) { sb += ' '; val vi = Vc from v(i); sb ++= nf fmt vi.x; sb += ' '; sb ++= nf fmt vi.y; i += 1 }
      sb ++= "\" stroke-linejoin=\"round\""
      sb ++= f"${af fmt this} fill=${q}none${q}/>"
      Vector(IndentedSvg(sb.result))
    }
  }

  final case class DataRange(xs: Q[Array[Float]], los: Q[Array[Float]], his: Q[Array[Float]], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    override def turnOff = Set(FACE, WIDE, STRO)
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vxa = xs.value.clone
      val vxb = vxa.clone
      val vh = his.value.clone
      val vl = los.value.clone
      val L = math.min(vxa.length, math.min(vh.length, vl.length))
      var i = 0
      while (i < L) {
        val l = xform(Vc(vxa(i), vl(i)))
        val h = xform(Vc(vxb(i), vh(i)))
        vxa(i) = l.x
        vl(i) = l.y
        vxb(i) = h.x
        vh(i) = h.y
        i += 1
      }
      val sb = new StringBuilder
      sb ++= "<path d=\""
      i = 0
      while (i < math.min(1,L)) { sb ++= "M "; sb ++= nf fmt vxa(i); sb += ' '; sb ++= nf fmt vl(i); i += 1 }
      while (i < math.min(2,L)) { sb ++= " L "; sb ++= nf fmt vxa(i); sb += ' '; sb ++= nf fmt vl(i); i += 1 }
      while (i < L) { sb += ' '; sb ++= nf fmt vxa(i); sb += ' '; sb ++= nf fmt vl(i); i += 1 }
      while (i > math.max(L,1)) { i -= 1; sb += ' '; sb ++= nf fmt vxb(i); sb += ' '; sb ++= nf fmt vh(i) }
      while (i > 0) { i -= 1; if (vxa.length == 1) sb ++= " L " else sb += ' '; sb ++= nf fmt vxb(i); sb += ' '; sb ++= nf fmt vh(i) }
      if (L > 0) sb ++= " Z"
      sb ++= f"$q${af fmt this}/>"
      Vector(IndentedSvg(sb.result))
    }
  }

  final case class ErrorBarYY(x: Q[Float], lo: Q[Float], hi: Q[Float], across: Q[Float], bias: Q[Float], appear: Q[Appearance])
  extends ProxyAppear with InSvg { self =>
    override def turnOff = Set(FACE, FILL)
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vx = x.value
      val va = across.value
      val vb = bias.value.clip(0f, 1f)
      val rl = Vc(vx, lo.value)
      val rh = Vc(vx, hi.value)
      val cll = Vc(vx - va/2, rl.y)
      val clr = Vc(vx + va/2, rl.y)
      val chl = Vc(vx - va/2, rh.y)
      val chr = Vc(vx + va/2, rh.y)
      if (vb == 0) Vector(IndentedSvg(
        f"<path d=${q}M ${nf space rl} L ${nf space rh}$q fill=${q}none${q}${af fmt this}/>"
      ))
      else if (vb == 1) Vector(IndentedSvg(
        f"<path d=${q}M ${nf space cll} L ${nf space clr} M ${nf space chl} L ${nf space chr}$q fill=${q}none${q}${af fmt this}/>"
      ))
      else if (vb closeTo 0.5f) Vector(IndentedSvg(
        f"<path d=${q}M ${nf space rl} L ${nf space rh}$q M ${nf space cll} L ${nf space clr} M ${nf space chl} L ${nf space chr}$q fill=${q}none${q}${af fmt this}/>"
      ))
      else {
        val smaller = 2*math.min(vb, 1-vb);
        val little = new ProxyAppear{ def appear = Q(self); override lazy val wide = self.wide.map(_ * smaller); override lazy val opacity = Plain.opacity; override lazy val stroke = self.stroke.map((r: Rgba) => r.aTo(1)) }
        val big = new ProxyAppear { def appear = Q(self); override lazy val opacity = Plain.opacity; override lazy val stroke = self.stroke.map((r: Rgba) => r.aTo(1)) }
        val ver = if (vb < 0.5) little else big
        val hor = if (vb < 0.5) big else little
        val opq = if (stroke.alive && stroke.value.a < 1) stroke.value.a else if (opacity.alive && opacity.value < 1) opacity.value else 1f
        Vector(
          IndentedSvg(if (opq < 1) f"<g opacity=$q$opq%.3f$q>" else "<g>"),
          IndentedSvg(f"<path d=${q}M ${nf space rl} L ${nf space rh}$q fill=${q}none${q}${af fmt ver}/>", 1),
          IndentedSvg(f"<path d=${q}M ${nf space cll} L ${nf space clr} M ${nf space chl} L ${nf space chr}$q fill=${q}none${q}${af fmt hor}/>",1),
          IndentedSvg("</g>")
        )
      }
    }
  }

  trait Arrowhead {
    def setback: Float
    def stroked(tip: Vc, direction: Vc)(xform: Xform, appear: Appearance)(implicit nf: NumberFormatter, af: AppearanceFormatter): (Float, String)
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
    def stroked(tip: Vc, direction: Vc)(xform: Xform, appear: Appearance)(implicit nf: NumberFormatter, af: AppearanceFormatter): (Float, String) = {
      val qt = xform(tip)
      val deltadir = if (direction.lenSq < 0.1f*tip.lenSq) direction else direction*(1f/(50*math.max(1e-3f, tip.len.toFloat)))
      val dirx = (xform(tip + deltadir) - qt).hat
      val diry = dirx.ccw
      val w = (af adjust appear).wide.value
      val ap = if (thickness closeTo 1) Plain else new Plainly { override def wide = Q.eval((af adjust appear).wide.value*thickness) }
      val s = w * setback
      val px = w * pointx
      val bx = w * barbx
      val by = w * barby
      val qA = qt - dirx*bx + diry*by
      val qB = qt - dirx*bx - diry*by
      val qC = qt - dirx*px
      val miterfix = if (3.999*sinx < 1) " stroke-miterlimit=\"%d\"".format(math.ceil(1/sinx+1e-3).toInt) else ""
      val ans = 
        if (flat) f"<path d=${q}M ${nf space qA} L ${nf space qB}${q}${af fmt ap}/>"
        else f"<path d=${q}M ${nf space qA} L ${nf space qC} ${nf space qB}${q} stroke-linejoin=${q}miter${q}$miterfix${af fmt ap}/>"
      (s, ans)
    }
  }


  final case class GoTo(from: Q[Vc], to: Q[Vc], indirection: Q[Float], arrow: Q[Arrowhead], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    override def turnOff = Set(FACE, FILL)
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vf = from.value
      val vt = to.value
      val vi = indirection.value
      val v = vt - vf;
      val ip = vf + v*0.5f - v.ccw*(2f*vi)
      val uf = xform(vf)
      val ut = xform(vt)
      val iq = xform(ip)
      if (arrow.alive) {
        val ar = arrow.value
        val (setback, arrowline) = ar.stroked(vt, (vt - ip).hat)(xform, this)(nf, af)
        val wt = ut - setback*(ut - iq).hat
        val mainline =
          if (indirection.alive)
            f"<path d=${q}M ${nf space uf} Q ${nf space iq} ${nf space wt}${q} fill=${q}none${q}/>"      
          else
            f"<path d=${q}M ${nf space uf} L ${nf space wt}${q} fill=${q}none${q}/>"
        Vector(
          IndentedSvg(f"<g fill=${q}none${q} ${af fmt this}>"),
          IndentedSvg(mainline, 1),
          IndentedSvg(arrowline, 1),
          IndentedSvg("</g>")
        )
      }
      else Vector(IndentedSvg(
        if (indirection.alive)
          f"<path d=${q}M ${nf space uf} Q ${nf space iq} ${nf space ut}${q} fill=${q}none${q}${af fmt this}/>"      
        else
          f"<path d=${q}M ${nf space uf} L ${nf space ut}${q} fill=${q}none${q}${af fmt this}/>"
      ))
    }
  }

  final case class PolyGo(points: Q[Array[Long]], fwdarrow: Q[Arrowhead], bkwarrow: Q[Arrowhead], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    override def turnOff = Set(FACE, FILL)
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val vp = points.value
      if (vp.length < 2) return Vector.empty
      val up = {
        val ans = new Array[Long](vp.length)
        var i = 0
        while (i < vp.length) { ans(i) = xform(Vc from vp(i)).underlying; i += 1 }
        ans
      }
      val fwd = (Vc.from(vp(vp.length-1)) - Vc.from(vp(vp.length-2))).hat
      val bkw = (Vc.from(vp(0)) - Vc.from(vp(1))).hat
      var arrows = List.empty[String]
      if (bkwarrow.alive) {
        val ar = bkwarrow.value
        val (setback, arrowline) = ar.stroked(Vc from vp(0), bkw)(xform, this)(nf, af)
        up(0) = (Vc.from(up(0)) - setback*(Vc.from(up(0)) - Vc.from(up(1))).hat).underlying
        arrows = arrowline :: arrows
      }
      if (fwdarrow.alive) {
        val ar = fwdarrow.value
        val (setback, arrowline) = ar.stroked(Vc from vp(vp.length-1), fwd)(xform, this)(nf, af)
        up(up.length-1) = (Vc.from(up(up.length-1)) - setback*(Vc.from(up(up.length-1)) - Vc.from(up(up.length-2))).hat).underlying
        arrows = arrowline :: arrows
      }
      val line = f"d=${q}M ${nf space Vc.from(up(0))} L ${up.drop(1).map(l => nf space Vc.from(l)).mkString(" ")}${q}"

      Vector(
        IndentedSvg(f"<g fill=${q}none${q} ${af fmt this}>"),
        IndentedSvg(f"<path fill=${q}none${q} $line/>", 1)
      ) ++
      arrows.map(s => IndentedSvg(s, 1)) ++
      Vector(IndentedSvg("</g>"))
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
          f"<text x=$q${nf fmt vl.x}$q y=$q${nf fmt vl.y}$q font-size=$q${nf fmt size}$q text-anchor=${"\"middle\""}${af fmt this}>${text.value}</text>"
        ))        
      }
      else ???
    }
  }

  final case class Tick(x: Float, yL: Float, yH: Float) {}
  final case class TickMarks(origin: Q[Vc], axis: Q[Vc], left: Q[Float], right: Q[Float], values: Q[Array[Float]], appear: Q[Appearance])
  extends ProxyAppear with InSvg {
    override def turnOff = Set(FILL)
    def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter): Vector[IndentedSvg] = {
      val va = axis.value.hat
      val vb = va.ccw
      val w = (af adjust appear.value).wide.value
      val vl = vb*(left.value*w)
      val vr = vb*(right.value*w)
      val vo = origin.value
      val pts = values.value.map{ x =>
        val vc = vo + x*va
        val l = xform(vc + vl)
        val r = xform(vc + vr)
        f"M ${nf space l} L ${nf space r}"
      }
      Vector(IndentedSvg(f"<path d=${q}${pts.mkString(" ")}${q} fill=${q}none${q}${af fmt this}/>"))
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
    def apply(elements: InSvg*) = new MuGroup(elements.toVector)
  }

  final class Origin(origin: Q[Vc], scaling: Q[Vc], theElements: Vector[InSvg]) extends MuGroup(theElements) {
    private[this] def undo: Xform = Xform.shiftscale(origin.value, scaling.value).inverted
    override def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter) =
      super.inSvg(undo andThen xform)(nf, af)
  }
  object Origin {
    def apply(origin: Q[Vc], scaling: Q[Vc], elements: InSvg*) = new Origin(origin, scaling, elements.toVector)
  }

  final class ZoomLines(scaling: Q[Float], theElements: Vector[InSvg]) extends MuGroup(theElements) {
    override def inSvg(xform: Xform)(implicit nf: NumberFormatter, af: AppearanceFormatter) =
      super.inSvg(xform)(nf, ZoomingFormatter(scaling.value, af))
  }
  object ZoomLines {
    def apply(scaling: Q[Float], elements: InSvg*) = new ZoomLines(scaling, elements.toVector)
    def apply(scaling: Float, elements: InSvg*) = new ZoomLines(Q(scaling), elements.toVector)
  }

  def quick(i: InSvg) {
    val svg = 
      Vector("<html>", "<body>", """<svg width="640" height="480">""").map(x => IndentedSvg(x)) ++
      i.inSvg(Xform.flipy(480))(defaultNumberFormatter, defaultAppearanceFormatter).map(x => x.copy(level = x.level+1)) ++
      Vector("</svg>", "</body>", "</html>").map(x => IndentedSvg(x))
    println(svg.mkString("\n"))
    svg.map(_.toString).toFile("test.html".file)
  }
}
