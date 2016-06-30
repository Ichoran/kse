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
  trait AppearanceFormatter { def fmt(a: Appearance): String }

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
    val theta = (math.Pi/2 - phi).toFloat
    val cosx = if (theta < phi) math.cos(theta).toFloat else math.cos(phi).toFloat
    val sinx = if (theta < phi) math.sin(theta).toFloat else math.sin(phi).toFloat
    val setback = 
      if (phi closeTo (math.Pi/4).toFloat) 0f
      else if (phi < theta) ((0.5*cosx)/sinx).toFloat
      else if (2*thickness <= cosx) length*cosx + thickness*sinx
      else length*cosx + thickness*sinx + (cosx/2 - thickness)/sinx
    val pointx =
      if (phi closeTo (math.Pi/4).toFloat) Float.NaN
      else if (phi < theta) thickness*(2*sinx)
      else length*cosx + thickness*sinx + (cosx - thickness)/(2*sinx)
    val barbx =
      if (phi closeTo (math.Pi/4).toFloat) thickness/2
      else if (phi < theta) length*cosx - thickness*sinx/2 + cosx/sinx
      else thickness*sinx/2
    val barby =
      if (phi closeTo (math.Pi/4).toFloat) 0.5f+length
      else 0.5f + length*sinx + thickness*cosx/2
    def stroked(tip: Vc, direction: Vc)(xform: Xform, appear: Appearance)(implicit nf: NumberFormatter, af: AppearanceFormatter): (Float, String) = {
      val qt = xform(tip)
      val deltadir = if (direction.lenSq < 0.1f*tip.lenSq) direction else direction*(1f/(50*math.max(1e-3f, tip.len.toFloat)))
      val dirx = (xform(tip + deltadir) - qt).hat
      val diry = dirx.ccw
      val w = appear.wide.value
      val ap = if (thickness closeTo 1) Plain else new Plainly { override def wide = appear.wide.map(_ * thickness) }
      val s = w * setback
      val px = w * pointx
      val bx = w * barbx
      val by = w * barby
      val qA = qt - dirx*bx + diry*by
      val qB = qt - dirx*bx - diry*by
      val qC = qt - dirx*px
      val ans = 
        if (phi closeTo (math.Pi/4).toFloat) f"<path d=${q}M ${nf space qA} L ${nf space qB}${q}${af fmt ap}/>"
        else f"<path d=${q}M ${nf space qA} L ${nf space qC} ${nf space qB}${q} stroke-linejoin=${q}miter${q} stroke-miterlimit=${q}10${q}${af fmt ap}/>"
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

  def quick(i: InSvg) {
    val svg = 
      Vector("<html>", "<body>", """<svg width="640" height="480">""").map(x => IndentedSvg(x)) ++
      i.inSvg(Xform.flipy(480))(defaultNumberFormatter, defaultAppearanceFormatter).map(x => x.copy(level = x.level+1)) ++
      Vector("</svg>", "</body>", "</html>").map(x => IndentedSvg(x))
    println(svg.mkString("\n"))
    svg.map(_.toString).toFile("test.html".file)
  }



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
