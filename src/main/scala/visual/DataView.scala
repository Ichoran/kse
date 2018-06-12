// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015-2018 Rex Kerr, UCSF, and Calico Labs.

// This file uses glyphs (from Glyph.scala) and other graphical features
// to create a visual representation of entire data sets.

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

trait Composite[A] extends InSvg {
  def computed: A
  def translate(svg: A): InSvg
  final def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] =
    translate(computed).inSvg(xform, mag)(fm)
}

trait Derived[A <: InSvg] extends Composite[A] {
  final def translate(svg: A): InSvg = svg
}


final case class Lilypad(samples: Array[Long], summary: Vc, padR: Float, padStyle: Style, rootR: Float, rootStyle: Style)
extends Derived[Grouping] {
  lazy val computed = {
    val stemStyle = rootStyle.stroky
    val padEdgeOpacity = padStyle.elements.collectFirst{ case StrokeOpacity(o) if o > 0 && o < 1 => o }
    val stemOpacity = stemStyle.elements.collectFirst{ case StrokeOpacity(o) if o > 0 && o < 1 => o }
    val stemmed = stemStyle.elements.exists(_ match {
      case StrokeWidth(w) if w.finite && w > 0 => true
      case _ => false
    })
    val b = Vector.newBuilder[InSvg]
    samples.foreach{ sample =>
      val v = Vc from sample
      if (stemmed) {
        val delta = v - summary
        (padEdgeOpacity, stemOpacity) match {
          case (Some(peo), Some(so)) =>
            val hio = peo max so
            b += Grouping(
              Circ(v, padR, padStyle.filly),
              Grouping.faded(hio)(
                Circ(v, padR, padStyle.stroky + StrokeOpacity(peo/hio)),
                Line(summary, v, stemStyle + StrokeOpacity(so/hio))
              )
            )
          case _ =>
            b += Grouping(Circ(v, padR, padStyle), Line(summary, v, stemStyle))
        }

        if (delta.lenSq > padR.sq) {
          b += Line(summary, v, stemStyle)
        }        
      }
      else b += Circ(v, padR, padStyle)
    }
    if (rootR.finite && rootR > 0) b += Circ(summary, rootR, rootStyle.filly)
    Grouping(b.result)
  }
}
object Lilypad {
  def apply(samples: Array[Vc], summary: Vc, padR: Float, padStyle: Style, rootR: Float, rootStyle: Style) =
    new Lilypad(samples.map(_.underlying), summary, padR, padStyle, rootR, rootStyle)

  def many(sampleses: Array[Array[Long]], summarize: Array[Long] => Vc, padR: Float, padStyle: Style, rootR: Float, rootStyle: Style): Grouping = {
    val bundled = sampleses.map{ samples => apply(samples, summarize(samples), padR, padStyle, rootR, rootStyle) }
    if (rootR.finite && rootR > 0) {
      val leaves = bundled.map(g => Grouping(g.computed.stuff.dropRight(1)))
      val roots  = bundled.flatMap(_.computed.stuff.lastOption)
      Grouping(
        Grouping(leaves),
        Grouping(roots)
      )
    }
    else Grouping(bundled)
  }

  def datasets(summarize: Array[Long] => Vc, padR: Float, padStyle: Style, rootR: Float, rootStyle: Style)(
    data: (Array[Array[Long]], Rgba, Rgba, Rgba, Rgba)*
  ): Grouping = {
    val bundled = data.toVector.map{ case (aal, ipc, opc, sc, rc) =>
      many(aal, summarize, padR, padStyle ++ Stroke.alpha(opc) ++ Fill.alpha(ipc), rootR, rootStyle ++ Stroke.alpha(sc) ++ Fill.alpha(rc))
    }
    if (rootR.finite && rootR > 0) {
      val leaves = bundled.map(g => Grouping(g.stuff.dropRight(1)))
      val roots  = bundled.flatMap(_.stuff.lastOption)
      Grouping(
        Grouping(leaves),
        Grouping(roots)
      )
    }
    else Grouping(bundled)
  }

  def datasets(
    summarize: Array[Long] => Vc, padR: Float, padStyle: Style, rootR: Float, rootStyle: Style,
    inPadC: Rgba => Rgba, outPadC: Rgba => Rgba, stemC: Rgba => Rgba, rootC: Rgba => Rgba
  )(
    data: (Array[Array[Long]], Rgba)*
  ): Grouping =
    datasets(summarize, padR, padStyle, rootR, rootStyle)(data.map{ case (aal, c) => (aal, inPadC(c), outPadC(c), stemC(c), rootC(c)) }: _*)

  def dataVectored(summarize: Array[Float] => Float, padR: Float, padStyle: Style, rootR: Float, rootStyle: Style)(
    data: (Array[Array[Vc]], Rgba, Rgba, Rgba, Rgba)*
  ): Grouping = 
    datasets((al: Array[Long]) => Vc(summarize(al.map(v => Vc.from(v).x)), summarize(al.map(v => Vc.from(v).y))), padR, padStyle, rootR, rootStyle)(
      data.map(_._1Fn(_.map(_.map(_.underlying)))): _*
    )

  def dataVectored(
    summarize: Array[Float] => Float, padR: Float, padStyle: Style, rootR: Float, rootStyle: Style,
    inPadC: Rgba => Rgba, outPadC: Rgba => Rgba, stemC: Rgba => Rgba, rootC: Rgba => Rgba
  )(
    data: (Array[Array[Vc]], Rgba)*
  ): Grouping =
    dataVectored(summarize, padR, padStyle, rootR, rootStyle)(data.map{ case (aav, c) => (aav, inPadC(c), outPadC(c), stemC(c), rootC(c)) }: _*)
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


final case class HistInfo(total: Int, highest: Int, width: Float, avgbin: Float) {}
object HistInfo {
  val unitScale: HistInfo => Float = h => { if (h.highest == 0) 0 else (h.avgbin.toDouble/h.highest).toFloat }
  def sameAs(dh: DataHist): HistInfo => Float = {
    val lrg = dh.largestCount
    val wid = dh.borders.last - dh.borders.head
    val avgbin = if (dh.borders.length > 1) (wid.toDouble/(dh.borders.length - 1)).toFloat else wid
    val hpc = dh.heightPerCount
    (h: HistInfo) => if (avgbin > 0) ((hpc * h.avgbin.toDouble / avgbin)).toFloat else unitScale(h)
  }
  def leastOf(dhs: Seq[DataHist]): HistInfo => Float = {
    val lrgs = dhs.map(_.largestCount)
    val wids = dhs.map(dh => dh.borders.last - dh.borders.head)
    val avgbins = (dhs zip wids).map{ case (dh, wid) => if (dh.borders.length > 1) wid.toDouble/(dh.borders.length - 1) else wid }
    val hpcs = dhs.map(_.heightPerCount)
    val rats = (avgbins zip hpcs).map{ case (avgbin, hps) => if (avgbin > 0) hps / avgbin else 0 }
    val smallestRat = if (rats.length == 0) 0 else rats.min
    (h: HistInfo) => if (smallestRat > 0) (h.avgbin.toDouble * smallestRat).toFloat else unitScale(h)
  }
}

final case class DataHist(xs: Array[Float], scale: Option[Either[HistInfo => Float, Float]], range: Option[(Float, Float)], bins: Option[Int], style: Style) extends Shown {
  val viewedRange = range.getOrElse{
    var lo = Float.PositiveInfinity
    var hi = Float.NegativeInfinity
    var i = 0
    while (i < xs.length && !xs(i).finite) i += 1;
    if (i < xs.length) { lo = xs(i); hi = xs(i) }
    while (i < xs.length) {
      val xi = xs(i)
      if (xi.finite) {
        if (xi < lo) lo = xi
        else if (xi > hi) hi = xi
      }
      i += 1
    }
    if (lo < hi) (lo, hi) else (0f, 0f)
  }
  private def bestBinEstimate = math.max(5, math.pow(xs.length, 0.3).ceil.toInt)
  val borders: Array[Float] = bins.filter(_ > 1).orElse(if (!range.isEmpty) Some(bestBinEstimate) else None) match {
    case None =>
      val b = bestBinEstimate
      val tik = Tickify.select(viewedRange._1, viewedRange._2, b+1)
      val decs = tik.map(_._1.toBigDec)
      val ans = new Array[Float](decs.length + 1)
      var i = 0
      while (i < ans.length) {
        if (i == 0) ans(i) = (decs(0) + (decs(0) - decs(1))/2).toFloat
        else if (i == ans.length-1) ans(i) = (decs(decs.length-1) + (decs(decs.length-1) - decs(decs.length - 2)/2)).toFloat
        else ans(i) = (decs(i) + decs(i-1)).toFloat
        i += 1
      }
      ans
    case Some(b) => 
      val bigL = BigDecimal(viewedRange._1.toString)
      val bigR = BigDecimal(viewedRange._2.toString)
      val bigD = bigR - bigL
      val ans = new Array[Float](b+1)
      var i = 0
      while (i < ans.length) {
        ans(i) = (if (i <= b/2) bigL + (bigD*i)/b else bigR - (bigD*(b-i))/b).toFloat
        i += 1
      }
      ans
  }
  val binnedCounts = {
    val ans = new Array[Int](borders.length+1)
    var i = 0
    while (i < xs.length) {
      val xi = xs(i)
      if (xi.finite) {
        val ix = borders.bisect(xi)
        if (ix < 0) ans(0) += 1
        else if (ix >= ans.length) ans(ans.length-1) += 1
        else ans(ix.floor.toInt + 1) += 1
      }
      i += 1
    }
    ans
  }
  val (totalCount, largestCount) = {
    var total = 0
    var highest = 0
    var i = 0
    while (i < binnedCounts.length) {
      val ci = binnedCounts(i)
      total += ci
      if (ci > highest) highest = ci
      i += 1
    }
    (total, highest)
  }
  val heightPerCount = scale match {
    case None => 1f*(borders.last - borders.head)/math.max(1, borders.length-1)
    case Some(Right(f)) => f
    case Some(Left(fn)) =>
      val expanse = borders.last - borders.head
      fn(HistInfo(totalCount, largestCount, expanse, if (borders.length > 1) expanse/(borders.length - 1) else expanse))
  }
  val laidOutCenter = {
    var contiguousBlock: List[(Array[Float], Array[Float])] = Nil
    var i = binnedCounts.length - 2
    while (i > 0) {
      while (i > 0 && binnedCounts(i) == 0) i -= 1
      var j = i
      while (j > 0 && binnedCounts(j) != 0) j -= 1
      if (j < i) {
        val xs = new Array[Float](1+i-j)
        val ys = new Array[Float](i-j)
        var k = j
        while (k <= i) {
          xs(k - j) = borders(k)
          if (k > j) ys(k - j - 1) = binnedCounts(k) * heightPerCount / (borders(k) - borders(k-1))
          k += 1
        }
        contiguousBlock = ((xs, ys)) :: contiguousBlock
      }
      i = j
    }
    contiguousBlock.toArray
  }
  def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
    implicit val myMag = Magnification.one
    val paths = laidOutCenter.map{ case (xs, ys) =>
      val sb = new StringBuilder
      sb ++= "<path d=\""
      sb ++= "M "
      sb ++= fm(xform(xs(0) vc 0))
      var i = 0
      while (i < ys.length) {
        sb ++= " L"
        sb ++= fm(xform(xs(i) vc ys(i)))
        sb ++= " L"
        sb ++= fm(xform(xs(i+1) vc ys(i)))
        i += 1
      }
      sb ++= " L"
      sb ++= fm(xform(xs(i) vc 0))
      sb ++= f"$q$show/>"
      sb.result
    }
    if (paths.length == 1) Indent.V(paths.head)
    else Indent.V( (("<g>" +: paths) :+ "</g>"): _* )
  }
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


final case class Arch(origin: Vc, values: Array[Float], axes: Array[Long], colors: Option[Array[Rgba]], widths: Option[Array[Float]], style: Style)
extends Shown with Composite[Either[Grouping, DataLine]] {
  lazy val computed: Either[Grouping, DataLine] =
    if (colors.isEmpty && widths.isEmpty) {
      val data = Arches.projectSpan(origin, values, axes)
      Right(DataLine(data, style))
    }
    else {
      val rounded = style + StrokeJoin(Join.Round) + StrokeCap(Cap.Round)
      val c = colors.getOrElse(Array.empty[Rgba])
      val w = widths.getOrElse(Array.empty[Float])
      var commonOpacity = c.foldLeft(0.0f)((t, ci) => if (ci.a > 0) t max ci.a else t)
      val endpoints = Arches.projectSpan(origin, values, axes)
      val segments = Array.tabulate(0 max (endpoints.length - 1)){ ii => 
        val i = endpoints.length - 2 - ii
        var myStyle = rounded
        if (i < w.length && w(i).in(0, 1))
          myStyle = myStyle + StrokeWidth(w(i))
        if (i < c.length && c(i) != null)
          myStyle = myStyle + StrokeColor(if (commonOpacity > 0 && commonOpacity < 1) c(i).aFn(a => (a/commonOpacity) max 1f) else c(i))
        DataLine(Array(endpoints(i), endpoints(i+1)), myStyle)
      }
      Left(Grouping(segments, if (commonOpacity > 0 && commonOpacity < 1) commonOpacity else 1f))
    }
  def translate(svg: Either[Grouping, DataLine]): InSvg = computed match {
    case Right(dl) => dl
    case Left(g)   => g
  }
  override def styled = style.stroky.promoteStrokeOpacity
}
object Arch {
  import Arches.defaultAxes
  def apply(origin: Vc, values: Array[Float], axes: Array[Long], colors: Array[Rgba], style: Style) = new Arch(origin, values, axes, Some(colors), None, style)
  def apply(origin: Vc, values: Array[Float], axes: Array[Long], widths: Array[Float], style: Style) = new Arch(origin, values, axes, None, Some(widths), style)
  def apply(origin: Vc, values: Array[Float], axes: Array[Long], style: Style) = new Arch(origin, values, axes, None, None, style)
  def apply(origin: Vc, values: Array[Float], colors: Array[Rgba], widths: Array[Float], style: Style) = new Arch(origin, values, defaultAxes(values.length), Some(colors), Some(widths), style)
  def apply(origin: Vc, values: Array[Float], colors: Array[Rgba], style: Style) = new Arch(origin, values, defaultAxes(values.length), Some(colors), None, style)
  def apply(origin: Vc, values: Array[Float], widths: Array[Float], style: Style) = new Arch(origin, values, defaultAxes(values.length), None, Some(widths), style)
  def apply(origin: Vc, values: Array[Float], style: Style) = new Arch(origin, values, defaultAxes(values.length), None, None, style)  
}

final case class Arches(origin: Vc, data: Array[Array[Float]], axes: Array[Long], colors: Option[Array[Rgba]], widths: Option[Array[Float]], archStyle: Style, dot: Vc, dotStyle: Style)
extends Derived[Grouping] {
  lazy val computed: Grouping = {
    val origins = data.map(datum => Arches.travel(datum, axes) + origin)
    val hops = (data zip data.tail).map{ case (a, b) => (a zip b).map{ case (i, j) => j - i } }
    Grouping(
      Grouping(origins.map(o => Oval(o, dot, dotStyle))),
      Grouping((origins zip hops).map{ case (o, h) => Arch(o, h, axes, colors, widths, archStyle )})
    )
  }
}
object Arches {
  def defaultAxes(n: Int): Array[Long] = {
    val arcRadius = 0.5*math.Pi*(1 - 1.0/(n max 1))
    val a = new Array[Long](n max 0)
    var i = 0
    while (i < a.length) {
      val theta = arcRadius*(1 - 2*(i.toDouble/((n-1) max 1)))
      a(i) = Vc.angle(theta).underlying
      i += 1
    }
    a
  }
  def travel(values: Array[Float], axes: Array[Long], i0: Int = 0, iN: Int = Int.MaxValue): Vc = {
    var x, y = 0.0
    val n = values.length min axes.length min iN
    var i = i0.clip(0, n)
    while (i < n) {
      val u = Vc.from(axes(i))
      val v = values(i).toDouble
      x += v*u.x
      y += v*u.y
      i += 1
    }
    Vc.from(x, y)
  }
  def projectSpan(origin: Vc, values: Array[Float], axes: Array[Long]): Array[Long] = {
    var x, y = 0.0
    val n = values.length min axes.length
    var i = 0
    val pj = new Array[Long](n + 1)
    pj(0) = origin.underlying
    while (i < n) {
      val u = Vc.from(axes(i))
      val v = values(i).toDouble
      x += v*u.x
      y += v*u.y
      pj(i+1) = Vc.from(x + origin.x, y + origin.y).underlying
      i += 1
    }
    pj
  }
  def apply(origin: Vc, data: Array[Array[Float]], axes: Array[Long], colors: Array[Rgba], archStyle: Style, dot: Vc, dotStyle: Style) =
    new Arches(origin, data, axes, Some(colors), None, archStyle, dot, dotStyle)
  def apply(origin: Vc, data: Array[Array[Float]], axes: Array[Long], widths: Array[Float], archStyle: Style, dot: Vc, dotStyle: Style) = 
    new Arches(origin, data, axes, None, Some(widths), archStyle, dot, dotStyle)
  def apply(origin: Vc, data: Array[Array[Float]], axes: Array[Long], archStyle: Style, dot: Vc, dotStyle: Style) = 
    new Arches(origin, data, axes, None, None, archStyle, dot, dotStyle)
  def apply(origin: Vc, data: Array[Array[Float]], colors: Array[Rgba], widths: Array[Float], archStyle: Style, dot: Vc, dotStyle: Style) = 
    new Arches(origin, data, defaultAxes(data.headOption.map(_.length).getOrElse(0)), Some(colors), Some(widths), archStyle, dot, dotStyle)
  def apply(origin: Vc, data: Array[Array[Float]], colors: Array[Rgba], archStyle: Style, dot: Vc, dotStyle: Style) = 
    new Arches(origin, data, defaultAxes(data.headOption.map(_.length).getOrElse(0)), Some(colors), None, archStyle, dot, dotStyle)
  def apply(origin: Vc, data: Array[Array[Float]], widths: Array[Float], archStyle: Style, dot: Vc, dotStyle: Style) = 
    new Arches(origin, data, defaultAxes(data.headOption.map(_.length).getOrElse(0)), None, Some(widths), archStyle, dot, dotStyle)
  def apply(origin: Vc, data: Array[Array[Float]], archStyle: Style, dot: Vc, dotStyle: Style) = 
    new Arches(origin, data, defaultAxes(data.headOption.map(_.length).getOrElse(0)), None, None, archStyle, dot, dotStyle)

  def datasets(origin: Vc, axes: Array[Long], dot: Vc)(
    data: (Array[Array[Float]], Style, Style)*
  ): Grouping = Grouping(data.map{ case (aaf, as, ds) => apply(origin, aaf, axes, as, dot, ds) })

  def datasets(origin: Vc, axes: Array[Long], archStyle: Style, dotR: Float, dotStyle: Style, aC: Rgba => Rgba, dC: Rgba => Rgba)(
    data: (Array[Array[Float]], Rgba)*
  ): Grouping = Grouping(data.map{ case (aaf, c) => 
    apply(origin, aaf, axes, archStyle ++ Stroke.alpha(aC(c)), Vc(1, 0.5f)*dotR, dotStyle ++ Fill.alpha(dC(c)))
  })
}
