// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015-2018 Rex Kerr, UCSF, and Calico Labs.

// This contains the organizational parts of charts--axes, transformations, and so on.
// For the elements to put inside, look in Glyph.scala

package kse.visual

import scala.math._
import scala.util._
import scala.collection.mutable.{ AnyRefMap => RMap }

import kse.coll._
import kse.coll.packed._
import kse.maths._
import kse.maths.stats._
import kse.flow._
import kse.eio._

package object chart {
  /** Produce SVG with a user-specified conversion factor between mm and pixels */
  def svgMm(sizeInMm: Vc, pixelsPerMm: Float, stuff: InSvg*): Vector[Indent] =
    Vector(
      """<svg xmlns="http://www.w3.org/2000/svg" width="%.2fmm" height="%.2fmm" viewBox="0 0 %.2f %.2f">""".
        format(sizeInMm.x, sizeInMm.y, sizeInMm.x*pixelsPerMm, sizeInMm.y*pixelsPerMm),
      "<g>"
    ).map(x => Indent(x)) ++
    stuff.flatMap(_.inSvg(Xform.flipy(sizeInMm.y*pixelsPerMm), None)(DefaultFormatter)).map(x => x.in) ++
    Vector("</g>", "</svg>").map(x => Indent(x))

  def svgMmInvertedY(sizeInMm: Vc, pixelsPerMm: Float, stuff: InSvg*): Vector[Indent] =
    Vector(
      """<svg xmlns="http://www.w3.org/2000/svg" width="%.2fmm" height="%.2fmm" viewBox="0 0 %.2f %.2f">""".
        format(sizeInMm.x, sizeInMm.y, sizeInMm.x*pixelsPerMm, sizeInMm.y*pixelsPerMm),
      "<g>"
    ).map(x => Indent(x)) ++
    stuff.flatMap(_.inSvg(Xform.identity, None)(DefaultFormatter)).map(x => x.in) ++
    Vector("</g>", "</svg>").map(x => Indent(x))

  def svg(size: Vc, stuff: InSvg*): Vector[Indent] =
    Vector("""<svg xmlns="http://www.w3.org/2000/svg" width="%.2f" height="%.2f">""".format(size.x, size.y), "<g>").map(x => Indent(x)) ++
    stuff.flatMap(_.inSvg(Xform.flipy(size.y), None)(DefaultFormatter)).map(x => x.in) ++
    Vector("</g>", "</svg>").map(x => Indent(x))

  def svgInvertedY(size: Vc, stuff: InSvg*): Vector[Indent] =
    Vector("""<svg xmlns="http://www.w3.org/2000/svg" width="%.2f" height="%.2f">""".format(size.x, size.y), "<g>").map(x => Indent(x)) ++
    stuff.flatMap(_.inSvg(Xform.identity, None)(DefaultFormatter)).map(x => x.in) ++
    Vector("</g>", "</svg>").map(x => Indent(x))

  def svgHtml(size: Vc, bg: Rgba, stuff: InSvg*): Vector[String] = (
    Vector(Indent("<html>"), Indent(f"<body${if (!(bg.a closeTo 0)) f" bgcolor=$q${DefaultFormatter(bg)}$q" else ""}>")) ++
    svg(size, stuff: _*).map(x => x.in) ++
    Vector(Indent("</body>"), Indent("</html>"))
  ).map(_.toString)

  def svgHtml(size: Vc, stuff: InSvg*): Vector[String] = svgHtml(size, Rgba.Empty, stuff: _*)

  def svgHtmlInvertedY(size: Vc, bg: Rgba, stuff: InSvg*): Vector[String] = (
    Vector(Indent("<html>"), Indent(f"<body${if (!(bg.a closeTo 0)) f" bgcolor=$q${DefaultFormatter(bg)}$q" else ""}>")) ++
    svgInvertedY(size, stuff: _*).map(x => x.in) ++
    Vector(Indent("</body>"), Indent("</html>"))
  ).map(_.toString)

  def svgHtmlInvertedY(size: Vc, stuff: InSvg*): Vector[String] = svgHtmlInvertedY(size, Rgba.Empty, stuff: _*)

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

  // This is the third attempt at this kind of thing and it is HORRIBLE
  // Throw it all away and rewrite it properly once you have time!!!
  object Tickify {
    case class Num(digits: Array[Byte], decimal: Int, negative: Boolean) {
      /** Read a single digit */
      def apply(position: Int) = {
        if (position >= decimal) 0
        else if (position >= 0) digits(decimal - position - 1)
        else if (decimal - position <= digits.length) digits(decimal - position - 1)
        else 0
      }

      /** Write a single digit--throws an exception if out of bounds, clips values to 0-9 */
      def update(position: Int, value: Int) {
        val v = (if (value < 0) 0 else if (value > 9) 9 else value).toByte
        digits(decimal - position - 1) = v
      }

      /** Number of leading zeros */
      def leadingZeros = {
        var i = 0
        while (i < digits.length && digits(i) == 0) i += 1
        i
      }

      /** Position of first nonzero digit */
      def leading = decimal - leadingZeros - 1

      /** Number of trailing zeros */
      def trailingZeros = {
        var i = digits.length - 1
        while (i > 0 && digits(i) == 0) i -= 1
        digits.length - 1 - i
      }

      /** Position of the last nonzero digit */
      def lagging = decimal - digits.length + trailingZeros

      /** Position of first digit that differs from `n` */
      def diffdig(n: Num) = {
        val l = leading
        val nl = n.leading
        if (negative != n.negative || l != nl) math.max(l, nl)
        else {
          var i = leadingZeros
          var j = n.leadingZeros
          while (i < digits.length && j < n.digits.length && digits(i) == n.digits(j)) { i += 1; j += 1 }
          decimal - i - 1
        }
      }

      /** Creates a new copy (truncating leading/trailing zeros) */
      def copy(): Num = {
        val lz = leadingZeros
        val tz = math.min(trailingZeros, digits.length - decimal)
        if (lz + tz >= digits.length) Num.zero
        else new Num(java.util.Arrays.copyOfRange(digits, lz, digits.length - tz), decimal - lz, negative)
      }

      /** Creates a new copy preserving leading and trailing zeros */
      def copyExactly(): Num = 
        if (digits.length == 0) this
        else new Num(java.util.Arrays.copyOf(digits, digits.length), decimal, negative)

      /** Creates a new copy, truncating leading zeros but leaving trailing ones intact. */
      def copyWithTrailing(): Num = {
        val lz = leadingZeros
        if (lz + trailingZeros >= digits.length) Num.zero
        else new Num(java.util.Arrays.copyOfRange(digits, lz, digits.length), decimal - lz, negative)
      }

      /** Creates a new copy, truncating trailing zeros but leaving leading ones intact. */
      def copyWithLeading(): Num = {
        val tz = math.min(trailingZeros, digits.length - decimal)
        if (tz + leadingZeros >= digits.length) Num.zero
        else new Num(java.util.Arrays.copyOf(digits, digits.length - tz), decimal, negative)
      }

      /** Creates a new copy with a specified number of digits after the decimal point */
      def copyToPosition(p: Int, initialZeros: Int, roundUp: Boolean): Num = {
        val lz = leadingZeros
        val tz = math.min(trailingZeros, digits.length - decimal)
        val before = decimal - lz
        val after = digits.length - decimal - tz
        val newbefore = math.max(1, before + math.max(0, initialZeros))
        val newafter = math.max(1-newbefore, -p)
        var a = new Array[Byte](newbefore + newafter)
        if (lz + tz >= digits.length) {
          new Num(a, newbefore, negative)
        }
        else {
          var i = lz
          var j = newbefore - before
          while (i < digits.length - tz && j < a.length) {
            a(j) = digits(i)
            j += 1
            i += 1
          }
          if (i < digits.length - tz) {
            if (j > a.length) j = a.length
            if (roundUp) {
              j -= 1
              var overflow = a(j) >= 9
              while (overflow && j > 0) {
                a(j) = 0
                j -= 1
                overflow = a(j) >= 9
              }
              if (!overflow) a(j) = (a(j) + 1).toByte
              else {
                a(0) = 0
                val b = a
                a = new Array[Byte](b.length + 1)
                a(0) = 1
                System.arraycopy(b, 0, a, 1, b.length)
              }
            }
          }
          new Num(a, a.length-newafter, negative)
        }
      }

      def toBigDec = BigDecimal(this.toString)

      def toDouble = this.toString.toDouble

      /** String representation */
      override def toString = {
        val negN = (if (negative) 1 else 0)
        val dpN  = (if (decimal < digits.length) 1 else 0)
        val ezN  = (if (decimal > digits.length) decimal - digits.length else 0)
        val padN = (if (decimal < 1) 1-decimal else 0)
        val a = new Array[Char](digits.length + negN + dpN + padN + ezN)
        if (negative) a(0) = '-'
        if (dpN > 0) {
          if (decimal >= 1) a(decimal + negN) = '.'
          else a(1 + negN) = '.'
        }
        if (padN > 0) {
          a(0 + negN) = '0'
          var i = 1
          while (i < padN) { a(1 + i + negN) = '0'; i += 1 }
        }
        var i = negN + padN
        var j = 0
        while (j < decimal && j < digits.length) {
          a(i) = (digits(j) + '0').toChar
          i += 1
          j += 1
        }
        if (j == decimal) {
          i += 1
          while (j < digits.length) {
            a(i) = (digits(j) + '0').toChar
            i += 1
            j += 1
          }
        }
        else {
          while (i < a.length) {
            a(i) = '0'
            i += 1
          }
        }
        new String(a)
      }

      /** Only allow equality to other Nums, not any old number or string */
      override def equals(a: Any) = a match {
        case n: Num => diffdig(n) < math.min(lagging, n.lagging)
        case _      => false
      }
    }
    object Num {
      /** The empty number (NaN) */
      val empty = new Num(new Array[Byte](0), 0, false)

      /** A canonical encoding of zero */
      def zero = new Num(new Array[Byte](1), 1, false)

      /** Creates a Num from a presumably correctly-formatted string (NOT checked!) */
      private[this] def fromString(s: String): Num = {
        val i = s.indexOf('.')
        val neg = s.charAt(0) == '-'
        val dig = new Array[Byte](s.length - (if (neg) 1 else 0) - (if (i < 0) 0 else 1))
        var j = if (neg) 1 else 0
        var k = 0
        while (j < i) { dig(k) = (s.charAt(j) - '0').toByte; j += 1; k += 1 }
        if (j == i) j += 1
        while (j < s.length) { dig(k) = (s.charAt(j) - '0').toByte; j += 1; k += 1 }
        new Num(dig, if (i < 0) dig.length else if (neg) i-1 else i, neg)        
      }

      /** Creates a Num from the String representation of a Double */
      def apply(x: Double): Num = {
        if (!x.finite) return empty
        if (x == 0) return zero
        val s = new java.math.BigDecimal(x.toString).stripTrailingZeros.toPlainString;
        fromString(s)
      }

      def apply(bd: BigDecimal): Num = fromString(bd.underlying.toPlainString)
    }

    case class Anchor(a: Num, b: Num, value: Num, inverted: Boolean) {}
    object Anchor {
      def apply(a: Double, b: Double, seekBetter: Boolean = false, seekLarger: Boolean = false): Anchor = {
        if (!a.finite || !b.finite) return new Anchor(Num.empty, Num.empty, Num.empty, false)
        if (a > b) return apply(b, a, seekBetter, !seekLarger).copy(inverted = true)
        val na = Num(a)
        if (a == b) return new Anchor(na, na, na, false)
        val nb = Num(b)
        if (na.negative != nb.negative) return new Anchor(na, nb, Num.zero, false)
        val p = na diffdig nb
        val lead = (nb.leading - na.leading)
        val v = na.copyToPosition(p, if (seekBetter) max(0, lead) else 0, a > 0)
        val nbp = nb(p)
        if (seekBetter) {
          if (a < 0) {
            if (v(p) > 5 && (nbp < 5 || (nbp == 5 && p == nb.lagging))) v(p) = 5
            else {
              if ((v(p) & 1) == 1 && (v(p)-1 > nbp || (v(p)-1 == nbp && p == nb.lagging))) v(p) = (v(p) - 1).toByte
              if (seekLarger != na.negative) {
                val inc = (if ((v(p) & 1) == 1) 1 else 2)
                while (v(p) - inc > math.max(0, nbp)) v(p) = (v(p) - inc).toByte
              }
            }
          }
          else {
            if (v(p) < 5 && nbp >= 5) v(p) = 5
            else {
              if ((v(p) & 1) == 1 && v(p)+1 <= nbp) v(p) = (v(p) + 1).toByte
              if (seekLarger != na.negative) {
                val inc = (if ((v(p) & 1) == 1) 1 else 2)
                while (v(p) + inc <= math.min(9, nbp)) v(p) = (v(p) + inc).toByte
              }
            }
          }
        }
        new Anchor(na, nb, if (seekBetter) v.copyWithTrailing else v, false)
      }

      def plausible(a: Double, b: Double): Array[Anchor] = {
        val anch1 = apply(a, b)
        val anch5 = apply(a, b, true)
        if (anch1.value != anch5.value) {
          val p = anch1.value.lagging
          if (p != anch5.value.lagging || (anch1.value(p) & 1) != 1 || anch5.value(p) != 5) Array(anch5, anch1)
          else {
            val anch2 = Anchor(anch1.a, anch1.b, anch1.value.copy, anch1.inverted)
            anch2.value(p) = (anch2.value(p) + (if (anch1.value(p) > anch5.value(p)) 1 else -1)).toInt
            Array(anch5, anch2, anch1)
          }
        }
        else Array(anch1)
      }
    }

    def lastDigitScore(n: Num): Double = {
      var score = 1.0
      val last = n.lagging
      var l = last
      val digit = n(last)
      if (n(last) == 0) return 1e186   // Greater than math.pow(4, math.log10(Double.MaxValue))
      while (l > 0) { score *= 4; l -= 1 }
      while (l < 0) { score *= 0.25; l += 1 }
      score * (if (digit == 5) n(last+1) match { case 2 | 7 => 5; case _ => 3 } else if ((digit &1) == 0) 2 else 1)
    }

    val trialGapsAsDouble = Array(7.5, 5, 4, 3, 2.5, 2, 1.5, 1)
    val trialGaps = trialGapsAsDouble.map(x => BigDecimal(x))

    val typicalScorer: (Array[Num], Double, Int, Double) => Double = (ns, position, target, avg) =>
      if (target <= 0) { if (ns.length == 0) 1e9*avg else if (ns.length == 1) avg else 0 }
      else {
        val badBounds = 
          if (position < -0.05 || position > 1.05) 0
          else if (position < -0.001) -0.01/(position-0.01)
          else if (position > 1.001) 0.01/(position-0.99)
          else 1.0
        val score = avg*((target + 1.0 - 2.0*(target - ns.length).abs)/(target + 1.0)).sqrt
        if (score == 0) 1-badBounds else score*badBounds
      }

    def select(a: Double, b: Double, number: Int, scorer: (Array[Num], Double, Int, Double) => Double = typicalScorer): Array[(Num, Double)] = {
      if (number == 0) return new Array[(Num,Double)](0)
      if (number == 1) {
        val anch = Anchor(a, b, true)
        return Array((anch.value, if (a < b) (anch.value.toDouble - a)/(b-a) else if (b < a) (anch.value.toDouble - b)/(a-b) else 0.5))
      }
      val plausibles = Anchor.plausible(a,b)
      val check = plausibles.flatMap{ anch =>
        val v = anch.value.toDouble
        val left = (v-a).abs
        val right = (b-v).abs
        var multiplier = 1.0
        var shift = 0
        // Order of these two is important because we want to overshoot before finding low bound!
        while (1 + (multiplier*left).floor + (multiplier*right).floor > number) { shift += 1; multiplier /= 10 }
        while (1 + (multiplier*left).floor + (multiplier*right).floor < number) { shift -= 1; multiplier *= 10 }
        var index = trialGapsAsDouble.length - 2
        while (1 + (multiplier*left/trialGapsAsDouble(index)).floor + (multiplier*right/trialGapsAsDouble(index)).floor > 0.5*number) { 
          index -= 1; if (index < 0) { index = trialGapsAsDouble.length - 1; multiplier /= 10; shift += 1 }
        }
        index += 1; if (index >= trialGapsAsDouble.length) { index = 0; multiplier *= 10; shift -= 1 }
        val shLo = shift
        val ixLo = index
        while (1 + (multiplier*left/trialGapsAsDouble(index)).floor + (multiplier*right/trialGapsAsDouble(index)).floor < 1.5*number) {
          index += 1; if (index >= trialGapsAsDouble.length) { index = 0; multiplier *= 10; shift -= 1 } 
        }
        index -= 1; if (index < 0) { index = trialGapsAsDouble.length - 1; multiplier /= 10; shift += 1 }
        val shHi = shift
        val ixHi = index
        val ab = Array.newBuilder[(Int, Int)]
        index = ixLo
        shift = shLo
        while (shift > shHi || (shift >= shHi && index <= ixHi)) {
          ab += ((shift, index))
          index += 1
          if (index >= trialGapsAsDouble.length) {
            index = 0
            shift -= 1
          }
        }
        ab.result.map{ case (sh, ix) => (anch, sh, ix) }
      }
      if (check.isEmpty) return new Array[(Num, Double)](0)
      def elaborate(anch: Anchor, sh: Int, ix: Int): Array[Num] = {
        val delta = BigDecimal((if (b < a) -trialGaps(ix) else trialGaps(ix)).underlying.scaleByPowerOfTen(sh))
        val center = anch.value.toBigDec
        val left = anch.a.toBigDec
        val right = anch.b.toBigDec
        var runner = center - delta
        val leftBuilder = Array.newBuilder[Num]
        while (if (a > b) runner <= left else runner >= left) { 
          leftBuilder += Num(runner.underlying.stripTrailingZeros)
          runner = runner - delta
        }
        val builder = Array.newBuilder[Num]
        builder ++= leftBuilder.result().reverse
        builder += anch.value
        runner = center + delta
        while (if (a > b) runner >= right else runner <= right) {
          builder += Num(runner.underlying.stripTrailingZeros)
          runner = runner + delta
        }
        builder.result()
      }
      def avscore(ns: Array[Num], ignoreValue: Option[Double]): Double = {
        var s = 0.0
        var i, n = 0
        var ignore = ignoreValue.getOrElse(0.0)
        while (i < ns.length) { 
          val score = lastDigitScore(ns(i))
          if (score == ignore) { if (ignore > 0) ignore = 0 else n += 1 }
          else {
            s += score
            n += 1
          }
          i += 1
        }
        if (n > 0) s/n else 0.0
      }
      def scorify(anch: Anchor, sh: Int, ix: Int): (Array[Num], Double) = {
        val e = elaborate(anch, sh, ix)
        val worstBound = e.map(_.toDouble).sortBy(x => -(x-0.5).abs).headOption.getOrElse(0.5)
        val av = avscore(e, if (plausibles.length == 1) Some(lastDigitScore(plausibles.head.value)) else None)
        (e, scorer(e, worstBound, number, av))
      }
      def scorifyTup(asi: (Anchor, Int, Int)) = scorify(asi._1, asi._2, asi._3)
      val best = (scorifyTup(check.head) /: check.tail){ (scored, x) => 
        val newscore = scorifyTup(x)
        if (newscore._2 > scored._2) newscore else scored
      }
      def clipIfClose(d: Double) = if (d < 0 && d > -1e-5) 0 else if (d > 1 && d < 1 + 1e-5) 1 else d
      best._1.map(n => (n, clipIfClose(if (b > a) (n.toDouble - a)/(b - a) else (n.toDouble - b)/(a - b))))
    }
  }

  final case class AutoTick(from: Vc, to: Vc, number: Int, left: Float, right: Float, anchor: Float, style: Style, sub: Option[(Int,Style)] = None) extends Shown {
    private[this] lazy val tickInfo: (TickLabels, Option[Ticky]) = {
      val delta = to - from
      if (delta.lenSq == 0 || !delta.finite) (TickLabels(from, to, Nil, left, right, anchor, style), None)
      else {
        val (fa, fb) = if (delta.x.abs >= delta.y.abs) (from.x, to.x) else (from.y, to.y)
        val subdivs = Tickify.select(fa, fb, number)
        val labels = subdivs.collect{ case (num, frac) if frac == frac.clip(-0.001, 1.001) => Tik(frac.toFloat, num.toString) }
        (TickLabels(from, to, labels, left, right, anchor, style), None)
      }
    }
    def theTicks = tickInfo._1
    def subTicks = tickInfo._2


    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = theTicks.inSvg(xform, mag)(fm)
  }


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

  final case class Grouping(stuff: Seq[InSvg]) extends Shown {
    def style = Style.empty
    def this(thing: InSvg, morestuff: InSvg*) = this(thing +: morestuff)
    def inSvg(xform: Xform, mag: Option[Float => Float])(implicit fm: Formatter): Vector[Indent] = {
      Indent.V("<g>") ++
      stuff.toVector.flatMap(_.inSvg(xform, mag).map(_.in)) ++
      Indent.V("</g>")
    }
  }
  object Grouping {
    def apply(thing: InSvg, morestuff: InSvg*) = new Grouping(thing +: morestuff)
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
    def at(newOrigin: Vc, stuff: InSvg*) =
      new Assembly(0 vc 0, 1 vc 1, newOrigin, None, Style.empty, stuff)
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
      val earc = ErrorArc(177 vc 277, 197 vc 297, 197 vc 277, 177 vc 297, 2, Stroke.alpha(Rgba.Black.aTo(0.6f), 3))
      val ah = Option(LineArrow((math.Pi/180).toFloat*30, 3, 0.71f))
      val pie = Pie(Vector(Piece(15, "red", Rgba.Red), Piece(5, "green", Rgba.Green, Rgba.DarkGreen), Piece(0.1f, "gold", Rgba.Gold), Piece(0.3f, "logically blue", Rgba.Empty, Rgba.Blue)), 150 vc 350, 20, Style.empty, Some(math.Pi.toFloat/12))
      val c = Circ(100 vc 100, 20, Fill(Rgba(0, 0.8f, 0)))
      val b = Bar(200 vc 200, 10 vc 80, Fill(Rgba(1, 0.3f, 0.3f)))
      val dl = DataLine(Array(Vc(50, 300).underlying, Vc(90, 240).underlying, Vc(130, 280).underlying, Vc(170, 260).underlying), Stroke(Rgba(1, 0, 1), 4))
      val dr = DataRange(Array(90, 130, 170, 210), Array(230, 220, 240, 210), Array(270, 310, 270, 260), Fill alpha Rgba(0, 0, 1, 0.3f))
      val hg = DataHist(Array.tabulate(100)(i => 1f/(10+i)), None, None, Some(5), Fill.alpha(Rgba(0, 1, 0, 0.4f)) ++ Stroke(Rgba(0,0.5f,0), 1.5f))
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
        Assembly(0 vc 0, 800f vc 8f, 350 vc 350, hg),
        tl.copy(to = 100 vc 200, left = -20, right = 0),
        Assembly(
          0 vc 0, 0.3333f vc 0.3333f, 400 vc 200, Option((x: Float) => x.sqrt.toFloat), Opacity(0.5f),
          c, pa, pie.copy(pieces = pie.pieces.dropRight(1).map(p => p.copy(legend = ""))), cbr.copy(ticks = Some((5, 3f, 5f, 0.5f)))
        ),
        gr, earc
      )
    }
  }
}
