// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-16 Rex Kerr

package kse.maths
package optimization

import scala.math._

import kse.flow._

abstract class Approximator {
  val parameters: Array[Double]
  def apply(datum: Double): Double
  def computeInPlace(data: Array[Double]) { data(1) = apply(data(0)) }
  def computeAll(data: Array[Double], target: Array[Double] = null): Array[Double] = {
    val a = if ((target eq null) || target.length < data.length) new Array[Double](data.length) else target
    var i = 0
    while (i < a.length) { a(i) = apply(data(i)); i += 1 }
    a
  }
}

trait ApproximatorCompanion[App <: Approximator] {
  def guess(ts: Array[Double], xs: Array[Double], finitize: Boolean): List[App]
}

object Approximator {
  def finiteCopies(ts: Array[Double], xs: Array[Double]): (Array[Double], Array[Double]) = {
    val n = math.min(ts.length, xs.length)
    var m, i = 0
    while (i < n) { if (ts(i).finite && xs(i).finite) m += 1; i += 1 }
    val nts, nxs = new Array[Double](m)
    i = 0
    var j = 0
    while (i < m) { if (ts(i).finite && xs(i).finite) { nts(j) = ts(i); nxs(j) = xs(i); j += 1 }; i += 1 }
    (nts, nxs)
  }

  private[this] def findCleanLeftRightSlopes(
    ts: Array[Double], xs: Array[Double], finitize: Boolean = true
  ): (fits.FitTX, fits.FitTX, Array[Double], Array[Double], Int) = {
    var fts = ts
    var fxs = xs
    if (finitize && (!ts.finite || !xs.finite)) {
      finiteCopies(ts, xs) match { case (t, x) => fts = t; fxs = x }
    }
    val n = math.min(fts.length, fxs.length)
    if (n <= 0) return (null, null, fts, fxs, 0)
    val left, right = new fits.FitTX
    var i = 0
    var j = n-1
    var winner = 0
    while (i < j && winner.abs < 10) {
      left += (fts(i), fxs(i))
      right += (fts(j), fxs(j))
      if (i > 4) {
        val lb = left.betaX.abs
        val rb = right.betaX.abs
        val call = if (lb > rb*1.2) -1 else if (rb > lb*1.2) 1 else 0
        if (call == 0) winner = 0
        else if (winner == 0) winner = call
        else if (winner*call < 0) winner = call
        else winner += call
      }
      i += 1
      j -= 1
    }
    (left, right, fts, fxs, winner)
  }


  final class Affine(x0: Double, slope: Double) extends Approximator {
    val parameters = Array(x0, slope)
    def apply(t: Double) = parameters(0) + parameters(1)*t
    override def toString = f"x = ${parameters(0)} + ${parameters(1)}*t"
  }
  object Affine extends ApproximatorCompanion[Affine] {
    def guess(ts: Array[Double], xs: Array[Double], finitize: Boolean = true): List[Affine] = {
      val n = math.min(ts.length, xs.length)
      if (n <= 0) return Nil
      var ftx = fits.FitTX(ts, xs, 0, math.min(ts.length, xs.length))
      if (finitize && !ftx(0).finite) {
        val (nts, nxs) = finiteCopies(ts, xs)
        guess(nts, nxs, false)
      }
      else new Affine(ftx(0), ftx.betaX) :: Nil
    }
  }

  final class Quadratic(t0: Double, x0: Double, slopeAtOne: Double) extends Approximator {
    val parameters = Array(t0, x0, slopeAtOne)
    def apply(t: Double) = { val dt = t - parameters(0); parameters(1) + parameters(2)*dt*dt }
    override def toString = f"x = ${parameters(1)} + ${parameters(2)}*(t - ${parameters(0)})^2"
  }
  object Quadratic extends ApproximatorCompanion[Quadratic] {
    def guess(ts: Array[Double], xs: Array[Double], finitize: Boolean = true): List[Quadratic] = ???
  }

  final class Exponential(offset: Double, height: Double, slope: Double) extends Approximator {
    // Equation is x = offset + height*exp((slope/height)*t), so that dx/dt(0) = slope
    val parameters = Array(offset, height, slope)
    def apply(t: Double) = { val h = parameters(1); parameters(0) + (if (h != 0) h*exp(parameters(2)*t/h) else 0.0) }
    override def toString = f"x = ${parameters(0)} + ${parameters(1)}*e^${parameters(2)/parameters(1)}t"
  }
  object Exponential extends ApproximatorCompanion[Exponential] {
    private[this] def fromSlopes(ours: fits.FitTX, theirs: fits.FitTX, correctionLimit: Double = 0.9) = {
        // Exponential of form x = x0 + a*exp((b/a)*t) so that dx/dt(0) = b
        val tc = ours.meanT    // time at which we have a slope estimate
        val xc = ours.meanX    // height at that time
        val bc = ours.betaX    // slope
        // If slopes change, just take difference as contribution of exponential
        // If both slopes have same sign, height is proportional to slope so we can correct
        val nonconst = 
          if (signum(xc - theirs.meanX)*signum(tc - theirs.meanT) == signum(bc)) // No weird wiggles
            (xc - theirs.meanX)/(1 - (if (theirs.betaX * bc > 0) min(correctionLimit, theirs.betaX/bc) else 0))
          else xc - theirs.meanX
        // Scale factor due to time not being zero
        val scale = exp(bc*tc / nonconst)
        val height = nonconst / scale
        new Exponential(xc - nonconst, height, bc/scale)
    }
    def guess(ts: Array[Double], xs: Array[Double], finitize: Boolean = true): List[Exponential] = {
      val (left, right, _, _, winner) = findCleanLeftRightSlopes(ts, xs, finitize)
      if ((left eq null) || (right eq null)) return Nil
      if (winner.abs < 10) {
        // Can't reliably tell whether it's growing or shrinking, so we'd better return both
        if (signum(left.betaX) != signum(right.betaX)) fromSlopes(left, right, 0) :: fromSlopes(right, left, 0) :: Nil
        else {
          val cx = (left.meanX + right.meanX)/2
          val ct = (left.meanT + right.meanT)/2
          val dt = right.meanT - left.meanT
          val dx = right.meanX - left.meanX
          val cs = dx/dt
          val big = exp(4)
          // Have cx = x0 + a*exp((b/a)*ct)
          // Could choose a same sign as dx or opposite sign
          val rates = (dt :: (-dt) :: Nil).map(t => 1/(big*t))  // Change of dt produces change of about 1/big
          rates.map{ r =>
            val scale = exp(r*ct)
            val height = big*dx/scale * signum(r)
            val offset = cx - height*scale
            new Exponential(offset, height, height*r)
          }
        }
      }
      else {
        // One side is clearly steeper than the other.  Use it.
        if (winner < 0) fromSlopes(left, right) :: Nil   // Decaying exponential, use left edge slope
        else            fromSlopes(right, left) :: Nil   // Rising exponential, use right edge slope
      }
    }
  }

  final class Power(offset: Double, amplitude: Double, slope: Double) extends Approximator {
    val parameters = Array(offset, amplitude, slope)
    def exponent =  if (parameters(1) == 0) 0 else parameters(2)/parameters(1)
    def apply(t: Double) = 
      if (t <= 0) { 
        if (slope >= 0) parameters(0)
        else if (amplitude >= 0) Double.PositiveInfinity
        else Double.NegativeInfinity
      }
      else {
        val h = parameters(1)
        parameters(0) + (if (h != 0) h*pow(t, parameters(2)/h) else 0.0)
      }
    override def toString = f"x = ${parameters(0)} + ${parameters(1)}*t^$exponent"
  }
  object Power extends ApproximatorCompanion[Power] {
    def guess(ts: Array[Double], xs: Array[Double], finitize: Boolean = true): List[Power] = {
      val (left, right, fts, fxs, winner) = findCleanLeftRightSlopes(ts, xs, finitize)
      if ((left eq null) || (right eq null) || (fts eq null) || (fxs eq null)) return Nil
      if (fts(0) < 0) return Nil // Powers of negative values are a mess.
      println(winner)
      val leftly =
        if (winner >= 10) Nil
        else {
          val k = 1 + (if (left.betaX * right.betaX > 0) max(-5, log(right.betaX/left.betaX)) else -5)/log(right.meanT/left.meanT)
          val ltPk = pow(left.meanT, k)
          val a = (left.meanX - right.meanX)/(ltPk - pow(right.meanT, k))
          val x0 = left.meanX - a*ltPk
          new Power(x0, a, k*a) :: Nil
        }
      val rightly =
        if (winner <= -10) Nil
        else {
          val k = 1 + (if (left.betaX * right.betaX > 0) min(5, log(right.betaX/left.betaX)) else 5)/log(right.meanT/left.meanT)
          val rtPk = pow(right.meanT, k)
          val a = (left.meanX - right.meanX)/(pow(left.meanT, k) - rtPk)
          val x0 = right.meanX - a*rtPk
          new Power(x0, a, k*a) :: Nil
        }
      val affly =
        if (winner.abs >= 10) Nil
        else {
          Affine.guess(fts, fxs, false).map{ aff =>
            new Power(aff.parameters(0), aff.parameters(1), aff.parameters(1))
          }
        }
      val curvely = 
        if (leftly.size == rightly.size) {
          if ((leftly zip rightly).forall{ case (l, r) =>
            signum(l.parameters(1)) == signum(r.parameters(1)) &&
            (l.exponent - r.exponent).abs/(l.exponent.abs + r.exponent.abs) < 0.05
          }) leftly
          else leftly ::: rightly
        }
        else leftly ::: rightly
      if (curvely.size == affly.size) {
        if ((curvely zip affly).forall{ case (l, r) => 
          signum(l.parameters(1)) == signum(r.parameters(1)) &&
          (l.exponent - 1).abs/(l.exponent.abs + 1) < 0.05/min(100, (fts(fts.length-1)/fts(0)).tap(println))
        }) affly
        else curvely ::: affly
      }
      else curvely ::: affly
    }
  }

  final class Bilinear(t0: Double, x0: Double, leftSlope: Double, rightSlope: Double) extends Approximator {
    val parameters = Array(t0, x0, leftSlope, rightSlope)
    def apply(t: Double) = { val dt = t - parameters(0); parameters(1) + dt*(if (dt < 0) parameters(2) else parameters(3)) }
    override def toString = f"x = ${parameters(1)} + (t - ${parameters(0)})*{ t < ${parameters(0)}: ${parameters(2)}; otherwise ${parameters(3)} }"
  }
  object Bilinear extends ApproximatorCompanion[Bilinear] {
    def guess(ts: Array[Double], xs: Array[Double], finitize: Boolean = true): List[Bilinear] = ???
  }
}

abstract class Optimizer {}
object Optimizer {
}