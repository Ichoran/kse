// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr and Calico Labs.

package kse.visual

import scala.reflect.{ ClassTag => Tag }

import kse.maths._

abstract class Xform { self =>
  /** Apply the transform */
  def apply(v: Vc): Vc

  /** Apply the inverse transform */
  def inverse(u: Vc): Vc

  /** Apply the transform in place */
  def inPlace(values: Array[Long]) {
    var i = 0
    while (i < values.length) { values(i) = apply(Vc from values(i)).underlying; i += 1 }
  }

  /** Apply the inverse transform in place */
  def invertInPlace(values: Array[Long]) {
    var i = 0
    while (i < values.length) { values(i) = inverse(Vc from values(i)).underlying; i += 1 }    
  }

  /** Find the transformed direction corresponding to a local step in direction `axis` from point `v` */
  def dir(v: Vc, axis: Vc): Vc = {
    val step = math.max(1e-3f, 1e3f * math.max(math.abs(v.x), math.abs(v.y)))
    val dx = axis.hat*step
    (apply(v + dx) - apply(v - dx)).hat
  }

  /** Find the untransformed direction corresponding to a step in direction `uaxis` in the transformed space around transformed point `u` */
  def inverseDir(u: Vc, uaxis: Vc): Vc = {
    val step = math.max(1e-3f, 1e-3f*math.max(math.abs(u.x), math.abs(u.y)))
    val dx = uaxis.hat*step
    (inverse(u + dx) - inverse(u - dx)).hat
  }

  /** Calculate the local scaling of this transform at `center` for direction and distance +- `axis` */
  def scale(center: Vc, axis: Vc): Float =
    math.sqrt((apply(center + axis) - apply(center - axis)).lenSq / (4*axis.lenSq)).toFloat

  /** Calculate an appropriate radius for a transformed circle with original radius `r` at `center` */
  def radius(center: Vc, r: Vc): Float = {
    val s = r.ccw
    val d2a = (apply(center + r) - apply(center - r)).lenSq
    def d2b = (apply(center + s) - apply(center - s)).lenSq
    math.sqrt((d2a*d2b)/(2*(d2a + d2b))).toFloat
  }

  /** Calculate the local scaling at a point `v` */
  def mag(v: Vc): Float = {
    val step = math.max(1e-3f, 1e-3f*math.max(math.abs(v.x), math.abs(v.y)))
    val lr = apply(Vc(v.x + step, v.y)) - apply(Vc(v.x - step, v.y))
    val ud = apply(Vc(v.x, v.y + step)) - apply(Vc(v.x, v.y - step))
    val A2 = lr.lenSq
    val B2 = ud.lenSq
    (math.sqrt((A2*B2)/(2*(A2 + B2)))/step).toFloat
  }

  /** Calculate the local scaling at a point `v` in direction `axis` (and orthogonal to `axis`) */
  def mag(v: Vc, axis: Vc): Vc = {
    val step = math.max(1e-3f, 1e-3f*math.max(math.abs(v.x), math.abs(v.y)))
    val dx = axis.hat*step
    val dy = axis.ccw*step
    val major = apply(v + dx) - apply(v - dx)
    val minor = apply(v + dy) - apply(v - dy)
    Vc from (major.len/(2*step), minor.len/(2*step))
  }

  /** Produce the inverted transformation that undoes the operations of this one */
  def inverted = this match {
    case xi: Xform.Inverted => xi.original
    case _ => new Xform.Inverted(this)
  }

  /** Chain transformations (do this one first, then that one) */
  def andThen(that: Xform): Xform = new Xform {
    def apply(v: Vc) = that(self(v))
    def inverse(v: Vc) = self.inverse(that.inverse(v))
  }
}
object Xform {
  private[kse] final case class Inverted(original: Xform) extends Xform {
    def apply(v: Vc) = original inverse v
    def inverse(v: Vc) = original apply v
  }
  def identity: Xform = new Xform {
    def apply(v: Vc) = v
    def inverse(v: Vc) = v
    override def inverted = this
  }
  def origin(there: Vc): Xform = new Xform {
    def apply(v: Vc) = v - there
    def inverse(v: Vc) = v + there
    override def inverted = origin(-there)
  }
  def scale(factor: Vc): Xform = new Xform {
    def apply(v: Vc) = Vc(v.x*factor.x, v.y*factor.y)
    def inverse(v: Vc) = Vc(v.x/factor.x, v.y/factor.y)
  }
  def flipx(about: Float): Xform = new Xform {
    def apply(v: Vc) = Vc(about - v.x, v.y)
    def inverse(v: Vc) = Vc(about - v.x, v.y)
  }
  def flipy(about: Float): Xform = new Xform {
    def apply(v: Vc) = Vc(v.x, about - v.y)
    def inverse(v: Vc) = Vc(v.x, about - v.y)
  }
  def shiftscale(shifted: Vc, scaled: Vc): Xform = new Xform {
    def apply(v: Vc) = { val u = v + shifted; Vc(u.x * scaled.x, u.y * scaled.y) }
    def inverse(v: Vc) = { val u = Vc(v.x / scaled.x, v.y / scaled.y); u - shifted }
  }
  def scaleshift(shifted: Vc, scaled: Vc): Xform = new Xform {
    def apply(v: Vc) = { Vc(v.x * scaled.x, v.y * scaled.y) + shifted }
    def inverse(v: Vc) = { val u = v - shifted; Vc(u.x * scaled.x, u.y * scaled.y) }
  }
  def reorigin(oldori: Vc, newscale: Vc, newori: Vc) = new Xform {
    def apply(v: Vc) = { val u = v - oldori; Vc(u.x * newscale.x, u.y * newscale.y) + newori }
    def inverse(v: Vc) = { val u = v - newori; Vc(u.x / newscale.x, u.y / newscale.y) + oldori }
  }
  def rotate(theta: Float): Xform = new Xform {
    private[this] val xaxis = Vc(math.cos(theta).toFloat, math.sin(theta).toFloat)
    private[this] val yaxis = xaxis.ccw
    def apply(v: Vc) = Vc(v * xaxis, v*yaxis)
    def inverse(v: Vc) = xaxis * v.x + yaxis * v.y
    override def inverted = rotate(-theta)
  }
}

final class BspTree[A: Tag](minx: Float, miny: Float, nmax: Int, coord: A => Vc) {
  private var myMinX, myMinY, myMaxX, myMaxY = Float.NaN
  private var myN = 0
  private var mine: Array[A] = null
  private var smaller: BspTree[A] = null
  private var bigger: BspTree[A] = null
  private var cut: Float = Float.NaN
  private var cutOrientation = 0   // 1 = x, 2 = y, 0 = don't know yet, -1 = can't cut me
  def suggestBounds(lower: Vc, upper: Vc): this.type = {
    if (!myMinX.finite || myMinX > lower.x) myMinX = lower.x
    if (!myMaxX.finite || myMaxX < upper.x) myMaxX = upper.x
    if (!myMinY.finite || myMinY > lower.y) myMinY = lower.y
    if (!myMaxY.finite || myMaxY > upper.y) myMaxY = upper.y
    this
  }
  def +=(a: A): this.type = {
    val v = coord(a)
    val x = v.x
    val y = v.y
    if (!(x.finite && y.finite)) this
    else this += (a, x, y)
  }
  private def +=(a: A, x: Float, y: Float): this.type = {
    if (myN == 0) { 
      if (!myMinX.finite || myMinX > x) myMinX = x
      if (!myMaxX.finite || myMaxX < x) myMaxX = x
      if (!myMinY.finite || myMinY > y) myMinY = y
      if (!myMaxY.finite || myMaxY < y) myMaxY = y
    }
    else {
      if (x < myMinX) myMinX = x else if (x > myMaxX) myMaxX = x
      if (y < myMinY) myMinY = y else if (y > myMaxY) myMaxY = y
    }
    if (myN < nmax || cutOrientation < 0) {
      if (mine eq null) mine = new Array[A](8)
      else if (myN >= mine.length) {
        val more = new Array[A](mine.length*2)
        System.arraycopy(mine, 0, more, 0, mine.length)
        mine = more
      }
      mine(myN) = a;
      myN += 1
    }
    else if (cutOrientation > 0) {
      myN += 1
      if (cutOrientation == 1) { (if (cut < x) bigger else smaller) += (a, x, y) }
      else                     { (if (cut < y) bigger else smaller) += (a, x, y) }
    }
    else {
      val nx = (myMaxX - myMinX) / minx
      val ny = (myMaxY - myMinY) / miny
      smaller = new BspTree[A](minx, miny, nmax, coord)
      bigger = new BspTree[A](minx, miny, nmax, coord)
      if (nx > ny) {
        cutOrientation = 1
        cut = (myMaxX - myMinX)/2
        smaller.suggestBounds(myMinX vc myMinY, cut vc myMaxY)
        bigger.suggestBounds(cut vc myMinY, myMaxX vc myMaxY)
        if (nx < 2 && ny < 1) { smaller.cutOrientation = -1; bigger.cutOrientation = -1 }
        var i = 0
        while (i < myN) {
          val v = coord(mine(i))
          (if (cut < v.x) bigger else smaller) += (mine(i), v.x, v.y)
          i += 1
        }
      }
      else {
        cutOrientation = 2
        cut = (myMaxY - myMinY)/2
        smaller.suggestBounds(myMinX vc myMinY, myMaxX vc cut)
        bigger.suggestBounds(myMinX vc cut, myMaxX vc myMaxY)
        if (nx < 2 && ny < 1) { smaller.cutOrientation = -1; bigger.cutOrientation = -1 }
        var i = 0
        while (i < myN) {
          val v = coord(mine(i))
          (if (cut < v.y) bigger else smaller) += (mine(i), v.x, v.y)
          i += 1
        }
      }
      mine = null
    }
    this
  }
  def leaf: Option[Array[A]] =
    if (smaller ne null) None
    else if (mine eq null) Some(new Array[A](0))
    else if (myN == mine.length) Some(mine)
    else {
      val less = new Array[A](myN)
      System.arraycopy(mine, 0, less, 0, myN)
      mine = less
      Some(less)
    }
  def xSplit: Option[(BspTree[A], Float, BspTree[A])] = if (cutOrientation == 1) Some((smaller, cut, bigger)) else None
  def ySplit: Option[(BspTree[A], Float, BspTree[A])] = if (cutOrientation == 2) Some((smaller, cut, bigger)) else None
  def forleaves[U](f: (Array[A], Vc, Vc) => U) {
    if (smaller ne null) { smaller.forleaves(f); bigger.forleaves(f) }
    else f(mine, myMinX vc myMinY, myMaxX vc myMaxY)
  }
}
