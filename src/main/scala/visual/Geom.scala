// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr and Calico Labs.

package kse.visual

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
