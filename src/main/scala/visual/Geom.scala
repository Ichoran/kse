// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr and Calico Labs.

package kse.visual

import kse.maths._

abstract class Xform { self =>
  def apply(v: Vc): Vc
  def revert(v: Vc): Vc
  def inPlace(values: Array[Long]) {
    var i = 0
    while (i < values.length) { values(i) = apply(Vc from values(i)).underlying; i += 1 }
  }
  def revertInPlace(values: Array[Long]) {
    var i = 0
    while (i < values.length) { values(i) = revert(Vc from values(i)).underlying; i += 1 }    
  }
  def mag(v: Vc): Float = {
    val step = math.max(1e-3f, math.max(math.abs(v.x), math.abs(v.y)))
    val left = apply(Vc(v.x + step, v.y)) - apply(Vc(v.x - step, v.y))
    val up = apply(Vc(v.x, v.y + step)) - apply(Vc(v.x, v.y - step))
    val A2 = left.lenSq
    val B2 = up.lenSq
    math.sqrt((A2*B2)/(2*(A2 + B2))).toFloat
  }
  def mag(v: Vc, axis: Vc): Vc = {
    val step = math.max(1e-3f, math.max(math.abs(v.x), math.abs(v.y)))
    val dx = axis*step
    val dy = axis.ccw*step
    val major = apply(v + dx) - apply(v - dx)
    val minor = apply(Vc(v.x, v.y + step)) - apply(Vc(v.x, v.y - step))
    Vc from (major.len/(2*step), minor.len/(2*step))
  }
  def dir(v: Vc, axis: Vc): Vc = {
    val step = math.max(1e-3f, math.max(math.abs(v.x), math.abs(v.y)))
    val dx = axis*step
    (apply(v + dx) - apply(v - dx)).hat
  }
  def revertDir(v: Vc, axis: Vc): Vc = {
    val step = math.max(1e-3f, 1e-3f*math.max(math.abs(v.x), math.abs(v.y)))
    val dx = axis*step
    (revert(v + dx) - revert(v - dx)).hat
  }
  def inverted = this match {
    case xi: Xform.Inverted => xi.original
    case _ => new Xform.Inverted(this)
  }
  def andThen(that: Xform): Xform = new Xform {
    def apply(v: Vc) = that(self(v))
    def revert(v: Vc) = self.revert(that.revert(v))
  }
}
object Xform {
  private[kse] final case class Inverted(original: Xform) extends Xform {
    def apply(v: Vc) = original revert v
    def revert(v: Vc) = original apply v
  }
  def identity: Xform = new Xform {
    def apply(v: Vc) = v
    def revert(v: Vc) = v
    override def inverted = this
  }
  def origin(there: Vc): Xform = new Xform {
    def apply(v: Vc) = v - there
    def revert(v: Vc) = there - v
    override def inverted = origin(-there)
  }
  def scale(factor: Vc): Xform = new Xform {
    def apply(v: Vc) = Vc(v.x*factor.x, v.y*factor.y)
    def revert(v: Vc) = Vc(v.x/factor.x, v.y/factor.y)
  }
  def flipx(about: Float): Xform = new Xform {
    def apply(v: Vc) = Vc(about - v.x, v.y)
    def revert(v: Vc) = Vc(about - v.x, v.y)
  }
  def flipy(about: Float): Xform = new Xform {
    def apply(v: Vc) = Vc(v.x, about - v.y)
    def revert(v: Vc) = Vc(v.x, about - v.y)
  }
  def shiftscale(shift: Vc, scale: Vc): Xform = new Xform {
    def apply(v: Vc) = { val u = v - shift; Vc(u.x * scale.x, u.y * scale.y) }
    def revert(v: Vc) = { val u = Vc(v.x / scale.x, v.y / scale.y); u + shift }
  }
  def rotate(theta: Float): Xform = new Xform {
    private[this] val xaxis = Vc(math.cos(theta).toFloat, math.sin(theta).toFloat)
    private[this] val yaxis = xaxis.ccw
    def apply(v: Vc) = Vc(v * xaxis, v*yaxis)
    def revert(v: Vc) = xaxis * v.x + yaxis * v.y
    override def inverted = rotate(-theta)
  }
}
