// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr.

package kse.visual

import kse.maths._

case class Frame(origin: Vc, e: Vc) {
  def into(v: Vc, that: Frame): Vc = {
    val x = v.x.toDouble * e.x - v.y.toDouble * e.y + origin.x - that.origin.x
    val y = v.x.toDouble * e.y + v.y.toDouble * e.x + origin.y - that.origin.y
    Vc.from((x*that.e.x + y*that.e.y)/that.e.lenSq, (-x*that.e.y + y*that.e.x)/that.e.lenSq)
  }
}
object Frame {
  val natural = Frame(0 vc 0, 1 vc 0)
}

abstract class Xform(val from: Frame, val to: Frame) {
  def apply(v: Vc): Vc
  def revert(v: Vc): Vc
  def inverted = this match {
    case xi: Xform.Inverted => xi.original
    case _ => new Xform.Inverted(this)
  }
}
object Xform {
  final class Inverted(val original: Xform) extends Xform(original.to, original.from) {
    def apply(v: Vc) = original revert v
    def revert(v: Vc) = original apply v
  }
  final class Natural(from: Frame, to: Frame) extends Xform(from, to) {
    def apply(v: Vc) = from into (v, to)
    def revert(v: Vc) = to into (v, from)
    override def inverted = new Natural(to, from)
  }

  def natural(from: Frame, to: Frame): Xform = new Natural(from, to)
}

trait Boxed { def lu: Vc; def rb: Vc }
trait Centered extends Boxed { def center: Vc }
trait Enclosed extends Boxed { def encloses(v: Vc): Boolean }

case class Circle(center: Vc, radius: Float) extends Centered with Enclosed {
  def encloses(v: Vc) = (v - center).lenSq <= radius
  def lu = center - radius
  def rb = center + radius
}
case class Rect(center: Vc, axes: Vc) extends Centered with Enclosed {
  def encloses(v: Vc) = (v.x - center.x).abs <= axes.x && (v.y - center.y).abs <= axes.y
  def lu = center - axes
  def rb = center + axes
}
