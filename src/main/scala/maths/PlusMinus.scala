// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2020 Rex Kerr and Calico Life Sciences LLC

package kse.maths

import kse.coll.packed._

// TODO--finish this!!!

/*
final class RichFloatToPm(underlying: Float) extends AnyVal {
  def +-(err: Double) = new RichFloatToPM(Floatx2(underlying, err.toFloat).L)
  def +-(err: Float) = new RichFloatToPM(Floatx2(underlying, err).L)
  def +-(err: Long) = new RichFloatToPM(Floatx2(underlying, err.toFloat).L)
  def +(that: PlusMinus) = that + underlying
  def -(that: PlusMinus) = that subtractedFrom underlying
  def *(that: PlusMinus) = that * underlying
  def /(that: PlusMinus) = (this +- 0f) / that
  def pow(that: PlusMinus) = 
}


final class PlusMinus(underlying: Long) extends AnyVal {
  override def toString = {
    val ve = new FloatX2(underlying)
    val e = math.abs(ve.f2.toDouble)
    val v = ve.f1.toDouble
    val vabs = math.abs(v)
    if (e*1e6 < ) v.toString
    else {
      val vScale = v.log10.floor.toInt
      if (vScale > 9 || vScale < -3) {
        val vText = 
      }
      else {

      }
      val eScale = e.log10.rint.toInt
    }
  }
}
*/
