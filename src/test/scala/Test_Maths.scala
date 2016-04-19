// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr and Calico Life Sciences

package kse.tests

import java.nio._

import kse.flow._
import kse.coll._
import kse.maths._

object Test_Maths extends Test_Kse {
  def test_finiteness: Boolean = {
    val r = new scala.util.Random(957815)
    val fs = Array.fill(16384){ r.nextInt(4) match {
      case 0 => r.nextInt(4) match { case 0 => Float.PositiveInfinity; case 1 => Float.NegativeInfinity; case _ => Float.NaN }
      case 1 => java.lang.Float.intBitsToFloat(0x7F800000 | r.nextInt)
      case 2 => r.nextDouble.toFloat
      case _ => java.lang.Float.intBitsToFloat(r.nextInt)
    }}
    val ds = Array.fill(fs.length){ r.nextInt(4) match {
      case 0 => r.nextInt(4) match { case 0 => Double.PositiveInfinity; case 1 => Double.NegativeInfinity; case _ => Double.NaN }
      case 1 => java.lang.Double.longBitsToDouble(0x7FF0000000000000L | r.nextInt)
      case 2 => r.nextDouble
      case _ => java.lang.Double.longBitsToDouble(r.nextLong)
    }}
    var i = 0
    while (i < fs.length) {
      val f = fs(i)
      if (f.finite != !(f.isNaN || f.isInfinite)) { println(f + " finiteness " + f.bits.hex); return false }
      if (f.nan != f.isNaN || f.inf != f.isInfinite) { println(f + " naninf " + f.bits.hex); return false }
      val d = ds(i)
      if (d.finite != !(d.isNaN || d.isInfinite)) { println(d + " finiteness " + d.bits.hex); return false }
      if (d.nan != d.isNaN || d.inf != d.isInfinite) { println(d + " naninf " + d.bits.hex); return false }
      i += 1
    }
    true
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Maths_from_JUnit {
  @org.junit.Test
  def test() { Test_Maths.main(Array()) }
}
