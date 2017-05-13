// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr

package kse.maths
package stochastic

import scala.math._

import kse.coll.Copy

abstract class Prng extends Copy[Prng] {
  def Z: Boolean = (L & 0x1) != 0
  def B: Byte = L.toByte
  def S: Short = L.toShort
  def C: Char = L.toChar
  def I: Int = L.toInt
  def L: Long
  def F: Float = Prng.symmetricFloatFromInt(I)
  def D: Double = Prng.symmetricDoubleFromLong(L)
  def %(n: Int): Int = 
    if (n <= 0) 0
    else {
      var l = 0L
      val mask = 0xFFFFFFFFL >>> java.lang.Integer.numberOfLeadingZeros(n)
      var more = true
      do {
        l = L
        if ((l & mask) < n) more = false
        else {
          l = l >>> 32;
          if ((l & mask) < n) more = false
        }
      } while (more);
      (l & mask).toInt
    }
  def %(n: Long): Long =
    if (n <= 0) 0
    else {
      var l = L
      val mask = 0xFFFFFFFFFFFFFFFFL >>> java.lang.Long.numberOfLeadingZeros(n)
      while ((l & mask) >= n) l = L
      l & mask
    }
  def gaussian: Double = {
    // Polar Box-Muller transform
    var x, y, rr = 0.0
    do {
      x = D*2 - 1
      y = D*2 - 1
      rr = x*x + y*y
    } while (rr >= 1);
    x * sqrt( (-2 * log(rr)) / rr )  // Discard y, but it's valid too!
  }
  def gaussianVc: Vc = {
    var x, y, rr = 0.0
    do {
      x = D*2 - 1
      y = D*2 - 1
      rr = x*x + y*y
    } while (rr >= 1);
    val scale = sqrt( (-2 * log(rr)) / rr )
    Vc.from(x * scale, y * scale)
  }
  def arrayZ(n: Int): Array[Boolean] = {
    val a = new Array[Boolean](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x3F) == 0) l = L
      a(i) = ((l & 0x1) != 0)
      l = l >>> 1
      i += 1
    }
    a
  }
  def arrayB(n: Int): Array[Byte] = {
    val a = new Array[Byte](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x7) == 0) l = L
      a(i) = (l & 0xFF).toByte
      l = l >>> 8
      i += 1
    }
    a
  }
  def arrayS(n: Int): Array[Short] = {
    val a = new Array[Short](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x3) == 0) l = L
      a(i) = (l & 0xFFFF).toShort
      l = l >>> 16
      i += 1
    }
    a
  }
  def arrayC(n: Int): Array[Char] = {
    val a = new Array[Char](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x3) == 0) l = L
      a(i) = l.toChar
      l = l >>> 16
      i += 1
    }
    a    
  }
  def arrayI(n: Int): Array[Int] = {
    val a = new Array[Int](n)
    var i = 0
    while (i < a.length) {
      val l = L
      val p = (l & 0xFFFFFFFF).toInt
      a(i) = p
      i += 1
      if (i < a.length) {
        a(i) = (l >>> 32).toInt
        i += 1
      }
    }
    a
  }
  def arrayL(n: Int): Array[Long] = {
    val a = new Array[Long](n)
    var i = 0
    while (i < a.length) {
      a(i) = L
      i += 1
    }
    a
  }
  def arrayF(n: Int): Array[Float] = {
    val a = new Array[Float](n)
    var i = 0
    while (i < a.length) {
      val l = L
      val p = (l & 0xFFFFFFFF).toInt
      a(i) = Prng.symmetricFloatFromInt(p)
      i += 1
      if (i < a.length) {
        a(i) = Prng.symmetricFloatFromInt((l >>> 32).toInt)
        i += 1
      }
    }
    a 
  }
  def arrayD(n: Int): Array[Double] = {
    val a = new Array[Double](n)
    var i = 0
    while (i < a.length) {
      a(i) = Prng.symmetricDoubleFromLong(L)
      i += 1
    }
    a    
  }
  /** Todo--improve efficiency by inlining and reusing Long */
  def arrayMod(n: Int, mod: Int): Array[Int] = {
    val a = new Array[Int](n)
    var i = 0
    while (i < a.length) {
      a(i) = this % mod
      i += 1
    }
    a
  }
  def arrayMod(n: Int, mod: Long): Array[Long] = {
    val a = new Array[Long](n)
    var i = 0
    while (i < a.length) {
      a(i) = this % mod
      i += 1
    }
    a
  }
  def arrayGaussian(n: Int): Array[Double] = {
    val a = new Array[Double](n)
    var i = 0
    while (i < a.length) {
      var x, y, rr =  0.0
      do {
        x = D*2 - 1
        y = D*2 - 1
        rr = x*x + y*y
      } while (rr >= 1);
      val scale = sqrt( (-2 * log(rr)) / rr )
      a(i) = x * scale
      i += 1
      if (i < a.length) {
        a(i) = y * scale
        i += 1
      }
    }
    a
  }
  def shuffle(a: Array[Byte]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def shuffle(a: Array[Short]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def shuffle(a: Array[Char]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def shuffle(a: Array[Int]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def shuffle(a: Array[Float]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def shuffle(a: Array[Long]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def shuffle(a: Array[Double]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def shuffle[A <: AnyRef](a: Array[A]) {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }
  def state: Array[Byte]
  def bits: Int
  def setFrom(bin: Array[Byte]): Boolean
  def setFrom(i: Int): Boolean
  def setFrom(l: Long): Boolean
  def setFrom(a: Long, b: Long): Boolean
  def setFromClock: Boolean
}
object Prng {
  def symmetricFloatFromInt(i: Int): Float = {
    val leadingZeros = java.lang.Integer.numberOfLeadingZeros(i)
    if (leadingZeros <= 22) {
      val exponent = 126 - leadingZeros
      val mantissa = ((i >>> 9) << leadingZeros) & 0x003FFFFF
      java.lang.Float.intBitsToFloat( (exponent << 22) | mantissa )
    }
    else 0.001953125f*i + 9.765625E-4f     // Subnormal values aren't symmetric, so we remap them equally spaced between 0 and 1
  }
  def symmetricDoubleFromLong(l: Long): Double = {
    val leadingZeros = java.lang.Long.numberOfLeadingZeros(l)
    if (leadingZeros <= 52) {
      val exponent = 1022L - leadingZeros
      val mantissa = ((l >>> 11) << leadingZeros) & 0x000FFFFFFFFFFFFFL
      java.lang.Double.longBitsToDouble( (exponent << 52) | mantissa )
    }
    else 4.8828125E-4*l + 2.44140625E-4   // Subnormal values aren't symmetric, so we remap them equally spaced between 0 and 1
  }
}

abstract class PrngState64 extends Prng with Copy[PrngState64] {
  def state64: Long
  def state = {
    val a = new Array[Byte](8)
    val b = new kse.coll.packed.Bytex8(state64)
    a(0) = b.b0
    a(1) = b.b1
    a(2) = b.b2
    a(3) = b.b3
    a(4) = b.b4
    a(5) = b.b5
    a(6) = b.b6
    a(7) = b.b7
    a
  }
  final def bits = 64
  def setFrom(bin: Array[Byte]): Boolean =
    if (bin.length < 8) { setFromClock; false }
    else {
      var l = bin(0) & 0xFFL
      l |= (bin(1) & 0xFFL) << 8
      l |= (bin(2) & 0xFFL) << 16
      l |= (bin(3) & 0xFFL) << 24
      l |= (bin(4) & 0xFFL) << 32
      l |= (bin(5) & 0xFFL) << 40
      l |= (bin(6) & 0xFFL) << 48
      l |= (bin(7) & 0xFFL) << 56
      setFrom(l)
    }
  def setFrom(i: Int): Boolean = { setFromClock; false }
  def setFrom(a: Long, b: Long) = setFrom(a)
  def setFromClock = setFrom(java.lang.System.nanoTime)
}

// From public domain code by Sebastiano Vigna
final class ShiftMix64(state0: Long = java.lang.System.nanoTime) extends PrngState64 with Copy[ShiftMix64] {
  private[this] var myState = state0
  def copy: ShiftMix64 = new ShiftMix64(myState)
  def state64 = myState
  def setFrom(l: Long): Boolean = { myState = l; true }
  final def L = {
    myState += 0x9E3779B97F4A7C15L;
    var l = (myState ^ (myState >>> 30)) * 0xBF58476D1CE4E5B9L
    l = (l ^ (l >>> 27)) * 0x94D049BB133111EBL
    l ^ (l >>> 31)
  }
}

// Algorithm taken from PCG generators by Melissa O'Niell (Apache 2 license); RXS M XS 64 variant (one sequence)
final class Pcg64(state0: Long = java.lang.System.nanoTime) extends PrngState64 with Copy[Pcg64] {
  private[this] var myState = state0
  def copy: Pcg64 = new Pcg64(myState)
  def state64 = myState
  def setFrom(l: Long): Boolean = { myState = l; true }
  final def L = {
    myState = (myState * 6364136223846793005L) + 1442695040888963407L
    val l = ((myState >>> ((myState >>> 59) + 5)) ^ myState) * 0xAEF17502108EF2D9L   // 12605985483714917081 base 10
    (l >>> 43) ^ l
  }
}
