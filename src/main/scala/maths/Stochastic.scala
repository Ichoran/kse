// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr

package kse.maths
package stochastic

abstract class Prng {
  def Z: Boolean = (L & 0x1) != 0
  def B: Byte = L.toByte
  def S: Short = L.toShort
  def C: Char = L.toChar
  def I: Int = L.toInt
  def L: Long
  def F: Float = Prng.uniformFloatFromInt(I)
  def D: Double = Prng.uniformDoubleFromLong(L)
  def arrayZ(n: Int): Array[Boolean] = {
    val a = new Array[Boolean](n)
    var i = 0
    var l = 0L
    while (i < n.length) {
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
    while (i < n.length) {
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
    while (i < n.length) {
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
    while (i < n.length) {
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
    while (i < n.length) {
      val l = L
      val p = (l & 0xFFFFFFFF).toInt
      a(i) = p
      i += 1
      if (i < n.length) {
        a(i) = (l >>> 32).toInt
        i += 1
      }
    }
    a
  }
  def arrayL(n: Int): Array[Long] = {
    val a = new Array[Long](n)
    var i = 0
    while (i < n.length) {
      a(i) = L
      i += 1
    }
    a
  }
  def arrayF(n: Int): Array[Float] = {
    val a = new Array[Float](n)

    while (i < n.length) {
      val l = L
      val p = (l & 0xFFFFFFFF).toInt
      a(i) = Prng.uniformFloatFromInt(p)
      i += 1
      if (i < n.length) {
        a(i) = Prng.uniformFloatFromInt((l >>> 32).toInt)
        i += 1
      }
    }
    a 
  }
  def arrayD(n: Int): Array[Double] = {
    val a = new Array[Long](n)
    var i = 0
    while (i < n.length) {
      a(i) = Prng.uniformDoubleFromLong(L)
      i += 1
    }
    a    
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
  def uniformFloatFromInt(i: Int): Float = ???
  def uniformDoubleFromLong(l: Long): Double = {
    val bits = l >>> 11   // 53 bits
    ???
  }
}

abstract class PrngState64 extends Prng {
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
  def setFromClock = setFrom(java.lang.System.nanotime)
}

// From public domain code by Sebastiano Vigna
final class ShiftMix64(state0: Long = java.lang.System.nanotime) extends PrngState64 {
  private[this] var myState = state0
  final def L = {
    myState += 0x9E3779B97F4A7C15L;
    var l = (myState ^ (myState >>> 30)) * 0xBF58476D1CE4E5B9L
    l = (l ^ (l >> 27)) * 0x94D049BB133111EBL
    l ^ (l >>> 31)
  }
}
