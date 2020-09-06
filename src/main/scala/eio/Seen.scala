// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2020 Rex Kerr and Calico Labs.

package kse.eio.seen

import kse.maths._

/*
class Digits private (val charset: String, val table: Array[Int], val fixedWidthStyle: Int = 0) {
  def toInt(s: String): Int = {
    val l = toLong(s)
    if (l > Int.MaxValue) throw new IllegalArgumentException("Numeric value will not fit in an Int")
    else l.toInt
  }

  private[this] val zeroDigit = charset.charAt(0)
  private[this] val biggestSafeLong = Long.MaxValue / charset.length

  private[this] def checkValidLength(n: Int) {
    if ({
      if (fixedWidthStyle == 0) n < 1
      else if (fixedWidthStyle < 0) n + fixedWidthStyle < 0
      else n != fixedWidthStyle
    }) throw new IllegalArgumentException("Invalid number of digits")
  }

  def toLong(s: String): Long = {
    checkValidLength(s.length)
    var offset = if (fixedWidthStyle == 0) 0 else 1
    if (fixedWidthStyle < 0 && s.length + fixedWidthStyle > 0) {
      var k: Long = charset.length
      var i = 1
      while (i + fixedWidthStyle < 0 && k <= biggestSafeLong) {
        k *= charset.length
        i += 1
      }
      while (i < s.length && k <= biggestSafeLong) {
        offset += k
        k *= charset.length
        i += 1
      }
      if (i+1 < s.length) throw new IllegalArgumentException("Numeric value will not fit in a Long")
      else if (i+1 == s.length) {
        if (k + offset < 0L) throw new IllegalArgumentException("Numeric value will not fit in a Long")
        else {
          i += 1
          offset += k
        }
      }
    }
    var number = 0L
    var i = 0
    while (i < s.length && number <= biggestSafeLong) {
      val c = s.charAt(i)
      val v = if (c == zeroDigit) 0 else table(c - zeroDigit)
      number = number*charset.length + v
      if (number < 0) throw new IllegalArgumentException("Numeric value will not fit in a Long")
      i += 1
    }
  }

  def toBigInt(s: String): BigInt = ???

  def toString(i: Int): String = toString(i.toLong)
  def toString(l: Long): String = ???
  def toString(b: BigInt): String = ???
}
*/
