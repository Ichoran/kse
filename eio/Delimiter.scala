// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse.eio

import language.implicitConversions

import java.lang.Character
import kse.coll.packed._

/** A `Delimiter` is a class which delimits strings or byte arrays into tokens.
  */
trait Delimiter {
  /** Finds the index of the end of the block of up to `n` delimiters starting at `i0` and not surpassing `iN` (i.e. the start of the next token).
    * If the input is empty (`iN` not greater than `i0`, or `i0` is a terminator), `-1` will be returned.
    */
  def apply(s: String, i0: Int, iN: Int, n: Int): Int
  
  /** Finds the index of the next delimiter starting at `i0` and not surpassing `iN` (i.e. the end of the next token).
    * If the input is empty, `i0` will be returned.
    */
  def not(s: String, i0: Int, iN: Int): Int
  
  /** Finds the index of the end of the block of up to `n` delimiters starting at `i0` and not surpassing `iN` (i.e. the start of the next token).
    * If the input is empty (`iN` not greater than `i0`, or `i0` is a terminator), `-1` will be returned.
    */
  def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int
  
  /** Finds the index of the next delimiter starting at `i0` and not surpassing `iN` (i.e. the end of the next token).
    * If the input is empty, `i0` will be returned.
    */
  def not(ab: Array[Byte], i0: Int, iN: Int): Int
  
  /** Creates a new `Delimiter` that stops returning tokens once a delimiter given by `d2` is reached. */
  def terminatedBy(d2: Delimiter) = new TerminatedDelim(this, d2)
}

final class CharDelim(c: Char) extends Delimiter {
  final def apply(s: String, i0: Int, iN: Int, n: Int): Int =
    if (i0 >= iN) -1
    else {
      var i = i0
      var k = n
      while (i < iN && k > 0 && s.charAt(i) == c) { i += 1; k -= 1 }
      i
    }
  final def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int =
    if (i0 >= iN) -1
    else {
      var i = i0
      var k = n
      while (i < iN && k > 0 && ab(i) == c) { i += 1; k -= 1 }
      i
    }
  final def not(s: String, i0: Int, iN: Int): Int = {
    var i = i0
    while (i < iN && s.charAt(i) != c) i += 1
    i
  }
  final def not(ab: Array[Byte], i0: Int, iN: Int): Int = {
    var i = i0
    while (i < iN && ab(i) != c) i += 1
    i
  }
}

final class WhiteDelim extends Delimiter  {
  @inline final def whiteByte(b: Byte) = ((b & 0xFF) <= 32) && ((1L << b) & 0x1f0003e00L) != 0
  @inline final def darkByte(b: Byte) = ((b & 0xFF) > 32) || ((1L << b) & 0x1f0003e00L) == 0
  final def apply(s: String, i0: Int, iN: Int, n: Int): Int =
    if (i0 >= iN) -1
    else {
      var i = i0
      var k = n
      while (i < iN && k > 0 && Character.isWhitespace(s.charAt(i))) { i += 1; k -= 1 }
      i
    }
  final def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int =
   if (i0 >= iN) -1
   else {
      var i = i0
      var k = n
      while (i < iN && k > 0 && whiteByte(ab(i))) { i += 1; k -= 1 }
      i
    }
  final def not(s: String, i0: Int, iN: Int): Int = {
    var i = i0
    while (i < iN && !Character.isWhitespace(s.charAt(i))) i += 1
    i
  }
  final def not(ab: Array[Byte], i0: Int, iN: Int): Int = {
    var i = i0
    while (i < iN && darkByte(ab(i))) i += 1
    i
  }
}

final class LineDelim extends Delimiter  {
  final def apply(s: String, i0: Int, iN: Int, n: Int): Int =
    if (i0 >= iN) -1
    else {
      var i = i0
      var k = n
      while (i < iN && k > 0) {
        val c = s.charAt(i);
        if (c == '\n') { i += 1; k -= 1 }
        else if (c == '\r') {
          i += 1
          if (i >= iN || s.charAt(i) != '\n') k -= 1
        }
        else return i
      }
      i
    }
  final def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int =
    if (i0 >= iN) -1
    else {
      var i = i0
      var k = n
      while (i < iN && k > 0) {
        val c = ab(i);
        if (c == '\n') { i += 1; k -= 1 }
        else if (c == '\r') {
          i += 1
          if (i >= iN || ab(i) != '\n') k -= 1
        }
        else return i
      }
      i
    }
  final def not(s: String, i0: Int, iN: Int): Int = {
    var i = i0
    while (i < iN && { val c = s.charAt(i); c != '\n' && c != '\r' }) i += 1
    i
  }
  final def not(ab: Array[Byte], i0: Int, iN: Int): Int = {
    var i = i0
    while (i < iN && { val c = ab(i); c != '\n' && c != '\r'}) i += 1
    i
  }
}

final class TerminatedDelim(tokenizer: Delimiter, terminator: Delimiter) extends Delimiter {
  private[this] var cachedData: AnyRef = null
  private[this] var cachedStart: Int = Int.MaxValue
  private[this] var cachedEnd: Int = Int.MaxValue
    
  private def loadCaches(s: String, i0: Int, iN: Int) {
    if (!(s eq cachedData) || (cachedStart > i0) || (cachedEnd < i0)) {
      cachedData = s
      cachedStart = i0
      cachedEnd = terminator.not(s, i0, iN) match { case x => if (x < 0) i0 else x }
    }
  }
  private def loadCaches(ab: Array[Byte], i0: Int, iN: Int) {
    if (!(ab eq cachedData) || (cachedStart > i0)) {
      cachedData = ab
      cachedStart = i0
      cachedEnd = terminator.not(ab, i0, iN) match { case x => if (x < 0) i0 else x }
    }
  }
    
  def apply(s: String, i0: Int, iN: Int, n: Int): Int = {
    if (i0 >= iN) { cachedData = null; return -1 }
    loadCaches(s, i0, iN)
    tokenizer(s, i0, math.min(iN, cachedEnd), n)
  }
  def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int = {
    if (i0 >= iN) { cachedData = null; return -1 }
    loadCaches(ab, i0, iN)
    tokenizer(ab, i0, math.min(iN, cachedEnd), n)
  }
  def not(s: String, i0: Int, iN: Int): Int = {
    if (i0 >= iN) { cachedData = null; return i0 }
    loadCaches(s, i0, iN)
    tokenizer.not(s, i0, math.min(iN, cachedEnd))
  }
  def not(ab: Array[Byte], i0: Int, iN: Int): Int = {
    if (i0 >= iN) { cachedData = null; return i0 }
    loadCaches(ab, i0, iN)
    tokenizer.not(ab, i0, math.min(iN, cachedEnd))
  }
}

object Delimiter {
  val zero = new CharDelim(0: Char)
  val comma = new CharDelim(',')
  val semi = new CharDelim(';')
  val colon = new CharDelim(':')
  val space = new CharDelim(' ')
  val tab = new CharDelim('\t')
  val white = new WhiteDelim
  val newline = new LineDelim
}
 
