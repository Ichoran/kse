package kse.eio

import java.lang.Character
import kse.coll.packed._

trait Delimiter {
  def apply(s: String, i0: Int, iN: Int, n: Int): Int
  def _tok(s: String, i0: Int, iN: Int, n: Int): Long
  def tok_(s: String, i0: Int, iN: Int, n: Int): Long
  def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int
  def _tok(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long
  def tok_(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long
}

final class CharDelim(c: Char) extends Delimiter {
  final def apply(s: String, i0: Int, iN: Int, n: Int): Int = {
    if (i0 >= iN) -1
    else {
      var i = i0
      var k = n
      while (i < iN && k > 0 && s.charAt(i) == c) { i += 1; k -= 1 }
      i
    }
  }
  final def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int = {
   if (i0 >= iN) -1
   else {
     var i = i0
     var k = n
     while (i < iN && k > 0 && ab(i) == c) { i += 1; k -= 1 }
     i
   }
  }
  final def _tok(s: String, i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    var k = n
    while (i < iN && k > 0 && s.charAt(i) == c) { i += 1; k -= 1 }
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    while (j < iN && s.charAt(j) != c) j += 1
    (i packII j).L
  }
  final def _tok(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    var k = n
    while (i < iN && k > 0 && ab(i) == c) { i += 1; k -= 1 }
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    while (j < iN && ab(j) != c) j += 1
    (i packII j).L
  }
  final def tok_(s: String, i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    while (i < iN && s.charAt(i) != c) i += 1
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    var k = n-1
    while (j < iN && k > 0 && s.charAt(j) == c) j += 1
    (i packII j).L
  }
  final def tok_(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    while (i < iN && ab(i) != c) i += 1
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    var k = n-1
    while (j < iN && k > 0 && ab(j) == c) j += 1
    (i packII j).L
  }
}

final class WhiteDelim extends Delimiter  {
  @inline final def whiteByte(b: Byte) = ((b & 0xFF) < 64) && ((1L << b) & 0x1f0003e00L) != 0
  @inline final def darkByte(b: Byte) = ((b & 0xFF) >= 64) || ((1L << b) & 0x1f0003e00L) == 0
  final def apply(s: String, i0: Int, iN: Int, n: Int): Int = {
    if (i0 >= iN) -1
    else {
      var i = i0
      var k = n
      while (i < iN && k > 0 && Character.isWhitespace(s.charAt(i))) { i += 1; k -= 1 }
      i
    }
  }
  final def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int = {
   if (i0 >= iN) -1
   else {
     var i = i0
     var k = n
     while (i < iN && k > 0 && whiteByte(ab(i))) { i += 1; k -= 1 }
     i
   }
  }
  final def _tok(s: String, i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    var k = n
    while (i < iN && k > 0 && Character.isWhitespace(s.charAt(i))) { i += 1; k -= 1 }
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    while (j < iN && !Character.isWhitespace(s.charAt(j))) j += 1
    (i packII j).L
  }
  final def _tok(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    var k = n
    while (i < iN && k > 0 && whiteByte(ab(i))) { i += 1; k -= 1 }
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    while (j < iN && darkByte(ab(j))) j += 1
    (i packII j).L
  }
  final def tok_(s: String, i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    while (i < iN && !Character.isWhitespace(s.charAt(i))) i += 1
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    var k = n-1
    while (j < iN && k > 0 && Character.isWhitespace(s.charAt(j))) j += 1
    (i packII j).L
  }
  final def tok_(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    while (i < iN && darkByte(ab(i))) i += 1
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    var j = i+1
    var k = n-1
    while (j < iN && k > 0 && whiteByte(ab(j))) j += 1
    (i packII j).L
  }
}

final class LineDelim extends Delimiter  {
  final def apply(s: String, i0: Int, iN: Int, n: Int): Int = {
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
  }
  final def apply(ab: Array[Byte], i0: Int, iN: Int, n: Int): Int = {
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
  }
  final def _tok(s: String, i0: Int, iN: Int, n: Int): Long = {
    val i1 = apply(s, i0, iN, n)
    if (i1 >= iN) {
      return if (i1 == i0) (-1 packII -1).L else (i1 packII i1).L
    }
    var j = i1+1
    while (j < iN && {val c = s.charAt(j); c != '\n' && c != '\r'}) j += 1
    (i1 packII j).L
  }
  final def _tok(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long = {
    val i1 = apply(ab, i0, iN, n)
    if (i1 >= iN) {
      return if (i1 == i0) (-1 packII -1).L else (i1 packII i1).L
    }
    var j = i1+1
    while (j < iN && {val c = ab(j); c != '\n' && c != '\r'}) j += 1
    (i1 packII j).L
  }
  final def tok_(s: String, i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    while (i < iN &&  {val c = s.charAt(i); c != '\n' && c != '\r'}) i += 1
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    val i1 = apply(s, i, iN, n)
    (i packII i1).L
  }
  final def tok_(ab: Array[Byte], i0: Int, iN: Int, n: Int): Long = {
    var i = i0
    while (i < iN &&  {val c = ab(i); c != '\n' && c != '\r'}) i += 1
    if (i >= iN) {
      return if (i == i0) (-1 packII -1).L else (i packII i).L
    }
    val i1 = apply(ab, i, iN, n)
    (i packII i1).L
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
 
