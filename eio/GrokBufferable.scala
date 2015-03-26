// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015 Rex Kerr and UCSF

package kse.eio

import language.postfixOps

import scala.annotation.tailrec
import scala.reflect.ClassTag
import kse.flow._
import kse.coll.packed._

final class GrokBufferable/*(more: (Int, Array[Byte], Int) => Int, goto: (Long => Long) => Boolean, initialStart: Long, initialEnd: Long, maxBufferSize: Int, knownSize: Option[Long])
extends Grok {
  private[this] var myPosition = 0L
  private[this] var myStart = math.max(0, knownSize match { case Some(x) => math.min(initialStart, x); case _ => initialStart })
  private[this] var myEnd = math.max(myStart, knownSize match { case Some(x) => math.min(initialEnd, x); case _ => initialEnd })
  private[this] var buffer = new Array[Byte](
    if (knownSize.isEmpty) GrokBufferable.InitialBufferSize
    else math.max(GrokBufferable.InitialBufferSize, math.min(maxBufferSize, myEnd - myStart)).toInt
  )
  if (myStart > 0 && myEnd > myStart) {
    var skip = myStart
    while (skip > 0) {
      val n = more( (math.min(0x40000000, skip), null, -1) )
      if (n <= 0) skip = 0
      else {
        myPosition += n
        skip -= n
      }
    }
  }
  if (myPosition == myStart) {
    val n = more(math.min(buffer.length, myEnd - myStart).toInt, buffer, 0)
    if (n > 0) iN = n
  }
  delim = Delimiter.zero
  nSep = 1
  reqSep = false
  private[this] val myGrok = new GrokBuffer(buffer, 0, iN, delim, 0, false)
  
}*/

/*
final class GrokBufferable(
  more: (Int, Array[Byte], Int) => Int,
  reposition: (Long => Long) => Boolean,
  initialStart: Long, initialEnd: Long,
  initialDelimiter: Delimiter,
  initialnSep: Int = 1,
  initialReqSep: Boolean = false
  maximumBuffer = GrokBufferable.TypicalMaximumSize)
extends Grok {
  import kse.eio.{GrokErrorCodes => e}
  
  nSep = math.max(1, initalnSep)
  reqSep = initialReqSep
  delim = initialDelimiter
  
  buffer = new Array[Byte](GrokBufferable.InitialSize)

  private[this] var myG = new GrokBuffer(GrokBufferable.EmptyByteArray, 0, 0, delim, nSep, reqSep)

  private[this] var myStart = math.max(0L, initialStart)
  private[this] var myEnd = math.max(myStart, initialEnd)
  private[this] var myZero = 0L
  private[this] var ahead = GrokBufferable.InitialSize >> 1
  
  private def room = i - iN
  
  
  if (myStart > 0) {
    var ms = myStart
    while (ms > 0) {
      val skipped = more(math.min(ms, 0x40000000).toInt, null, -1)
      myZero += skipped
      ms = if (skipped <= 0) 0 else ms - skipped
    }
    if (myZero == myStart) {
      val read = more(buffer.size, buffer, 0)
      if (read > 0) iN = read
    }
  }
  
  private[this] def err(fail: GrokHop[this.type], what: Byte, who: Byte) = ???
  
  def customError: GrokError = ???
  def position: Long = myZero + myG.position
  
  override def isEmpty: Boolean =
    if (super.isEmpty) {
      if (error == e.read) true
      else { loadMore; error != e.read && super.isEmpty }
    }
    else false
  
  override def nonEmpty: Boolean =
    if (super.nonEmpty) true
    else if (error == e.read) false
    else { loadMore; error != e.read && super.nonEmpty }
  
  override def trim: Int = {
    val n = super.trim
    if (i >= iN && nonEmpty) n + trim
    else n
  }
  
  override def trimmed: this.type = { trim; this }
  
  override def trySkip: Boolean = {
  
  override def trySkip(n: Int): Int = {
    var k = 0
    while (k < n && trySkip) k += 1
    k
  }
  
/*
    def B: ((implicit fail: kse.eio.GrokHop[_18.type])Byte) forSome { val _18: kse.eio.GrokBufferable } = ???
  def C: ((implicit fail: kse.eio.GrokHop[_19.type])Char) forSome { val _19: kse.eio.GrokBufferable } = ???
  def D: ((implicit fail: kse.eio.GrokHop[_20.type])Double) forSome { val _20: kse.eio.GrokBufferable } = ???
  def F: ((implicit fail: kse.eio.GrokHop[_21.type])Float) forSome { val _21: kse.eio.GrokBufferable } = ???
  def I: ((implicit fail: kse.eio.GrokHop[_22.type])Int) forSome { val _22: kse.eio.GrokBufferable } = ???
  def L: ((implicit fail: kse.eio.GrokHop[_23.type])Long) forSome { val _23: kse.eio.GrokBufferable } = ???
  def S: ((implicit fail: kse.eio.GrokHop[_24.type])Short) forSome { val _24: kse.eio.GrokBufferable } = ???
  def Z: ((implicit fail: kse.eio.GrokHop[_25.type])Boolean) forSome { val _25: kse.eio.GrokBufferable } = ???
  def aI: ((implicit fail: kse.eio.GrokHop[_26.type])Int) forSome { val _26: kse.eio.GrokBufferable } = ???
  def aL: ((implicit fail: kse.eio.GrokHop[_27.type])Long) forSome { val _27: kse.eio.GrokBufferable } = ???
  def aZ: ((implicit fail: kse.eio.GrokHop[_28.type])Boolean) forSome { val _28: kse.eio.GrokBufferable } = ???
  def attempt: ([A](parse: => A)(implicit fail: kse.eio.GrokHop[_29.type])kse.flow.Ok[kse.eio.GrokError,A]) forSome { val _29: kse.eio.GrokBufferable } = ???
  def base64: ((implicit fail: kse.eio.GrokHop[_30.type])Array[Byte]) forSome { val _30: kse.eio.GrokBufferable } = ???
  def base64in: ((target: Array[Byte], start: Int)(implicit fail: kse.eio.GrokHop[_31.type])Int) forSome { val _31: kse.eio.GrokBufferable } = ???
  def bytes: ((n: Int)(implicit fail: kse.eio.GrokHop[_32.type])Array[Byte]) forSome { val _32: kse.eio.GrokBufferable } = ???
  def bytesIn: ((n: Int, target: Array[Byte], start: Int)(implicit fail: kse.eio.GrokHop[_33.type])_33.type) forSome { val _33: kse.eio.GrokBufferable } = ???
  def context: ([A](description: => String)(parse: => A)(implicit fail: kse.eio.GrokHop[_34.type])A) forSome { val _34: kse.eio.GrokBufferable } = ???
  def customError: kse.eio.GrokError = ???
  def each: ([A](f: => A)(implicit fail: kse.eio.GrokHop[_35.type], implicit tag: scala.reflect.ClassTag[A])Array[A]) forSome { val _35: kse.eio.GrokBufferable } = ???
  def exact: ((s: String)(implicit fail: kse.eio.GrokHop[_36.type])_36.type) forSome { val _36: kse.eio.GrokBufferable } = ???
  def exact: ((c: Char)(implicit fail: kse.eio.GrokHop[_37.type])_37.type) forSome { val _37: kse.eio.GrokBufferable } = ???
  def exactNoCase: ((s: String)(implicit fail: kse.eio.GrokHop[_38.type])_38.type) forSome { val _38: kse.eio.GrokBufferable } = ???
  def filterMap: ([A, B](parse: => A)(p: A => Boolean)(f2: A => B)(implicit fail: kse.eio.GrokHop[_39.type], implicit tag: scala.reflect.ClassTag[B])Array[B]) forSome { val _39: kse.eio.GrokBufferable } = ???
  def grokEach: ([A](delimiter: kse.eio.Delimiter)(f: kse.eio.GrokHop[_40.type] => A)(implicit evidence$1: scala.reflect.ClassTag[A])kse.flow.Ok[(Array[A], Array[kse.eio.GrokError]),Array[A]]) forSome { val _40: kse.eio.GrokBufferable } = ???
  def isEmpty: ((implicit fail: kse.eio.GrokHop[_41.type])Boolean) forSome { val _41: kse.eio.GrokBufferable } = ???
  def nonEmpty: Boolean = ???
  def oC: Option[Char] = ???
  def oD: Option[Double] = ???
  def oI: Option[Int] = ???
  def oL: Option[Long] = ???
  def oQuotedBy(left: Char,right: Char,esc: Char,escaper: kse.eio.GrokEscape): Option[String] = ???
  def oTok: Option[String] = ???
  def oZ: Option[Boolean] = ???
  def oneOf: ((s: String*)(implicit fail: kse.eio.GrokHop[_42.type])String) forSome { val _42: kse.eio.GrokBufferable } = ???
  def oneOfNoCase: ((s: String*)(implicit fail: kse.eio.GrokHop[_43.type])String) forSome { val _43: kse.eio.GrokBufferable } = ???
  def peek: ((implicit fail: kse.eio.GrokHop[_44.type])Int) forSome { val _44: kse.eio.GrokBufferable } = ???
  def peekAt(distance: Int): Int = ???
  def peekBinIn: ((n: Int, target: Array[Byte], start: Int)(implicit fail: kse.eio.GrokHop[_45.type])Int) forSome { val _45: kse.eio.GrokBufferable } = ???
  def peekTok: ((implicit fail: kse.eio.GrokHop[_46.type])String) forSome { val _46: kse.eio.GrokBufferable } = ???
  def qtok: ((implicit fail: kse.eio.GrokHop[_47.type])String) forSome { val _47: kse.eio.GrokBufferable } = ???
  def qtokBy: ((left: Char, right: Char, esc: Char, escaper: kse.eio.GrokEscape)(implicit fail: kse.eio.GrokHop[_48.type])String) forSome { val _48: kse.eio.GrokBufferable } = ???
  def quoted: ((implicit fail: kse.eio.GrokHop[_49.type])String) forSome { val _49: kse.eio.GrokBufferable } = ???
  def quotedBy: ((left: Char, right: Char, esc: Char, escaper: kse.eio.GrokEscape)(implicit fail: kse.eio.GrokHop[_50.type])String) forSome { val _50: kse.eio.GrokBufferable } = ???
  def skip: ((n: Int)(implicit fail: kse.eio.GrokHop[_51.type])_51.type) forSome { val _51: kse.eio.GrokBufferable } = ???
  def skip: ((implicit fail: kse.eio.GrokHop[_52.type])_52.type) forSome { val _52: kse.eio.GrokBufferable } = ???
  def tangent: ([A](parse: => A)(implicit fail: kse.eio.GrokHop[_53.type])A) forSome { val _53: kse.eio.GrokBufferable } = ???
  def tok: ((implicit fail: kse.eio.GrokHop[_54.type])String) forSome { val _54: kse.eio.GrokBufferable } = ???
  def trim: ((implicit fail: kse.eio.GrokHop[_55.type])_55.type) forSome { val _55: kse.eio.GrokBufferable } = ???
  def tryExact(s: String): Boolean = ???
  def tryExact(c: Char): Boolean = ???
  def trySkip(n: Int): Int = ???
  def trySkip: Boolean = ???
  def uB: ((implicit fail: kse.eio.GrokHop[_56.type])Byte) forSome { val _56: kse.eio.GrokBufferable } = ???
  def uI: ((implicit fail: kse.eio.GrokHop[_57.type])Int) forSome { val _57: kse.eio.GrokBufferable } = ???
  def uL: ((implicit fail: kse.eio.GrokHop[_58.type])Long) forSome { val _58: kse.eio.GrokBufferable } = ???
  def uS: ((implicit fail: kse.eio.GrokHop[_59.type])Short) forSome { val _59: kse.eio.GrokBufferable } = ???
  def xD: ((implicit fail: kse.eio.GrokHop[_60.type])Double) forSome { val _60: kse.eio.GrokBufferable } = ???
  def xF: ((implicit fail: kse.eio.GrokHop[_61.type])Float) forSome { val _61: kse.eio.GrokBufferable } = ???
  def xI: ((implicit fail: kse.eio.GrokHop[_62.type])Int) forSome { val _62: kse.eio.GrokBufferable } = ???
  def xL: ((implicit fail: kse.eio.GrokHop[_63.type])Long) forSome { val _63: kse.eio.GrokBufferable } = ???
*/
}
*/
object GrokBufferable {
  private[eio] val EmptyByteArray = new Array[Byte](0)
  private[eio] final val InitialSize = 0x200            // 512b
  private[eio] final val TypicalMaximumSize = 0x100000  // 1M
}
