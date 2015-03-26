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
  private[this] var t = 0L
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
  
  def binary(mode: Boolean): this.type = { myGrok.binary(mode); this }
  def bigEnd(mode: Boolean): this.type = { myGrok.bigEnd(mode); this }
  
  def delimit(required: Boolean, count: Int, delimiter: Delimiter): this.type = { super.delimit(required, count, delimiter); myGrok.delimit(delim); this }
  def delimit(required: Boolean, count: Int, delimiter: Char): this.type = { super.delimit(required, count delimiter); myGrok.delimit(delim); this }
  def delimit(count: Int, delimiter: Delimiter): this.type = { super.delimit(count, delimiter); myGrok.delimit(delim); this }
  def delimit(count: Int, delimiter: Char): this.type = { super.delimit(count, delimiter); myGrok.delimit(delim); this }
  def delimit(delimiter: Delimiter): this.type = { delim = delimiter; myGrok.delimit(delim); this }
  def delimit(delimiter: Char): this.type = { super.delimit(delimiter); myGrok.delimit(delim); this }
  
  def customError: GrokError = ???
  
  def skip(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    if (binaryMode < 0) {
      ensureBuffering()
      while({ myGrok.skip(null); error == 0 && (myGrok.error > 0 || (myGrok.i >= myGrok.iN && myPosition + iN < myEnd)) }) enlargeBuffering()
      if (error == 0) error = myGrok.error
      if (error > 0) err(fail, error.toByte, e.tok)
      i = myGrok.i
    }
    else {
      if (i >= iN) ensureBuffering()
      if (i >= iN) err(fail, if (myPosition + iN < myEnd) e.over else e.end, e.tok)
      else i += 1
    }
    this
  }
  def skip(n: Int)(implicit fail: GrokHop[this.type]): this.type = {
    error = 0
    if (binaryMode < 0) {
      var k = 0; while (k < n) { skip(fail); k += 1 }; this
    }
    else if (n <= 0) this
    else {
      if (i + n.toLong < iN)
    }
  }
}*/

object GrokBufferable {
  private[eio] val EmptyByteArray = new Array[Byte](0)
  private[eio] final val InitialSize = 0x200            // 512b
  private[eio] final val TypicalMaximumSize = 0x100000  // 1M
}
