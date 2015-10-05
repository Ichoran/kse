// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

package kse.coll

import scala.language.implicitConversions

import kse.typecheck._
import kse.flow._

/** A train is like a linked list, except it is made in segments.
  * This allows for more efficient memory usage and faster indexing
  * at the cost of slightly slower insertions and deletions.
  * It is doubly-linked.  It can contain an arbitrary number of
  * elements.  It has a notion of a cursor point.  For tiny
  * lists (< 5 elements, approximately) it is less efficient (in all ways)
  * than a simple singly linked list.
  *
  * It is essentially similar to a Rope (as compared to a String).
  */
class Train[A] {
  /*
  import Train._

  /** Total size of the train. */
  private var mySize = 0L

  /** Number of elements stored on the left */
  private var nL: Int = 0

  /** Number of elements stored on the right */
  private var nR: Int = 0

  /** Indices 0-9 used for data.  Indices 10 and 11 used for previous and next, respectively. 
    * If carriage is not full, index 9 contains size.  (Always left-justified when not active.)
    */
  private var carriage = new Array[AnyRef](12)

  /** Current position within the carriage */
  private var i: Int = -1

  private def packup(to: Array[AnyRef]) {
    if (nR > 0) {
      var k = 10 - nR
      while (k < 10) {
        carriage(nL) = carraige(k)
        k += 1
        nL += 1
      }
      k = if (nL + nR < 10) 10 - nR else nL
      while (k < 10) {
        current(k) = null
        k += 1
      }
      nR = 0
    }
    if (nL < 5 && nL < mySize) {
      val pv = carriage(10).asInstanceOf[Array[AnyRef]]
      val nx = carriage(11).asInstanceOf[Array[AnyRef]]
      if (nL > 0) {
        if (pv ne to && pv ne null) {
          var n = pv(9) match { case s: Size => s.size; case _ => 10 }
          var k = 0
          while (k < nL && n < 10) {
            pv(n) = carriage(k)
            n += 1
            k += 1
          }
          if (k > 0 && n < 10) pv(9) = Size.of(n)
          n = 0
          while (k < nL) {
            carriage(n) = carriage(k)
            n += 1
            k += 1
          }
          if (n > 0) {
            current(9) = Size.of(n)
            while (n < nL) {
              current(n) = null
              n += 1
            }
          }
        }
        else if (nx ne to && nx ne null) {
          var n = nx(9) match { case s: Size => s.size; case _ => 10 }
          var k = math.min(9, n + nL - 1)
          val N = k
          if (k > n) {
            while (n >= 0) {
              nx(k) = nx(n)
              k -= 1
              n -= 1
            }
            n = nL - 1
            while (k >= 0) {
              nx(k) = carriage(n)
              carriage(n) = null
              k -= 1
              n -= 1
            }
            if (N < 10) nx(9) = Size.of(N)
            if (n >= 0) carriage(9) = Size.of(n+1)
            nL = n + 1
          }
        }
      }
      if (nL == 0) {
        if (nx ne null) nx(10) = pv
        if (pv ne null) pv(11) = nx
      }
    }
    to(9) match {
      case s: Size => nL = s.size
      case _ => nL = 10
    }
    nR = 0
  }

  private def unpack(atLeftOfGap: Boolean) {
    if (nL + nR < 10) {
      var m = nL - 1
      var n = 10 - nR
      val t = (if (i < nL) i else i + nL + nR - 10) + (if (atLeftOfGap) 0 else -1)
      if (t < m) {
        val delta = m - t
        while (t < m) {
          n -= 1
          carriage(n) = carriage(m)
          m -= 1
        }

      }
      else if (t > m) {
        ???
      }
    }
  }

  private def lefty() { unpack(true) }

  private def righty() { unpack(false) }

  def tryFwd: Boolean = 
    if (i < nL) { i += 1; true }
    else if (nR > 0 && i < 9) {
      if (i == nL) i = 10 - nR
      else i += 1
      true
    }
    else {
      val nx = carriage(11).asInstanceOf[Array[AnyRef]]
      if (nx eq null) false
      else {
        packup(nx)
        i = 0
      }
      true
    }

  def tryBkw: Boolean =
    if (i > 0 && (nL > 0 || i > 10 - nR)) { 
      if (i == 10 - nR) i = nL - 1
      else i -= 1
      true
    }
    else {
      val pv = carriage(10).asInstanceOf[Array[AnyRef]]
      if (pv eq null) false
      else {
        packup(pv)
        i = nL - 1
      }
    }

  def current: A: this.type =
    if (i < 0) throw new NoSuchElementException("Empty Train")
    else carriage(i).asInstanceOf[A]

  def add(a: A): this.type = ???

  def backspace: Boolean = if (mySize == 0 || i < 0) false else {
    if (i != nL-1) lefty()
    carriage(i) = null
    i -= 1
    nL -= 1
    mySize -= 1
    true
  }

  def push(a: A): this.type = ???

  def delete: this.type = if (mySize == 0 || i > 9) false else {
    if (i != 10 - nR) righty()
    carriage(i) = null
    i += 1
    nR -= 1
    mySize -= 1
    if (i > 9)
  }

  def size: Long = mySize
  */
}
object Train {
  /** For use inside Trains (only!) to keep track of the size of each segment */
  private final class Size(val size: Int) {}
  private object Size {
    val Zero = new Size(0)
    val One = new Size(1)
    val Two = new Size(2)
    val Three = new Size(3)
    val Four = new Size(4)
    val Five = new Size(5)
    val Six = new Size(6)
    val Seven = new Size(7)
    val Eight = new Size(8)
    val Nine = new Size(9)
    val Ten = new Size(10)
    val of = Array[Size](Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten)
  }
}
