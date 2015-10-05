// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Labs.

package kse.coll

import scala.language.implicitConversions

import kse.typecheck._
import kse.flow._

// This collection is not yet fully implemented.
/*
private[coll] class InsDelTree[A](val content: Array[AnyRef]) {
  var count: Long = 0
  var i0, iN: Int = (content.length >>> 1)
  var parent: InsDelTree[A] = null
  var iParent: Int = 0

  def fwdSib: InsDelTree[A] =
    if (parent == null) null
    else if (iParent + 1 < parent.iN) parent.content(iParent+1).asInstanceOf[InsDelTree[A]]
    else {
      val psib = parent.fwdSib
      if (psib != null) psib.content(psib.i0).asInstanceOf[InsDelTree[A]]
      else null
    }

  def bkwSib: InsDelTree[A] =
    if (parent == null) null
    else if (iParent > parent.i0) parent.content(iParent-1).asInstanceOf[InsDelTree[A]]
    else {
      val psib = parent.bkwSib
      if (psib != null) psib.content(psib.iN-1).asInstanceOf[InsDelTree[A]]
      else null
    }

  def countBy(n: Int) {
    var me = this
    while (me != null) {
      me.count += n
      me = me.parent
    }
  }

  def cutAt(index: Int, keepLeft: Boolean): InsDelTree[A] = ???

  def putAt(index: Int, element: AnyRef): Int = {
    if (index >= iN) {
      if (iN >= content.length) {
        var j = i0
        while (j < content.length) {
          content(j-i0) = content(j)
          j += 1
        }
        iN -= i0
        i0 = 0
      }
      content(iN) = element
      iN += 1
      iN - 1
    }
    else if (index <= i0) {
      if (i0 == 0) {
        val n = content.length - iN
        var j = iN-1
        while (j >= 0) {
          content(j+n) = content(j)
          j -= 1
        }
        iN += n
        i0 = n
      }
      i0 -= 1
      content(i0 = element)
      i0
    }
    else {
      val m = index - i0
      val n = iN - index
      if (i0 == 0 || (n <= m && iN < content.length)) {
        var j = iN
        while (j > index) {
          content(j) = content(j-1)
          j -= 1
        }
        iN += 1
        content(index) = element
        index
      }
      else {
        var j = i0
        while (j < index) {
          content(j-1) = content(j)
          j += 1
        }
        i0 -= 1
        content(index-1) = element
        index - 1
      }
    }
  }
}
class InsDel[A] {
  private var tree: InsDelTree[A] = new InsDelTree[A](new Array[AnyRef](8))
  private var i, delta = 0
  private var myPosition = 0L

  def size = { var x = tree; while (x.parent != null) x = x.parent; x.count + delta }

  def value: A = if (i == tree.iN) throw new NoSuchElementException("Empty InsDel") else tree.content(i).asIndexOf[A]

  def fwd: Boolean =
    if (i+1 < tree.iN) { i += 1; position += 1; true }
    else tree.fwdSib match {
      case null => false
      case t =>
        if (delta != 0) { tree.countBy(delta); delta = 0 }
        tree = t
        i = tree.i0
        position += 1
        true
    }

  def bkw: Boolean =
    if (i > tree.i0) { i -= 1; position -= 1; true }
    else tree.bkwSib match {
      case null => false
      case t => 
        if (delta != 0) { tree.countBy(delta); delta = 0 }
        tree = t
        i = tree.iN - 1
        position -= 1
        true
    }

  def seek(index: Long, closeAsPossible: Boolean = false): Boolean = {
    if (delta != 0) { tree.countBy(delta); delta = 0 }
    var ix = index
    var t = tree
    while (t.parent != null) t = t.parent
    while (t.content.length != 8) {
      var j = t.i0
      var sub: InsDelTree[A] = t.contents(j).asInstanceOf[InsDelTree[A]]
      while (j+1 < t.iN && sub.count >= ix) { ix -= sub.count; j += 1; sub = t.contents(j).asInstanceOf[InsDelTree[A]] }
      t = sub
    }
    if (ix < 0 || ix >= t.count) {
      if (closeAsPossible) {
        tree = t
        if (ix < 0) {
          position = 0
          i = tree.i0
        }
        else {
          position = index - (ix - t.count) - 1
          i = tree.iN - 1
        }
      }
      false
    }
    else {
      tree = t
      i = tree.i0 + ix
      position = index
      true
    }
  }

  def :=(a: A) { if (i == tree.iN) this add a else tree.content(i) = a.asInstanceOf[AnyRef] }

  private def split(cutAfterI: Boolean) {
    if (delta != 0) { tree.countBy(delta); delta = 0 }
    if (i < 3) tree = tree.cutAt(3, true)
    else if (i >= 5) { tree = tree.cutAt(5, false); i -= 5 }
    else {
      tree = tree.cutAt(i + (if (cutAfterI) 1 else 0), cutAfterI)
      if (cutAfterI) i = 0
    }
  }

  def add(a: A) {
    if (tree.count + delta >= tree.contents.length) split(true)
    position += 1
    delta += 1
    i = tree.putAt(i+1, a.asInstanceOf[AnyRef])
  }

  def insert(a: A) {
    if (tree.count + delta >= tree.contents.length) split(false)
    delta += 1
    i = tree.putAt(i, a.asInstanceOf[AnyRef])
  }

  def delete: Boolean = ???

  def backspace: Boolean = ???
}
*/
