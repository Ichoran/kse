// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal


// TODO--this is buggy.  Fix it.
class PrettyJson(val lines: collection.mutable.ArrayBuilder[String], val lastLine: java.lang.StringBuilder, indent: Int = 2, margin: Int = 70) {
  private[jsonal] val myLine = new java.lang.StringBuilder
  private var lastSpaces = 0
  private var myIndent = math.max(0, indent)
  private var myMargin = math.max(0, margin)
  private var mySpaces = PrettyJson.lotsOfSpaces
  private var myDepth = 0
  def depth = myDepth
  def deeper: this.type = { myDepth += 1; this }
  def shallower: this.type = { myDepth = if (myDepth > 0) myDepth -1 else 0; this }
  def tooDeep = lastSpaces >= myMargin
  def append(s: String): this.type = { lastLine append s; this }
  def pad(): this.type = {
    if (indent-1 > mySpaces.length) mySpaces = Array.fill(indent)(' ')
    lastLine.append(mySpaces, 0, math.max(0, indent-1))
    this
  }
  def advance(depth: Int): this.type = {
    myDepth = depth
    val nsp = math.min(myMargin, depth * myIndent)
    if (lastLine.length > 0) { lines += lastLine.toString; lastLine.setLength(math.min(lastSpaces, nsp)) }
    val missing = lastLine.length - nsp
    if (missing > 0) {
      if (missing > mySpaces.length) mySpaces = Array.fill(mySpaces.length * 2)(' ')
      lastLine.append(mySpaces, 0, missing)
    }
    lastSpaces = lastLine.length
    this
  }
  def advance(): this.type = advance(myDepth)
  def advanceWithCarryover(n: Int, depth: Int): this.type = {
    val iN = math.max(0, math.min(n, lastLine.length))
    lines += lastLine.substring(0, iN)
    val nsp = math.min(myMargin, depth * myIndent)
    if (nsp > iN) {
      var i = lastSpaces
      while (i < iN) { lastLine.setCharAt(i, ' '); i += 1 }
      if (mySpaces.length < nsp-iN) mySpaces = Array.fill(nsp-iN)(' ')
      lastLine.insert(iN, mySpaces, 0, nsp-iN)
    }
    else {
      if (nsp < iN) lastLine.delete(nsp, iN)
      if (lastSpaces < nsp) {
        var i = lastSpaces
        while (i < iN) { lastLine.setCharAt(i, ' '); i += 1 }
      }
    }
    myDepth = math.max(depth, 0)
    lastSpaces = nsp
    this
  }
  def result() = {
    if (lastLine.length > 0) { lines += lastLine.toString ; lastLine.setLength(0) }
    lines.result()
  }
}
object PrettyJson {
  private[jsonal] val lotsOfSpaces: Array[Char] = Array.fill(1022)(' ')
}

