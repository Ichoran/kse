// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

/*

package kse.jsonic.printers

import kse.jsonic.ast._

class PrintIndented(
  val tab: Int = 2,
  val ascii: Boolean = false,
  val condenseUnder: Int = 80,
  val keepMargin: Boolean = false,
  val newLineBracket: Boolean = false,
  val singleLineKeyValues: Boolean = false
) {
  private val lb = collection.mutable.ArrayBuffer.empty[String]
  private val sb = new StringBuilder
  private var buf: String = null
  private def prespace(tabs: Int) {
    sb.clear()
    var t = 0
    while (t < tabs) {
      var i = 0
      while (i < tab) {
        sb append ' '
        i += 1
      }
      i += 1
    }
  }
  private def postbracket(bracket: Char, tabs: Int) {
    if (newLineBracket && sb.length > 0) {
      lb += sb.result()
      prespace(tabs)
    }
    sb append ' '
    sb append bracket
  }
  private def clear() {
    lb.clear()
    sb.clear()
    buf = null
  }

  private def linesObj(jo: JsObj, depth: Int) { ??? }
  private def linesArrD(ds: Array[Double], depth: Int) { ??? }
  private def linesArrV(jvs: Array[JsVal], depth: Int) { ??? }
  private def linesVal(jv: JsVal, depth: Int) { jv match {
    case jo: JsObj => linesObj(jo, depth)
    case jad: JsArrD => linesArrD(jad.doubles, depth)
    case jav: JsArrV => linesArrV(jav.values, depth)
    case js: JsStr => sb ++= (if (ascii) JsStr.escaped(js.value, quotes = true, ascii = ascii) else js.toString)
    case _ => sb ++= jv.toString
  }}

  def lines(jr: JsResult): Array[String] = {
    clear()
    jr match {
      case je: JsError => Array(je.toString)
      case jv: JsVal => linesVal(jv, 0)
    }
    val ans = lb.toArray
    clear()
    ans
  }
  def apply(jr: JsResult): String = lines(jr).mkString("\n")
}
object PrintIndented {
  def lines(jr: JsResult): Array[String] = (new PrintIndented()).lines(jr)
  def apply(jr: JsResult): String = (new PrintIndented())(jr)
  def ascii(jr: JsResult): String = (new PrintIndented(ascii = true))(jr)
  def asciiLines(jr: JsResult): Array[String] = (new PrintIndented(ascii = true)).lines(jr)
}

*/
