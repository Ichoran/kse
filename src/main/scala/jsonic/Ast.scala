// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

package kse.jsonic.ast

sealed trait JsResult

sealed trait JsVal extends JsResult

case object JsNull extends JsVal

sealed trait JsBool extends JsVal { def value: Boolean }
case object JsTrue extends JsBool { def value = true }
case object JsFalse extends JsBool { def value = false }

final case class JsStr(value: String) extends JsVal {}
object JsStr { def empty = new JsStr("") }

final case class JsNum(value: Double, literal: String) extends JsVal {}
object JsNum { def nan = new JsNum(Double.NaN, "null") }

sealed trait JsArr extends JsVal { def values: Array[JsVal] }
final case class JsArrV(values: Array[JsVal]) extends JsArr {}
final case class JsArrD(doubles: Array[Double]) extends JsArr {
  def values = { var i = 0; val v = new Array[JsVal](doubles.length); while (i < doubles.length) { v(i) = JsNum(doubles(i), doubles(i).toString); i += 1 }; v }
}
object JsArr { def empty: JsArr = new JsArrV(new Array[JsVal](0)) }

final case class JsObj(keys: Array[String], values: Array[JsVal], table: collection.Map[String, JsVal]) extends JsVal {
  def apply(s: String): Option[JsVal] = if (table ne null) table get s else {
    var i = 0
    while (i < keys.length) {
      if (s == keys(i)) return Some(values(i))
      i += 1
    }
    None
  }
  def hasDuplicateKeys = table.size < keys.length
}
object JsObj { def empty = new JsObj(new Array[String](0), new Array[JsVal](0), Map.empty[String, JsVal]) }

case class JsError(msg: String, from: Int, to: Int, because: Option[JsError]) extends JsResult {}
