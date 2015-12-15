// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

package kse.jsonic.ast

sealed trait JsResult

sealed trait JsVal extends JsResult

case object JsNull extends JsVal { override def toString = "null" }

sealed trait JsBool extends JsVal { def value: Boolean }
case object JsTrue extends JsBool { def value = true; override def toString = "true" }
case object JsFalse extends JsBool { def value = false; override def toString = "false" }

final case class JsStr(value: String) extends JsVal { override def toString = JsStr.escaped(value, quotes=true) }
object JsStr {
  private[this] def hx(i: Int) = { val j = i&0xF; if (j < 10) (j + '0').toChar else (j + 55).toChar }
  def empty = new JsStr("")
  def escaped(s: String, quotes: Boolean = false, ascii: Boolean = false): String = {
    var i = 0
    var n = 0
    while (i < s.length) {
      val c = s.charAt(i)
      if (c < 32) {
        if (c == '\n' || c == '\r' || c == '\t' || c == '\r' || c == '\f' || c == '\b') n += 2
        else n += 6
      }
      else if (c == '"' || c == '\\') n += 2
      else if ({ if (ascii) c >= 127 else java.lang.Character.isSurrogate(c) }) n += 6
      else n += 1
      i += 1
    }
    if (n == i) { if (quotes) "\"" + s + "\"" else s }
    else {
      if (quotes) n += 2
      var buf = new Array[Char](n)
      if (quotes) { buf(0) = '"'; n = 1 } else n = 0
      i = 0
      while (i < s.length) {
        val c = s.charAt(i)
        if (c < 32) {
          if      (c == '\n') { buf(n) = '\\'; buf(n+1) = 'n'; n += 2 }
          else if (c == '\t') { buf(n) = '\\'; buf(n+1) = 't'; n += 2 }
          else if (c == '\r') { buf(n) = '\\'; buf(n+1) = 'r'; n += 2 }
          else if (c == '\f') { buf(n) = '\\'; buf(n+1) = 'f'; n += 2 }
          else if (c == '\b') { buf(n) = '\\'; buf(n+1) = 'b'; n += 2 }
          else {
            buf(n) = '\\'; buf(n+1) = 'u'; buf(n+2) = hx(c >> 12); buf(n+3) = hx(c >> 8); buf(n+4) = hx(c >> 4); buf(n+5) = hx(c); n += 6
          }
        }
        else if (c == '\\') { buf(n) = '\\'; buf(n+1) = '\\'; n += 2 }
        else if (c == '"') { buf(n) = '\\'; buf(n+1) = '"'; n += 2 }
        else if ({ if (ascii) c >= 127 else java.lang.Character.isSurrogate(c) }) {
          buf(n) = '\\'; buf(n+1) = 'u'; buf(n+2) = hx(c >> 12); buf(n+3) = hx(c >> 8); buf(n+4) = hx(c >> 4); buf(n+5) = hx(c); n += 6
        }
        else { buf(n) = c; n += 1 }
        i += 1
      }
      if (quotes) { buf(n) = '"'; n += 1 }
      new String(buf)
    }
  }
}

final case class JsNum(value: Double, literal: String) extends JsVal {
  def hasValue(d: Double) = value == d || (d.isNaN && value.isNaN)
  override def equals(a: Any) = a match {
    case JsNum(v, l) => value == v || (v.isNaN && value.isNaN)
    case d: Double => value == d || (d.isNaN && value.isNaN)
    case _ => false
  } 
  override def toString = literal
}
object JsNum {
  def nan = new JsNum(Double.NaN, "null")
  def approx(value: Double): String = ???
}

sealed trait JsArr extends JsVal { def values: Array[JsVal] }
final case class JsArrV(values: Array[JsVal]) extends JsArr {
  override def equals(a: Any): Boolean = a match {
    case JsArrV(v) => 
      v.length == values.length && 
      { var i = 0; while (i < values.length) { if (v(i) != values(i)) return false; i += 1 }; true }
    case JsArrD(ds) => 
      ds.length == values.length && 
      { 
        var i = 0
        while (i < values.length) {
          values(i) match {
            case jn: JsNum => if (!(jn hasValue ds(i))) return false
            case _ => return false
          }
          i += 1
        }
        true
      }
    case _ => false
  }
  override def toString = {
    val parts = new Array[String](values.length)
    var i = 0
    var n = 0
    while (i < parts.length) { parts(i) = values(i).toString; n += parts(i).length; i += 1 }
    val text = new Array[Char](2 + n + math.max(parts.length-1, 0)*2)
    i = 0
    n = 1
    text(0) = '['
    if (parts.length > 0) {
      parts(0).getChars(0, parts(0).length, text, 1)
      n += parts(0).length
      i = 1
    }
    while (i < parts.length) {
      text(n) = ','
      text(n+1) = ' '
      parts(i).getChars(0, parts(i).length, text, n+2)
      n += parts(i).length + 2
      i += 1
    }
    text(n) = ']'
    new String(text)
  }
}
final case class JsArrD(doubles: Array[Double]) extends JsArr {
  lazy val values = { var i = 0; val v = new Array[JsVal](doubles.length); while (i < doubles.length) { v(i) = JsNum(doubles(i), doubles(i).toString); i += 1 }; v }
  override def equals(a: Any): Boolean = a match {
    case JsArrD(ds) => 
      ds.length == doubles.length && 
      { 
        var i = 0
        while (i < doubles.length) {
          if (doubles(i) != ds(i) && !(doubles(i).isNaN && ds(i).isNaN)) return false
          i += 1
        }
        true
      }
    case jv: JsArrV => jv == this 
    case _ => false
  }
  override def toString = {
    val parts = new Array[String](doubles.length)
    var i = 0
    var n = 0
    while (i < parts.length) {
      val vi = doubles(i)
      parts(i) = if (vi.isNaN || vi.isInfinite) "null" else vi.toString
      n += parts(i).length
      i += 1
    }
    val text = new Array[Char](2 + n + math.max(parts.length-1, 0)*2)
    i = 0
    n = 1
    text(0) = '['
    if (parts.length > 0) {
      parts(0).getChars(0, parts(0).length, text, 1)
      n += parts(0).length
      i = 1
    }
    while (i < parts.length) {
      text(n) = ','
      text(n+1) = ' '
      parts(i).getChars(0, parts(i).length, text, n+2)
      n += parts(i).length + 2
      i += 1
    }
    text(n) = ']'
    new String(text)
  }
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
  def hasDuplicateKeys = ((table eq null) && keys.length > 0) || table.size < keys.length
  override def equals(a: Any): Boolean = a match {
    case JsObj(k, v, t) =>
      if (k.length == 0 && keys.length == 0) true
      else if (k.length != keys.length) false
      else if (hasDuplicateKeys) {
        var i = 0
        while (i < keys.length) {
          if (k(i) != keys(i) || v(i) != values(i)) return false;
          i += 1
        }
        true
      }
      else table == t
    case _ => false
  }
  override def toString = {
    val parts = new Array[String](2 * values.length)
    var i, j = 0
    var n = 0
    while (i < keys.length) { 
      val ks = JsStr.escaped(keys(i))
      val vs = values(i).toString
      n += ks.length + 3 + vs.length
      parts(j) = ks
      j += 1
      parts(j) = vs
      j += 1
      i += 1
    }
    val text = new Array[Char](2 + n + math.max(parts.length-2, 0))
    j = 0
    n = 1
    text(0) = '{'
    if (parts.length > 1) {
      text(1) = '"'
      parts(0).getChars(0, parts(0).length, text, 2)
      n += 1 + parts(0).length
      text(n) = '"'
      text(n+1) = ':'
      parts(1).getChars(0, parts(1).length, text, n+2)
      n += 2 + parts(1).length
      j = 2
    }
    while (j < parts.length) {
      text(n) = ','
      text(n+1) = ' '
      text(n+2) = '"'
      parts(j).getChars(0, parts(j).length, text, n+3)
      n += 3 + parts(j).length
      j += 1
      text(n) = '"'
      text(n+1) = ':'
      parts(j).getChars(0, parts(j).length, text, n+2)
      n += 2 + parts(j).length
      j += 1
    }
    text(n) = '}'
    new String(text)
  }
}
object JsObj { def empty = new JsObj(new Array[String](0), new Array[JsVal](0), Map.empty[String, JsVal]) }

case class JsError(msg: String, from: Int, to: Int, because: Option[JsError]) extends JsResult {}
