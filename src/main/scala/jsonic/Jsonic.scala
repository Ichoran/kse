// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

package kse

package object jsonic {
  import kse.jsonic.ast._

  implicit class JsValCanDoThings(private val j: JsResult) extends AnyVal {
    def isNull = j eq JsNull
    def isTrue = j eq JsTrue
    def isFalse= j eq JsFalse
    def isError= j.isInstanceOf[JsError]
    def bool: Option[Boolean] = j match { case jb: JsBool => Some(jb.value); case _ => None }
    def str: Option[String] = j match { case js: JsStr => Some(js.value); case _ => None }
    def num: Option[JsNum] = j match { case jn: JsNum => Some(jn); case _ => None }
    def dbl: Double = j match { case jn: JsNum if !jn.value.isNaN => jn.value; case _ => Double.NaN }
    def arr: Option[JsArr] = j match { case ja: JsArr => Some(ja); case _ => None }
    def obj: Option[JsObj] = j match { case jo: JsObj => Some(jo); case _ => None }
    def error: Option[JsError] = j match { case je: JsError => Some(je); case _ => None }
    def apply(i: Int): Option[JsVal] = j match { case ja: JsArr if i >= 0 && i < ja.values.length => Some(ja.values(i)); case _ => None }
    def apply(s: String): Option[JsVal] = j match { case jo: JsObj => jo(s); case _ => None }
  }

  implicit class OptionalJsValCanDoThings(private val oj: Option[JsResult]) extends AnyVal {
    def isNull = oj match { case Some(j) => j eq ast.JsNull; case _ => false }    
    def isTrue = oj match { case Some(j) => j eq ast.JsTrue; case _ => false }
    def isFalse= oj match { case Some(j) => j eq ast.JsFalse; case _ => false }
    def bool: Option[Boolean] = oj match { case Some(jb: JsBool) => Some(jb.value); case _ => None }
    def str: Option[String] = oj match { case Some(js: JsStr) => Some(js.value); case _ => None }
    def num: Option[JsNum] = oj match { case Some(jn: JsNum) => Some(jn); case _ => None }
    def dbl: Double = oj match { case Some(jn: JsNum) if !jn.value.isNaN => jn.value; case _ => Double.NaN }
    def arr: Option[JsArr] = oj match { case Some(ja: JsArr) => Some(ja); case _ => None }
    def obj: Option[JsObj] = oj match { case Some(jo: JsObj) => Some(jo); case _ => None }
    def error: Option[JsError] = oj match { case Some(je: JsError) => Some(je); case _ => None }
    def apply(i: Int): Option[JsVal] = oj match { case Some(ja: JsArr) if i >= 0 && i < ja.values.length => Some(ja.values(i)); case _ => None }
    def apply(s: String): Option[JsVal] = oj match { case Some(jo: JsObj) => jo(s); case _ => None }
  }
}
