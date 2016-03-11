// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

object JsonConverters {
  implicit val booleanIsImplicitlyJsonized = new Jsonize[Boolean] { def jsonize(b: Boolean) = Json.Bool(b) }
  implicit val doubleIsImplicitlyJsonized = new Jsonize[Double] { def jsonize(d: Double) = Json.Num(d) }
  implicit val stringIsImplicitlyJsonized = new Jsonize[String] { def jsonize(s: String) = Json.Str(s) }
  implicit def asJsonIsImplicitlyJsonized[A <: AsJson] = new Jsonize[A] { def jsonize(a: A) = a.json }
  implicit def implicitJsonizationPassesThroughOption[A](implicit jser: Jsonize[A]) = 
    new Jsonize[Option[A]] { def jsonize(o: Option[A]) = o match { case None => Json.Null; case Some(a) => jser.jsonize(a) }}
}
