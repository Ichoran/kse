// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import scala.language.higherKinds

import scala.util.control.NonFatal

trait PriorityThreeJsonConverters {
  implicit def implicitJsonizationPassesThroughOption[A](implicit jser: Jsonize[A]) = 
    new Jsonize[Option[A]] { def jsonize(o: Option[A]) = o match { case None => Json.Null; case Some(a) => jser.jsonize(a) }}
  implicit def implicitJsonizationPassesThroughArray[A](implicit jser: Jsonize[A]) =
    new Jsonize[Array[A]] { def jsonize(a: Array[A]) = Json.Arr ~~ a ~~ Json.Arr } 
  implicit def implicitJsonizationPassesThroughMap[A, M[String, A] <: collection.Map[String, A]](implicit jser: Jsonize[A]) =
    new Jsonize[M[String, A]]{ def jsonize(m: M[String, A]) = Json.Obj ~~ m ~~ Json.Obj }
}

trait PriorityTwoJsonConverters extends PriorityThreeJsonConverters {
  implicit def optionIsImplicitlyJsonized[A <: AsJson] = new Jsonize[Option[A]] {
    def jsonize(o: Option[A]) = o match { case None => Json.Null; case Some(a) => a.json }
  }
  implicit def arrayIsImplicitlyJsonized[A <: AsJson] = new Jsonize[Array[A]]{ def jsonize(a: Array[A]) = Json.Arr(a.map(_.json)) }
  implicit def mapIsImplicitlyJsonized[A <: AsJson] =
    new Jsonize[collection.Map[String, A]]{ 
      def jsonize(m: collection.Map[String, A]) = Json.Obj.~~(m)(JsonConverters.asJsonIsImplicitlyJsonized[A]).~~(Json.Obj) 
    }
}

object JsonConverters extends PriorityTwoJsonConverters {
  implicit val booleanIsImplicitlyJsonized = new Jsonize[Boolean] { def jsonize(b: Boolean) = Json.Bool(b) }
  implicit val doubleIsImplicitlyJsonized = new Jsonize[Double] { def jsonize(d: Double) = Json.Num(d) }
  implicit val stringIsImplicitlyJsonized = new Jsonize[String] { def jsonize(s: String) = Json.Str(s) }
  implicit val instantIsImplicitlyJsonized = new Jsonize[java.time.Instant] { 
    def jsonize(i: java.time.Instant) = Json.Str(i.toString)
  }
  private val genericAsJsonIsJsonized: Jsonize[AsJson] = new Jsonize[AsJson] { def jsonize(aj: AsJson) = aj.json }
  implicit def asJsonIsImplicitlyJsonized[A <: AsJson] = genericAsJsonIsJsonized.asInstanceOf[Jsonize[A]]
  private val genericJsonOptionIsJsonized: Jsonize[Option[Json]] =
    new Jsonize[Option[Json]] { def jsonize(oj: Option[Json]) = if (oj.isDefined) oj.get else Json.Null }
  implicit def jsonOptionIsImplicitlyJsonized[J <: Json] = genericJsonOptionIsJsonized.asInstanceOf[Jsonize[Option[J]]]
  implicit def jsonArrayIsImplicitlyJsonized[J <: Json] =
    new Jsonize[Array[J]] { def jsonize(aj: Array[J]) = Json ~~ aj ~~ Json }
  implicit def jsonMapIsImplicitlyJsonized[J <: Json] =
    new Jsonize[collection.Map[String, J]] { def jsonize(mj: collection.Map[String, J]) = Json ~~ mj ~~ Json }

  implicit val booleanFromJson: FromJson[Boolean] = new FromJson[Boolean] {
    def parse(js: Json): Either[JastError, Boolean] = js.bool match {
      case None => Left(JastError("Not a boolean"))
      case Some(b) => Right(b)
    }
  }

  implicit val doubleFromJson: FromJson[Double] = new FromJson[Double] {
    def parse(js: Json): Either[JastError, Double] = js match {
      case Json.Null => Right(Double.NaN)
      case n: Json.Num => Right(n.double)
      case _ => Left(JastError("Not a double"))
    }
  }

  implicit val stringFromJson: FromJson[String] = new FromJson[String] {
    def parse(js: Json): Either[JastError, String] = js match {
      case Json.Str(text) => Right(text)
      case _ => Left(JastError("Not a string"))
    }
  }

  implicit def arrayFromJson[A](implicit fj: FromJson[A], tag: reflect.ClassTag[A]) = new FromJson[Array[A]] {
    def parse(js: Json): Either[JastError, Array[A]] = js match {
      case ja: Json.Arr => fj.parseArray(ja)
      case _ => Left(JastError("Not an array"))
    }
  }

  private val patternForInstant = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z$""".r.pattern

  implicit val instantFromJson: FromJson[java.time.Instant] = new FromJson[java.time.Instant] {
    def parse(js: Json): Either[JastError, java.time.Instant] = js match {
      case Json.Str(text) =>
        if (patternForInstant.matcher(text).matches)
          try { return Right(java.time.Instant.parse(text)) }
          catch { case t if NonFatal(t) => }
        Left(JastError("Not properly formatted as an Instant"))
      case _ => Left(JastError("Not an Instant because not a string"))
    }
  }
}
