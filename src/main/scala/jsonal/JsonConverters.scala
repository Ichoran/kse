// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

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
  private val genericAsJsonIsJsonized: Jsonize[AsJson] = new Jsonize[AsJson] { def jsonize(aj: AsJson) = aj.json }
  implicit def asJsonIsImplicitlyJsonized[A <: AsJson] = genericAsJsonIsJsonized.asInstanceOf[Jsonize[A]]
  private val genericJsonOptionIsJsonized: Jsonize[Option[Json]] =
    new Jsonize[Option[Json]] { def jsonize(oj: Option[Json]) = if (oj.isDefined) oj.get else Json.Null }
  implicit def jsonOptionIsImplicitlyJsonized[J <: Json] = genericJsonOptionIsJsonized.asInstanceOf[Jsonize[Option[J]]]
  implicit def jsonArrayIsImplicitlyJsonized[J <: Json] =
    new Jsonize[Array[J]] { def jsonize(aj: Array[J]) = Json ~~ aj ~~ Json }
  implicit def jsonMapIsImplicitlyJsonized[J <: Json] =
    new Jsonize[collection.Map[String, J]] { def jsonize(mj: collection.Map[String, J]) = Json ~~ mj ~~ Json }
}
