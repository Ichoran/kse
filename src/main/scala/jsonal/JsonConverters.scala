// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import scala.language.higherKinds

import scala.util.control.NonFatal

import kse.flow._

trait PriorityThreeJsonConverters {
  implicit def implicitJsonizationPassesThroughOption[A](implicit jser: Jsonize[A]) = 
    new Jsonize[Option[A]] { def jsonize(o: Option[A]) = o match { case None => Json.Null; case Some(a) => jser.jsonize(a) }}
  implicit def implicitJsonizationPassesThroughSeq[A, CC[A] <: collection.Seq[A]](implicit jser: Jsonize[A]) =
    new Jsonize[CC[A]] { def jsonize(cca: CC[A]) = Json.Arr ~~ cca ~~ Json.Arr }
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
  // Handy abbreviation to summon a FromJson parse instance
  @inline def Fj[A](implicit fj: FromJson[A]): Json => Jast.To[A] = fj.parse _

  //  This is fast and safe!
  private[this] val encoderOfUrlBase64 = java.util.Base64.getUrlEncoder

  // Utility function for decoder.  Note that 'G' = 'a' - 26 and -4 = '0' - 52.
  private[this] def c2i(c: Char) =
    if (c >= 'A' && c <= 'Z') c - 'A'
    else if (c >= 'a' && c <= 'z') c - 'G'
    else if (c >= '0' && c <= '9') c + 4;
    else if (c == '-') 62
    else if (c == '_') 63
    else -1

  // Java default decoder is not as fast as this one, and also throws exceptions which we want to avoid
  private[this] def decodeUrl64(s: String, i0: Int = 0, iN: Int = Int.MaxValue): Jast.To[Array[Byte]] = {
      var m = math.min(s.length, iN)
      var i = math.max(i0, 0)
      while (m > i && s.charAt(m-1) == '=') m -= 1
      val a = new Array[Byte](((3L*(m-i)) >> 2).toInt)
      var j = 0
      while (i < m - 3) { 
        var x = (c2i(s.charAt(i)) << 18)
        x = x | (c2i(s.charAt(i+1)) << 12)
        x = x | (c2i(s.charAt(i+2)) << 6)
        x = x | (c2i(s.charAt(i+3)))
        a(j+2) = (x&0xFF).toByte
        a(j+1) = ((x >> 8)&0xFF).toByte
        a(j) = (x >> 16).toByte
        if (x == -1) return No(JastError("Invalid character in Base64 encoded string", i))
        i += 4
        j += 3
      }
      var x = 0
      var n = 0
      if (i < m) {
        if (i == m - 1) return No(JastError("A single Base64 character (mod 4) is not a valid encoding!"))
        else if (i == m - 2) {
          val x = (c2i(s.charAt(i)) << 6) | c2i(s.charAt(i+1))
          if ((x & 0xF) != 0) return No(JastError("Wrong encoding at end of block: these two chars do not fit in a byte"))
          a(j) = (x >> 4).toByte
        }
        else {
          val x = (c2i(s.charAt(i)) << 12) | (c2i(s.charAt(i+1)) << 6) | c2i(s.charAt(i+2))
          if ((x & 0x3) != 0) return No(JastError("Wrong encoding at end of block: these three chars do not fit in two bytes"))
          a(j+1) = ((x >> 2) & 0xFF).toByte
          a(j) = ((x >> 10) & 0xFF).toByte
        }
      }
      Yes(a)
    }


  implicit val booleanIsImplicitlyJsonized = new Jsonize[Boolean] { def jsonize(b: Boolean) = Json.Bool(b) }
  implicit val intIsImplicitlyJsonized = new Jsonize[Int] { def jsonize(i: Int) = Json.Num(i) }
  implicit val longIsImplictlyJsonized = new Jsonize[Long] { def jsonize(l: Long) = Json.Num(l) }
  @deprecated("Direct Float to Double conversion can create lots of insignificant decimal digits.  Manually use .toString.toDouble for a nice decimal representation or .toDouble if you do not care.", "0.3.0")
  implicit val floatIsImplicitlyJsonized = new Jsonize[Float] { def jsonize(f: Float) = Json.Num(f.toDouble) }
  implicit val doubleIsImplicitlyJsonized = new Jsonize[Double] { def jsonize(d: Double) = Json.Num(d) }
  implicit val stringIsImplicitlyJsonized = new Jsonize[String] { def jsonize(s: String) = Json.Str(s) }
  implicit val durationIsImplicitlyJsonized = new Jsonize[java.time.Duration] {
    def jsonize(d: java.time.Duration) = Json.Str(d.toString)
  }
  implicit val instantIsImplicitlyJsonized = new Jsonize[java.time.Instant] { 
    def jsonize(i: java.time.Instant) = Json.Str(i.toString)
  }
  implicit val localDateTimeIsImplicitlyJsonized = new Jsonize[java.time.LocalDateTime] {
    def jsonize(ldt: java.time.LocalDateTime) = Json.Str(ldt.toString)
  }
  implicit val offsetDateTimeIsImplicitlyJsonzed = new Jsonize[java.time.OffsetDateTime] {
    def jsonize(odt: java.time.OffsetDateTime) = Json.Str(odt.toString)
  }
  implicit val zonedDateTimeIsImplicitlyJsonized = new Jsonize[java.time.ZonedDateTime] {
    def jsonize(zdt: java.time.ZonedDateTime) = Json.Str(zdt.toString)
  }
  implicit val byteArrayIsImplicitlyBase64Jasonized = new Jsonize[Array[Byte]] {
    def jsonize(ab: Array[Byte]) = Json.Str(encoderOfUrlBase64.encodeToString(ab))
  }
  private val genericAsJsonIsJsonized: Jsonize[AsJson] = new Jsonize[AsJson] { def jsonize(aj: AsJson) = aj.json }
  implicit def asJsonIsImplicitlyJsonized[A <: AsJson] = genericAsJsonIsJsonized.asInstanceOf[Jsonize[A]]
  private val genericJsonOptionIsJsonized: Jsonize[Option[Json]] =
    new Jsonize[Option[Json]] { def jsonize(oj: Option[Json]) = if (oj.isDefined) oj.get else Json.Null }
  implicit def jsonOptionIsImplicitlyJsonized[J <: Json] = genericJsonOptionIsJsonized.asInstanceOf[Jsonize[Option[J]]]
  implicit val bareNoneIsImplicitlyJsonized: Jsonize[None.type] = new Jsonize[None.type] { def jsonize(n: None.type) = Json.Null }
  implicit def jsonArrayIsImplicitlyJsonized[J <: Json] =
    new Jsonize[Array[J]] { def jsonize(aj: Array[J]) = Json ~~ aj ~~ Json }
  implicit def jsonMapIsImplicitlyJsonized[J <: Json] =
    new Jsonize[collection.Map[String, J]] { def jsonize(mj: collection.Map[String, J]) = Json ~~ mj ~~ Json }

  implicit def tuple2isImplicitlyJsonized[A, B](implicit ja: Jsonize[A], jb: Jsonize[B]) =
    new Jsonize[(A, B)] { def jsonize(x: (A, B)) = Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ Json }
  implicit def tuple3isImplicitlyJsonized[A, B, C](implicit ja: Jsonize[A], jb: Jsonize[B], jc: Jsonize[C]) =
    new Jsonize[(A, B, C)] { def jsonize(x: (A, B, C)) = Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ jc.jsonize(x._3) ~ Json }
  implicit def tuple4isImplicitlyJsonized[A, B, C, D](implicit ja: Jsonize[A], jb: Jsonize[B], jc: Jsonize[C], jd: Jsonize[D]) =
    new Jsonize[(A, B, C, D)] {
      def jsonize(x: (A, B, C, D)) = Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ jc.jsonize(x._3) ~ jd.jsonize(x._4) ~ Json
    }
  implicit def tuple5isImplicitlyJsonized[A, B, C, D, E](
    implicit ja: Jsonize[A], jb: Jsonize[B], jc: Jsonize[C], jd: Jsonize[D], je: Jsonize[E]
  ) = new Jsonize[(A, B, C, D, E)] {
    def jsonize(x: (A, B, C, D, E)) =
      Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ jc.jsonize(x._3) ~ jd.jsonize(x._4) ~ je.jsonize(x._5) ~ Json
  }
  implicit def tuple6isImplicitlyJsonized[A, B, C, D, E, F](
    implicit ja: Jsonize[A], jb: Jsonize[B], jc: Jsonize[C], jd: Jsonize[D], je: Jsonize[E], jf: Jsonize[F]
  ) = new Jsonize[(A, B, C, D, E, F)] {
    def jsonize(x: (A, B, C, D, E, F)) =
      Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ jc.jsonize(x._3) ~ jd.jsonize(x._4) ~ je.jsonize(x._5) ~ jf.jsonize(x._6) ~ Json
  }
  implicit def tuple7isImplicitlyJsonized[A, B, C, D, E, F, G](
    implicit ja: Jsonize[A], jb: Jsonize[B], jc: Jsonize[C], jd: Jsonize[D], je: Jsonize[E], jf: Jsonize[F], jg: Jsonize[G]
  ) = new Jsonize[(A, B, C, D, E, F, G)] {
    def jsonize(x: (A, B, C, D, E, F, G)) =
      Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ jc.jsonize(x._3) ~ jd.jsonize(x._4) ~
             je.jsonize(x._5) ~ jf.jsonize(x._6) ~ jg.jsonize(x._7) ~ Json
  }
  implicit def tuple8isImplicitlyJsonized[A, B, C, D, E, F, G, H](
    implicit ja: Jsonize[A], jb: Jsonize[B], jc: Jsonize[C], jd: Jsonize[D],
             je: Jsonize[E], jf: Jsonize[F], jg: Jsonize[G], jh: Jsonize[H]
  ) = new Jsonize[(A, B, C, D, E, F, G, H)] {
    def jsonize(x: (A, B, C, D, E, F, G, H)) =
      Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ jc.jsonize(x._3) ~ jd.jsonize(x._4) ~
             je.jsonize(x._5) ~ jf.jsonize(x._6) ~ jg.jsonize(x._7) ~ jh.jsonize(x._8) ~ Json
  }
  implicit def tuple9isImplicitlyJsonized[A, B, C, D, E, F, G, H, I](
    implicit ja: Jsonize[A], jb: Jsonize[B], jc: Jsonize[C], jd: Jsonize[D], je: Jsonize[E],
             jf: Jsonize[F], jg: Jsonize[G], jh: Jsonize[H], ji: Jsonize[I]
  ) = new Jsonize[(A, B, C, D, E, F, G, H, I)] {
    def jsonize(x: (A, B, C, D, E, F, G, H, I)) =
      Json ~ ja.jsonize(x._1) ~ jb.jsonize(x._2) ~ jc.jsonize(x._3) ~ jd.jsonize(x._4) ~ je.jsonize(x._5) ~
             jf.jsonize(x._6) ~ jg.jsonize(x._7) ~ jh.jsonize(x._8) ~ ji.jsonize(x._9) ~ Json
  }

  implicit val jsonFromJson: FromJson[Json] = new FromJson[Json] {
    def parse(js: Json): Jast.To[Json] = Yes(js)
  }

  implicit val jsonObjFromJson: FromJson[Json.Obj] = new FromJson[Json.Obj] {
    def parse(js: Json): Jast.To[Json.Obj] = js match {
      case o: Json.Obj => Yes(o)
      case _ => No(JastError("Not a JSON object"))
    }
  }

  implicit val jsonArrFromJson: FromJson[Json.Arr] = new FromJson[Json.Arr] {
    def parse(js: Json): Jast.To[Json.Arr] = js match {
      case a: Json.Arr => Yes(a)
      case _ => No(JastError("Not a JSON array"))
    }
  }

  implicit val booleanFromJson: FromJson[Boolean] = new FromJson[Boolean] {
    def parse(js: Json): Jast.To[Boolean] = js.bool match {
      case Some(b) => Yes(b)
      case _ => No(JastError("Not a boolean"))
    }
  }

  implicit val intFromJson: FromJson[Int] = new FromJson[Int] {
    def parse(js: Json): Jast.To[Int] = js match {
      case n: Json.Num if n.isLong =>
        val x = n.long
        if (x < Int.MinValue || x > Int.MaxValue) No(JastError("Number out of Int range"))
        else Yes(x.toInt)
      case _ => No(JastError("Not an int"))
    }
  }

  implicit val longFromJson: FromJson[Long] = new FromJson[Long] {
    def parse(js: Json): Jast.To[Long] = js match {
      case n: Json.Num if n.isLong => Yes(n.long)
      case _ => No(JastError("Not a long"))
    }
  }

  implicit val doubleFromJson: FromJson[Double] = new FromJson[Double] {
    def parse(js: Json): Jast.To[Double] = js match {
      case Json.Null => Yes(Double.NaN)
      case n: Json.Num => Yes(n.double)
      case _ => No(JastError("Not a double"))
    }
  }

  implicit val stringFromJson: FromJson[String] = new FromJson[String] {
    def parse(js: Json): Jast.To[String] = js match {
      case s: Json.Str => Yes(s.text)
      case _ => No(JastError("Not a string"))
    }
  }

  implicit def byteArrayFromBase64Json: FromJson[Array[Byte]] = new FromJson[Array[Byte]] {
    def parse(js: Json) = js match {
      case s: Json.Str => decodeUrl64(s.text)
      case _ => No(JastError("Not a string"))
    }
  }

  implicit def arrayFromJson[A](implicit fj: FromJson[A], tag: reflect.ClassTag[A]) = new FromJson[Array[A]] {
    def parse(js: Json): Jast.To[Array[A]] = js match {
      case ja: Json.Arr => fj.parseArray(ja)
      case _ => No(JastError("Not an array"))
    }
  }

  implicit def collectionFromJson[A, Coll[_]](implicit fj: FromJson[A], cbf: collection.generic.CanBuildFrom[Nothing, A, Coll[A]]) = new FromJson[Coll[A]] {
    def parse(js: Json): Jast.To[Coll[A]] = js match {
      case ja: Json.Arr => fj.parseTo[Coll](ja)
      case _ => No(JastError("Not a JSON array"))
    }
  }

  implicit def mapFromJson[A, M[String, _] <: collection.Map[String, _]](implicit fj: FromJson[A], cbf: collection.generic.CanBuildFrom[Nothing, (String, A), M[String, A]]) = new FromJson[M[String, A]] {
    def parse(js: Json): Jast.To[M[String, A]] = js match {
      case jo: Json.Obj =>
        val b = cbf()
        jo.foreach{ (k,v) =>
          val a = fj.parse(v) match {
            case Yes(x) => x
            case No(e) => return No(JastError(f"Error parsing key '$k'", because = e))
          }
          b += ((k, a))
        }
        Yes(b.result)
      case _ => No(JastError("Not a JSON object"))
    }
  }

  private[this] val myRightNone = Yes(None)

  implicit def optionFromJson[A](implicit fj: FromJson[A]) = new FromJson[Option[A]] {
    def parse(js: Json): Jast.To[Option[A]] = js match {
      case jn: Json.Null => myRightNone
      case _ => fj.parse(js) match {
        case Yes(r) => Yes(Some(r))
        case n: No[JastError] => n
      }
    }
  }

  implicit def eitherFromJson[L, R](implicit fjl: FromJson[L], fjr: FromJson[R]) = new FromJson[Either[L, R]] {
    def parse(js: Json): Jast.To[Either[L, R]] = fjr.parse(js) match {
      case Yes(r) => Yes(Right(r))
      case _ => fjl.parse(js) match {
        case Yes(l) => Yes(Left(l))
        case No(el) => No(JastError("Neither option was valid", because = el))
      }
    }
  }

  implicit def okFromJson[N, Y](implicit fjn: FromJson[N], fjy: FromJson[Y]) = new FromJson[Ok[N, Y]] {
    def parse(js: Json): Jast.To[Ok[N, Y]] = fjy.parse(js) match {
      case y: Yes[Y] => Yes(y)
      case _ => fjn.parse(js) match {
        case Yes(n) => Yes(No(n))
        case No(el) => No(JastError("Neither option was valid", because = el))
      }
    }
  }

  private val patternForDuration = """PT(?:\d+H)?(?:\d+M)?(?:\d+(?:.\d+)?S)?""".r.pattern
  private val patternForInstant = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,9})?Z$""".r.pattern
  private val patternForLocalDateTime = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?""".r.pattern
  private val patternForOffsetDateTime = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?(?:Z|[+-]\d{2}:\d{2})""".r.pattern
  private val patternForZonedDateTime = """-?\d{4,}-\d{2}-\d{2}T\d{2}:\d{2}(?::\d{2}(?:\.\d{1,9})?)?(?:Z|[+-]\d{2}:\d{2})(?:\[[^\]]+])?""".r.pattern

  implicit val durationFromJson: FromJson[java.time.Duration] = new FromJson[java.time.Duration] {
    def parse(js: Json): Jast.To[java.time.Duration] = js match {
      case Json.Str(text) =>
        if (patternForDuration.matcher(text).matches)
          try { return Yes(java.time.Duration.parse(text)) }
          catch { case t if NonFatal(t) => }
        No(JastError("Not properly formatted as an Duration"))
      case _ => No(JastError("Not an Duration because not a string"))
    }
  }

  implicit val instantFromJson: FromJson[java.time.Instant] = new FromJson[java.time.Instant] {
    def parse(js: Json): Jast.To[java.time.Instant] = js match {
      case s: Json.Str =>
        if (patternForInstant.matcher(s.text).matches)
          try { return Yes(java.time.Instant.parse(s.text)) }
          catch { case t if NonFatal(t) => }
        No(JastError("Not properly formatted as an Instant"))
      case _ => No(JastError("Not an Instant because not a string"))
    }
  }

  implicit val localDateTimeFromJson: FromJson[java.time.LocalDateTime] = new FromJson[java.time.LocalDateTime] {
    def parse(js: Json): Jast.To[java.time.LocalDateTime] = js match {
      case s: Json.Str =>
        if (patternForLocalDateTime.matcher(s.text).matches)
          try { return Yes(java.time.LocalDateTime.parse(s.text)) }
          catch { case t if NonFatal(t) => }
        No(JastError("Not properly formatted as a LocalDateTime"))
      case _ => No(JastError("Not a LocalDateTime because not a string"))
    }
  }

  implicit val offsetDateTimeFromJson: FromJson[java.time.OffsetDateTime] = new FromJson[java.time.OffsetDateTime] {
    def parse(js: Json): Jast.To[java.time.OffsetDateTime] = js match {
      case s: Json.Str =>
        if (patternForOffsetDateTime.matcher(s.text).matches)
          try { return Yes(java.time.OffsetDateTime.parse(s.text)) }
          catch { case t if NonFatal(t) => }
        No(JastError("Not properly formatted as a OffsetDateTime"))
      case _ => No(JastError("Not a OffsetDateTime because not a string"))
    }
  }

  implicit val zonedDateTimeFromJson: FromJson[java.time.ZonedDateTime] = new FromJson[java.time.ZonedDateTime] {
    def parse(js: Json): Jast.To[java.time.ZonedDateTime] = js match {
      case s: Json.Str =>
        if (patternForZonedDateTime.matcher(s.text).matches)
          try { return Yes(java.time.ZonedDateTime.parse(s.text)) }
          catch { case t if NonFatal(t) => }
        No(JastError("Not properly formatted as a ZonedDateTime"))
      case _ => No(JastError("Not a ZonedDateTime because not a string"))
    }
  }

  implicit def tuple2FromJson[A, B](implicit fja: FromJson[A], fjb: FromJson[B]) = new FromJson[(A, B)] {
    type FJ = Jast.To[(A, B)]
    def parse(js: Json): FJ = js match {
      case ja: Json.Arr =>
        if (ja.size != 2) No(JastError("Exactly two elements required."))
        val a = fja.parseJast(ja(0)).?
        val b = fjb.parseJast(ja(1)).?
        Yes((a, b))
      case _ => No(JastError("Not a tuple because not a JSON array"))
    }
  }
  implicit def tuple3FromJson[A, B, C](implicit fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C]) = new FromJson[(A, B, C)] {
    type FJ = Jast.To[(A, B, C)]
    def parse(js: Json): FJ = js match {
      case ja: Json.Arr =>
        if (ja.size != 3) No(JastError("Exactly three elements required."))
        val a = fja.parseJast(ja(0)).?
        val b = fjb.parseJast(ja(1)).?
        val c = fjc.parseJast(ja(2)).?
        Yes((a, b, c))
      case _ => No(JastError("Not a tuple because not a JSON array"))
    }
  }
  implicit def tuple4FromJson[A, B, C, D](implicit fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D]) =
    new FromJson[(A, B, C, D)] {
      type FJ = Jast.To[(A, B, C, D)]
      def parse(js: Json): FJ = js match {
        case ja: Json.Arr =>
          if (ja.size != 4) No(JastError("Exactly four elements required."))
          val a = fja.parseJast(ja(0)).?
          val b = fjb.parseJast(ja(1)).?
          val c = fjc.parseJast(ja(2)).?
          val d = fjd.parseJast(ja(3)).?
          Yes((a, b, c, d))
        case _ => No(JastError("Not a tuple because not a JSON array"))
      }
    }
  implicit def tuple5FromJson[A, B, C, D, E](
    implicit fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E]
  ) = new FromJson[(A, B, C, D, E)] {
    type FJ = Jast.To[(A, B, C, D, E)]
    def parse(js: Json): FJ = js match {
      case ja: Json.Arr =>
        if (ja.size != 5) No(JastError("Exactly five elements required."))
        val a = fja.parseJast(ja(0)).?
        val b = fjb.parseJast(ja(1)).?
        val c = fjc.parseJast(ja(2)).?
        val d = fjd.parseJast(ja(3)).?
        val e = fje.parseJast(ja(4)).?
        Yes((a, b, c, d, e))
      case _ => No(JastError("Not a tuple because not a JSON array"))
    }
  }
  implicit def tuple6FromJson[A, B, C, D, E, F](
    implicit fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E],
             fjf: FromJson[F]
  ) = new FromJson[(A, B, C, D, E, F)] {
    type FJ = Jast.To[(A, B, C, D, E, F)]
    def parse(js: Json): FJ = js match {
      case ja: Json.Arr =>
        if (ja.size != 6) No(JastError("Exactly six elements required."))
        val a = fja.parseJast(ja(0)).?
        val b = fjb.parseJast(ja(1)).?
        val c = fjc.parseJast(ja(2)).?
        val d = fjd.parseJast(ja(3)).?
        val e = fje.parseJast(ja(4)).?
        val f = fjf.parseJast(ja(5)).?
        Yes((a, b, c, d, e, f))
      case _ => No(JastError("Not a tuple because not a JSON array"))
    }
  }
  implicit def tuple7FromJson[A, B, C, D, E, F, G](
    implicit fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E],
             fjf: FromJson[F], fjg: FromJson[G]
  ) = new FromJson[(A, B, C, D, E, F, G)] {
    type FJ = Jast.To[(A, B, C, D, E, F, G)]
    def parse(js: Json): FJ = js match {
      case ja: Json.Arr =>
        if (ja.size != 7) No(JastError("Exactly seven elements required."))
        val a = fja.parseJast(ja(0)).?
        val b = fjb.parseJast(ja(1)).?
        val c = fjc.parseJast(ja(2)).?
        val d = fjd.parseJast(ja(3)).?
        val e = fje.parseJast(ja(4)).?
        val f = fjf.parseJast(ja(5)).?
        val g = fjg.parseJast(ja(6)).?
        Yes((a, b, c, d, e, f, g))
      case _ => No(JastError("Not a tuple because not a JSON array"))
    }
  }
  implicit def tuple8FromJson[A, B, C, D, E, F, G, H](
    implicit fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E],
             fjf: FromJson[F], fjg: FromJson[G], fjh: FromJson[H]
  ) = new FromJson[(A, B, C, D, E, F, G, H)] {
    type FJ = Jast.To[(A, B, C, D, E, F, G, H)]
    def parse(js: Json): FJ = js match {
      case ja: Json.Arr =>
        if (ja.size != 8) No(JastError("Exactly eight elements required."))
        val a = fja.parseJast(ja(0)).?
        val b = fjb.parseJast(ja(1)).?
        val c = fjc.parseJast(ja(2)).?
        val d = fjd.parseJast(ja(3)).?
        val e = fje.parseJast(ja(4)).?
        val f = fjf.parseJast(ja(5)).?
        val g = fjg.parseJast(ja(6)).?
        val h = fjh.parseJast(ja(7)).?
        Yes((a, b, c, d, e, f, g, h))
      case _ => No(JastError("Not a tuple because not a JSON array"))
    }
  }
  implicit def tuple9FromJson[A, B, C, D, E, F, G, H, I](
    implicit fja: FromJson[A], fjb: FromJson[B], fjc: FromJson[C], fjd: FromJson[D], fje: FromJson[E],
             fjf: FromJson[F], fjg: FromJson[G], fjh: FromJson[H], fji: FromJson[I]
  ) = new FromJson[(A, B, C, D, E, F, G, H, I)] {
    type FJ = Jast.To[(A, B, C, D, E, F, G, H, I)]
    def parse(js: Json): FJ = js match {
      case ja: Json.Arr =>
        if (ja.size != 8) No(JastError("Exactly nine elements required."))
        val a = fja.parseJast(ja(0)).?
        val b = fjb.parseJast(ja(1)).?
        val c = fjc.parseJast(ja(2)).?
        val d = fjd.parseJast(ja(3)).?
        val e = fje.parseJast(ja(4)).?
        val f = fjf.parseJast(ja(5)).?
        val g = fjg.parseJast(ja(6)).?
        val h = fjh.parseJast(ja(7)).?
        val i = fji.parseJast(ja(8)).?
        Yes((a, b, c, d, e, f, g, h, i))
      case _ => No(JastError("Not a tuple because not a JSON array"))
    }
  }
}
