package kse.eio

import collection.mutable.ArrayBuffer
import collection.mutable.{HashMap => MHashMap}

import kse.flow._
import kse.jsonal._
import kse.jsonal.JsonConverters._

final class Configgy private (val entries: Vector[Configgy.Singleton], val dictionary: Map[String, Configgy.Value] = Map.empty) {
  final override def toString = entries.mkString
  def gap(lines: Int) = new Configgy(entries :+ Configgy.Gap(lines), dictionary)
  def comment(lines: String*) = new Configgy(entries :+ Configgy.Comment(lines.toArray), dictionary)
  def plain(line: String) = new Configgy(entries :+ Configgy.Literal(Configgy.Value.S(line)), dictionary)
  def plain(json: Json) = new Configgy(entries :+ Configgy.Literal(Configgy.Value.J(json)), dictionary)
  def plain(lines: Array[String]) = new Configgy(entries :+ Configgy.Literal(Configgy.Value.B(lines)), dictionary)
  def entry(key: String) = new Configgy.Configger(this, key)
  def entry(e: Configgy.Entry) = {
    val d2 = dictionary.updated(e.key, e.value)
    if (d2.size > dictionary.size) new Configgy(entries :+ e, d2)
    else {
      val i = entries.indexWhere{
        case ee: Configgy.Entry if e.key == ee.key => true
        case _ => false
      }
      val es2 =
        if (i == entries.length-1) entries.dropRight(1)
        else if (i == 0)           entries.drop(1)
        else if (i > 0)            entries.patch(i, Vector.empty, 1)
        else                       entries
      new Configgy(es2 :+ e, d2)
    }
  }

  def contains(key: String) = dictionary contains key
  def get(key: String) = dictionary get key
}
object Configgy {
  val empty = new Configgy(Vector.empty)
  def apply(entries: Vector[Configgy.Singleton]) = {
    val ee = entries.collect{ case e: Entry => e }
    if (ee.isEmpty) new Configgy(entries)
    else {
      val dict = ee.map{ e => (e.key, e.value) }.toMap
      if (dict.size == ee.size) new Configgy(entries, dict)
      else {
        var vs: List[Configgy.Singleton] = Nil
        val i = entries.reverseIterator
        val seen = collection.mutable.HashMap.empty[String, Value]
        while (i.hasNext) {
          i.next match {
            case e: Entry =>
              if (!seen.contains(e.key)) {
                seen(e.key) = e.value
                vs = e :: vs
              }
            case x => vs = x :: vs
          }
        }
        new Configgy(vs.toVector, seen.toMap)
      }
    }
  }

  final class Configger(val configgy: Configgy, val key: String) {
    def apply(line: String) = configgy.entry(Entry(key, Value.S(line)))
    def apply(json: Json) = configgy.entry(Entry(key, Value.J(json)))
    def apply(lines: Array[String]) = configgy.entry(Entry(key, Value.B(lines)))
    def apply[E, T <: Value.T](e: E)(implicit in: E => T) = configgy.entry(Entry(key, in(e)))
  }

  trait Singleton {
    def literal: String
    final override def toString = literal
  }

  final case class Gap(lines: Int) extends Singleton {
    lazy val literal = if (lines <= 0) "" else "\n"*lines
  }
  final case class Comment(text: Array[String]) extends Singleton {
    lazy val literal = if (text.length == 0) "" else text.mkString("# ", "\n# ", "\n")
  }
  final case class Literal(value: Value.Plain) extends Singleton {
    lazy val literal = value.display + "\n"
  }
  final case class Entry(key: String, value: Value) extends Singleton {
    lazy val literal = s"$key = $value\n"
  }

  sealed trait Value {
    def display: String
    final override def toString = display
  }
  object Value {
    sealed trait Plain extends Value {}
    sealed trait Fancy extends Value {}
    final case class J(json: Json) extends Plain {
      lazy val display = {
        val c = json match {
          case a: Json.Arr => JDisplayJArr
          case o: Json.Obj => JDisplayJObj
          case _           => null
        }
        if (c == null) json.toString
        else {
          val lines = PrettyJson.vector(json).toArray
          if (lines.length > 1 || (lines.length == 1 && lines(0).length > 56)) {
            lines(0) = " " + lines(0).substring(1)
            var last = lines(lines.length - 1)
            var i = last.lastIndexOf(c.r) - 1
            while (i > 0 && last(i).isWhitespace) i -= 1
            if (i > 0) {
              lines(lines.length - 1) = last.substring(0, i+1)
              lines.mkString(c.begin, "\n", c.end)
            }
            else {
              lines(lines.length - 1) = c.end
              lines.mkString(c.begin, "\n", "")
            }
          }
          else if (lines.length == 0) c.lr
          else lines(0)          
        }
      }
    }
    final case class S(line: String) extends Plain {
      def display = line
    }
    final case class B(text: Array[String]) extends Plain {
      def display = 
        if (text.isEmpty) "\"\"\"\n\"\"\""
        else text.mkString("\"\"\"\n", "\n", "\n\"\"\"")
    }
    trait T extends Fancy {
      type E
      def decoded: E
      def encoded: Plain
      final lazy val display = encoded.display
    }
  }
  sealed trait jDisplayConstants {
    def l: Char
    def r: Char
    def lr: String
    def begin: String
    def end: String
  }
  final object JDisplayJArr extends jDisplayConstants {
    val l = '['
    val r = ']'
    val lr = "[]"
    val begin = "[\n"
    val end = "\n]"
  }
  final object JDisplayJObj extends jDisplayConstants {
    val l = '{'
    val r = '}'
    val lr = "{}"
    val begin = "{\n"
    val end = "\n}"
  }

  private def isBlank(line: String): Boolean = {
    var i = 0
    while (i < line.length) {
      if (!line(i).isWhitespace) return false
      i += 1
    }
    true
  }

  private def keyIndex(line: String) = {
    val i = line.indexOf('=')
    if (i >= 0) line(0) match {
      case '[' | '{' | '"' => -1
      case _               => i
    }
    else -1
  }

  private def parseSingle(
    lines: Array[String], index: Int,
    es: ArrayBuffer[Singleton], ds: MHashMap[String, (Int, Value)],
    decoder: String => Option[Value.Plain => Ok[String, Value.T]]
  ): Ok[String, Int] = {
    if (index < 0 || index > lines.length) return No(s"Parse index out of bounds: $index")
    var j = index
    var line = lines(j)
    j += 1
    if (line startsWith "#") {
      while (j < lines.length && lines(j).startsWith("#")) j += 1
      es += Comment(
        lines.slice(index, j).
          map(s => if (s.length > 1 && s(1).isWhitespace) s.substring(2) else s.substring(1))
      )
    }
    else if (isBlank(line)) {
      while (j < lines.length && isBlank(lines(j))) j += 1
      es += Gap(j - index)
    }
    else {
      import Value.Plain
      var k = keyIndex(line)
      val key =
        if (k < 0) ""
        else line.substring(0, k).trim match {
          case x if x.isEmpty =>
            return No(s"Entry on line ${index+1} ")
          case x =>
            k += 1
            while (k < line.length && line(k).isWhitespace) k += 1
            line = line.substring(k)
            x
        }
      var v: Value.Plain = null
      if (line startsWith "\"\"\"") {
        if (line.trim.length > 3) return No(s"Text blocks must start with three quotes with nothing after; on line ${index+1} found $line")
        while (j < lines.length && !lines(j).startsWith("\"\"\"")) j += 1
        if (j == lines.length) return No(s"Text block starting on line ${index+1} does not terminate")
        if (lines(j).trim.length > 3) return No(s"Text blocks must end with three quotes with nothing after; on line ${j+1} found $line")
        v = Value.B(lines.slice(index + 1, j))
        j += 1
      }
      else if (line.startsWith("[") || line.startsWith("{")) {
        val trim = line.trim
        val end = if (line startsWith "[") "]" else "}"
        val name = if (line startsWith "[") "array" else "object"
        val adjoined =
          if (trim endsWith end) trim
          else {
            val sb = new java.lang.StringBuilder
            sb append trim
            sb append '\n'
            while (j < lines.length && !lines(j).startsWith(end)) {
              sb append lines(j)
              sb append '\n'
              j += 1
            }
            if (j == lines.length) return No(s"JSON $name starting on line ${index+1} does not terminate with $end on its own line")
            else if (lines(j).trim != end) return No(s"JSON $name from lines ${index+1} to ${j+1} has stray content after ending $end: ${lines(j)}")
            sb append end
            sb.toString
          }
        v = Value.J(
          Jast.parse(adjoined) match {
            case e: JastError => return No(
              s"Could not parse JSON $name on line${if (index < j) "s" else ""} ${index+1}${if (index < j) s" to ${j+1}" else ""}:\n$e"
            )
            case js: Json => js
          }
        )
        if (!(trim endsWith end)) j += 1
      }
      else if (line.startsWith("\""))
        v = Value.J(Jast.parse(line.trim) match {
          case e: JastError => return No(s"Could not parse JSON string on line ${index+1}:\n$e")
          case js: Json => js
        })
      else line.trim match {
        case "true" | "false" | "null" => 
          v = Value.J(Jast.parse(line.trim) match {
            case e: JastError => return No(s"Strange JSON error:\n$e")
            case js: Json => js
          })
        case x if x.length > 0 && x.head.isDigit => 
          v = Jast.parse(x) match {
            case jj: Json => Value.J(jj)
            case _        => Value.S(x)
          }
        case x =>
          v = Value.S(x)
      }

      if (key.isEmpty) es += Literal(v)
      else {
        val u = decoder(key) match {
          case Some(f) => f(v).mapNo(e => s"Error decoding at line ${index+1}:\n$e").?
          case _       => v
        }
        ds.put(key, (es.length, u)) match {
          case Some((n, _)) => es(n) = null
          case _ =>
        }
        es += Entry(key, u)
      }
    }
    Yes(j)
  }
  def parse(lines: Array[String], decoder: String => Option[Value.Plain => Ok[String, Value.T]] = _ => None): Ok[String, Configgy] = {
    val es = ArrayBuffer.empty[Singleton]
    val ds = MHashMap.empty[String, (Int, Value)]
    var i = 0
    while (i < lines.length) i = parseSingle(lines, i, es, ds, decoder).?

    val vb = Vector.newBuilder[Singleton]
    i = 0
    while (i < es.length) {
      val e = es(i)
      if (e ne null) vb += e
      i += 1
    }
    Yes(new Configgy(vb.result, ds.mapValues(_._2).toMap))
  }
}
