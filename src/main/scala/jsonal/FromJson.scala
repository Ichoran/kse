// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import java.nio._

import scala.language.higherKinds

import scala.util.control.NonFatal

import kse.flow._

/** Classes or objects implementing this trait are able to deserialize objects from a JSON representation. */
trait FromJson[A] {
  /** Recover the object from its JSON representation. */
  def parse(input: Json): Jast.To[A]

  /** Recover the object from its JSON representation if there was a JSON representation */
  def parseJast(input: Jast): Jast.To[A] = input match {
    case je: JastError => No(je)
    case js: Json => parse(js)
  }

  /** Deserialize the object from a string containing its JSON representation, keeping track of the end of parsing. */
  def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Jast.To[A] = parse(Json.parse(input, i0, iN, ep).?)

  /** Deserialize the object from a string containing its JSON representation. */
  def parse(input: String): Jast.To[A] = parse(input, 0, input.length, new FromJson.Endpoint(0))

  /** Deserialize the object from a ByteBuffer containing its JSON representation. */
  def parse(input: ByteBuffer): Jast.To[A] = parse(Json.parse(input).?)

  /** Deserialize the object from a CharBuffer containing its JSON representation. */
  def parse(input: CharBuffer): Jast.To[A] = parse(Json.parse(input).?)

  /** Deserialize the object from an InputStream containing its JSON representation, keeping track of the end of parsing. */
  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Jast.To[A] = parse(Json.parse(input, ep).?)

  /** Deserialize the object from an InputStream containing its JSON representation. */
  def parse(input: java.io.InputStream): Jast.To[A] = parse(input, ep = null)

  /** Recover an array of these objects from their JSON representation */
  def parseArray(input: Json.Arr)(implicit tag: reflect.ClassTag[A]): Jast.To[Array[A]] = {
    var a = new Array[A](input.size)
    var i = 0
    while (i < input.size) {
      val ji = input(i) match {
        case jx: Json => jx
        case je: JastError => return No(JastError("Error retriving index "+i, because = je))
      }
      parse(ji) match {
        case No(e) => return No(JastError("Error parsing index " + i, because = e))
        case Yes(x) => a(i) = x
      }
      i += 1
    }
    Yes(a)
  }

  /** Recover a collection of these objects from their JSON representation */
  def parseTo[Coll[_]](input: Json.Arr)(implicit cbf: collection.generic.CanBuildFrom[Nothing, A, Coll[A]]): Jast.To[Coll[A]] = {
    val b = cbf()
    var i = 0
    while (i < input.size) {
      val ji = input(i) match {
        case jx: Json => jx
        case je: JastError => return No(JastError("Error retriving index "+i, because = je))
      }
      parse(ji) match {
        case No(e) => return No(JastError("Error parsing index " + i, because = e))
        case Yes(x) => b += x
      }
      i += 1
    }
    Yes(b.result)
  }
}
object FromJson {
  /** A utility class that holds the last index of a input used for JSON parsing */
  case class Endpoint(var index: Long) {}

  /** Helper method that makes an instance of a FromJson */
  def apply[A](name: String)(pf: PartialFunction[Json, Jast.To[A]]): FromJson[A] = new FromJson[A] {
    def parse(json: Json): Jast.To[A] =
      pf.applyOrElse(json, (_: Json) => No(JastError(s"Incorrect structure for JSON representation of $name")))
  }

  def obj[A, Z](
    name: String,
    fa: (String, Json => Jast.To[A])
  )(
    zf: (Json.Obj, A) => Ok[JastError, Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Ok[JastError, Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        zf(o, a)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }

  def obj[A, B, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B])
  )(
    zf: (Json.Obj, A, B) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        zf(o, a, b)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }


  def obj[A, B, C, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C])
  )(
    zf: (Json.Obj, A, B, C) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        val c = o(fc._1) match { 
          case jj: Json => fc._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fc._1}"))
        }
        zf(o, a, b, c)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }

  def obj[A, B, C, D, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D])
  )(
    zf: (Json.Obj, A, B, C, D) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        val c = o(fc._1) match { 
          case jj: Json => fc._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fc._1}"))
        }
        val d = o(fd._1) match { 
          case jj: Json => fd._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fd._1}"))
        }
        zf(o, a, b, c, d)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }

  def obj[A, B, C, D, E, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E])
  )(
    zf: (Json.Obj, A, B, C, D, E) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        val c = o(fc._1) match { 
          case jj: Json => fc._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fc._1}"))
        }
        val d = o(fd._1) match { 
          case jj: Json => fd._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fd._1}"))
        }
        val e = o(fe._1) match { 
          case jj: Json => fe._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fe._1}"))
        }
        zf(o, a, b, c, d, e)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }

  def obj[A, B, C, D, E, F, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F])
  )(
    zf: (Json.Obj, A, B, C, D, E, F) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        val c = o(fc._1) match { 
          case jj: Json => fc._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fc._1}"))
        }
        val d = o(fd._1) match { 
          case jj: Json => fd._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fd._1}"))
        }
        val e = o(fe._1) match { 
          case jj: Json => fe._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fe._1}"))
        }
        val f = o(ff._1) match { 
          case jj: Json => ff._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${ff._1}"))
        }
        zf(o, a, b, c, d, e, f)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }

  def obj[A, B, C, D, E, F, G, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F]),
    fg: (String, Json => Jast.To[G])
  )(
    zf: (Json.Obj, A, B, C, D, E, F, G) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        val c = o(fc._1) match { 
          case jj: Json => fc._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fc._1}"))
        }
        val d = o(fd._1) match { 
          case jj: Json => fd._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fd._1}"))
        }
        val e = o(fe._1) match { 
          case jj: Json => fe._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fe._1}"))
        }
        val f = o(ff._1) match { 
          case jj: Json => ff._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${ff._1}"))
        }
        val g = o(fg._1) match { 
          case jj: Json => fg._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fg._1}"))
        }
        zf(o, a, b, c, d, e, f, g)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }

  def obj[A, B, C, D, E, F, G, H, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F]),
    fg: (String, Json => Jast.To[G]),
    fh: (String, Json => Jast.To[H])
  )(
    zf: (Json.Obj, A, B, C, D, E, F, G, H) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        val c = o(fc._1) match { 
          case jj: Json => fc._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fc._1}"))
        }
        val d = o(fd._1) match { 
          case jj: Json => fd._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fd._1}"))
        }
        val e = o(fe._1) match { 
          case jj: Json => fe._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fe._1}"))
        }
        val f = o(ff._1) match { 
          case jj: Json => ff._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${ff._1}"))
        }
        val g = o(fg._1) match { 
          case jj: Json => fg._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fg._1}"))
        }
        val h = o(fh._1) match { 
          case jj: Json => fh._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fh._1}"))
        }
        zf(o, a, b, c, d, e, f, g, h)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }

  def obj[A, B, C, D, E, F, G, H, I, Z](
    name: String,
    fa: (String, Json => Jast.To[A]),
    fb: (String, Json => Jast.To[B]),
    fc: (String, Json => Jast.To[C]),
    fd: (String, Json => Jast.To[D]),
    fe: (String, Json => Jast.To[E]),
    ff: (String, Json => Jast.To[F]),
    fg: (String, Json => Jast.To[G]),
    fh: (String, Json => Jast.To[H]),
    fi: (String, Json => Jast.To[I])
  )(
    zf: (Json.Obj, A, B, C, D, E, F, G, H, I) => Jast.To[Z]
  ): FromJson[Z] = new FromJson[Z] {
    def parse(json: Json): Jast.To[Z] = json match {
      case o: Json.Obj => 
        val a = o(fa._1) match { 
          case jj: Json => fa._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fa._1}"))
        }
        val b = o(fb._1) match { 
          case jj: Json => fb._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fb._1}"))
        }
        val c = o(fc._1) match { 
          case jj: Json => fc._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fc._1}"))
        }
        val d = o(fd._1) match { 
          case jj: Json => fd._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fd._1}"))
        }
        val e = o(fe._1) match { 
          case jj: Json => fe._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fe._1}"))
        }
        val f = o(ff._1) match { 
          case jj: Json => ff._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${ff._1}"))
        }
        val g = o(fg._1) match { 
          case jj: Json => fg._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fg._1}"))
        }
        val h = o(fh._1) match { 
          case jj: Json => fh._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fh._1}"))
        }
        val i = o(fi._1) match { 
          case jj: Json => fi._2(jj).?
          case _ => return No(JastError(s"$name is missing field ${fi._1}"))
        }
        zf(o, a, b, c, d, e, f, g, h, i)
      case _ => No(JastError(s"$name must be encoded in a JSON object"))
    }
  }
}

/** A class for functionality that allows one to parse to a a JSON AST. */
class ParseToJast private[jsonal] (protected val isRelaxed: Boolean) {
  /** Parses a section of a `String` and indicates the end of the parse in a `FromJson.Endpoint` class */
  def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Jast =
    if (input eq null) Json.Null
    else {
      val jsp = (new JsonStringParser).relaxedNumbers(isRelaxed)
      val ans = jsp.parseVal(input, math.max(0, i0), math.min(iN, input.length))
      if (ep ne null) jsp.setEndpoint(ep)
      ans
    }

  /** Parses a string to a JSON AST. */
  def parse(input: String): Jast =
    if (input eq null) Json.Null
    else (new JsonStringParser).relaxedNumbers(isRelaxed).parseVal(input, 0, input.length)

  /** Parses a `ByteBuffer`, starting at its current position, to a JSON AST. */
  def parse(input: ByteBuffer): Jast =
    (new JsonByteBufferParser).relaxedNumbers(isRelaxed).parseVal(input)

  /** Parses a `CharBuffer`, starting at its current position, to a JSON AST. */
  def parse(input: CharBuffer): Jast =
    (new JsonCharBufferParser).relaxedNumbers(isRelaxed).parseVal(input)

  /** Parses an input stream to a JSON AST, keeping a record of the number of bytes consumed (in `ep`).
    *
    * Note: if the end of the stream is not reached, no indication will be given.  The stream will not be closed.
    */
  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Jast = {
    val jrp = (new JsonRecyclingParser).refresh(JsonRecyclingParser recycleInputStream input).relaxedNumbers(isRelaxed)
    val ans = jrp.recycle().parseVal()
    if ((ep ne null) && ans.isInstanceOf[Json]) ep.index = jrp.offset + jrp.start
    ans
  }

  /** Parses an input stream to a JSON AST. */
  def parse(input: java.io.InputStream): Jast = parse(input, null)

  /** Parses the contents of a file to a JSON AST.  The file will be closed.
    *
    * Note: if the file contains additional information beyond the end of the JSON object, it will be ignored.
    */
  def parse(filename: java.io.File): Jast = {
    if (!filename.exists) JastError("File does not exist: "+filename.getPath)
    else {
      try {
        val fis = new java.io.FileInputStream(filename)
        try { parse(fis) }
        finally { fis.close }
      }
      catch { 
        case t if NonFatal(t) => JastError("File read error: "+t.getClass.getName+" "+t.getMessage) 
      }
    }
  }
}

trait FromJsonImplicit[A] {
  implicit def fromJson: FromJson[A]
}

trait FromJsonCompanion[A] extends FromJson[A] with FromJsonImplicit[A] {
  final implicit def fromJson: FromJson[A] = this
}

trait FromJsonCapable[A] extends FromJson[A] with FromJsonImplicit[A] {
  final def parse(json: Json): Jast.To[A] = fromJson.parse(json)
}
