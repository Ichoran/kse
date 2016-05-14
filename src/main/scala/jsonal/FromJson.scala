// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import scala.language.higherKinds

import java.nio._

import scala.util.control.NonFatal

/** Classes or objects implementing this trait are able to deserialize objects from a JSON representation. */
trait FromJson[A] {
  /** Recover the object from its JSON representation. */
  def parse(input: Json): Either[JastError, A]

  /** Recover the object from its JSON representation if there was a JSON representation */
  def parseJast(input: Jast): Either[JastError, A] = input match {
    case je: JastError => Left(je)
    case js: Json => parse(js)
  }

  /** Deserialize the object from a string containing its JSON representation, keeping track of the end of parsing. */
  def parse(input: String, i0: Int, iN: Int, ep: FromJson.Endpoint): Either[JastError, A] = Json.parse(input, i0, iN, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Deserialize the object from a string containing its JSON representation. */
  def parse(input: String): Either[JastError, A] = parse(input, 0, input.length, new FromJson.Endpoint(0))

  /** Deserialize the object from a ByteBuffer containing its JSON representation. */
  def parse(input: ByteBuffer): Either[JastError, A] = Json.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Deserialize the object from a CharBuffer containing its JSON representation. */
  def parse(input: CharBuffer): Either[JastError, A] = Json.parse(input) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Deserialize the object from an InputStream containing its JSON representation, keeping track of the end of parsing. */
  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Either[JastError, A] = Json.parse(input, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Deserialize the object from an InputStream containing its JSON representation. */
  def parse(input: java.io.InputStream): Either[JastError, A] = parse(input, ep = null)

  /** Recover an array of these objects from their JSON representation */
  def parseArray(input: Json.Arr)(implicit tag: reflect.ClassTag[A]): Either[JastError, Array[A]] = {
    var a = new Array[A](input.size)
    var i = 0
    while (i < input.size) {
      val ji = input(i) match {
        case jx: Json => jx
        case je: JastError => return Left(JastError("Error retriving index "+i, because = je))
      }
      parse(ji) match {
        case Left(e) => return Left(JastError("Error parsing index " + i, because = e))
        case Right(x) => a(i) = x
      }
      i += 1
    }
    Right(a)
  }

  /** Recover a collection of these objects from their JSON representation */
  def parseTo[Coll[_]](input: Json.Arr)(implicit cbf: collection.generic.CanBuildFrom[Nothing, A, Coll[A]]): Either[JastError, Coll[A]] = {
    val b = cbf()
    var i = 0
    while (i < input.size) {
      val ji = input(i) match {
        case jx: Json => jx
        case je: JastError => return Left(JastError("Error retriving index "+i, because = je))
      }
      parse(ji) match {
        case Left(e) => return Left(JastError("Error parsing index " + i, because = e))
        case Right(x) => b += x
      }
      i += 1
    }
    Right(b.result)
  }
}
object FromJson {
  /** A utility class that holds the last index of a input used for JSON parsing */
  case class Endpoint(var index: Long) {}
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
