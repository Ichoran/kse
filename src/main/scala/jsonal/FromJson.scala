// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import scala.language.higherKinds

import java.nio._

/** Classes or objects implementing this trait are able to deserialize objects from a JSON representation. */
trait FromJson[A] {
  /** Recover the object from its JSON representation. */
  def parse(input: Json): Either[JastError, A]

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

  /** Deserialize the object from a InputStream containing its JSON representation, keeping track of the end of parsing. */
  def parse(input: java.io.InputStream, ep: FromJson.Endpoint): Either[JastError, A] = Json.parse(input, ep) match {
    case Left(je)  => Left(je)
    case Right(js) => parse(js)
  }

  /** Recover an array of these objects from their JSON representation */
  def parseArray(input: Json.Arr)(implicit tag: reflect.ClassTag[A]): Either[JastError, Array[A]] = {
    var a = new Array[A](input.size)
    var i = 0
    while (i < input.size) {
      val ji = input(i) match {
        case jx: Json => jx
        case je: JastError => return Left(je)
      }
      parse(ji) match {
        case Left(e) => return Left(e)
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
        case je: JastError => return Left(je)
      }
      parse(ji) match {
        case Left(e) => return Left(e)
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

