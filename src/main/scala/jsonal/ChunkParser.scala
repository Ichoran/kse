// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

/*

package kse.jsonic.parsers

import kse.jsonic.ast._

trait JsonWisdom[@specialized(Byte, Char, Int) A] {
  def in(i: Int, p: JsonParser[A]): Unit
  def in(key: String, p: JsonParser[A]): Unit
  def out(i: Int, p: JsonParser[A]): Unit
  def out(key: String, p: JsonParser[A]): Unit
  def edit(jr: JsResult, p: JsonParser[A]): JsResult
  def fail(je: JsError, p: JsonParser[A]): JsError
  def reset(): Unit
}

class JsonNaiveBytes extends JsonWisdom[Byte] {
  def in(i: Int, p: JsonParser[Byte]) {}
  def in(key: String, p: JsonParser[Byte]) {}
  def out(i: Int, p: JsonParser[Byte]) {}
  def out(key: String, p: JsonParser[Byte]) {}
  def edit(jr: JsResult, p: JsonParser[Byte]) = jr
  def fail(je: JsError, p: JsonParser[Byte]) = je
  def reset() {}
}

class JsonNaiveChars extends JsonWisdom[Char] {
  def in(i: Int, p: JsonParser[Char]) {}
  def in(key: String, p: JsonParser[Char]) {}
  def out(i: Int, p: JsonParser[Char]) {}
  def out(key: String, p: JsonParser[Char]) {}
  def edit(jr: JsResult, p: JsonParser[Char]) = jr
  def fail(je: JsError, p: JsonParser[Char]) = je
  def reset() {}
}

class JsonNaiveCodePoints extends JsonWisdom[Int] {
  def in(i: Int, p: JsonParser[Int]) {}
  def in(key: String, p: JsonParser[Int]) {}
  def out(i: Int, p: JsonParser[Int]) {}
  def out(key: String, p: JsonParser[Int]) {}
  def edit(jr: JsResult, p: JsonParser[Int]) = jr
  def fail(je: JsError, p: JsonParser[Int]) = je
  def reset() {}
}


trait JsonChunker[@specialized(Byte, Char, Int) A] {
  def goto(delta: Long): Long
  def available: Long
  def read(buffer: Array[A], start: Int, end: Int): Int
}

// Some code may be cut and pasted from BytesParser.  Sorry!  You will have to maintain by hand.
abstract class GenericChunkedParser[@specialized(Byte, Char, Int) A: scala.reflect.ClassTag](wisdom: JsonWisdom[JsonChunker[A]])
extends JsonParser[JsonChunker[A]] {
  protected var discarded: Long = 0L
  protected var data: Array[A] = Array()
  protected var dlen: Int = 0
  protected var idx: Int = 0
  protected var cache: JsResult = null
  protected var myParseDoubleArrays: Boolean = false

  protected def reinitialized: this.type = {
    discarded = 0L
    dlen = 0
    idx = 0
    cache = null
    this
  }

  def parse(chunker: JsonChunker[A]): JsResult = ???
}

// Pretty much a cut and paste from BytesParser, except Wisdom is inserted.
// Keep synced with everyone including CharChunkParser and CodePointChunkParser below!
class ByteChunkParser(_wisdom: JsonWisdom[JsonChunker[Byte]]) extends GenericChunkedParser[Byte](_wisdom) {}

// Pretty much a cut and paste from BytesParser, except Wisdom is inserted.
// Keep synced with everyone, including ByteChunkParser above and CodePointChunkParser below!
class CharChunkParser(_wisdom: JsonWisdom[JsonChunker[Char]]) extends GenericChunkedParser[Char](_wisdom) {}

// Pretty much a cut and paste from BytesParser, except Wisdom is inserted.
// Keep synced with everyone including BytesChunkParser and CharChunkParser above!
class CodePointChunkParser(_wisdom: JsonWisdom[JsonChunker[Int]]) extends GenericChunkedParser[Int](_wisdom) {}

object ChunkParser {
  val naiveBytes = new JsonNaiveBytes
  val naiveChars = new JsonNaiveChars 
  val naiveCodePoints = new JsonNaiveCodePoints
}

*/
