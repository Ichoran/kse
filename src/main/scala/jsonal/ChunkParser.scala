// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Life Sciences.

package kse.jsonic.parsers

import kse.jsonic.ast._

trait JsonWisdom[A] {
  def in(i: Int, p: JsonParser[A]): Unit
  def in(key: String, p: JsonParser[A]): Unit
  def out(i: Int, p: JsonParser[A]): Unit
  def out(key: String, p: JsonParser[A]): Unit
  def edit(jr: JsResult, p: JsonParser[A]): JsResult
  def fail(je: JsError, p: JsonParser[A]): JsError
}

trait JsonChunker[@specialized(Byte, Char, Int) A] {
  def goto(delta: Long): Long
  def available: Long
  def read(buffer: Array[A], start: Int, end: Int): Int
}

// Pretty much a cut and paste from BytesParser.  Keep synced with that, StringParser, and CharChunkParser below!
class ByteChunkParser(val wisdom: JsonWisdom[JsonChunker[Byte]]) extends JsonParser[JsonChunker[Byte]] {
  def parse(chunker: JsonChunker[Byte]): JsResult = ???
}

// Pretty much a cut and paste from BytesParser.  Keep synced with that, StringParser, and ByteChunkParser above!
class CharChunkParser(val wisdom: JsonWisdom[JsonChunker[Char]]) extends JsonParser[JsonChunker[Char]] {
  def parse(chunker: JsonChunker[Char]): JsResult = ???
}
