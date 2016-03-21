// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import java.nio._


/** Represents an object that can be mapped to JSON without error.
  * Methods for serializing the JSON representation are also provided.
  */
trait AsJson {
  /** The JSON representation of this object. */
  def json: Json

  /** Accumulate the serialized JSON representation of this object in a prettyprinter. */
  def jsonPretty(pretty: PrettyJson, depth: Int) { json.jsonPretty(pretty, depth) }

  /** Accumulate the serialized JSON representation of this object in a Java StringBuilder. */
  def jsonString(sb: java.lang.StringBuilder) { json.jsonString(sb) }

  /** Accumulate the serialized JSON representation of this object in a ByteBuffer.
    *
    * Note: if the `ByteBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 bytes more will be made available per call.
    */
  def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = json.jsonBytes(bb, refresh)

  /** Accumulate the serialized JSON representation of this object in a CharBuffer.
    *
    * Note: if the `CharBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 chars more will be made available per call.
    */
  def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = json.jsonChars(cb, refresh)
}

