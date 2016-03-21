// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import java.nio._


/** A type class that provides JSONizing functionality. */
trait Jsonize[A] {
  /** Convert the object to its JSON representation. */
  def jsonize(a: A): Json

  /** Accumulate the serialized JSON representation of the object in a prettyprinter. */
  def jsonizePretty(a: A, pretty: PrettyJson, depth: Int) { jsonize(a).jsonPretty(pretty, depth) }

  /** Accumulate the serialized JSON representation of the object in a Java StringBuilder. */
  def jsonizeString(a: A, sb: java.lang.StringBuilder) { jsonize(a).jsonString(sb) }

  /** Accumulate the serialized JSON representation of the object in a ByteBuffer.
    *
    * Note: if the `ByteBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 bytes more will be made available per call.
    */
  def jsonizeBytes(a: A, bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = jsonize(a).jsonBytes(bb, refresh)

  /** Accumulate the serialized JSON representation of this object in a CharBuffer.
    *
    * Note: if the `CharBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 chars more will be made available per call.
    */
  def jsonizeChars(a: A, cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = jsonize(a).jsonChars(cb, refresh)
}
