// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-2015 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.


package kse.eio

import scala.language.implicitConversions

import java.nio._
import kse.flow._

package object base64 {
  def spaceRequiredEncoded(n: Int, pad: Boolean, wrapAt: Int, wrapAdds: Int) = {
    val m =
      if (pad) 4*((n+2 - ((n+2)%3))/3)
      else (4*n + 2)/3
    m + { if (wrapAt == Int.MaxValue) 0 else wrapAdds * ((m-1 max 0)/wrapAt) }
  }

  def encodeToBase64(source: Array[Byte], start: Int, end: Int, pad: Boolean, wrapAt: Int, wrapAdds: Int, dest: Array[Byte], at: Int, encoder: Array[Byte]) {
    var i = 0
    val n = end - start
    var j = at
    var bits = 0
    var c0, c1, c2, c3 = (0: Byte)

    def cload {
      c0 = ((bits>>18)&0x3F).toByte
      c1 = ((bits>>12)&0x3F).toByte
      c2 = ((bits>>6)&0x3F).toByte
      c3 = (bits&0x3F).toByte
    }

    // Must call with n == 2 or 3 or 4
    def cstor(n: Int) {
      if (wrapAt < Int.MaxValue) {
        val l = wrapAt + wrapAdds
        val wrapidx = if (pad) 65 else 64
        var shift = 6*(n - 1)
        var m = n
        var jmod = (j-at) % l
        while (m > 0) {
          if (jmod == wrapAt) {
            nFor(wrapAdds){ k => dest(j) = encoder(wrapidx + k); j += 1 }
            jmod = 0
          }
          dest(j) = encoder((bits>>shift) & 0x3F)
          j += 1
          jmod += 1
          shift -= 6
          m -= 1
        }
      }
      else {
        dest(j) = encoder(c0)
        dest(j+1) = encoder(c1)
        if (n<3) { j += 2; return }
        dest(j+2) = encoder(c2)
        if (n<4) { j += 3; return }
        dest(j+3) = encoder(c3)
        j += 4
      }
    }

    var mod3 = 0
    nFor(n){ i =>
      bits = (bits<<8) | (source(start+i)&0xFF)
      mod3 += 1
      if (mod3 > 2) {
        cload
        cstor(4)
        bits = 0
        mod3 = 0
      }
    }
    if (mod3 != 0) {
      bits = bits << (if (mod3 == 2) 8 else 16)
      cload
      if (pad) {
        cstor(4)
        val npad = 3-mod3
        var k = 1
        while (k <= npad) { dest(j-k) = encoder(64); k += 1 }
      }
      else cstor(1+mod3)
    }
  }
  
  def encodeToBase64(source: Array[Byte], pad: Boolean, wrapAt: Int, wrapAdds: Int, dest: Array[Byte], at: Int, encoder: Array[Byte]) {
    encodeToBase64(source, 0, source.length, pad, wrapAt, wrapAdds, dest, at, encoder)
  }
  
  def encodeToBase64(source: Array[Byte], start: Int, end: Int, pad: Boolean, wrapAt: Int, wrapAdds: Int, encoder: Array[Byte]): Array[Byte] = {
    val z = new Array[Byte](spaceRequiredEncoded(source.length, pad, wrapAt, wrapAdds))
    encodeToBase64(source, start, end, pad, wrapAt, wrapAdds, z, 0, encoder)
    z
  }

  def encodeToBase64(source: Array[Byte], pad: Boolean, wrapAt: Int, wrapAdds: Int, encoder: Array[Byte]): Array[Byte] = {
    val z = new Array[Byte](spaceRequiredEncoded(source.length, pad, wrapAt, wrapAdds))
    encodeToBase64(source, 0, source.length, pad, wrapAt, wrapAdds, z, 0, encoder)
    z
  }

  def decodeFromBase64(coded: Array[Byte], start: Int, end: Int, dest: Array[Byte], at: Int, decoder: Array[Byte]): Int = {
    var i = start
    var j = at
    var bits, n = 0
    while (i < end) {
      val x = decoder(coded(i))
      if (x < 0) return -1-(i-start)
      else if (x < 64) {
        n += 1
        bits = (bits << 6) | x
        if (n > 3) {
          dest(j) = ((bits>>16) & 0xFF).toByte
          dest(j+1) = ((bits>>8) & 0xFF).toByte
          dest(j+2) = (bits & 0xFF).toByte
          bits = 0
          n = 0
          j += 3
        }
      }
      i += 1
    }
    if (n == 2) {
      dest(j) = ((bits >> 4) & 0xFF).toByte
      j += 1
    }
    else if (n == 3) {
      dest(j) = ((bits >> 10) & 0xFF).toByte
      dest(j+1) = ((bits >> 2) & 0xFF).toByte
      j += 2
    }
    j
  }
  
  def decodeFromBase64String(coded: String, start: Int, end: Int, dest: Array[Byte], at: Int, decoder: Array[Byte]): Int = {
    var i = start
    var j = at
    var bits, n = 0
    while (i < end) {
      val c = coded.charAt(i)
      if (c > 127) return -1-(i-start)
      val x = decoder(c)
      if (x < 0) return -1-(i-start)
      else if (x < 64) {
        n += 1
        bits = (bits << 6) | x
        if (n > 3) {
          dest(j) = ((bits>>16) & 0xFF).toByte
          dest(j+1) = ((bits>>8) & 0xFF).toByte
          dest(j+2) = (bits & 0xFF).toByte
          bits = 0
          n = 0
          j += 3
        }
      }
      i += 1
    }
    if (n == 2) {
      dest(j) = ((bits >> 4) & 0xFF).toByte
      j += 1
    }
    else if (n == 3) {
      dest(j) = ((bits >> 10) & 0xFF).toByte
      dest(j+1) = ((bits >> 2) & 0xFF).toByte
      j += 2
    }
    j
  }
  
  
  def decodeFromBase64(coded: Array[Byte], start: Int, end: Int, decoder: Array[Byte])(implicit oops: Oops): Array[Byte] = {
    val buffer = new Array[Byte](((end - start).toLong*3/4).toInt)
    val n = decodeFromBase64(coded, start, end, buffer, 0, decoder)
    if (n < 0) OOPS
    if (n < buffer.length) java.util.Arrays.copyOf(buffer, n) else buffer
  }
  
  def decodeFromBase64Option(coded: Array[Byte], start: Int, end: Int, decoder: Array[Byte]): Option[Array[Byte]] = {
    val buffer = new Array[Byte](((end - start).toLong*3/4).toInt)
    val n = decodeFromBase64(coded, start, end, buffer, 0, decoder)
    if (n < 0) None
    else Some(if (n < buffer.length) java.util.Arrays.copyOf(buffer, n) else buffer)
  }
  
  def decodeFromBase64(coded: Array[Byte], decoder: Array[Byte])(implicit oops: Oops): Array[Byte] = decodeFromBase64(coded, 0, coded.length, decoder)(oops)
  
  def decodeFromBase64Option(coded: Array[Byte], decoder: Array[Byte]): Option[Array[Byte]] = decodeFromBase64Option(coded, 0, coded.length, decoder)
  

  // Common encoders

  val Mime64 = new Base64(true, 72, CommonBase64Encodings.Mime.getBytes) {}

  val DataURI = new Base64(true, Int.MaxValue, CommonBase64Encodings.Mime.getBytes) {}
  
  val Url64 = new Base64(false, Int.MaxValue, CommonBase64Encodings.Url.getBytes) {}

    
  val UucodeLine = new Base64(false, Int.MaxValue, CommonBase64Encodings.Uucode.getBytes) {}
  
  val BinhexCore = new Base64(true, 64, CommonBase64Encodings.Binhex.getBytes) {}
}

package base64 {
  object CommonBase64Encodings {
    val Core = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')).mkString
    val Mime = Core + "+/=\r\n"
    val Url  = Core + "-_+"
    val Uucode = (' ' to (' '+63).toChar).mkString + "`"
    val Binhex = "!\"#$%&'()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr:\r"
  }

  case class Base64(pad: Boolean, val wrapAt: Int, charset: Array[Byte]) {
    final val encoder = {
      val a = new Array[Byte](256)
      Array.copy(charset, 0, a, 0, 256 min charset.length)
      a
    }
    final val decoder = {
      val a = Array.fill[Byte](256)(-1)
      aFor(charset)( (c,i) => a(c & 0xFF) = (i min 64).toByte )
      a
    }
    val wrapAdds = charset.length - 65;
    
    def decode(s: String)(implicit oops: Oops) = decodeFromBase64(s.getBytes, decoder)
    def encode(a: Array[Byte], start: Int = -1, end: Int = Int.MaxValue) =
      if (start >= end) ""
      else new String(encodeToBase64(a, math.max(0, start), math.min(a.length, end), pad, wrapAt, wrapAdds, charset))
  }
}
