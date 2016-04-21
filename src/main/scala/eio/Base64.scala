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
}

package base64 {
  object CommonBase64Encodings {
    val Core = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')).mkString
    val Mime = Core + "+/=\r\n"
    val Url  = Core + "-_+"
    val Uucode = (' ' to (' '+63).toChar).mkString + "`"
    val Binhex = "!\"#$%&'()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr:\r"
  }

  abstract class Base64(val pad: Boolean, val wrapAt: Int, charset: Array[Byte]) {
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
  
  object Mime64 extends Base64(true, 72, CommonBase64Encodings.Mime.getBytes) {}
  
  object Url64 extends Base64(false, Int.MaxValue, CommonBase64Encodings.Url.getBytes) {}

    
  object UucodeLine extends Base64(false, Int.MaxValue, CommonBase64Encodings.Uucode.getBytes) {}
  
  object BinhexCore extends Base64(true, 64, CommonBase64Encodings.Binhex.getBytes) {}
}

/*
package base64 {
}

  private[eio] def validate(a: Array[Byte], start: Int, end: Int, decoder: Array[Byte]): Boolean = {
    var i = start
    while (i < end) {
      while (i < end && decoder(a(i) & 0xFF) != 64) i += 1
      if (i >= end) return true
      i += 1
      while (i < end && decoder(a(i) & 0xFF) == 64) i += 1
    }
    false  // Exited in the middle of padding--this is not allowed
  }


object Base64 {

  def binaryFrom64(a: Array[Byte], b: Array[Byte] = null) = {
    var i,j = 0
    while (i<a.length) {
      if (a(i)<64) j += 1
      i += 1
    }
    val z = ( if (b==null || b.length!=(j*3)/4) new Array[Byte]( (j*3)/4 ) else b )
    i = 0; j = 0
    var bits,k,n = 0
    var c0,c1,c2 = 0:Byte
    while (i<a.length) {
      if (a(i)<64) {
        n += 1
        k += 1
        bits = (bits << 6) | a(i)
        if ((k&0x3)==0) {
          c0 = ((bits>>16)&0xFF).toByte
          c1 = ((bits>>8)&0xFF).toByte
          c2 = (bits&0xFF).toByte
          z(j) = c0
          z(j+1) = c1
          z(j+2) = c2
          bits = 0
          n = 0
          j += 3
        }
      }
      i += 1
    }
    val m = (n*3)/4
    while (n<4) { bits = bits<<6; n+= 1 }
    n = m
    while (n>0) {
      z(j) = ((bits >> (16 - 8*(m-n)))&0xFF).toByte
      j += 1
      n -= 1
    }
    z
  }
  
  
  def encodedBy64(a: Array[Byte], e: Array[Byte] = base64url) = {
    var i = 0
    while (i < a.length) {
      val x = a(i)&0x7F
      if (x < e.length) a(i) = e(x)
      else a(i) = e(64)
      i += 1
    }
    a
  }
  
  def encodedAs64(a: Array[Byte], einv: Array[Byte] = url64base) = {
    var i = 0
    while (i<a.length) {
      a(i) = einv(a(i)&0xFF)
      i += 1
    }
    a
  }

  final class Petite private (i: Int, private[this] val a: Array[Byte]) {
    val value = (i<<8)>>8
    def this(i: Int) = this(i, new Array[Byte](3))
    def petite = this
    override def toString = value.toString
    override def equals(o: Any) = { value==o }
    override def hashCode = value
    def to64 = {
      a(0) = ((value>>16)&0xFF).toByte; a(1) = ((value>>8)&0xFF).toByte; a(2) = (value&0xFF).toByte
      encodedBy64( binaryTo64( a ) )
    }
  }
  object Petite {
    def from64(b: Array[Byte]): Petite = new Petite( ((b(0)&0xFF)<<16) + ((b(1)&0xFF)<<8) + (b(2)&0xFF), b )
    def from64(s: String): Petite = {
      val c = encodedAs64(s.getBytes)
      val b = binaryFrom64(c)
      from64(b)
    }
  }
  implicit def int2petite(i: Int) = new Petite(i)

  final class Grande private (l: Long, private[this] val a: Array[Byte]) {
    val value = (l<<16)>>16
    def this(l: Long) = this(l, new Array[Byte](6))
    def grande = this
    override def toString = value.toString
    override def equals(o: Any) = { value==o }
    override def hashCode = l.hashCode
    def to64 = {
      a(0) = ((value>>40)&0xFF).toByte; a(1) = ((value>>32)&0xFF).toByte; a(2) = ((value>>24)&0xFF).toByte
      a(3) = ((value>>16)&0xFF).toByte; a(4) = ((value>>8)&0xFF).toByte; a(5) = (value&0xFF).toByte
      encodedBy64( binaryTo64( a ) )
    }
  }
  object Grande {
    def from64(b: Array[Byte]): Grande = {
      new Grande( ((b(0)&0xFFL)<<40) + ((b(1)&0xFFL)<<32) + ((b(2)&0xFFL)<<24) + ((b(3)&0xFF)<<16) + ((b(4)&0xFF)<<8) + (b(5)&0xFF), b )
    }
    def from64(s: String): Grande = {
      val c = encodedAs64(s.getBytes)
      val b = binaryFrom64(c)
      from64(b)
    }
  }
  implicit def long2grande(l: Long) = new Grande(l)
  
  class LiteralString(val s: String) { }
  class Base64Converter(encoding: Array[Byte], decoding: Array[Byte], pad: Boolean = false, wrap: Option[(Int,Int)] = None) {
    def encodeByte(b: Byte) = new String(encodedBy64( binaryTo64( Array(b), pad, wrap ) , encoding ))
    def decodeByte(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else {
        val a = binaryFrom64( encodedAs64(b, decoding) )
        if (a.length != 1) None
        else Some(a(0))
      }
    }
    def encodeShort(s: Short) = new String(encodedBy64( binaryTo64( Array( ((s>>8)&0xFF).toByte, (s&0xFF).toByte ) , pad , wrap ) , encoding ))
    def decodeShort(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else {
        val a = binaryFrom64( encodedAs64(b, decoding) )
        if (a.length != 2) None
        else Some( (((a(0)&0xFF)<<8) + (a(1)&0xFF)).toShort )
      }
    }
    def encodePetite(p: Petite) = new String(encodedBy64( binaryTo64(
      Array( ((p.value>>16)&0xFF).toByte, ((p.value>>8)&0xFF).toByte, (p.value&0xFF).toByte ) , pad , wrap
    ) , encoding ))
    def decodePetite(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else {
        val a = binaryFrom64( encodedAs64(b, decoding) )
        if (a.length != 3) None
        else Some( Petite.from64(a) )
      }
    }
    def encodeInt(i: Int) = new String(encodedBy64( binaryTo64(
      Array( ((i>>24)&0xFF).toByte, ((i>>16)&0xFF).toByte, ((i>>8)&0xFF).toByte, (i&0xFF).toByte ) , pad , wrap
    ) , encoding ))
    def decodeInt(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else {
        val a = binaryFrom64( encodedAs64(b, decoding) )
        if (a.length != 4) None
        else Some( ((a(0)&0xFF)<<24) + ((a(1)&0xFF)<<16) + ((a(2)&0xFF)<<8) + (a(3)&0xFF) )
      }
    }
    def encodeGrande(g: Grande) = new String(encodedBy64( binaryTo64(
      Array( ((g.value>>40)&0xFF).toByte, ((g.value>>32)&0xFF).toByte, ((g.value>>24)&0xFF).toByte,
        ((g.value>>16)&0xFF).toByte, ((g.value>>8)&0xFF).toByte, (g.value&0xFF).toByte
      ) , pad , wrap
    ) , encoding ))
    def decodeGrande(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else {
        val a = binaryFrom64( encodedAs64(s.getBytes, decoding) )
        if (a.length != 6) None
        else Some( Grande.from64(a) )
      }
    }
    def encodeLong(l: Long) = new String( encodedBy64( binaryTo64( {
      val a = new Array[Byte](8)
      var i=7
      while (i>=0) {
        a(7-i) = ((l >> (8*i))&0xFF).toByte
        i -= 1
      }
      a
    } , pad , wrap ) , encoding ) )
    def decodeLong(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else {
        val a = binaryFrom64( encodedAs64(b, decoding) )
        if (a.length != 8) None
        else {
          var i = 0
          var l = 0L
          while (i<8) {
            l += (a(i)&0xFF)<<(8L*(7-i))
            i += 1
          }
          Some( l )
        }
      }
    }
    def encodeBoolean(b: Boolean) = new String(encoding, if (b) 1 else 0, 1)
    def decodeBoolean(s: String) = {
      if (s.length != 1) None
      else Some(s.charAt(0) == encoding(1))
    }
    def encodeFloat(f: Float) = encodeInt( java.lang.Float.floatToRawIntBits(f) )
    def decodeFloat(s: String) = decodeInt(s).map(x => java.lang.Float.intBitsToFloat(x))
    def encodeDouble(d: Double) = encodeLong( java.lang.Double.doubleToRawLongBits(d) )
    def decodeDouble(s: String) = decodeLong(s).map(x => java.lang.Double.longBitsToDouble(x))
    def encodeString(s: String) = new String(encodedBy64( binaryTo64( s.getBytes , pad , wrap ) , encoding ))
    def decodeString(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else Some(new String(binaryFrom64( encodedAs64(b, decoding) )))
    }
    def encodeBytes(a: Array[Byte]) = new String(encodedBy64( binaryTo64(a, pad, wrap) , encoding))
    def decodeBytes(s: String) = {
      val b = s.getBytes
      if (!validate(b,decoding)) None
      else Some( binaryFrom64( encodedAs64(s.getBytes, decoding) ) )
    }
    def encode(a: Any*) = {
      a.map(_ match {
        case b: Boolean => encodeBoolean(b)
        case b: Byte => encodeByte(b)
        case s: Short => encodeShort(s)
        case c: Char => encodeShort(c.toShort)
        case i: Int => encodeInt(i)
        case l: Long => encodeLong(l)
        case f: Float => encodeFloat(f)
        case d: Double => encodeDouble(d)
        case s: String => encodeString(s)
        case a: Array[Byte] => encodeBytes(a)
        case l: LiteralString => l.s
        case _ => ""
      })
    }
    def decode(encoded: Seq[String], format: String) = {
      (format zip encoded).map(_ match {
        case ('B',x) => decodeBoolean(x)
        case ('b',x) => decodeByte(x)
        case ('s',x) => decodeShort(x)
        case ('c',x) => decodeShort(x).map(_.toChar)
        case ('i',x) => decodeInt(x)
        case ('f',x) => decodeFloat(x)
        case ('l',x) => decodeLong(x)
        case ('d',x) => decodeDouble(x)
        case ('S',x) => decodeString(x)
        case ('A',x) => decodeBytes(x)
        case (_,x) => x
      })
    }
  }
  
  val Mime64 = new Base64Converter(base64mime, mime64base, true, wrapmime)
  val Url64 = new Base64Converter(base64url, url64base, false, None)
}
*/
