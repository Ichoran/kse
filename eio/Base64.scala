// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-2013 Rex Kerr and the HHMI Janelia Farm Research Campus.


package kse.eio

import scala.language.implicitConversions
import java.nio._

object Base64 {
  
  def backmap(m: Array[Byte], a0: Byte) = {
    val a = Array.fill(256)(a0)
    var i=0
    while (i<m.length) {
      a( m(i)&0xFF ) = i.toByte
      i += 1
    }
    a
  }
  val base64mime = (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ List('+','/','=','\r','\n')).toArray.map(_.toByte)
  val mime64base = backmap(base64mime,64:Byte)
  val wrapmime = Some(76,2)
  val base64url = base64mime.take(62) ++ Array('-','_','+').map(_.toByte)
  val url64base = backmap(base64url,64:Byte)
  
  def lengthAs64(a: Array[Byte], pad: Boolean = false, wrap: Option[(Int,Int)] = None) = {
    val n = a.length
    val m = if (pad) 4*((n+2 - ((n+2)%3))/3)
            else (4*n + 2)/3
    m + wrap.map(x => x._2 * ((m-1 max 0)/x._1)).getOrElse(0)
  }
  def validate(a: Array[Byte], d: Array[Byte]): Boolean = {
    var i = 0
    var lastpad = false
    while (i < a.length) {
      if (d(a(i)&0xFF) == (64: Byte)) lastpad = true
      else if (lastpad) return false
      i += 1
    }
    return true
  }

  def binaryTo64(a: Array[Byte], pad: Boolean = false, wrap: Option[(Int,Int)] = None) = {
    val z = new Array[Byte](lengthAs64(a,pad,wrap))
    var i,j,bits = 0
    var c0,c1,c2,c3 = 0:Byte
    def cload {
      c0 = ((bits>>18)&0x3F).toByte
      c1 = ((bits>>12)&0x3F).toByte
      c2 = ((bits>>6)&0x3F).toByte
      c3 = (bits&0x3F).toByte
    }
    def cstor(n: Int) {
      if (wrap.isDefined) wrap.foreach(w => {
	val l = w._1 + w._2
	val padone = if (pad) 1 else 0
	var shift = 6*(n-1)
	var m = n
	while (m > 0) {
	  if (j%l==w._1) {
	    var k=0
	    while (k<w._2) {
	      z(j) = (64 + padone + k).toByte
	      j += 1
	      k += 1
	    }
	  }
	  z(j) = ((bits>>shift)&0x3F).toByte
	  j += 1
	  shift -= 6
	  m -= 1
	}
      })
      else {
	z(j) = c0
	z(j+1) = c1
	if (n<3) { j += 2; return }
	z(j+2) = c2
	if (n<4) { j += 3; return }
	z(j+3) = c3
	j += 4
      }
    }
    while (i<a.length) {
      bits = (bits<<8) | (a(i)&0xFF)
      i += 1
      if (i%3==0) {
	cload
	cstor(4)
        bits = 0
      }
    }
    val mod3 = i%3
    if (mod3 != 0) {
      while ((i%3) != 0) { bits = bits<<8; i+=1 }
      cload
      if (pad) {
	cstor(4)
	val pad = 3-mod3
	var k = 1
	while (k<=pad) { z(j-k) = 64:Byte; k += 1 }
      }
      else cstor(1+mod3)
    }
    z
  }
  
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
