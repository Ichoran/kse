// For now, just use TwelveMonkeys instead!

/*

// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2018 Rex Kerr and Calico Life Sciences LLC

package kse.visual.tiffio

import java.io._
import java.awt.image._
import java.nio.ByteOrder._
import java.nio.ByteBuffer

import kse.flow._
import kse.coll._
import kse.eio._

sealed trait Field {
  def count: Int
  def oneInto(index: Int, target: ByteBuffer): Boolean
  def into(target: ByteBuffer): Boolean
}
sealed trait WholeField extends Field {
  def firstToLong: Long
  def oneToLong(index: Int): Long
  def allToLong: Array[Long]
  def into(target: Array[Long], offset: Int, max: Int, startAt: Int): Int
}
sealed trait RealField extends Field {
  def firstToDouble: Double
  def oneToDouble(index: Int): Double
  def allToDouble: Array[Double]
  def into(target: Array[Double], offset: Int, max: Int, startAt: Int): Int
}
object Field {
  sealed trait TypeID { def typeid: Int }
  sealed trait Decoder extends TypeId {}
  sealed trait Decode[A] extends Decoder { def decode(offset: Int, source: ByteBuffer, store: Boolean = false): Ok[String, A] }
  sealed trait DecodedBy[A <: Decoder] extends TypeID { def companion: A; final def typeid = companion.typeid }

  sealed trait OfBytes {
    def byteCount: Int
    def firstByte: Byte
    def oneByte(index: Int): Byte
    def allBytes: Array[Byte]
    def intoBytes(target: Array[Byte], offset: Int, max: Int, startAt: Int): Int
  }
  sealed trait OfShorts {
    def shortCount: Int
    def firstShort: Short
    def oneShort(index: Int): Short
    def allShorts: Array[Short]
    def intoShorts(target: Array[Short], offset: Int, max: Int, startAt: Int): Int
  }
  sealed trait OfInts {
    def intCount: Int
    def firstInt: Int
    def oneInt(index: Int): Int
    def allInts: Array[Int]
    def intoInts(target: Array[Int], offset: Int, max: Int, startAt: Int): Int
  }
  sealed trait OfFloats {
    def floatCount: Int
    def firstFloat: Float
    def oneFloat(index: Int): Float
    def allFloats: Array[Float]
    def intoFloats(target: Array[Float], offset: Int, max: Int, startAt: Int): Int
  }
  sealed trait OfDoubles {
    def doubleCount: Int
    def firstDouble: Double
    def oneDouble(index: Int): Double
    def allDoubles: Array[Double]
    def intoDoubles(target: Array[Double], offset: Int, max: Int, startAt: Int): Int
  }

  sealed trait Stored { protected def inFileSize: Int }
  sealed trait ByteStore extends Stored { def values: Array[Byte] }
  sealed trait ShortStore extends Stored {}
  sealed trait IntStore extends Stored {}
  sealed trait FloatStore extends Stored {}
  sealed trait DoubleStore extends Stored {}
  sealed trait StringStore extends Stored {}
  sealed trait Proxied { protected def offset: Int; protected def buffer: ByteBuffer }
  sealed trait ByteProxy extends Proxied {}
  sealed trait ShortProxy extends Proxied {}
  sealed trait IntProxy extends Proxied {}
  sealed trait FloatProxy extends Proxied {}
  sealed trait DoubleProxy extends Proxied {}
  sealed trait StringProxy extends Proxied {}

  sealed abstract class U8 extends WholeField with OfBytes with DecodedBy[U8.type] {
    final def companion = U8
    final def firstToLong = firstByte & 0xFF
    final def oneToLong(index: Int): Long = oneByte(index) & 0xFF
    final def byteCount = count
  }
  case object U8 extends Decode[U8] {
    final val typeid = 1
    case class Box(value: Array[Byte]) extends U8 with Stored {}
    case class Proxy(offset: Int, buffer: ByteBuffer) extends U8 with Proxied with StoreIn[Box] {}
  }
  class Ascii extends Field with ByteStore with DecodedBy[Ascii.type] {}
  object Ascii extends Decode[Ascii]
}

sealed trait Field {
  type Repr
  def count: Int
  def first: Repr
  def all: Array[Repr]
  def into(target: Array[Data], offset: Int, max: Int): Int
}
sealed trait LongField extends Field {
  def firstLong: Long
  def allLongs: Array[Long] = { val a = new Array[Long](count); longsInto(a, 0, a.length); a }
  def longsInto(target: Array[Long], offset: Int, max: Int): Int
}
sealed trait DoubleField extends Field {
  def firstDouble: Double
  def allDoubles: Array[Double]
  def doublesInt(target: Array[Double], offset: Int, max: Int): Int
}
sealed trait ReadField extends Field {
  def offset: Int
  protected def buffer: ByteBuffer
}
object Field {
  object U8 {
  }
}

sealed trait TiffData {}
object TiffData {
  final class Bytes(values: Array[Byte]) extends TiffData {}
  final class Shorts(values: Array[Short]) extends TiffData {}
  final class Ints(values: Array[Int]) extends TiffData {}
  final class Floats(values: Array[Float]) extends TiffData {}
  final class Doubles(values: Array[Double]) extends TiffData {}
}

final class IfdEntry(val id: Short, val fieldtype: Short, val count: Int, offsetInFile: Int) {
  private[this] var myFieldsize = 0
  private[this] var myFieldReader: FieldReader = null
  private[this] var theBuffer: ByteBuffer = null
  private[this] var myValue: TiffData = null
  private[this] var theTag: TiffTag = null

  fieldtype match {
    case  1 => myFieldSize = 1; myFieldReader = FieldReader.U8
    case  2 => myFieldSize = 1; myFieldReader = FieldReader.ASCII
    case  3 => myFieldSize = 2; myFieldReader = FieldReader.U16
    case  4 => myFieldSize = 4; myFieldReader = FieldReader.U32
    case  5 => myFieldSize = 8; myFieldReader = FieldReader.URational
    case  6 => myFieldSize = 1; myFieldReader = FieldReader.I8
    case  7 => myFieldSize = 1; myFieldReader = FieldReader.X8
    case  8 => myFieldSize = 2; myFieldReader = FieldReader.I16
    case  9 => myFieldSize = 4; myFieldReader = FieldReader.I32
    case 10 => myFieldSize = 8; myFieldReader = FieldReader.IRational
    case 11 => myFieldSize = 4; myFieldReader = FieldReader.F32
    case 12 => myFieldSize = 8; myFieldReader = FieldReader.F64
    case  _ => // Error, do not read
  }
}



  object TiffImpl {
    // Returns the offset of the first IFD
    def readHeader(data: ByteBuffer): Ok[String, Int] = {
      data.position(0)
      if (data.remaining < 8) return No("Data ends before file header is complete")
      data.getShort match {
        case 0x4949 => data.order(LITTLE_ENDIAN)
        case 0x4D4D => data.order(BIG_ENDIAN)
        case _      => return No("Not a tiff file--bad byte order mark")
      }
      if (data.getShort != 42) return No("Not a tiff file--bad magic constant")
      Yes(data.getInt)
    }

    sealed trait Tag { def id: Short }
    object Tag {
      sealed trait Read { def id: Short }
      sealed trait Kid extends Tag { def parent: Read;  final def id = parent.id }

      case object Missing extends Tag { val id = -1 }
      case object Uninterpreted extends Tag { val id = 0 }

      object ImageWidth extends Read { val id = 256; }
      case class ImageWidth(value: Int) extends Tag { val id = 256 }
      case class ImageHeight(value: Int) extends Tag { val id = 257 }
      case class BitsPerSample(value: Int) extends Tag { val id = 258 }
      case class Compression(value: Int) extends Tag { val id = 259 }
      case class PhotometricInterpretation(value: Int) extends Tag { val id = 262 }
      case class StripOffsets
    }

    object Ifd {
      def read(data: ByteBuffer, index: Int): Ok[String, (Array[IfdEntry], Option[Int])] = {
        if (index < 0 || index >= data.limit - 18) return No(s"Data of length ${data.limit} cannot contain an IFD at $index")
        data.position(index)
        val n = (data.getShort & 0xFFFF) // Unsigned short -> int
        if (n == 0) return No(s"Empty IFD at $index")
        if (data.remaining < n.toLong*12 + 4) return No(s"Data cannot contain $n IFD entries past index $index")
        val entries = new Array[IfdEntry](n)
        nFor(n){ i =>
          val entry = IfdEntry(data.getShort, data.getShort, data.getInt, data.getInt)
          if (entry.internal) entry.offset = data.position - 4
          entries(i) = entry
        } 
        val next = data.getInt
        Yes((entries, if (next > 0) Some(next) else None))
      }

      def readAll(data: ByteBuffer, index: Int): Ok[String, Array[Array[IfdEntry]]] = {
        val aaib = Array.newBuilder[Array[IfdEntry]]
        var ix = Option(index)
        while (ix.isDefined) aaib += read(data, ix.get).?.pipe{ case (aie, i) => ix = i; aie }
        Yes(aaib.result())
      }
    }
  }

*/
