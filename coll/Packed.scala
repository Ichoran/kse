// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse.coll

package object packed {
  final class BytePack(val repr: Byte) extends AnyVal {
    def bit0 = (repr & 0x01) != 0
    def bit1 = (repr & 0x02) != 0
    def bit2 = (repr & 0x04) != 0
    def bit3 = (repr & 0x08) != 0
    def bit4 = (repr & 0x10) != 0
    def bit5 = (repr & 0x20) != 0
    def bit6 = (repr & 0x40) != 0
    def bit7 = (repr & 0x80) != 0
    def bit(i: Int) = (repr & (1<<i)) != 0
    def bits(i: Int, n: Int) = ((repr >>> i) & (0xFF >>> (8-n))).toShort
    def bit0(b: Boolean) = new BytePack((if (b) (repr | 0x01) else (repr & 0xFE)).toByte)
    def bit1(b: Boolean) = new BytePack((if (b) (repr | 0x02) else (repr & 0xFD)).toByte)
    def bit2(b: Boolean) = new BytePack((if (b) (repr | 0x04) else (repr & 0xFB)).toByte)
    def bit3(b: Boolean) = new BytePack((if (b) (repr | 0x08) else (repr & 0xF7)).toByte)
    def bit4(b: Boolean) = new BytePack((if (b) (repr | 0x10) else (repr & 0xEF)).toByte)
    def bit5(b: Boolean) = new BytePack((if (b) (repr | 0x20) else (repr & 0xDF)).toByte)
    def bit6(b: Boolean) = new BytePack((if (b) (repr | 0x40) else (repr & 0xBF)).toByte)
    def bit7(b: Boolean) = new BytePack((if (b) (repr | 0x80) else (repr & 0x7F)).toByte)
    def bit(i: Int)(b: Boolean) = new BytePack((if (b) repr | (1<<i) else repr & (0xFF - (1<<i))).toByte)
    def bits(i: Int, n: Int)(value: Byte) = {
      val m = (0xFF >>> (8-n)) << i
      new BytePack( ((repr & (0xFF - m)) | ((value << i) & m)).toByte )
    }

    def n0 = ((repr << 4).toByte >> 4).toByte
    def n0(n: Byte) = new BytePack(((repr&0xF0) | (n&0xF)).toByte)
    def n1 = (repr >> 4).toByte
    def n1(n: Byte) = new BytePack((((n<<4)&0xF0) | (repr&0xF)).toByte)
    def swapN = new BytePack((((repr&0xF0)>>4) | ((repr&0xF)<<4)).toByte)
  }
  implicit final class ByteAsPacked(val b: Byte) extends AnyVal {
    def packB = new BytePack(b)
    def packN(c: Byte) = new BytePack(((b&0xF) | (c<<4)).toByte)
  }
  
  final class ShortPack(val repr: Short) extends AnyVal {
    def bit0  = (repr & 0x0001) != 0
    def bit1  = (repr & 0x0002) != 0
    def bit2  = (repr & 0x0004) != 0
    def bit3  = (repr & 0x0008) != 0
    def bit4  = (repr & 0x0010) != 0
    def bit5  = (repr & 0x0020) != 0
    def bit6  = (repr & 0x0040) != 0
    def bit7  = (repr & 0x0080) != 0
    def bit8  = (repr & 0x0100) != 0
    def bit9  = (repr & 0x0200) != 0
    def bit10 = (repr & 0x0400) != 0
    def bit11 = (repr & 0x0800) != 0
    def bit12 = (repr & 0x1000) != 0
    def bit13 = (repr & 0x2000) != 0
    def bit14 = (repr & 0x4000) != 0
    def bit15 = (repr & 0x8000) != 0
    def bit(i: Int) = (repr & (1<<i)) != 0
    def bits(i: Int, n: Int) = ((repr >>> i) & (0xFFFF >>> (16-n))).toShort
    def bit0(b: Boolean)  = new ShortPack((if (b) (repr | 0x0001) else (repr & 0xFFFE)).toShort)
    def bit1(b: Boolean)  = new ShortPack((if (b) (repr | 0x0002) else (repr & 0xFFFD)).toShort)
    def bit2(b: Boolean)  = new ShortPack((if (b) (repr | 0x0004) else (repr & 0xFFFB)).toShort)
    def bit3(b: Boolean)  = new ShortPack((if (b) (repr | 0x0008) else (repr & 0xFFF7)).toShort)
    def bit4(b: Boolean)  = new ShortPack((if (b) (repr | 0x0010) else (repr & 0xFFEF)).toShort)
    def bit5(b: Boolean)  = new ShortPack((if (b) (repr | 0x0020) else (repr & 0xFFDF)).toShort)
    def bit6(b: Boolean)  = new ShortPack((if (b) (repr | 0x0040) else (repr & 0xFFBF)).toShort)
    def bit7(b: Boolean)  = new ShortPack((if (b) (repr | 0x0080) else (repr & 0xFF7F)).toShort)
    def bit8(b: Boolean)  = new ShortPack((if (b) (repr | 0x0100) else (repr & 0xFEFF)).toShort)
    def bit9(b: Boolean)  = new ShortPack((if (b) (repr | 0x0200) else (repr & 0xFDFF)).toShort)
    def bit10(b: Boolean) = new ShortPack((if (b) (repr | 0x0400) else (repr & 0xFBFF)).toShort)
    def bit11(b: Boolean) = new ShortPack((if (b) (repr | 0x0800) else (repr & 0xF7FF)).toShort)
    def bit12(b: Boolean) = new ShortPack((if (b) (repr | 0x1000) else (repr & 0xEFFF)).toShort)
    def bit13(b: Boolean) = new ShortPack((if (b) (repr | 0x2000) else (repr & 0xDFFF)).toShort)
    def bit14(b: Boolean) = new ShortPack((if (b) (repr | 0x4000) else (repr & 0xBFFF)).toShort)
    def bit15(b: Boolean) = new ShortPack((if (b) (repr | 0x8000) else (repr & 0x7FFF)).toShort)
    def bit(i: Int)(b: Boolean) = new ShortPack((if (b) repr | (1<<i) else repr & (0xFFFF - (1<<i))).toShort)
    def bits(i: Int, n: Int)(value: Short) = {
      val m = (0xFFFF >>> (16-n)) << i
      new ShortPack( ((repr & (0xFFFF - m)) | ((value << i) & m)).toShort )
    }

    @inline private def s = repr
    def b0 = (s & 0xFF).toByte
    def b0(b: Byte) = new ShortPack(((s&0xFF00)|(b&0xFF)).toShort)
    def b1 = ((s&0xFFFF)>>8).toByte
    def b1(b: Byte) = new ShortPack(((s&0xFF)|((b&0xFF)<<8)).toShort)
    def swapB = new ShortPack((((s&0xFF00)>>8) | ((s&0xFF)<<8)).toShort)
  }
  implicit final class PackBytesInShort(val b: Byte) extends AnyVal {
    def packBB(c: Byte) = new ShortPack(((b&0xFF) | ((c&0xFF)<<8)).toShort)
  }
  implicit final class ShortAsPacked(val s: Short) extends AnyVal {
    def packS = new ShortPack(s)
  }
  
  final class IntPack(val repr: Int) extends AnyVal {
    def bit0  = (repr & 0x00000001) != 0
    def bit1  = (repr & 0x00000002) != 0
    def bit2  = (repr & 0x00000004) != 0
    def bit3  = (repr & 0x00000008) != 0
    def bit4  = (repr & 0x00000010) != 0
    def bit5  = (repr & 0x00000020) != 0
    def bit6  = (repr & 0x00000040) != 0
    def bit7  = (repr & 0x00000080) != 0
    def bit8  = (repr & 0x00000100) != 0
    def bit9  = (repr & 0x00000200) != 0
    def bit10 = (repr & 0x00000400) != 0
    def bit11 = (repr & 0x00000800) != 0
    def bit12 = (repr & 0x00001000) != 0
    def bit13 = (repr & 0x00002000) != 0
    def bit14 = (repr & 0x00004000) != 0
    def bit15 = (repr & 0x00008000) != 0
    def bit16 = (repr & 0x00010000) != 0
    def bit17 = (repr & 0x00020000) != 0
    def bit18 = (repr & 0x00040000) != 0
    def bit19 = (repr & 0x00080000) != 0
    def bit20 = (repr & 0x00100000) != 0
    def bit21 = (repr & 0x00200000) != 0
    def bit22 = (repr & 0x00400000) != 0
    def bit23 = (repr & 0x00800000) != 0
    def bit24 = (repr & 0x01000000) != 0
    def bit25 = (repr & 0x02000000) != 0
    def bit26 = (repr & 0x04000000) != 0
    def bit27 = (repr & 0x08000000) != 0
    def bit28 = (repr & 0x10000000) != 0
    def bit29 = (repr & 0x20000000) != 0
    def bit30 = (repr & 0x40000000) != 0
    def bit31 = (repr & 0x80000000) != 0
    def bit(i: Int) = (repr & (1<<i)) != 0
    def bits(i: Int, n: Int) = (repr >>> i) & (-1 >>> (32-n))
    def bit0(b: Boolean)  = new IntPack(if (b) (repr | 0x00000001) else (repr & 0xFFFFFFFE))
    def bit1(b: Boolean)  = new IntPack(if (b) (repr | 0x00000002) else (repr & 0xFFFFFFFD))
    def bit2(b: Boolean)  = new IntPack(if (b) (repr | 0x00000004) else (repr & 0xFFFFFFFB))
    def bit3(b: Boolean)  = new IntPack(if (b) (repr | 0x00000008) else (repr & 0xFFFFFFF7))
    def bit4(b: Boolean)  = new IntPack(if (b) (repr | 0x00000010) else (repr & 0xFFFFFFEF))
    def bit5(b: Boolean)  = new IntPack(if (b) (repr | 0x00000020) else (repr & 0xFFFFFFDF))
    def bit6(b: Boolean)  = new IntPack(if (b) (repr | 0x00000040) else (repr & 0xFFFFFFBF))
    def bit7(b: Boolean)  = new IntPack(if (b) (repr | 0x00000080) else (repr & 0xFFFFFF7F))
    def bit8(b: Boolean)  = new IntPack(if (b) (repr | 0x00000100) else (repr & 0xFFFFFEFF))
    def bit9(b: Boolean)  = new IntPack(if (b) (repr | 0x00000200) else (repr & 0xFFFFFDFF))
    def bit10(b: Boolean) = new IntPack(if (b) (repr | 0x00000400) else (repr & 0xFFFFFBFF))
    def bit11(b: Boolean) = new IntPack(if (b) (repr | 0x00000800) else (repr & 0xFFFFF7FF))
    def bit12(b: Boolean) = new IntPack(if (b) (repr | 0x00001000) else (repr & 0xFFFFEFFF))
    def bit13(b: Boolean) = new IntPack(if (b) (repr | 0x00002000) else (repr & 0xFFFFDFFF))
    def bit14(b: Boolean) = new IntPack(if (b) (repr | 0x00004000) else (repr & 0xFFFFBFFF))
    def bit15(b: Boolean) = new IntPack(if (b) (repr | 0x00008000) else (repr & 0xFFFF7FFF))
    def bit16(b: Boolean) = new IntPack(if (b) (repr | 0x00010000) else (repr & 0xFFFEFFFF))
    def bit17(b: Boolean) = new IntPack(if (b) (repr | 0x00020000) else (repr & 0xFFFDFFFF))
    def bit18(b: Boolean) = new IntPack(if (b) (repr | 0x00040000) else (repr & 0xFFFBFFFF))
    def bit19(b: Boolean) = new IntPack(if (b) (repr | 0x00080000) else (repr & 0xFFF7FFFF))
    def bit20(b: Boolean) = new IntPack(if (b) (repr | 0x00100000) else (repr & 0xFFEFFFFF))
    def bit21(b: Boolean) = new IntPack(if (b) (repr | 0x00200000) else (repr & 0xFFDFFFFF))
    def bit22(b: Boolean) = new IntPack(if (b) (repr | 0x00400000) else (repr & 0xFFBFFFFF))
    def bit23(b: Boolean) = new IntPack(if (b) (repr | 0x00800000) else (repr & 0xFF7FFFFF))
    def bit24(b: Boolean) = new IntPack(if (b) (repr | 0x01000000) else (repr & 0xFEFFFFFF))
    def bit25(b: Boolean) = new IntPack(if (b) (repr | 0x02000000) else (repr & 0xFDFFFFFF))
    def bit26(b: Boolean) = new IntPack(if (b) (repr | 0x04000000) else (repr & 0xFBFFFFFF))
    def bit27(b: Boolean) = new IntPack(if (b) (repr | 0x08000000) else (repr & 0xF7FFFFFF))
    def bit28(b: Boolean) = new IntPack(if (b) (repr | 0x10000000) else (repr & 0xEFFFFFFF))
    def bit29(b: Boolean) = new IntPack(if (b) (repr | 0x20000000) else (repr & 0xDFFFFFFF))
    def bit30(b: Boolean) = new IntPack(if (b) (repr | 0x40000000) else (repr & 0xBFFFFFFF))
    def bit31(b: Boolean) = new IntPack(if (b) (repr | 0x80000000) else (repr & 0x7FFFFFFF))
    def bit(i: Int)(b: Boolean) = new IntPack(if (b) repr | (1<<i) else repr & (0xFFFFFFFF - (1<<i)))
    def bits(i: Int, n: Int)(value: Int) = {
      val m = (-1 >>> (32-n)) << i
      new IntPack( (repr & (-1 - m)) | ((value << i) & m) )
    }
    @inline private def i = repr
    def b0 = (i&0xFF).toByte
    def b0(b: Byte) = new IntPack((i&0xFFFFFF00) | (b&0xFF))
    def b1 = ((i&0xFF00)>>8).toByte
    def b1(b: Byte) = new IntPack((i&0xFFFF00FF) | ((b&0xFF)<<8))
    def b2 = ((i&0xFF0000)>>16).toByte
    def b2(b: Byte) = new IntPack((i&0xFF00FFFF) | ((b&0xFF)<<16))
    def b3 = (i>>>24).toByte
    def b3(b: Byte) = new IntPack((i&0xFFFFFF) | ((b&0xFF)<<24))
    def rotrB = new IntPack((i >>> 8) | (i << 24))
    def rotlB = new IntPack((i >>> 24) | (i << 8))
    def swap2B = new IntPack(((i&0xFF00FF00)>>>8) | ((i&0x00FF00FF)<<8))
    def flipB = new IntPack(((i&0xFF000000)>>24) | ((i&0xFF0000)>>8) | ((i&0xFF00)<<8) | ((i&0xFF)<<24))

    def s0 = (i & 0xFFFF).toShort
    def s0(s: Short) = new IntPack((i&0xFFFF0000) | (s&0xFFFF))
    def s1 = (i>>>16).toShort
    def s1(s: Short) = new IntPack((i&0xFFFF) | ((s&0xFFFF)<<16))
    def swapS = new IntPack((i>>>16) | (i<<16))

    def f0 = java.lang.Float.intBitsToFloat(i)
    def f0(f: Float) = java.lang.Float.floatToRawIntBits(f)
  }
  implicit final class PackBytesInInt(val b: Byte) extends AnyVal {
    def packBBBB(c: Byte, d: Byte, e: Byte) = new IntPack((b&0xFF) | ((c&0xFF)<<8) | ((d&0xFF)<<16) | (e.toInt<<24))
  }
  implicit final class PackShortsInInt(val s: Short) extends AnyVal {
    def packSS(t: Short) = new IntPack((s&0xFFFF) | (t.toInt << 16))
  }
  implicit final class PackFloatInInt(val f: Float) extends AnyVal {
    def packF = new IntPack(java.lang.Float.floatToRawIntBits(f))
  }
  implicit final class IntAsPacked(val i: Int) extends AnyVal {
    def packI = new IntPack(i)
  }
  
  final class LongPack(val repr: Long) extends AnyVal {
    def bit0  = (repr & 0x0000000000000001L) != 0
    def bit1  = (repr & 0x0000000000000002L) != 0
    def bit2  = (repr & 0x0000000000000004L) != 0
    def bit3  = (repr & 0x0000000000000008L) != 0
    def bit4  = (repr & 0x0000000000000010L) != 0
    def bit5  = (repr & 0x0000000000000020L) != 0
    def bit6  = (repr & 0x0000000000000040L) != 0
    def bit7  = (repr & 0x0000000000000080L) != 0
    def bit8  = (repr & 0x0000000000000100L) != 0
    def bit9  = (repr & 0x0000000000000200L) != 0
    def bit10 = (repr & 0x0000000000000400L) != 0
    def bit11 = (repr & 0x0000000000000800L) != 0
    def bit12 = (repr & 0x0000000000001000L) != 0
    def bit13 = (repr & 0x0000000000002000L) != 0
    def bit14 = (repr & 0x0000000000004000L) != 0
    def bit15 = (repr & 0x0000000000008000L) != 0
    def bit16 = (repr & 0x0000000000010000L) != 0
    def bit17 = (repr & 0x0000000000020000L) != 0
    def bit18 = (repr & 0x0000000000040000L) != 0
    def bit19 = (repr & 0x0000000000080000L) != 0
    def bit20 = (repr & 0x0000000000100000L) != 0
    def bit21 = (repr & 0x0000000000200000L) != 0
    def bit22 = (repr & 0x0000000000400000L) != 0
    def bit23 = (repr & 0x0000000000800000L) != 0
    def bit24 = (repr & 0x0000000001000000L) != 0
    def bit25 = (repr & 0x0000000002000000L) != 0
    def bit26 = (repr & 0x0000000004000000L) != 0
    def bit27 = (repr & 0x0000000008000000L) != 0
    def bit28 = (repr & 0x0000000010000000L) != 0
    def bit29 = (repr & 0x0000000020000000L) != 0
    def bit30 = (repr & 0x0000000040000000L) != 0
    def bit31 = (repr & 0x0000000080000000L) != 0
    def bit32 = (repr & 0x0000000100000000L) != 0
    def bit33 = (repr & 0x0000000200000000L) != 0
    def bit34 = (repr & 0x0000000400000000L) != 0
    def bit35 = (repr & 0x0000000800000000L) != 0
    def bit36 = (repr & 0x0000001000000000L) != 0
    def bit37 = (repr & 0x0000002000000000L) != 0
    def bit38 = (repr & 0x0000004000000000L) != 0
    def bit39 = (repr & 0x0000008000000000L) != 0
    def bit40 = (repr & 0x0000010000000000L) != 0
    def bit41 = (repr & 0x0000020000000000L) != 0
    def bit42 = (repr & 0x0000040000000000L) != 0
    def bit43 = (repr & 0x0000080000000000L) != 0
    def bit44 = (repr & 0x0000100000000000L) != 0
    def bit45 = (repr & 0x0000200000000000L) != 0
    def bit46 = (repr & 0x0000400000000000L) != 0
    def bit47 = (repr & 0x0000800000000000L) != 0
    def bit48 = (repr & 0x0001000000000000L) != 0
    def bit49 = (repr & 0x0002000000000000L) != 0
    def bit50 = (repr & 0x0004000000000000L) != 0
    def bit51 = (repr & 0x0008000000000000L) != 0
    def bit52 = (repr & 0x0010000000000000L) != 0
    def bit53 = (repr & 0x0020000000000000L) != 0
    def bit54 = (repr & 0x0040000000000000L) != 0
    def bit55 = (repr & 0x0080000000000000L) != 0
    def bit56 = (repr & 0x0100000000000000L) != 0
    def bit57 = (repr & 0x0200000000000000L) != 0
    def bit58 = (repr & 0x0400000000000000L) != 0
    def bit59 = (repr & 0x0800000000000000L) != 0
    def bit60 = (repr & 0x1000000000000000L) != 0
    def bit61 = (repr & 0x2000000000000000L) != 0
    def bit62 = (repr & 0x4000000000000000L) != 0
    def bit63 = (repr & 0x8000000000000000L) != 0
    def bit(i: Int) = (repr & (1L<<i)) != 0
    def bits(i: Int, n: Int) = (repr >>> i) & (-1L >>> (64-n))
    def bit0(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000001L) else (repr & 0xFFFFFFFFFFFFFFFEL))
    def bit1(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000002L) else (repr & 0xFFFFFFFFFFFFFFFDL))
    def bit2(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000004L) else (repr & 0xFFFFFFFFFFFFFFFBL))
    def bit3(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000008L) else (repr & 0xFFFFFFFFFFFFFFF7L))
    def bit4(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000010L) else (repr & 0xFFFFFFFFFFFFFFEFL))
    def bit5(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000020L) else (repr & 0xFFFFFFFFFFFFFFDFL))
    def bit6(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000040L) else (repr & 0xFFFFFFFFFFFFFFBFL))
    def bit7(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000080L) else (repr & 0xFFFFFFFFFFFFFF7FL))
    def bit8(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000100L) else (repr & 0xFFFFFFFFFFFFFEFFL))
    def bit9(b: Boolean)  = new LongPack(if (b) (repr | 0x0000000000000200L) else (repr & 0xFFFFFFFFFFFFFDFFL))
    def bit10(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000000400L) else (repr & 0xFFFFFFFFFFFFFBFFL))
    def bit11(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000000800L) else (repr & 0xFFFFFFFFFFFFF7FFL))
    def bit12(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000001000L) else (repr & 0xFFFFFFFFFFFFEFFFL))
    def bit13(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000002000L) else (repr & 0xFFFFFFFFFFFFDFFFL))
    def bit14(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000004000L) else (repr & 0xFFFFFFFFFFFFBFFFL))
    def bit15(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000008000L) else (repr & 0xFFFFFFFFFFFF7FFFL))
    def bit16(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000010000L) else (repr & 0xFFFFFFFFFFFEFFFFL))
    def bit17(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000020000L) else (repr & 0xFFFFFFFFFFFDFFFFL))
    def bit18(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000040000L) else (repr & 0xFFFFFFFFFFFBFFFFL))
    def bit19(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000080000L) else (repr & 0xFFFFFFFFFFF7FFFFL))
    def bit20(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000100000L) else (repr & 0xFFFFFFFFFFEFFFFFL))
    def bit21(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000200000L) else (repr & 0xFFFFFFFFFFDFFFFFL))
    def bit22(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000400000L) else (repr & 0xFFFFFFFFFFBFFFFFL))
    def bit23(b: Boolean) = new LongPack(if (b) (repr | 0x0000000000800000L) else (repr & 0xFFFFFFFFFF7FFFFFL))
    def bit24(b: Boolean) = new LongPack(if (b) (repr | 0x0000000001000000L) else (repr & 0xFFFFFFFFFEFFFFFFL))
    def bit25(b: Boolean) = new LongPack(if (b) (repr | 0x0000000002000000L) else (repr & 0xFFFFFFFFFDFFFFFFL))
    def bit26(b: Boolean) = new LongPack(if (b) (repr | 0x0000000004000000L) else (repr & 0xFFFFFFFFFBFFFFFFL))
    def bit27(b: Boolean) = new LongPack(if (b) (repr | 0x0000000008000000L) else (repr & 0xFFFFFFFFF7FFFFFFL))
    def bit28(b: Boolean) = new LongPack(if (b) (repr | 0x0000000010000000L) else (repr & 0xFFFFFFFFEFFFFFFFL))
    def bit29(b: Boolean) = new LongPack(if (b) (repr | 0x0000000020000000L) else (repr & 0xFFFFFFFFDFFFFFFFL))
    def bit30(b: Boolean) = new LongPack(if (b) (repr | 0x0000000040000000L) else (repr & 0xFFFFFFFFBFFFFFFFL))
    def bit31(b: Boolean) = new LongPack(if (b) (repr | 0x0000000080000000L) else (repr & 0xFFFFFFFF7FFFFFFFL))
    def bit32(b: Boolean) = new LongPack(if (b) (repr | 0x0000000100000000L) else (repr & 0xFFFFFFFEFFFFFFFFL))
    def bit33(b: Boolean) = new LongPack(if (b) (repr | 0x0000000200000000L) else (repr & 0xFFFFFFFDFFFFFFFFL))
    def bit34(b: Boolean) = new LongPack(if (b) (repr | 0x0000000400000000L) else (repr & 0xFFFFFFFBFFFFFFFFL))
    def bit35(b: Boolean) = new LongPack(if (b) (repr | 0x0000000800000000L) else (repr & 0xFFFFFFF7FFFFFFFFL))
    def bit36(b: Boolean) = new LongPack(if (b) (repr | 0x0000001000000000L) else (repr & 0xFFFFFFEFFFFFFFFFL))
    def bit37(b: Boolean) = new LongPack(if (b) (repr | 0x0000002000000000L) else (repr & 0xFFFFFFDFFFFFFFFFL))
    def bit38(b: Boolean) = new LongPack(if (b) (repr | 0x0000004000000000L) else (repr & 0xFFFFFFBFFFFFFFFFL))
    def bit39(b: Boolean) = new LongPack(if (b) (repr | 0x0000008000000000L) else (repr & 0xFFFFFF7FFFFFFFFFL))
    def bit40(b: Boolean) = new LongPack(if (b) (repr | 0x0000010000000000L) else (repr & 0xFFFFFEFFFFFFFFFFL))
    def bit41(b: Boolean) = new LongPack(if (b) (repr | 0x0000020000000000L) else (repr & 0xFFFFFDFFFFFFFFFFL))
    def bit42(b: Boolean) = new LongPack(if (b) (repr | 0x0000040000000000L) else (repr & 0xFFFFFBFFFFFFFFFFL))
    def bit43(b: Boolean) = new LongPack(if (b) (repr | 0x0000080000000000L) else (repr & 0xFFFFF7FFFFFFFFFFL))
    def bit44(b: Boolean) = new LongPack(if (b) (repr | 0x0000100000000000L) else (repr & 0xFFFFEFFFFFFFFFFFL))
    def bit45(b: Boolean) = new LongPack(if (b) (repr | 0x0000200000000000L) else (repr & 0xFFFFDFFFFFFFFFFFL))
    def bit46(b: Boolean) = new LongPack(if (b) (repr | 0x0000400000000000L) else (repr & 0xFFFFBFFFFFFFFFFFL))
    def bit47(b: Boolean) = new LongPack(if (b) (repr | 0x0000800000000000L) else (repr & 0xFFFF7FFFFFFFFFFFL))
    def bit48(b: Boolean) = new LongPack(if (b) (repr | 0x0001000000000000L) else (repr & 0xFFFEFFFFFFFFFFFFL))
    def bit49(b: Boolean) = new LongPack(if (b) (repr | 0x0002000000000000L) else (repr & 0xFFFDFFFFFFFFFFFFL))
    def bit50(b: Boolean) = new LongPack(if (b) (repr | 0x0004000000000000L) else (repr & 0xFFFBFFFFFFFFFFFFL))
    def bit51(b: Boolean) = new LongPack(if (b) (repr | 0x0008000000000000L) else (repr & 0xFFF7FFFFFFFFFFFFL))
    def bit52(b: Boolean) = new LongPack(if (b) (repr | 0x0010000000000000L) else (repr & 0xFFEFFFFFFFFFFFFFL))
    def bit53(b: Boolean) = new LongPack(if (b) (repr | 0x0020000000000000L) else (repr & 0xFFDFFFFFFFFFFFFFL))
    def bit54(b: Boolean) = new LongPack(if (b) (repr | 0x0040000000000000L) else (repr & 0xFFBFFFFFFFFFFFFFL))
    def bit55(b: Boolean) = new LongPack(if (b) (repr | 0x0080000000000000L) else (repr & 0xFF7FFFFFFFFFFFFFL))
    def bit56(b: Boolean) = new LongPack(if (b) (repr | 0x0100000000000000L) else (repr & 0xFEFFFFFFFFFFFFFFL))
    def bit57(b: Boolean) = new LongPack(if (b) (repr | 0x0200000000000000L) else (repr & 0xFDFFFFFFFFFFFFFFL))
    def bit58(b: Boolean) = new LongPack(if (b) (repr | 0x0400000000000000L) else (repr & 0xFBFFFFFFFFFFFFFFL))
    def bit59(b: Boolean) = new LongPack(if (b) (repr | 0x0800000000000000L) else (repr & 0xF7FFFFFFFFFFFFFFL))
    def bit60(b: Boolean) = new LongPack(if (b) (repr | 0x1000000000000000L) else (repr & 0xEFFFFFFFFFFFFFFFL))
    def bit61(b: Boolean) = new LongPack(if (b) (repr | 0x2000000000000000L) else (repr & 0xDFFFFFFFFFFFFFFFL))
    def bit62(b: Boolean) = new LongPack(if (b) (repr | 0x4000000000000000L) else (repr & 0xBFFFFFFFFFFFFFFFL))
    def bit63(b: Boolean) = new LongPack(if (b) (repr | 0x8000000000000000L) else (repr & 0x7FFFFFFFFFFFFFFFL))
    def bit(i: Int)(b: Boolean) = new LongPack(if (b) repr | (1L<<i) else repr & (-1L - (1L<<i)))
    def bits(i: Int, n: Int)(value: Long) = {
      val m = (-1L >>> (64-n)) << i
      new LongPack( (repr & (-1L - m)) | ((value << i) & m) )
    }

    @inline private def l = repr
    def b0 = (l & 0xFFL).toByte
    def b0(b: Byte) = new LongPack((l&0xFFFFFFFFFFFFFF00L) | (b&0xFF))
    def b1 = ((l&0xFF00)>>8).toByte
    def b1(b: Byte) = new LongPack((l&0xFFFFFFFFFFFF00FFL) | ((b&0xFF)<<8))
    def b2 = ((l&0xFF0000)>>16).toByte
    def b2(b: Byte) = new LongPack((l&0xFFFFFFFFFF00FFFFL) | ((b&0xFF)<<16))
    def b3 = ((l&0xFF000000)>>24).toByte
    def b3(b: Byte) = new LongPack((l&0xFFFFFFFF00FFFFFFL) | ((b&0xFF).toLong<<24))
    def b4 = ((l&0xFF00000000L)>>32).toByte
    def b4(b: Byte) = new LongPack((l&0xFFFFFF00FFFFFFFFL) | ((b&0xFF).toLong<<32))
    def b5 = ((l&0xFF0000000000L)>>40).toByte
    def b5(b: Byte) = new LongPack((l&0xFFFF00FFFFFFFFFFL) | ((b&0xFF).toLong<<40))
    def b6 = ((l&0xFF000000000000L)>>48).toByte
    def b6(b: Byte) = new LongPack((l&0xFF00FFFFFFFFFFFFL) | ((b&0xFF).toLong<<48))
    def b7 = (l>>>56).toByte
    def b7(b: Byte) = new LongPack((l&0x00FFFFFFFFFFFFFFL) | ((b&0xFF).toLong<<56))
    def rotrB = new LongPack((l >>> 8) | (l << 56))
    def rotlB = new LongPack((l >>> 56) | (l << 8))
    def swap4B = new LongPack(((l&0xFF00FF00FF00FF00L)>>>8) | ((l&0x00FF00FF00FF00FFL)<<8))
    def flipB = {
      val m = swap4B.repr
      new LongPack((m>>>48) | ((m&0xFFFF00000000L)>>16) | ((m&0xFFFF0000L)<<16) | (m<<48))
    }

    def s0 = (l & 0xFFFFL).toShort
    def s0(s: Short) = new LongPack((l&0xFFFFFFFFFFFF0000L) | (s&0xFFFF))
    def s1 = ((l&0xFFFF0000)>>16).toShort
    def s1(s: Short) = new LongPack((l&0xFFFFFFFF0000FFFFL) | ((s&0xFFFF).toLong << 16))
    def s2 = ((l&0xFFFF00000000L)>>32).toShort
    def s2(s: Short) = new LongPack((l&0xFFFF0000FFFFFFFFL) | ((s&0xFFFF).toLong << 32))
    def s3 = (l>>>48).toShort
    def s3(s: Short) = new LongPack((l&0x0000FFFFFFFFFFFFL) | (s.toLong << 48))
    def rotrS = new LongPack((l >>> 16) | (l << 48))
    def rotlS = new LongPack((l >>> 48) | (l << 16))
    def swap2S = new LongPack(((l&0xFFFF0000FFFF0000L)>>>16) | ((l&0x0000FFFF0000FFFFL)<<16))
    def flipS = new LongPack((l>>>48) | ((l&0xFFFF00000000L)>>16) | ((l&0xFFFF0000L)<<16) | (l<<48))
    
    def i0 = (l & 0xFFFFFFFFL).toInt
    def i0(i: Int) = new LongPack((l&0xFFFFFFFF00000000L) | (i&0xFFFFFFFFL))
    def i1 = (l >>> 32).toInt
    def i1(i: Int) = new LongPack((l&0xFFFFFFFFL) | (i.toLong<<32))
    def swapI = new LongPack((l >>> 32) | (l << 32))  
    
    def f0 = java.lang.Float.intBitsToFloat(i0)
    def f0(f: Float) = i0(java.lang.Float.floatToRawIntBits(f))
    def f1 = java.lang.Float.intBitsToFloat(i1)
    def f1(f: Float) = i1(java.lang.Float.floatToRawIntBits(f))
    def swapF = swapI
    
    def d0 = java.lang.Double.longBitsToDouble(l)
    def d0(d: Double) = new LongPack(java.lang.Double.doubleToRawLongBits(d))

    def unpackBoolean = l != 0
    def unpackByte = l.toByte
    def unpackShort = l.toShort
    def unpackChar = l.toChar
    def unpackInt = l.toInt
    def unpackLong = l
    def unpackFloat = java.lang.Float.intBitsToFloat(l.toInt)
    def unpackDouble = java.lang.Double.longBitsToDouble(l)
  }
  implicit final class PackBooleanInLong(val bool: Boolean) extends AnyVal {
    def packInLong = if (bool) 1 else 0
  }
  implicit final class PackBytesInLong(val b: Byte) extends AnyVal {
    def packBBBBBBBB(c: Byte, d: Byte, e: Byte, f: Byte, g: Byte, h: Byte, i: Byte) = {
      new LongPack((b&0xFFL) | ((c&0xFFL)<<8) | ((d&0xFFL)<<16) | ((e&0xFFL)<<24) | ((f&0xFFL)<<32) | ((g&0xFFL)<<40) | ((h&0xFFL)<<48) | (i.toLong<<56))
    }
    def packInLong = new LongPack(b)
  }
  implicit final class PackShortsInLong(val s: Short) extends AnyVal {
    def packSSSS(t: Short, u: Short, v: Short) = new LongPack((s&0xFFFFL) | ((t&0xFFFFL)<<16) | ((u&0xFFFFL)<<32) | (v.toLong<<48))
    def packInLong = new LongPack(s)
  }
  implicit final class PackCharInLong(val c: Char) extends AnyVal {
    def packInLong = new LongPack(c)
  }
  implicit final class PackIntsInLong(val i: Int) extends AnyVal {
    def packII(j: Int) = new LongPack((i&0xFFFFFFFFL) | (j.toLong << 32))
    def packInLong = new LongPack(i)
  }
  implicit final class PackFloatsInLong(val f: Float) extends AnyVal {
    def packF0 = new LongPack(java.lang.Float.floatToRawIntBits(f).toLong & 0xFFFFFFFFL)
    def pack0F = new LongPack(java.lang.Float.floatToRawIntBits(f).toLong << 32)
    def packFF(g: Float) = new LongPack((java.lang.Float.floatToRawIntBits(f).toLong & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(g).toLong << 32))
    def packInLong = new LongPack(java.lang.Float.floatToRawIntBits(f))
  }
  implicit final class PackDoubleInLong(val d: Double) extends AnyVal {
    def packD = new LongPack(java.lang.Double.doubleToRawLongBits(d))
    def packInLong = new LongPack(java.lang.Double.doubleToRawLongBits(d))
  }
  implicit final class LongAsPacked(val l: Long) extends AnyVal {
    def packed = new LongPack(l)
    def packInLong = new LongPack(l)
  }
}
