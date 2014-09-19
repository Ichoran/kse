// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse.coll

package packed {
  trait HasByteRepr extends Any { def B: Byte; def box: ByteAsBox }
  
  sealed trait Packed8bits extends Any with HasByteRepr {
    @inline def Z = bit0
    
    def bit0 = (B & 0x01) != 0
    def bit1 = (B & 0x02) != 0
    def bit2 = (B & 0x04) != 0
    def bit3 = (B & 0x08) != 0
    def bit4 = (B & 0x10) != 0
    def bit5 = (B & 0x20) != 0
    def bit6 = (B & 0x40) != 0
    def bit7 = (B & 0x80) != 0
    def bit(i: Int) = (B & (1<<i)) != 0
    def bits(i: Int, n: Int) = ((B >>> i) & (0xFF >>> (8-n))).toShort
    def bit0(b: Boolean): Packed8bits
    def bit1(b: Boolean): Packed8bits
    def bit2(b: Boolean): Packed8bits
    def bit3(b: Boolean): Packed8bits
    def bit4(b: Boolean): Packed8bits
    def bit5(b: Boolean): Packed8bits
    def bit6(b: Boolean): Packed8bits
    def bit7(b: Boolean): Packed8bits
    def bit(i: Int)(b: Boolean): Packed8bits
    def bits(i: Int, n: Int)(value: Byte): Packed8bits
  }
  
  final class ByteAsBox(val B: Byte) extends AnyVal with Packed8bits {
    @inline def box = this
    
    def bit0(b: Boolean) = new ByteAsBox((if (b) (B | 0x01) else (B & 0xFE)).toByte)
    def bit1(b: Boolean) = new ByteAsBox((if (b) (B | 0x02) else (B & 0xFD)).toByte)
    def bit2(b: Boolean) = new ByteAsBox((if (b) (B | 0x04) else (B & 0xFB)).toByte)
    def bit3(b: Boolean) = new ByteAsBox((if (b) (B | 0x08) else (B & 0xF7)).toByte)
    def bit4(b: Boolean) = new ByteAsBox((if (b) (B | 0x10) else (B & 0xEF)).toByte)
    def bit5(b: Boolean) = new ByteAsBox((if (b) (B | 0x20) else (B & 0xDF)).toByte)
    def bit6(b: Boolean) = new ByteAsBox((if (b) (B | 0x40) else (B & 0xBF)).toByte)
    def bit7(b: Boolean) = new ByteAsBox((if (b) (B | 0x80) else (B & 0x7F)).toByte)
    def bit(i: Int)(b: Boolean) = new ByteAsBox((if (b) B | (1<<i) else B & (0xFF - (1<<i))).toByte)
    def bits(i: Int, n: Int)(value: Byte) = {
      val m = (0xFF >>> (8-n)) << i
      new ByteAsBox( ((B & (0xFF - m)) | ((value << i) & m)).toByte )
    }
  }
  
  
  trait HasShortRepr extends Any { def S: Short; def box: ShortAsBox }
  
  sealed trait Packed16bits extends Any with HasShortRepr {
    @inline def Z = bit0
    def bit0  = (S & 0x0001) != 0
    def bit1  = (S & 0x0002) != 0
    def bit2  = (S & 0x0004) != 0
    def bit3  = (S & 0x0008) != 0
    def bit4  = (S & 0x0010) != 0
    def bit5  = (S & 0x0020) != 0
    def bit6  = (S & 0x0040) != 0
    def bit7  = (S & 0x0080) != 0
    def bit8  = (S & 0x0100) != 0
    def bit9  = (S & 0x0200) != 0
    def bit10 = (S & 0x0400) != 0
    def bit11 = (S & 0x0800) != 0
    def bit12 = (S & 0x1000) != 0
    def bit13 = (S & 0x2000) != 0
    def bit14 = (S & 0x4000) != 0
    def bit15 = (S & 0x8000) != 0
    def bit(i: Int) = (S & (1<<i)) != 0
    def bits(i: Int, n: Int) = ((S >>> i) & (0xFFFF >>> (16-n))).toShort
    def bit0(b: Boolean): Packed16bits
    def bit1(b: Boolean): Packed16bits
    def bit2(b: Boolean): Packed16bits
    def bit3(b: Boolean): Packed16bits
    def bit4(b: Boolean): Packed16bits
    def bit5(b: Boolean): Packed16bits
    def bit6(b: Boolean): Packed16bits
    def bit7(b: Boolean): Packed16bits
    def bit8(b: Boolean): Packed16bits
    def bit9(b: Boolean): Packed16bits
    def bit10(b: Boolean): Packed16bits
    def bit11(b: Boolean): Packed16bits
    def bit12(b: Boolean): Packed16bits
    def bit13(b: Boolean): Packed16bits
    def bit14(b: Boolean): Packed16bits
    def bit15(b: Boolean): Packed16bits
    def bit(i: Int)(b: Boolean): Packed16bits
    def bits(i: Int, n: Int)(value: Short): Packed16bits
  }
  
  sealed trait PackedBB extends Any with HasShortRepr {
    @inline def B = b0
    def b0 = (S & 0xFF).toByte
    def b1 = ((S&0xFFFF)>>8).toByte
    def b0(b: Byte): PackedBB
    def b1(b: Byte): PackedBB
    def swapB: PackedBB
  }
  
  sealed trait PackedC extends Any with HasShortRepr {
    @inline def C = c0
    def c0 = S.toChar
    def c0(c: Char): PackedC
  }
  
  final class ShortAsBox(val S: Short) extends AnyVal with Packed16bits with PackedBB with PackedC {    
    @inline def box = this
    
    def bit0(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0001) else (S & 0xFFFE)).toShort)
    def bit1(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0002) else (S & 0xFFFD)).toShort)
    def bit2(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0004) else (S & 0xFFFB)).toShort)
    def bit3(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0008) else (S & 0xFFF7)).toShort)
    def bit4(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0010) else (S & 0xFFEF)).toShort)
    def bit5(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0020) else (S & 0xFFDF)).toShort)
    def bit6(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0040) else (S & 0xFFBF)).toShort)
    def bit7(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0080) else (S & 0xFF7F)).toShort)
    def bit8(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0100) else (S & 0xFEFF)).toShort)
    def bit9(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0200) else (S & 0xFDFF)).toShort)
    def bit10(b: Boolean) = new ShortAsBox((if (b) (S | 0x0400) else (S & 0xFBFF)).toShort)
    def bit11(b: Boolean) = new ShortAsBox((if (b) (S | 0x0800) else (S & 0xF7FF)).toShort)
    def bit12(b: Boolean) = new ShortAsBox((if (b) (S | 0x1000) else (S & 0xEFFF)).toShort)
    def bit13(b: Boolean) = new ShortAsBox((if (b) (S | 0x2000) else (S & 0xDFFF)).toShort)
    def bit14(b: Boolean) = new ShortAsBox((if (b) (S | 0x4000) else (S & 0xBFFF)).toShort)
    def bit15(b: Boolean) = new ShortAsBox((if (b) (S | 0x8000) else (S & 0x7FFF)).toShort)
    def bit(i: Int)(b: Boolean) = new ShortAsBox((if (b) S | (1<<i) else S & (0xFFFF - (1<<i))).toShort)
    def bits(i: Int, n: Int)(value: Short) = {
      val m = (0xFFFF >>> (16-n)) << i
      new ShortAsBox( ((S & (0xFFFF - m)) | ((value << i) & m)).toShort )
    }    
    
    def b0(b: Byte) = new ShortAsBox(((S&0xFF00)|(b&0xFF)).toShort)
    def b1(b: Byte) = new ShortAsBox(((S&0xFF)|((b&0xFF)<<8)).toShort)
    def swapB = new ShortAsBox((((S&0xFF00)>>8) | ((S&0xFF)<<8)).toShort)
    
    def c0(c: Char) = new ShortAsBox(c.toShort)
  }
  
  
  trait HasIntRepr extends Any { def I: Int; def box: IntAsBox }
  
  sealed trait Packed32bits extends Any with HasIntRepr {
    @inline def Z = bit0
    def bit0  = (I & 0x00000001) != 0
    def bit1  = (I & 0x00000002) != 0
    def bit2  = (I & 0x00000004) != 0
    def bit3  = (I & 0x00000008) != 0
    def bit4  = (I & 0x00000010) != 0
    def bit5  = (I & 0x00000020) != 0
    def bit6  = (I & 0x00000040) != 0
    def bit7  = (I & 0x00000080) != 0
    def bit8  = (I & 0x00000100) != 0
    def bit9  = (I & 0x00000200) != 0
    def bit10 = (I & 0x00000400) != 0
    def bit11 = (I & 0x00000800) != 0
    def bit12 = (I & 0x00001000) != 0
    def bit13 = (I & 0x00002000) != 0
    def bit14 = (I & 0x00004000) != 0
    def bit15 = (I & 0x00008000) != 0
    def bit16 = (I & 0x00010000) != 0
    def bit17 = (I & 0x00020000) != 0
    def bit18 = (I & 0x00040000) != 0
    def bit19 = (I & 0x00080000) != 0
    def bit20 = (I & 0x00100000) != 0
    def bit21 = (I & 0x00200000) != 0
    def bit22 = (I & 0x00400000) != 0
    def bit23 = (I & 0x00800000) != 0
    def bit24 = (I & 0x01000000) != 0
    def bit25 = (I & 0x02000000) != 0
    def bit26 = (I & 0x04000000) != 0
    def bit27 = (I & 0x08000000) != 0
    def bit28 = (I & 0x10000000) != 0
    def bit29 = (I & 0x20000000) != 0
    def bit30 = (I & 0x40000000) != 0
    def bit31 = (I & 0x80000000) != 0    
    def bit(i: Int) = (I & (1<<i)) != 0
    def bits(i: Int, n: Int) = (I >>> i) & (-1 >>> (32-n))
    def bit0(b: Boolean): Packed32bits
    def bit1(b: Boolean): Packed32bits
    def bit2(b: Boolean): Packed32bits
    def bit3(b: Boolean): Packed32bits
    def bit4(b: Boolean): Packed32bits
    def bit5(b: Boolean): Packed32bits
    def bit6(b: Boolean): Packed32bits
    def bit7(b: Boolean): Packed32bits
    def bit8(b: Boolean): Packed32bits
    def bit9(b: Boolean): Packed32bits
    def bit10(b: Boolean): Packed32bits
    def bit11(b: Boolean): Packed32bits
    def bit12(b: Boolean): Packed32bits
    def bit13(b: Boolean): Packed32bits
    def bit14(b: Boolean): Packed32bits
    def bit15(b: Boolean): Packed32bits
    def bit16(b: Boolean): Packed32bits
    def bit17(b: Boolean): Packed32bits
    def bit18(b: Boolean): Packed32bits
    def bit19(b: Boolean): Packed32bits
    def bit20(b: Boolean): Packed32bits
    def bit21(b: Boolean): Packed32bits
    def bit22(b: Boolean): Packed32bits
    def bit23(b: Boolean): Packed32bits
    def bit24(b: Boolean): Packed32bits
    def bit25(b: Boolean): Packed32bits
    def bit26(b: Boolean): Packed32bits
    def bit27(b: Boolean): Packed32bits
    def bit28(b: Boolean): Packed32bits
    def bit29(b: Boolean): Packed32bits
    def bit30(b: Boolean): Packed32bits
    def bit31(b: Boolean): Packed32bits
    def bit(i: Int)(b: Boolean): Packed32bits
    def bits(i: Int, n: Int)(value: Int): Packed32bits
  }
  
  sealed trait PackedBBBB extends Any with HasIntRepr {
    @inline def B = b0
    def b0 =  (I & 0xFF)            .toByte
    def b1 = ((I & 0xFF00)   >>   8).toByte
    def b2 = ((I & 0xFF0000) >>  16).toByte
    def b3 =  (I             >>> 24).toByte
    def b0(b: Byte): PackedBBBB
    def b1(b: Byte): PackedBBBB
    def b2(b: Byte): PackedBBBB
    def b3(b: Byte): PackedBBBB
    def rotrB: PackedBBBB
    def rotlB: PackedBBBB
    def swapBB: PackedBBBB
    def flipB: PackedBBBB
  }
  
  sealed trait PackedSS extends Any with HasIntRepr {
    @inline def S = s0
    def s0 = (I & 0xFFFF).toShort
    def s1 = (I >>> 16).toShort
    def s0(s: Short): PackedSS
    def s1(s: Short): PackedSS
    def swapS: PackedSS
  }
  
  sealed trait PackedCC extends Any with HasIntRepr {
    @inline def C = c0
    def c0 = I.toChar
    def c1 = (I >>> 16).toChar
    def c0(c: Char): PackedCC
    def c1(c: Char): PackedCC
    def swapC: PackedCC
  }
  
  sealed trait PackedF extends Any with HasIntRepr {
    @inline def F = f0
    def f0 = java.lang.Float.intBitsToFloat(I)
  }
  
  final class IntAsBox(val I: Int) extends AnyVal with Packed32bits with PackedBBBB with PackedSS with PackedCC with PackedF {
    @inline def box = this
    
    def bit0(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000001) else (I & 0xFFFFFFFE))
    def bit1(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000002) else (I & 0xFFFFFFFD))
    def bit2(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000004) else (I & 0xFFFFFFFB))
    def bit3(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000008) else (I & 0xFFFFFFF7))
    def bit4(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000010) else (I & 0xFFFFFFEF))
    def bit5(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000020) else (I & 0xFFFFFFDF))
    def bit6(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000040) else (I & 0xFFFFFFBF))
    def bit7(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000080) else (I & 0xFFFFFF7F))
    def bit8(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000100) else (I & 0xFFFFFEFF))
    def bit9(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000200) else (I & 0xFFFFFDFF))
    def bit10(b: Boolean) = new IntAsBox(if (b) (I | 0x00000400) else (I & 0xFFFFFBFF))
    def bit11(b: Boolean) = new IntAsBox(if (b) (I | 0x00000800) else (I & 0xFFFFF7FF))
    def bit12(b: Boolean) = new IntAsBox(if (b) (I | 0x00001000) else (I & 0xFFFFEFFF))
    def bit13(b: Boolean) = new IntAsBox(if (b) (I | 0x00002000) else (I & 0xFFFFDFFF))
    def bit14(b: Boolean) = new IntAsBox(if (b) (I | 0x00004000) else (I & 0xFFFFBFFF))
    def bit15(b: Boolean) = new IntAsBox(if (b) (I | 0x00008000) else (I & 0xFFFF7FFF))
    def bit16(b: Boolean) = new IntAsBox(if (b) (I | 0x00010000) else (I & 0xFFFEFFFF))
    def bit17(b: Boolean) = new IntAsBox(if (b) (I | 0x00020000) else (I & 0xFFFDFFFF))
    def bit18(b: Boolean) = new IntAsBox(if (b) (I | 0x00040000) else (I & 0xFFFBFFFF))
    def bit19(b: Boolean) = new IntAsBox(if (b) (I | 0x00080000) else (I & 0xFFF7FFFF))
    def bit20(b: Boolean) = new IntAsBox(if (b) (I | 0x00100000) else (I & 0xFFEFFFFF))
    def bit21(b: Boolean) = new IntAsBox(if (b) (I | 0x00200000) else (I & 0xFFDFFFFF))
    def bit22(b: Boolean) = new IntAsBox(if (b) (I | 0x00400000) else (I & 0xFFBFFFFF))
    def bit23(b: Boolean) = new IntAsBox(if (b) (I | 0x00800000) else (I & 0xFF7FFFFF))
    def bit24(b: Boolean) = new IntAsBox(if (b) (I | 0x01000000) else (I & 0xFEFFFFFF))
    def bit25(b: Boolean) = new IntAsBox(if (b) (I | 0x02000000) else (I & 0xFDFFFFFF))
    def bit26(b: Boolean) = new IntAsBox(if (b) (I | 0x04000000) else (I & 0xFBFFFFFF))
    def bit27(b: Boolean) = new IntAsBox(if (b) (I | 0x08000000) else (I & 0xF7FFFFFF))
    def bit28(b: Boolean) = new IntAsBox(if (b) (I | 0x10000000) else (I & 0xEFFFFFFF))
    def bit29(b: Boolean) = new IntAsBox(if (b) (I | 0x20000000) else (I & 0xDFFFFFFF))
    def bit30(b: Boolean) = new IntAsBox(if (b) (I | 0x40000000) else (I & 0xBFFFFFFF))
    def bit31(b: Boolean) = new IntAsBox(if (b) (I | 0x80000000) else (I & 0x7FFFFFFF))
    def bit(i: Int)(b: Boolean) = new IntAsBox(if (b) I | (1<<i) else I & (0xFFFFFFFF - (1<<i)))
    def bits(i: Int, n: Int)(value: Int) = {
      val m = (-1 >>> (32-n)) << i
      new IntAsBox( (I & (-1 - m)) | ((value << i) & m) )
    }

    def b0(b: Byte) = new IntAsBox((I & 0xFFFFFF00) | (b & 0xFF))
    def b1(b: Byte) = new IntAsBox((I & 0xFFFF00FF) | ((b & 0xFF) << 8))
    def b2(b: Byte) = new IntAsBox((I & 0xFF00FFFF) | ((b & 0xFF) << 16))
    def b3(b: Byte) = new IntAsBox((I & 0x00FFFFFF) | ((b & 0xFF) << 24))
    def rotrB = new IntAsBox((I >>> 8) | (I << 24))
    def rotlB = new IntAsBox((I >>> 24) | (I << 8))
    def swapBB = new IntAsBox(((I & 0xFF00FF00) >>> 8) | ((I & 0x00FF00FF) << 8))
    def flipB = new IntAsBox(((I&0xFF000000)>>24) | ((I&0xFF0000)>>8) | ((I&0xFF00)<<8) | ((I&0xFF)<<24))

    def s0(s: Short) = new IntAsBox((I&0xFFFF0000) | (s&0xFFFF))
    def s1(s: Short) = new IntAsBox((I&0xFFFF) | ((s&0xFFFF)<<16))
    def swapS = new IntAsBox((I>>>16) | (I<<16))

    def c0(c: Char) = new IntAsBox((I&0xFFFF0000) | c)
    def c1(c: Char) = new IntAsBox((I&0xFFFF) | (c<<16))
    def swapC = new IntAsBox((I>>>16) | (I<<16))

    def f0(f: Float) = java.lang.Float.floatToRawIntBits(f)
  }


  sealed trait HasLongRepr extends Any { def L: Long; def box: LongAsBox }

  sealed trait Packed64bits extends Any with HasLongRepr {
    @inline def Z = bit0
    def bit0  = (L & 0x0000000000000001L) != 0
    def bit1  = (L & 0x0000000000000002L) != 0
    def bit2  = (L & 0x0000000000000004L) != 0
    def bit3  = (L & 0x0000000000000008L) != 0
    def bit4  = (L & 0x0000000000000010L) != 0
    def bit5  = (L & 0x0000000000000020L) != 0
    def bit6  = (L & 0x0000000000000040L) != 0
    def bit7  = (L & 0x0000000000000080L) != 0
    def bit8  = (L & 0x0000000000000100L) != 0
    def bit9  = (L & 0x0000000000000200L) != 0
    def bit10 = (L & 0x0000000000000400L) != 0
    def bit11 = (L & 0x0000000000000800L) != 0
    def bit12 = (L & 0x0000000000001000L) != 0
    def bit13 = (L & 0x0000000000002000L) != 0
    def bit14 = (L & 0x0000000000004000L) != 0
    def bit15 = (L & 0x0000000000008000L) != 0
    def bit16 = (L & 0x0000000000010000L) != 0
    def bit17 = (L & 0x0000000000020000L) != 0
    def bit18 = (L & 0x0000000000040000L) != 0
    def bit19 = (L & 0x0000000000080000L) != 0
    def bit20 = (L & 0x0000000000100000L) != 0
    def bit21 = (L & 0x0000000000200000L) != 0
    def bit22 = (L & 0x0000000000400000L) != 0
    def bit23 = (L & 0x0000000000800000L) != 0
    def bit24 = (L & 0x0000000001000000L) != 0
    def bit25 = (L & 0x0000000002000000L) != 0
    def bit26 = (L & 0x0000000004000000L) != 0
    def bit27 = (L & 0x0000000008000000L) != 0
    def bit28 = (L & 0x0000000010000000L) != 0
    def bit29 = (L & 0x0000000020000000L) != 0
    def bit30 = (L & 0x0000000040000000L) != 0
    def bit31 = (L & 0x0000000080000000L) != 0
    def bit32 = (L & 0x0000000100000000L) != 0
    def bit33 = (L & 0x0000000200000000L) != 0
    def bit34 = (L & 0x0000000400000000L) != 0
    def bit35 = (L & 0x0000000800000000L) != 0
    def bit36 = (L & 0x0000001000000000L) != 0
    def bit37 = (L & 0x0000002000000000L) != 0
    def bit38 = (L & 0x0000004000000000L) != 0
    def bit39 = (L & 0x0000008000000000L) != 0
    def bit40 = (L & 0x0000010000000000L) != 0
    def bit41 = (L & 0x0000020000000000L) != 0
    def bit42 = (L & 0x0000040000000000L) != 0
    def bit43 = (L & 0x0000080000000000L) != 0
    def bit44 = (L & 0x0000100000000000L) != 0
    def bit45 = (L & 0x0000200000000000L) != 0
    def bit46 = (L & 0x0000400000000000L) != 0
    def bit47 = (L & 0x0000800000000000L) != 0
    def bit48 = (L & 0x0001000000000000L) != 0
    def bit49 = (L & 0x0002000000000000L) != 0
    def bit50 = (L & 0x0004000000000000L) != 0
    def bit51 = (L & 0x0008000000000000L) != 0
    def bit52 = (L & 0x0010000000000000L) != 0
    def bit53 = (L & 0x0020000000000000L) != 0
    def bit54 = (L & 0x0040000000000000L) != 0
    def bit55 = (L & 0x0080000000000000L) != 0
    def bit56 = (L & 0x0100000000000000L) != 0
    def bit57 = (L & 0x0200000000000000L) != 0
    def bit58 = (L & 0x0400000000000000L) != 0
    def bit59 = (L & 0x0800000000000000L) != 0
    def bit60 = (L & 0x1000000000000000L) != 0
    def bit61 = (L & 0x2000000000000000L) != 0
    def bit62 = (L & 0x4000000000000000L) != 0
    def bit63 = (L & 0x8000000000000000L) != 0
    def bit(i: Int) = (L & (1L<<i)) != 0
    def bits(i: Int, n: Int) = (L >>> i) & (-1L >>> (64-n))
    def bit0(b: Boolean): Packed64bits
    def bit1(b: Boolean): Packed64bits
    def bit2(b: Boolean): Packed64bits
    def bit3(b: Boolean): Packed64bits
    def bit4(b: Boolean): Packed64bits
    def bit5(b: Boolean): Packed64bits
    def bit6(b: Boolean): Packed64bits
    def bit7(b: Boolean): Packed64bits
    def bit8(b: Boolean): Packed64bits
    def bit9(b: Boolean): Packed64bits
    def bit10(b: Boolean): Packed64bits
    def bit11(b: Boolean): Packed64bits
    def bit12(b: Boolean): Packed64bits
    def bit13(b: Boolean): Packed64bits
    def bit14(b: Boolean): Packed64bits
    def bit15(b: Boolean): Packed64bits
    def bit16(b: Boolean): Packed64bits
    def bit17(b: Boolean): Packed64bits
    def bit18(b: Boolean): Packed64bits
    def bit19(b: Boolean): Packed64bits
    def bit20(b: Boolean): Packed64bits
    def bit21(b: Boolean): Packed64bits
    def bit22(b: Boolean): Packed64bits
    def bit23(b: Boolean): Packed64bits
    def bit24(b: Boolean): Packed64bits
    def bit25(b: Boolean): Packed64bits
    def bit26(b: Boolean): Packed64bits
    def bit27(b: Boolean): Packed64bits
    def bit28(b: Boolean): Packed64bits
    def bit29(b: Boolean): Packed64bits
    def bit30(b: Boolean): Packed64bits
    def bit31(b: Boolean): Packed64bits
    def bit32(b: Boolean): Packed64bits
    def bit33(b: Boolean): Packed64bits
    def bit34(b: Boolean): Packed64bits
    def bit35(b: Boolean): Packed64bits
    def bit36(b: Boolean): Packed64bits
    def bit37(b: Boolean): Packed64bits
    def bit38(b: Boolean): Packed64bits
    def bit39(b: Boolean): Packed64bits
    def bit40(b: Boolean): Packed64bits
    def bit41(b: Boolean): Packed64bits
    def bit42(b: Boolean): Packed64bits
    def bit43(b: Boolean): Packed64bits
    def bit44(b: Boolean): Packed64bits
    def bit45(b: Boolean): Packed64bits
    def bit46(b: Boolean): Packed64bits
    def bit47(b: Boolean): Packed64bits
    def bit48(b: Boolean): Packed64bits
    def bit49(b: Boolean): Packed64bits
    def bit50(b: Boolean): Packed64bits
    def bit51(b: Boolean): Packed64bits
    def bit52(b: Boolean): Packed64bits
    def bit53(b: Boolean): Packed64bits
    def bit54(b: Boolean): Packed64bits
    def bit55(b: Boolean): Packed64bits
    def bit56(b: Boolean): Packed64bits
    def bit57(b: Boolean): Packed64bits
    def bit58(b: Boolean): Packed64bits
    def bit59(b: Boolean): Packed64bits
    def bit60(b: Boolean): Packed64bits
    def bit61(b: Boolean): Packed64bits
    def bit62(b: Boolean): Packed64bits
    def bit63(b: Boolean): Packed64bits
    def bit(i: Int)(b: Boolean): Packed64bits
    def bits(i: Int, n: Int)(value: Long): Packed64bits
  }

  sealed trait PackedB8 extends Any with HasLongRepr {
    @inline def B = b0
    def b0 =  (L & 0xFF)                     .toByte
    def b1 = ((L & 0xFF00)            >>   8).toByte
    def b2 = ((L & 0xFF0000)          >>  16).toByte
    def b3 = ((L & 0xFF000000L)       >>  24).toByte
    def b4 = ((L & 0xFF00000000L)     >>  32).toByte
    def b5 = ((L & 0xFF0000000000L)   >>  40).toByte
    def b6 = ((L & 0xFF000000000000L) >>  48).toByte
    def b7 = ( L                      >>> 56).toByte
    def b0(b: Byte): PackedB8
    def b1(b: Byte): PackedB8
    def b2(b: Byte): PackedB8
    def b3(b: Byte): PackedB8
    def b4(b: Byte): PackedB8
    def b5(b: Byte): PackedB8
    def b6(b: Byte): PackedB8
    def b7(b: Byte): PackedB8
    def rotrB: PackedB8
    def rotlB: PackedB8
    def swapBB: PackedB8
    def reverseB: PackedB8
  }

  sealed trait PackedSSSS extends Any with HasLongRepr {
    @inline def S = s0
    def s0 =  (L & 0xFFFF)                 .toShort
    def s1 = ((L & 0xFFFF0000L)     >>  16).toShort
    def s2 = ((L & 0xFFFF00000000L) >>  32).toShort
    def s3 = ( L                    >>> 48).toShort
    def s0(s: Short): PackedSSSS
    def s1(s: Short): PackedSSSS
    def s2(s: Short): PackedSSSS
    def s3(s: Short): PackedSSSS
    def rotrS: PackedSSSS
    def rotlS: PackedSSSS
    def swapSS: PackedSSSS
    def reverseS: PackedSSSS
  }
  
  sealed trait PackedCCCC extends Any with HasLongRepr {
    @inline def C = c0
    def c0 =  (L & 0xFFFF)                 .toChar
    def c1 = ((L & 0xFFFF0000L)     >>  16).toChar
    def c2 = ((L & 0xFFFF00000000L) >>  32).toChar
    def c3 = ( L                    >>> 48).toChar
    def c0(c: Char): PackedCCCC
    def c1(c: Char): PackedCCCC
    def c2(c: Char): PackedCCCC
    def c3(c: Char): PackedCCCC
    def rotrC: PackedCCCC
    def rotlC: PackedCCCC
    def swapCC: PackedCCCC
    def reverseC: PackedCCCC
  }

  sealed trait PackedII extends Any with HasLongRepr {
    @inline def I = i0
    def i0 = (L & 0xFFFFFFFFL).toInt
    def i1 = (L >>> 32).toInt
    def i0(i: Int): PackedII
    def i1(i: Int): PackedII
    def swapI: PackedII    
  }

  sealed trait PackedFF extends Any with HasLongRepr {
    @inline def F = f0
    def f0 = java.lang.Float.intBitsToFloat((L & 0xFFFFFFFFL).toInt)
    def f1 = java.lang.Float.intBitsToFloat((L >>> 32).toInt)
    def f0(f: Float): PackedFF
    def f1(f: Float): PackedFF
    def swapF: PackedFF
  }
  
  sealed trait PackedD extends Any with HasLongRepr {
    @inline def D = d0
    def d0 = java.lang.Double.longBitsToDouble(L)
    def d0(d: Double): PackedD
  }
  
  final class LongAsBox(val L: Long) extends AnyVal with Packed64bits with PackedB8 with PackedSSSS with PackedCCCC with PackedII with PackedFF with PackedD {
    def box = this
    
    def bit0(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000001L) else (L & 0xFFFFFFFFFFFFFFFEL))
    def bit1(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000002L) else (L & 0xFFFFFFFFFFFFFFFDL))
    def bit2(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000004L) else (L & 0xFFFFFFFFFFFFFFFBL))
    def bit3(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000008L) else (L & 0xFFFFFFFFFFFFFFF7L))
    def bit4(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000010L) else (L & 0xFFFFFFFFFFFFFFEFL))
    def bit5(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000020L) else (L & 0xFFFFFFFFFFFFFFDFL))
    def bit6(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000040L) else (L & 0xFFFFFFFFFFFFFFBFL))
    def bit7(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000080L) else (L & 0xFFFFFFFFFFFFFF7FL))
    def bit8(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000100L) else (L & 0xFFFFFFFFFFFFFEFFL))
    def bit9(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000200L) else (L & 0xFFFFFFFFFFFFFDFFL))
    def bit10(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000000400L) else (L & 0xFFFFFFFFFFFFFBFFL))
    def bit11(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000000800L) else (L & 0xFFFFFFFFFFFFF7FFL))
    def bit12(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000001000L) else (L & 0xFFFFFFFFFFFFEFFFL))
    def bit13(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000002000L) else (L & 0xFFFFFFFFFFFFDFFFL))
    def bit14(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000004000L) else (L & 0xFFFFFFFFFFFFBFFFL))
    def bit15(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000008000L) else (L & 0xFFFFFFFFFFFF7FFFL))
    def bit16(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000010000L) else (L & 0xFFFFFFFFFFFEFFFFL))
    def bit17(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000020000L) else (L & 0xFFFFFFFFFFFDFFFFL))
    def bit18(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000040000L) else (L & 0xFFFFFFFFFFFBFFFFL))
    def bit19(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000080000L) else (L & 0xFFFFFFFFFFF7FFFFL))
    def bit20(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000100000L) else (L & 0xFFFFFFFFFFEFFFFFL))
    def bit21(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000200000L) else (L & 0xFFFFFFFFFFDFFFFFL))
    def bit22(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000400000L) else (L & 0xFFFFFFFFFFBFFFFFL))
    def bit23(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000800000L) else (L & 0xFFFFFFFFFF7FFFFFL))
    def bit24(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000001000000L) else (L & 0xFFFFFFFFFEFFFFFFL))
    def bit25(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000002000000L) else (L & 0xFFFFFFFFFDFFFFFFL))
    def bit26(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000004000000L) else (L & 0xFFFFFFFFFBFFFFFFL))
    def bit27(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000008000000L) else (L & 0xFFFFFFFFF7FFFFFFL))
    def bit28(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000010000000L) else (L & 0xFFFFFFFFEFFFFFFFL))
    def bit29(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000020000000L) else (L & 0xFFFFFFFFDFFFFFFFL))
    def bit30(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000040000000L) else (L & 0xFFFFFFFFBFFFFFFFL))
    def bit31(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000080000000L) else (L & 0xFFFFFFFF7FFFFFFFL))
    def bit32(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000100000000L) else (L & 0xFFFFFFFEFFFFFFFFL))
    def bit33(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000200000000L) else (L & 0xFFFFFFFDFFFFFFFFL))
    def bit34(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000400000000L) else (L & 0xFFFFFFFBFFFFFFFFL))
    def bit35(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000800000000L) else (L & 0xFFFFFFF7FFFFFFFFL))
    def bit36(b: Boolean) = new LongAsBox(if (b) (L | 0x0000001000000000L) else (L & 0xFFFFFFEFFFFFFFFFL))
    def bit37(b: Boolean) = new LongAsBox(if (b) (L | 0x0000002000000000L) else (L & 0xFFFFFFDFFFFFFFFFL))
    def bit38(b: Boolean) = new LongAsBox(if (b) (L | 0x0000004000000000L) else (L & 0xFFFFFFBFFFFFFFFFL))
    def bit39(b: Boolean) = new LongAsBox(if (b) (L | 0x0000008000000000L) else (L & 0xFFFFFF7FFFFFFFFFL))
    def bit40(b: Boolean) = new LongAsBox(if (b) (L | 0x0000010000000000L) else (L & 0xFFFFFEFFFFFFFFFFL))
    def bit41(b: Boolean) = new LongAsBox(if (b) (L | 0x0000020000000000L) else (L & 0xFFFFFDFFFFFFFFFFL))
    def bit42(b: Boolean) = new LongAsBox(if (b) (L | 0x0000040000000000L) else (L & 0xFFFFFBFFFFFFFFFFL))
    def bit43(b: Boolean) = new LongAsBox(if (b) (L | 0x0000080000000000L) else (L & 0xFFFFF7FFFFFFFFFFL))
    def bit44(b: Boolean) = new LongAsBox(if (b) (L | 0x0000100000000000L) else (L & 0xFFFFEFFFFFFFFFFFL))
    def bit45(b: Boolean) = new LongAsBox(if (b) (L | 0x0000200000000000L) else (L & 0xFFFFDFFFFFFFFFFFL))
    def bit46(b: Boolean) = new LongAsBox(if (b) (L | 0x0000400000000000L) else (L & 0xFFFFBFFFFFFFFFFFL))
    def bit47(b: Boolean) = new LongAsBox(if (b) (L | 0x0000800000000000L) else (L & 0xFFFF7FFFFFFFFFFFL))
    def bit48(b: Boolean) = new LongAsBox(if (b) (L | 0x0001000000000000L) else (L & 0xFFFEFFFFFFFFFFFFL))
    def bit49(b: Boolean) = new LongAsBox(if (b) (L | 0x0002000000000000L) else (L & 0xFFFDFFFFFFFFFFFFL))
    def bit50(b: Boolean) = new LongAsBox(if (b) (L | 0x0004000000000000L) else (L & 0xFFFBFFFFFFFFFFFFL))
    def bit51(b: Boolean) = new LongAsBox(if (b) (L | 0x0008000000000000L) else (L & 0xFFF7FFFFFFFFFFFFL))
    def bit52(b: Boolean) = new LongAsBox(if (b) (L | 0x0010000000000000L) else (L & 0xFFEFFFFFFFFFFFFFL))
    def bit53(b: Boolean) = new LongAsBox(if (b) (L | 0x0020000000000000L) else (L & 0xFFDFFFFFFFFFFFFFL))
    def bit54(b: Boolean) = new LongAsBox(if (b) (L | 0x0040000000000000L) else (L & 0xFFBFFFFFFFFFFFFFL))
    def bit55(b: Boolean) = new LongAsBox(if (b) (L | 0x0080000000000000L) else (L & 0xFF7FFFFFFFFFFFFFL))
    def bit56(b: Boolean) = new LongAsBox(if (b) (L | 0x0100000000000000L) else (L & 0xFEFFFFFFFFFFFFFFL))
    def bit57(b: Boolean) = new LongAsBox(if (b) (L | 0x0200000000000000L) else (L & 0xFDFFFFFFFFFFFFFFL))
    def bit58(b: Boolean) = new LongAsBox(if (b) (L | 0x0400000000000000L) else (L & 0xFBFFFFFFFFFFFFFFL))
    def bit59(b: Boolean) = new LongAsBox(if (b) (L | 0x0800000000000000L) else (L & 0xF7FFFFFFFFFFFFFFL))
    def bit60(b: Boolean) = new LongAsBox(if (b) (L | 0x1000000000000000L) else (L & 0xEFFFFFFFFFFFFFFFL))
    def bit61(b: Boolean) = new LongAsBox(if (b) (L | 0x2000000000000000L) else (L & 0xDFFFFFFFFFFFFFFFL))
    def bit62(b: Boolean) = new LongAsBox(if (b) (L | 0x4000000000000000L) else (L & 0xBFFFFFFFFFFFFFFFL))
    def bit63(b: Boolean) = new LongAsBox(if (b) (L | 0x8000000000000000L) else (L & 0x7FFFFFFFFFFFFFFFL))
    def bit(i: Int)(b: Boolean) = new LongAsBox(if (b) L | (1L<<i) else L & (-1L - (1L<<i)))
    def bits(i: Int, n: Int)(value: Long) = {
      val m = (-1L >>> (64-n)) << i
      new LongAsBox( (L & (-1L - m)) | ((value << i) & m) )
    }

    def b0(b: Byte) = new LongAsBox((L&0xFFFFFFFFFFFFFF00L) |  (b&0xFF))
    def b1(b: Byte) = new LongAsBox((L&0xFFFFFFFFFFFF00FFL) | ((b&0xFF)<<8))
    def b2(b: Byte) = new LongAsBox((L&0xFFFFFFFFFF00FFFFL) | ((b&0xFF)<<16))
    def b3(b: Byte) = new LongAsBox((L&0xFFFFFFFF00FFFFFFL) | ((b&0xFF).toLong<<24))
    def b4(b: Byte) = new LongAsBox((L&0xFFFFFF00FFFFFFFFL) | ((b&0xFF).toLong<<32))
    def b5(b: Byte) = new LongAsBox((L&0xFFFF00FFFFFFFFFFL) | ((b&0xFF).toLong<<40))
    def b6(b: Byte) = new LongAsBox((L&0xFF00FFFFFFFFFFFFL) | ((b&0xFF).toLong<<48))
    def b7(b: Byte) = new LongAsBox((L&0x00FFFFFFFFFFFFFFL) | ((b&0xFF).toLong<<56))
    def rotrB = new LongAsBox((L >>> 8) | (L << 56))
    def rotlB = new LongAsBox((L >>> 56) | (L << 8))
    def swapBB = new LongAsBox(((L&0xFF00FF00FF00FF00L)>>>8) | ((L&0x00FF00FF00FF00FFL)<<8))
    def reverseB = {
      val m = swapBB.L
      new LongAsBox((m>>>48) | ((m&0xFFFF00000000L)>>16) | ((m&0xFFFF0000L)<<16) | (m<<48))
    }

    def s0(s: Short) = new LongAsBox((L & 0xFFFFFFFFFFFF0000L) | (s&0xFFFF))
    def s1(s: Short) = new LongAsBox((L & 0xFFFFFFFF0000FFFFL) | ((s&0xFFFF).toLong << 16))
    def s2(s: Short) = new LongAsBox((L & 0xFFFF0000FFFFFFFFL) | ((s&0xFFFF).toLong << 32))
    def s3(s: Short) = new LongAsBox((L & 0x0000FFFFFFFFFFFFL) | (s.toLong << 48))
    def rotrS = new LongAsBox((L >>> 16) | (L << 48))
    def rotlS = new LongAsBox((L >>> 48) | (L << 16))
    def swapSS = new LongAsBox(((L&0xFFFF0000FFFF0000L)>>>16) | ((L&0x0000FFFF0000FFFFL)<<16))
    def reverseS = new LongAsBox((L>>>48) | ((L&0xFFFF00000000L)>>16) | ((L&0xFFFF0000L)<<16) | (L<<48))
    
    def c0(c: Char) = new LongAsBox((L & 0xFFFFFFFFFFFF0000L) | c)
    def c1(c: Char) = new LongAsBox((L & 0xFFFFFFFF0000FFFFL) | (c.toLong << 16))
    def c2(c: Char) = new LongAsBox((L & 0xFFFF0000FFFFFFFFL) | (c.toLong << 32))
    def c3(c: Char) = new LongAsBox((L & 0x0000FFFFFFFFFFFFL) | (c.toLong << 48))
    def rotrC = new LongAsBox((L >>> 16) | (L << 48))
    def rotlC = new LongAsBox((L >>> 48) | (L << 16))
    def swapCC = new LongAsBox(((L&0xFFFF0000FFFF0000L)>>>16) | ((L&0x0000FFFF0000FFFFL)<<16))
    def reverseC = new LongAsBox((L>>>48) | ((L&0xFFFF00000000L)>>16) | ((L&0xFFFF0000L)<<16) | (L<<48))
    
    def i0(i: Int) = new LongAsBox((L&0xFFFFFFFF00000000L) | (L&0xFFFFFFFFL))
    def i1(i: Int) = new LongAsBox((L&0xFFFFFFFFL) | (i.toLong<<32))
    def swapI = new LongAsBox((L >>> 32) | (L << 32))
    
    def f0(f: Float) = i0(java.lang.Float.floatToRawIntBits(f))
    def f1(f: Float) = i1(java.lang.Float.floatToRawIntBits(f))
    def swapF = swapI
    
    def d0(d: Double) = new LongAsBox(java.lang.Double.doubleToRawLongBits(d))
  }
}

package object packed {
  implicit final class PackBooleanInByte(val z: Boolean) extends AnyVal {
    def inByte = new ByteAsBox(if (z) 1 else 0)
  }
  implicit final class PackByteInByte(val b: Byte) extends AnyVal {
    def inByte = new ByteAsBox(b)
  }
  
  implicit final class PackBooleanInShort(val z: Boolean) extends AnyVal {
    def inShort = new ShortAsBox(if (z) 1 else 0)
  }
  implicit final class PackBytesInShort(val b: Byte) extends AnyVal {
    def packBB(c: Byte) = new ShortAsBox(((b&0xFF) | ((c&0xFF)<<8)).toShort)
    def inShort = new ShortAsBox((b & 0xFF).toShort)
  }
  implicit final class PackCharInShort(val c: Char) extends AnyVal {
    def inShort = new ShortAsBox(c.toShort)
  }
  implicit final class PackShortInShort(val s: Short) extends AnyVal {
    def inShort = new ShortAsBox(s)
  }
  
  implicit final class PackBooleanInInt(val z: Boolean) extends AnyVal {
    def inInt = new IntAsBox(if (z) 1 else 0)
  }
  implicit final class PackBytesInInt(val b: Byte) extends AnyVal {
    def packBBBB(c: Byte, d: Byte, e: Byte) = new IntAsBox((b&0xFF) | ((c&0xFF)<<8) | ((d&0xFF)<<16) | (e.toInt<<24))
    def inInt = new IntAsBox((b & 0xFF))
  }
  implicit final class PackShortsInInt(val s: Short) extends AnyVal {
    def packSS(t: Short) = new IntAsBox((s&0xFFFF) | (t.toInt << 16))
    def inInt = new IntAsBox(s)
  }
  implicit final class PackCharsInInt(val c: Char) extends AnyVal {
    def packCC(d: Char) = new IntAsBox(c | (d<<16))
    def inInt = new IntAsBox(c)
  }
  implicit final class PackFloatInInt(val f: Float) extends AnyVal {
    def inInt = new IntAsBox(java.lang.Float.floatToRawIntBits(f))
  }
  implicit final class PackIntInInt(val i: Int) extends AnyVal {
    def inInt = new IntAsBox(i)
  }


  implicit final class PackBooleanInLong(val bool: Boolean) extends AnyVal {
    def inLong = if (bool) 1 else 0
  }
  implicit final class PackBytesInLong(val b: Byte) extends AnyVal {
    def packB8(c: Byte, d: Byte, e: Byte, f: Byte, g: Byte, h: Byte, i: Byte): PackedB8 = {
      new LongAsBox((b&0xFFL) | ((c&0xFFL)<<8) | ((d&0xFFL)<<16) | ((e&0xFFL)<<24) | ((f&0xFFL)<<32) | ((g&0xFFL)<<40) | ((h&0xFFL)<<48) | (i.toLong<<56))
    }
    def inLong = new LongAsBox(b)
  }
  implicit final class PackShortsInLong(val s: Short) extends AnyVal {
    def packSSSS(t: Short, u: Short, v: Short): PackedSSSS = new LongAsBox((s&0xFFFFL) | ((t&0xFFFFL)<<16) | ((u&0xFFFFL)<<32) | (v.toLong<<48))
    def inLong = new LongAsBox(s)
  }
  implicit final class PackCharInLong(val c: Char) extends AnyVal {
    def packCCCC(d: Char, e: Char, f: Char): PackedCCCC = new LongAsBox(c | (c.toLong<<16) | (c.toLong<<32) | (c.toLong<<48))
    def inLong = new LongAsBox(c)
  }
  implicit final class PackIntsInLong(val i: Int) extends AnyVal {
    def packII(j: Int) = new LongAsBox((i&0xFFFFFFFFL) | (j.toLong << 32))
    def inLong = new LongAsBox(i)
  }
  implicit final class PackFloatsInLong(val f: Float) extends AnyVal {
    def packFF(g: Float): PackedFF = new LongAsBox((java.lang.Float.floatToRawIntBits(f).toLong & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(g).toLong << 32))
    def inLong = new LongAsBox(java.lang.Float.floatToRawIntBits(f))
  }
  implicit final class PackDoubleInLong(val d: Double) extends AnyVal {
    def inLong = new LongAsBox(java.lang.Double.doubleToRawLongBits(d))
  }
  implicit final class PackLongInLong(val l: Long) extends AnyVal {
    def inLong = new LongAsBox(l)
  }
}
