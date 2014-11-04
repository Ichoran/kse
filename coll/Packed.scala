// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014 Rex Kerr and UCSF

package kse.coll

package packed {
  final class ByteAsBox(val B: Byte) extends AnyVal {
    @inline def Z = bit0
    
    @inline def bit0 = (B & 0x01) != 0
    @inline def bit1 = (B & 0x02) != 0
    @inline def bit2 = (B & 0x04) != 0
    @inline def bit3 = (B & 0x08) != 0
    @inline def bit4 = (B & 0x10) != 0
    @inline def bit5 = (B & 0x20) != 0
    @inline def bit6 = (B & 0x40) != 0
    @inline def bit7 = (B & 0x80) != 0
    @inline def bit(i: Int) = (B & (1<<i)) != 0
    @inline def bits(i: Int, n: Int) = ((B >>> i) & (0xFF >>> (8-n))).toByte
    
    @inline def bit0Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x01) else (B & 0xFE)).toByte)
    @inline def bit1Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x02) else (B & 0xFD)).toByte)
    @inline def bit2Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x04) else (B & 0xFB)).toByte)
    @inline def bit3Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x08) else (B & 0xF7)).toByte)
    @inline def bit4Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x10) else (B & 0xEF)).toByte)
    @inline def bit5Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x20) else (B & 0xDF)).toByte)
    @inline def bit6Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x40) else (B & 0xBF)).toByte)
    @inline def bit7Is(b: Boolean) = new ByteAsBox((if (b) (B | 0x80) else (B & 0x7F)).toByte)
    @inline def bitIs(i: Int)(b: Boolean) = new ByteAsBox((if (b) B | (1<<i) else B & (0xFF - (1<<i))).toByte)
    @inline def bitsAre(i: Int, n: Int)(value: Byte) = {
      val m = (0xFF >>> (8-n)) << i
      new ByteAsBox( ((B & (0xFF - m)) | ((value << i) & m)).toByte )
    }
  }
  
  final class ShortAsBox(val S: Short) extends AnyVal {    
    @inline def Z = bit0
    @inline def bit0  = (S & 0x0001) != 0
    @inline def bit1  = (S & 0x0002) != 0
    @inline def bit2  = (S & 0x0004) != 0
    @inline def bit3  = (S & 0x0008) != 0
    @inline def bit4  = (S & 0x0010) != 0
    @inline def bit5  = (S & 0x0020) != 0
    @inline def bit6  = (S & 0x0040) != 0
    @inline def bit7  = (S & 0x0080) != 0
    @inline def bit8  = (S & 0x0100) != 0
    @inline def bit9  = (S & 0x0200) != 0
    @inline def bit10 = (S & 0x0400) != 0
    @inline def bit11 = (S & 0x0800) != 0
    @inline def bit12 = (S & 0x1000) != 0
    @inline def bit13 = (S & 0x2000) != 0
    @inline def bit14 = (S & 0x4000) != 0
    @inline def bit15 = (S & 0x8000) != 0
    @inline def bit(i: Int) = (S & (1<<i)) != 0
    @inline def bits(i: Int, n: Int) = ((S >>> i) & (0xFFFF >>> (16-n))).toShort
    @inline def bit0Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0001) else (S & 0xFFFE)).toShort)
    @inline def bit1Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0002) else (S & 0xFFFD)).toShort)
    @inline def bit2Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0004) else (S & 0xFFFB)).toShort)
    @inline def bit3Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0008) else (S & 0xFFF7)).toShort)
    @inline def bit4Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0010) else (S & 0xFFEF)).toShort)
    @inline def bit5Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0020) else (S & 0xFFDF)).toShort)
    @inline def bit6Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0040) else (S & 0xFFBF)).toShort)
    @inline def bit7Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0080) else (S & 0xFF7F)).toShort)
    @inline def bit8Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0100) else (S & 0xFEFF)).toShort)
    @inline def bit9Is(b: Boolean)  = new ShortAsBox((if (b) (S | 0x0200) else (S & 0xFDFF)).toShort)
    @inline def bit10Is(b: Boolean) = new ShortAsBox((if (b) (S | 0x0400) else (S & 0xFBFF)).toShort)
    @inline def bit11Is(b: Boolean) = new ShortAsBox((if (b) (S | 0x0800) else (S & 0xF7FF)).toShort)
    @inline def bit12Is(b: Boolean) = new ShortAsBox((if (b) (S | 0x1000) else (S & 0xEFFF)).toShort)
    @inline def bit13Is(b: Boolean) = new ShortAsBox((if (b) (S | 0x2000) else (S & 0xDFFF)).toShort)
    @inline def bit14Is(b: Boolean) = new ShortAsBox((if (b) (S | 0x4000) else (S & 0xBFFF)).toShort)
    @inline def bit15Is(b: Boolean) = new ShortAsBox((if (b) (S | 0x8000) else (S & 0x7FFF)).toShort)
    @inline def bitIs(i: Int)(b: Boolean) = new ShortAsBox((if (b) S | (1<<i) else S & (0xFFFF - (1<<i))).toShort)
    @inline def bitsAre(i: Int, n: Int)(value: Short) = {
      val m = (0xFFFF >>> (16-n)) << i
      new ShortAsBox( ((S & (0xFFFF - m)) | ((value << i) & m)).toShort )
    }    
    
    @inline def B = b0
    @inline def b0 = (S & 0xFF).toByte
    @inline def b1 = ((S&0xFFFF)>>8).toByte
    @inline def b0(b: Byte) = new ShortAsBox(((S&0xFF00)|(b&0xFF)).toShort)
    @inline def b1(b: Byte) = new ShortAsBox(((S&0xFF)|((b&0xFF)<<8)).toShort)
    @inline def swapB = new ShortAsBox((((S&0xFF00)>>8) | ((S&0xFF)<<8)).toShort)
    
    @inline def C = c0
    @inline def c0 = S.toChar
    @inline def c0(c: Char) = new ShortAsBox(c.toShort)
  }
  
  final class IntAsBox(val I: Int) extends AnyVal {
    @inline def Z = bit0
    @inline def bit0  = (I & 0x00000001) != 0
    @inline def bit1  = (I & 0x00000002) != 0
    @inline def bit2  = (I & 0x00000004) != 0
    @inline def bit3  = (I & 0x00000008) != 0
    @inline def bit4  = (I & 0x00000010) != 0
    @inline def bit5  = (I & 0x00000020) != 0
    @inline def bit6  = (I & 0x00000040) != 0
    @inline def bit7  = (I & 0x00000080) != 0
    @inline def bit8  = (I & 0x00000100) != 0
    @inline def bit9  = (I & 0x00000200) != 0
    @inline def bit10 = (I & 0x00000400) != 0
    @inline def bit11 = (I & 0x00000800) != 0
    @inline def bit12 = (I & 0x00001000) != 0
    @inline def bit13 = (I & 0x00002000) != 0
    @inline def bit14 = (I & 0x00004000) != 0
    @inline def bit15 = (I & 0x00008000) != 0
    @inline def bit16 = (I & 0x00010000) != 0
    @inline def bit17 = (I & 0x00020000) != 0
    @inline def bit18 = (I & 0x00040000) != 0
    @inline def bit19 = (I & 0x00080000) != 0
    @inline def bit20 = (I & 0x00100000) != 0
    @inline def bit21 = (I & 0x00200000) != 0
    @inline def bit22 = (I & 0x00400000) != 0
    @inline def bit23 = (I & 0x00800000) != 0
    @inline def bit24 = (I & 0x01000000) != 0
    @inline def bit25 = (I & 0x02000000) != 0
    @inline def bit26 = (I & 0x04000000) != 0
    @inline def bit27 = (I & 0x08000000) != 0
    @inline def bit28 = (I & 0x10000000) != 0
    @inline def bit29 = (I & 0x20000000) != 0
    @inline def bit30 = (I & 0x40000000) != 0
    @inline def bit31 = (I & 0x80000000) != 0    
    @inline def bit(i: Int) = (I & (1<<i)) != 0
    @inline def bits(i: Int, n: Int) = (I >>> i) & (-1 >>> (32-n))
    @inline def bit0Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000001) else (I & 0xFFFFFFFE))
    @inline def bit1Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000002) else (I & 0xFFFFFFFD))
    @inline def bit2Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000004) else (I & 0xFFFFFFFB))
    @inline def bit3Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000008) else (I & 0xFFFFFFF7))
    @inline def bit4Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000010) else (I & 0xFFFFFFEF))
    @inline def bit5Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000020) else (I & 0xFFFFFFDF))
    @inline def bit6Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000040) else (I & 0xFFFFFFBF))
    @inline def bit7Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000080) else (I & 0xFFFFFF7F))
    @inline def bit8Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000100) else (I & 0xFFFFFEFF))
    @inline def bit9Is(b: Boolean)  = new IntAsBox(if (b) (I | 0x00000200) else (I & 0xFFFFFDFF))
    @inline def bit10Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00000400) else (I & 0xFFFFFBFF))
    @inline def bit11Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00000800) else (I & 0xFFFFF7FF))
    @inline def bit12Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00001000) else (I & 0xFFFFEFFF))
    @inline def bit13Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00002000) else (I & 0xFFFFDFFF))
    @inline def bit14Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00004000) else (I & 0xFFFFBFFF))
    @inline def bit15Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00008000) else (I & 0xFFFF7FFF))
    @inline def bit16Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00010000) else (I & 0xFFFEFFFF))
    @inline def bit17Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00020000) else (I & 0xFFFDFFFF))
    @inline def bit18Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00040000) else (I & 0xFFFBFFFF))
    @inline def bit19Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00080000) else (I & 0xFFF7FFFF))
    @inline def bit20Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00100000) else (I & 0xFFEFFFFF))
    @inline def bit21Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00200000) else (I & 0xFFDFFFFF))
    @inline def bit22Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00400000) else (I & 0xFFBFFFFF))
    @inline def bit23Is(b: Boolean) = new IntAsBox(if (b) (I | 0x00800000) else (I & 0xFF7FFFFF))
    @inline def bit24Is(b: Boolean) = new IntAsBox(if (b) (I | 0x01000000) else (I & 0xFEFFFFFF))
    @inline def bit25Is(b: Boolean) = new IntAsBox(if (b) (I | 0x02000000) else (I & 0xFDFFFFFF))
    @inline def bit26Is(b: Boolean) = new IntAsBox(if (b) (I | 0x04000000) else (I & 0xFBFFFFFF))
    @inline def bit27Is(b: Boolean) = new IntAsBox(if (b) (I | 0x08000000) else (I & 0xF7FFFFFF))
    @inline def bit28Is(b: Boolean) = new IntAsBox(if (b) (I | 0x10000000) else (I & 0xEFFFFFFF))
    @inline def bit29Is(b: Boolean) = new IntAsBox(if (b) (I | 0x20000000) else (I & 0xDFFFFFFF))
    @inline def bit30Is(b: Boolean) = new IntAsBox(if (b) (I | 0x40000000) else (I & 0xBFFFFFFF))
    @inline def bit31Is(b: Boolean) = new IntAsBox(if (b) (I | 0x80000000) else (I & 0x7FFFFFFF))
    @inline def bitIs(i: Int)(b: Boolean) = new IntAsBox(if (b) I | (1<<i) else I & (0xFFFFFFFF - (1<<i)))
    @inline def bitsAre(i: Int, n: Int)(value: Int) = {
      val m = (-1 >>> (32-n)) << i
      new IntAsBox( (I & (-1 - m)) | ((value << i) & m) )
    }

    @inline def B = b0
    @inline def b0 =  (I & 0xFF)            .toByte
    @inline def b1 = ((I & 0xFF00)   >>   8).toByte
    @inline def b2 = ((I & 0xFF0000) >>  16).toByte
    @inline def b3 =  (I             >>> 24).toByte
    @inline def b0(b: Byte) = new IntAsBox((I & 0xFFFFFF00) | (b & 0xFF))
    @inline def b1(b: Byte) = new IntAsBox((I & 0xFFFF00FF) | ((b & 0xFF) << 8))
    @inline def b2(b: Byte) = new IntAsBox((I & 0xFF00FFFF) | ((b & 0xFF) << 16))
    @inline def b3(b: Byte) = new IntAsBox((I & 0x00FFFFFF) | ((b & 0xFF) << 24))
    @inline def rotrB = new IntAsBox((I >>> 8) | (I << 24))
    @inline def rotlB = new IntAsBox((I >>> 24) | (I << 8))
    @inline def swapBB = new IntAsBox(((I & 0xFF00FF00) >>> 8) | ((I & 0x00FF00FF) << 8))
    @inline def reverseB = new IntAsBox(((I&0xFF000000)>>>24) | ((I&0xFF0000)>>8) | ((I&0xFF00)<<8) | ((I&0xFF)<<24))

    @inline def S = s0
    @inline def s0 = (I & 0xFFFF).toShort
    @inline def s1 = (I >>> 16).toShort
    @inline def s0(s: Short) = new IntAsBox((I&0xFFFF0000) | (s&0xFFFF))
    @inline def s1(s: Short) = new IntAsBox((I&0xFFFF) | ((s&0xFFFF)<<16))
    @inline def swapS = new IntAsBox((I>>>16) | (I<<16))

    @inline def C = c0
    @inline def c0 = I.toChar
    @inline def c1 = (I >>> 16).toChar
    @inline def c0(c: Char) = new IntAsBox((I&0xFFFF0000) | c)
    @inline def c1(c: Char) = new IntAsBox((I&0xFFFF) | (c<<16))
    @inline def swapC = new IntAsBox((I>>>16) | (I<<16))

    @inline def F = f0
    @inline def f0 = java.lang.Float.intBitsToFloat(I)
    @inline def f0(f: Float) = new IntAsBox(java.lang.Float.floatToRawIntBits(f))
  }
  
  final class LongAsBox(val L: Long) extends AnyVal {
    @inline def Z = bit0
    @inline def bit0  = (L & 0x0000000000000001L) != 0
    @inline def bit1  = (L & 0x0000000000000002L) != 0
    @inline def bit2  = (L & 0x0000000000000004L) != 0
    @inline def bit3  = (L & 0x0000000000000008L) != 0
    @inline def bit4  = (L & 0x0000000000000010L) != 0
    @inline def bit5  = (L & 0x0000000000000020L) != 0
    @inline def bit6  = (L & 0x0000000000000040L) != 0
    @inline def bit7  = (L & 0x0000000000000080L) != 0
    @inline def bit8  = (L & 0x0000000000000100L) != 0
    @inline def bit9  = (L & 0x0000000000000200L) != 0
    @inline def bit10 = (L & 0x0000000000000400L) != 0
    @inline def bit11 = (L & 0x0000000000000800L) != 0
    @inline def bit12 = (L & 0x0000000000001000L) != 0
    @inline def bit13 = (L & 0x0000000000002000L) != 0
    @inline def bit14 = (L & 0x0000000000004000L) != 0
    @inline def bit15 = (L & 0x0000000000008000L) != 0
    @inline def bit16 = (L & 0x0000000000010000L) != 0
    @inline def bit17 = (L & 0x0000000000020000L) != 0
    @inline def bit18 = (L & 0x0000000000040000L) != 0
    @inline def bit19 = (L & 0x0000000000080000L) != 0
    @inline def bit20 = (L & 0x0000000000100000L) != 0
    @inline def bit21 = (L & 0x0000000000200000L) != 0
    @inline def bit22 = (L & 0x0000000000400000L) != 0
    @inline def bit23 = (L & 0x0000000000800000L) != 0
    @inline def bit24 = (L & 0x0000000001000000L) != 0
    @inline def bit25 = (L & 0x0000000002000000L) != 0
    @inline def bit26 = (L & 0x0000000004000000L) != 0
    @inline def bit27 = (L & 0x0000000008000000L) != 0
    @inline def bit28 = (L & 0x0000000010000000L) != 0
    @inline def bit29 = (L & 0x0000000020000000L) != 0
    @inline def bit30 = (L & 0x0000000040000000L) != 0
    @inline def bit31 = (L & 0x0000000080000000L) != 0
    @inline def bit32 = (L & 0x0000000100000000L) != 0
    @inline def bit33 = (L & 0x0000000200000000L) != 0
    @inline def bit34 = (L & 0x0000000400000000L) != 0
    @inline def bit35 = (L & 0x0000000800000000L) != 0
    @inline def bit36 = (L & 0x0000001000000000L) != 0
    @inline def bit37 = (L & 0x0000002000000000L) != 0
    @inline def bit38 = (L & 0x0000004000000000L) != 0
    @inline def bit39 = (L & 0x0000008000000000L) != 0
    @inline def bit40 = (L & 0x0000010000000000L) != 0
    @inline def bit41 = (L & 0x0000020000000000L) != 0
    @inline def bit42 = (L & 0x0000040000000000L) != 0
    @inline def bit43 = (L & 0x0000080000000000L) != 0
    @inline def bit44 = (L & 0x0000100000000000L) != 0
    @inline def bit45 = (L & 0x0000200000000000L) != 0
    @inline def bit46 = (L & 0x0000400000000000L) != 0
    @inline def bit47 = (L & 0x0000800000000000L) != 0
    @inline def bit48 = (L & 0x0001000000000000L) != 0
    @inline def bit49 = (L & 0x0002000000000000L) != 0
    @inline def bit50 = (L & 0x0004000000000000L) != 0
    @inline def bit51 = (L & 0x0008000000000000L) != 0
    @inline def bit52 = (L & 0x0010000000000000L) != 0
    @inline def bit53 = (L & 0x0020000000000000L) != 0
    @inline def bit54 = (L & 0x0040000000000000L) != 0
    @inline def bit55 = (L & 0x0080000000000000L) != 0
    @inline def bit56 = (L & 0x0100000000000000L) != 0
    @inline def bit57 = (L & 0x0200000000000000L) != 0
    @inline def bit58 = (L & 0x0400000000000000L) != 0
    @inline def bit59 = (L & 0x0800000000000000L) != 0
    @inline def bit60 = (L & 0x1000000000000000L) != 0
    @inline def bit61 = (L & 0x2000000000000000L) != 0
    @inline def bit62 = (L & 0x4000000000000000L) != 0
    @inline def bit63 = (L & 0x8000000000000000L) != 0
    @inline def bit(i: Int) = (L & (1L<<i)) != 0
    @inline def bits(i: Int, n: Int) = (L >>> i) & (-1L >>> (64-n))
    @inline def bit0Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000001L) else (L & 0xFFFFFFFFFFFFFFFEL))
    @inline def bit1Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000002L) else (L & 0xFFFFFFFFFFFFFFFDL))
    @inline def bit2Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000004L) else (L & 0xFFFFFFFFFFFFFFFBL))
    @inline def bit3Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000008L) else (L & 0xFFFFFFFFFFFFFFF7L))
    @inline def bit4Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000010L) else (L & 0xFFFFFFFFFFFFFFEFL))
    @inline def bit5Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000020L) else (L & 0xFFFFFFFFFFFFFFDFL))
    @inline def bit6Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000040L) else (L & 0xFFFFFFFFFFFFFFBFL))
    @inline def bit7Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000080L) else (L & 0xFFFFFFFFFFFFFF7FL))
    @inline def bit8Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000100L) else (L & 0xFFFFFFFFFFFFFEFFL))
    @inline def bit9Is(b: Boolean)  = new LongAsBox(if (b) (L | 0x0000000000000200L) else (L & 0xFFFFFFFFFFFFFDFFL))
    @inline def bit10Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000000400L) else (L & 0xFFFFFFFFFFFFFBFFL))
    @inline def bit11Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000000800L) else (L & 0xFFFFFFFFFFFFF7FFL))
    @inline def bit12Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000001000L) else (L & 0xFFFFFFFFFFFFEFFFL))
    @inline def bit13Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000002000L) else (L & 0xFFFFFFFFFFFFDFFFL))
    @inline def bit14Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000004000L) else (L & 0xFFFFFFFFFFFFBFFFL))
    @inline def bit15Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000008000L) else (L & 0xFFFFFFFFFFFF7FFFL))
    @inline def bit16Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000010000L) else (L & 0xFFFFFFFFFFFEFFFFL))
    @inline def bit17Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000020000L) else (L & 0xFFFFFFFFFFFDFFFFL))
    @inline def bit18Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000040000L) else (L & 0xFFFFFFFFFFFBFFFFL))
    @inline def bit19Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000080000L) else (L & 0xFFFFFFFFFFF7FFFFL))
    @inline def bit20Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000100000L) else (L & 0xFFFFFFFFFFEFFFFFL))
    @inline def bit21Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000200000L) else (L & 0xFFFFFFFFFFDFFFFFL))
    @inline def bit22Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000400000L) else (L & 0xFFFFFFFFFFBFFFFFL))
    @inline def bit23Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000000800000L) else (L & 0xFFFFFFFFFF7FFFFFL))
    @inline def bit24Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000001000000L) else (L & 0xFFFFFFFFFEFFFFFFL))
    @inline def bit25Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000002000000L) else (L & 0xFFFFFFFFFDFFFFFFL))
    @inline def bit26Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000004000000L) else (L & 0xFFFFFFFFFBFFFFFFL))
    @inline def bit27Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000008000000L) else (L & 0xFFFFFFFFF7FFFFFFL))
    @inline def bit28Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000010000000L) else (L & 0xFFFFFFFFEFFFFFFFL))
    @inline def bit29Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000020000000L) else (L & 0xFFFFFFFFDFFFFFFFL))
    @inline def bit30Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000040000000L) else (L & 0xFFFFFFFFBFFFFFFFL))
    @inline def bit31Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000080000000L) else (L & 0xFFFFFFFF7FFFFFFFL))
    @inline def bit32Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000100000000L) else (L & 0xFFFFFFFEFFFFFFFFL))
    @inline def bit33Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000200000000L) else (L & 0xFFFFFFFDFFFFFFFFL))
    @inline def bit34Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000400000000L) else (L & 0xFFFFFFFBFFFFFFFFL))
    @inline def bit35Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000000800000000L) else (L & 0xFFFFFFF7FFFFFFFFL))
    @inline def bit36Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000001000000000L) else (L & 0xFFFFFFEFFFFFFFFFL))
    @inline def bit37Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000002000000000L) else (L & 0xFFFFFFDFFFFFFFFFL))
    @inline def bit38Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000004000000000L) else (L & 0xFFFFFFBFFFFFFFFFL))
    @inline def bit39Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000008000000000L) else (L & 0xFFFFFF7FFFFFFFFFL))
    @inline def bit40Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000010000000000L) else (L & 0xFFFFFEFFFFFFFFFFL))
    @inline def bit41Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000020000000000L) else (L & 0xFFFFFDFFFFFFFFFFL))
    @inline def bit42Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000040000000000L) else (L & 0xFFFFFBFFFFFFFFFFL))
    @inline def bit43Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000080000000000L) else (L & 0xFFFFF7FFFFFFFFFFL))
    @inline def bit44Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000100000000000L) else (L & 0xFFFFEFFFFFFFFFFFL))
    @inline def bit45Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000200000000000L) else (L & 0xFFFFDFFFFFFFFFFFL))
    @inline def bit46Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000400000000000L) else (L & 0xFFFFBFFFFFFFFFFFL))
    @inline def bit47Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0000800000000000L) else (L & 0xFFFF7FFFFFFFFFFFL))
    @inline def bit48Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0001000000000000L) else (L & 0xFFFEFFFFFFFFFFFFL))
    @inline def bit49Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0002000000000000L) else (L & 0xFFFDFFFFFFFFFFFFL))
    @inline def bit50Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0004000000000000L) else (L & 0xFFFBFFFFFFFFFFFFL))
    @inline def bit51Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0008000000000000L) else (L & 0xFFF7FFFFFFFFFFFFL))
    @inline def bit52Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0010000000000000L) else (L & 0xFFEFFFFFFFFFFFFFL))
    @inline def bit53Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0020000000000000L) else (L & 0xFFDFFFFFFFFFFFFFL))
    @inline def bit54Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0040000000000000L) else (L & 0xFFBFFFFFFFFFFFFFL))
    @inline def bit55Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0080000000000000L) else (L & 0xFF7FFFFFFFFFFFFFL))
    @inline def bit56Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0100000000000000L) else (L & 0xFEFFFFFFFFFFFFFFL))
    @inline def bit57Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0200000000000000L) else (L & 0xFDFFFFFFFFFFFFFFL))
    @inline def bit58Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0400000000000000L) else (L & 0xFBFFFFFFFFFFFFFFL))
    @inline def bit59Is(b: Boolean) = new LongAsBox(if (b) (L | 0x0800000000000000L) else (L & 0xF7FFFFFFFFFFFFFFL))
    @inline def bit60Is(b: Boolean) = new LongAsBox(if (b) (L | 0x1000000000000000L) else (L & 0xEFFFFFFFFFFFFFFFL))
    @inline def bit61Is(b: Boolean) = new LongAsBox(if (b) (L | 0x2000000000000000L) else (L & 0xDFFFFFFFFFFFFFFFL))
    @inline def bit62Is(b: Boolean) = new LongAsBox(if (b) (L | 0x4000000000000000L) else (L & 0xBFFFFFFFFFFFFFFFL))
    @inline def bit63Is(b: Boolean) = new LongAsBox(if (b) (L | 0x8000000000000000L) else (L & 0x7FFFFFFFFFFFFFFFL))
    @inline def bitIs(i: Int)(b: Boolean) = new LongAsBox(if (b) L | (1L<<i) else L & (-1L - (1L<<i)))
    @inline def bitsAre(i: Int, n: Int)(value: Long) = {
      val m = (-1L >>> (64-n)) << i
      new LongAsBox( (L & (-1L - m)) | ((value << i) & m) )
    }

    @inline def B = b0
    @inline def b0 =  (L & 0xFF)                     .toByte
    @inline def b1 = ((L & 0xFF00)            >>   8).toByte
    @inline def b2 = ((L & 0xFF0000)          >>  16).toByte
    @inline def b3 = ((L & 0xFF000000L)       >>  24).toByte
    @inline def b4 = ((L & 0xFF00000000L)     >>  32).toByte
    @inline def b5 = ((L & 0xFF0000000000L)   >>  40).toByte
    @inline def b6 = ((L & 0xFF000000000000L) >>  48).toByte
    @inline def b7 = ( L                      >>> 56).toByte
    @inline def b0(b: Byte) = new LongAsBox((L&0xFFFFFFFFFFFFFF00L) |  (b&0xFF))
    @inline def b1(b: Byte) = new LongAsBox((L&0xFFFFFFFFFFFF00FFL) | ((b&0xFF)<<8))
    @inline def b2(b: Byte) = new LongAsBox((L&0xFFFFFFFFFF00FFFFL) | ((b&0xFF)<<16))
    @inline def b3(b: Byte) = new LongAsBox((L&0xFFFFFFFF00FFFFFFL) | ((b&0xFF).toLong<<24))
    @inline def b4(b: Byte) = new LongAsBox((L&0xFFFFFF00FFFFFFFFL) | ((b&0xFF).toLong<<32))
    @inline def b5(b: Byte) = new LongAsBox((L&0xFFFF00FFFFFFFFFFL) | ((b&0xFF).toLong<<40))
    @inline def b6(b: Byte) = new LongAsBox((L&0xFF00FFFFFFFFFFFFL) | ((b&0xFF).toLong<<48))
    @inline def b7(b: Byte) = new LongAsBox((L&0x00FFFFFFFFFFFFFFL) | ((b&0xFF).toLong<<56))
    @inline def rotrB = new LongAsBox((L >>> 8) | (L << 56))
    @inline def rotlB = new LongAsBox((L >>> 56) | (L << 8))
    @inline def swapBBBB = new LongAsBox(((L&0xFF00FF00FF00FF00L)>>>8) | ((L&0x00FF00FF00FF00FFL)<<8))
    @inline def reverseB = {
      val m = swapBBBB.L
      new LongAsBox((m>>>48) | ((m&0xFFFF00000000L)>>16) | ((m&0xFFFF0000L)<<16) | (m<<48))
    }

    @inline def S = s0
    @inline def s0 =  (L & 0xFFFF)                 .toShort
    @inline def s1 = ((L & 0xFFFF0000L)     >>  16).toShort
    @inline def s2 = ((L & 0xFFFF00000000L) >>  32).toShort
    @inline def s3 = ( L                    >>> 48).toShort
    @inline def s0(s: Short) = new LongAsBox((L & 0xFFFFFFFFFFFF0000L) | (s&0xFFFF))
    @inline def s1(s: Short) = new LongAsBox((L & 0xFFFFFFFF0000FFFFL) | ((s&0xFFFF).toLong << 16))
    @inline def s2(s: Short) = new LongAsBox((L & 0xFFFF0000FFFFFFFFL) | ((s&0xFFFF).toLong << 32))
    @inline def s3(s: Short) = new LongAsBox((L & 0x0000FFFFFFFFFFFFL) | (s.toLong << 48))
    @inline def rotrS = new LongAsBox((L >>> 16) | (L << 48))
    @inline def rotlS = new LongAsBox((L >>> 48) | (L << 16))
    @inline def swapSS = new LongAsBox(((L&0xFFFF0000FFFF0000L)>>>16) | ((L&0x0000FFFF0000FFFFL)<<16))
    @inline def reverseS = new LongAsBox((L>>>48) | ((L&0xFFFF00000000L)>>16) | ((L&0xFFFF0000L)<<16) | (L<<48))
    
    @inline def C = c0
    @inline def c0 =  (L & 0xFFFF)                 .toChar
    @inline def c1 = ((L & 0xFFFF0000L)     >>  16).toChar
    @inline def c2 = ((L & 0xFFFF00000000L) >>  32).toChar
    @inline def c3 = ( L                    >>> 48).toChar
    @inline def c0(c: Char) = new LongAsBox((L & 0xFFFFFFFFFFFF0000L) | c)
    @inline def c1(c: Char) = new LongAsBox((L & 0xFFFFFFFF0000FFFFL) | (c.toLong << 16))
    @inline def c2(c: Char) = new LongAsBox((L & 0xFFFF0000FFFFFFFFL) | (c.toLong << 32))
    @inline def c3(c: Char) = new LongAsBox((L & 0x0000FFFFFFFFFFFFL) | (c.toLong << 48))
    @inline def rotrC = new LongAsBox((L >>> 16) | (L << 48))
    @inline def rotlC = new LongAsBox((L >>> 48) | (L << 16))
    @inline def swapCC = new LongAsBox(((L&0xFFFF0000FFFF0000L)>>>16) | ((L&0x0000FFFF0000FFFFL)<<16))
    @inline def reverseC = new LongAsBox((L>>>48) | ((L&0xFFFF00000000L)>>16) | ((L&0xFFFF0000L)<<16) | (L<<48))
    
    @inline def I = i0
    @inline def i0 = (L & 0xFFFFFFFFL).toInt
    @inline def i1 = (L >>> 32).toInt
    @inline def i0(i: Int) = new LongAsBox((L&0xFFFFFFFF00000000L) | (i&0xFFFFFFFFL))
    @inline def i1(i: Int) = new LongAsBox((L&0xFFFFFFFFL) | (i.toLong<<32))
    @inline def swapI = new LongAsBox((L >>> 32) | (L << 32))
    
    @inline def F = f0
    @inline def f0 = java.lang.Float.intBitsToFloat((L & 0xFFFFFFFFL).toInt)
    @inline def f1 = java.lang.Float.intBitsToFloat((L >>> 32).toInt)
    @inline def f0(f: Float) = i0(java.lang.Float.floatToRawIntBits(f))
    @inline def f1(f: Float) = i1(java.lang.Float.floatToRawIntBits(f))
    @inline def swapF = swapI
    
    @inline def D = d0
    @inline def d0 = java.lang.Double.longBitsToDouble(L)
    @inline def d0(d: Double) = new LongAsBox(java.lang.Double.doubleToRawLongBits(d))
  }
}

package object packed {
  implicit final class PackBooleanInPrimitives(private val z: Boolean) extends AnyVal {
    @inline def inByte = new ByteAsBox(if (z) 1 else 0)
    @inline def inShort = new ShortAsBox(if (z) 1 else 0)
    @inline def inInt = new IntAsBox(if (z) 1 else 0)
    @inline def inLong = new LongAsBox(if (z) 1 else 0)
  }
  
  implicit final class PackByteInPrimitives(private val b: Byte) extends AnyVal {
    @inline def inByte = new ByteAsBox(b)
    @inline def inShort = new ShortAsBox((b & 0xFF).toShort)
    @inline def inInt = new IntAsBox(b & 0xFF)
    @inline def inLong = new LongAsBox(b & 0xFF)
    @inline def packBB(b2: Byte) = new ShortAsBox(((b&0xFF) | ((b2&0xFF)<<8)).toShort)
    @inline def packBBBB(b2: Byte, b3: Byte, b4: Byte) = new IntAsBox((b&0xFF) | ((b2&0xFF)<<8) | ((b3&0xFF)<<16) | (b4.toInt<<24))
    @inline def packB8(b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte, b8: Byte) = {
      new LongAsBox((b&0xFFL) | ((b2&0xFFL)<<8) | ((b3&0xFFL)<<16) | ((b4&0xFFL)<<24) | ((b5&0xFFL)<<32) | ((b6&0xFFL)<<40) | ((b7&0xFFL)<<48) | (b8.toLong<<56))
    }
  }
  
  implicit final class PackCharInPrimitives(private val c: Char) extends AnyVal {
    @inline def inShort = new ShortAsBox(c.toShort)
    @inline def inInt = new IntAsBox(c)
    @inline def inLong = new LongAsBox(c)
    @inline def packCC(c2: Char) = new IntAsBox(c | (c2<<16))
    @inline def packCCCC(c2: Char, c3: Char, c4: Char) = new LongAsBox(c | (c2.toLong<<16) | (c3.toLong<<32) | (c4.toLong<<48))
  }
  
  implicit final class PackShortInPrimitives(private val s: Short) extends AnyVal {
    @inline def inShort = new ShortAsBox(s)
    @inline def inInt = new IntAsBox(s & 0xFFFF)
    @inline def inLong = new LongAsBox(s & 0xFFFF)
    @inline def packSS(s2: Short) = new IntAsBox((s&0xFFFF) | (s2.toInt << 16))
    @inline def packSSSS(s2: Short, s3: Short, s4: Short) = new LongAsBox((s&0xFFFFL) | ((s2&0xFFFFL)<<16) | ((s3&0xFFFFL)<<32) | (s4.toLong<<48))
  }
  
  implicit final class PackIntInPrimitives(private val i: Int) extends AnyVal {
    @inline def inInt = new IntAsBox(i)
    @inline def inLong = new LongAsBox(i & 0xFFFFFFFFL)
    @inline def packII(i2: Int) = new LongAsBox((i&0xFFFFFFFFL) | (i2.toLong << 32))
  }
  
  implicit final class PackFloatInPrimitives(private val f: Float) extends AnyVal {
    @inline def inInt = new IntAsBox(java.lang.Float.floatToRawIntBits(f))
    @inline def inLong = new LongAsBox(java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL)
    @inline def packFF(f2: Float) = new LongAsBox((java.lang.Float.floatToRawIntBits(f).toLong & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f2).toLong << 32))
  }
  
  implicit final class PackLongInPrimitives(private val l: Long) extends AnyVal {
    @inline def inLong = new LongAsBox(l)
  }

  implicit final class PackDoubleInPrimitives(private val d: Double) extends AnyVal {
    @inline def inLong = new LongAsBox(java.lang.Double.doubleToRawLongBits(d))
  }
}
