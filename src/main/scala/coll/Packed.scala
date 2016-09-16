// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2016 Rex Kerr, UCSF, and Calico Labs

package kse.coll

import language.implicitConversions

package packed {
  final class Bitx8(val B: Byte) extends AnyVal {
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
    
    @inline def bit0To(b: Boolean) = new Bitx8((if (b) (B | 0x01) else (B & 0xFE)).toByte)
    @inline def bit1To(b: Boolean) = new Bitx8((if (b) (B | 0x02) else (B & 0xFD)).toByte)
    @inline def bit2To(b: Boolean) = new Bitx8((if (b) (B | 0x04) else (B & 0xFB)).toByte)
    @inline def bit3To(b: Boolean) = new Bitx8((if (b) (B | 0x08) else (B & 0xF7)).toByte)
    @inline def bit4To(b: Boolean) = new Bitx8((if (b) (B | 0x10) else (B & 0xEF)).toByte)
    @inline def bit5To(b: Boolean) = new Bitx8((if (b) (B | 0x20) else (B & 0xDF)).toByte)
    @inline def bit6To(b: Boolean) = new Bitx8((if (b) (B | 0x40) else (B & 0xBF)).toByte)
    @inline def bit7To(b: Boolean) = new Bitx8((if (b) (B | 0x80) else (B & 0x7F)).toByte)
    @inline def bitTo(i: Int)(b: Boolean) = new Bitx8((if (b) B | (1<<i) else B & (0xFF - (1<<i))).toByte)
    @inline def bitsTo(i: Int, n: Int)(value: Byte) = {
      val m = (0xFF >>> (8-n)) << i
      new Bitx8( ((B & (0xFF - m)) | ((value << i) & m)).toByte )
    }
  }

  final class Bitx16(val S: Short) extends AnyVal {
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

    @inline def bit0To(b: Boolean)  = new Bitx16((if (b) (S | 0x0001) else (S & 0xFFFE)).toShort)
    @inline def bit1To(b: Boolean)  = new Bitx16((if (b) (S | 0x0002) else (S & 0xFFFD)).toShort)
    @inline def bit2To(b: Boolean)  = new Bitx16((if (b) (S | 0x0004) else (S & 0xFFFB)).toShort)
    @inline def bit3To(b: Boolean)  = new Bitx16((if (b) (S | 0x0008) else (S & 0xFFF7)).toShort)
    @inline def bit4To(b: Boolean)  = new Bitx16((if (b) (S | 0x0010) else (S & 0xFFEF)).toShort)
    @inline def bit5To(b: Boolean)  = new Bitx16((if (b) (S | 0x0020) else (S & 0xFFDF)).toShort)
    @inline def bit6To(b: Boolean)  = new Bitx16((if (b) (S | 0x0040) else (S & 0xFFBF)).toShort)
    @inline def bit7To(b: Boolean)  = new Bitx16((if (b) (S | 0x0080) else (S & 0xFF7F)).toShort)
    @inline def bit8To(b: Boolean)  = new Bitx16((if (b) (S | 0x0100) else (S & 0xFEFF)).toShort)
    @inline def bit9To(b: Boolean)  = new Bitx16((if (b) (S | 0x0200) else (S & 0xFDFF)).toShort)
    @inline def bit10To(b: Boolean) = new Bitx16((if (b) (S | 0x0400) else (S & 0xFBFF)).toShort)
    @inline def bit11To(b: Boolean) = new Bitx16((if (b) (S | 0x0800) else (S & 0xF7FF)).toShort)
    @inline def bit12To(b: Boolean) = new Bitx16((if (b) (S | 0x1000) else (S & 0xEFFF)).toShort)
    @inline def bit13To(b: Boolean) = new Bitx16((if (b) (S | 0x2000) else (S & 0xDFFF)).toShort)
    @inline def bit14To(b: Boolean) = new Bitx16((if (b) (S | 0x4000) else (S & 0xBFFF)).toShort)
    @inline def bit15To(b: Boolean) = new Bitx16((if (b) (S | 0x8000) else (S & 0x7FFF)).toShort)
    @inline def bitTo(i: Int)(b: Boolean) = new Bitx16((if (b) S | (1<<i) else S & (0xFFFF - (1<<i))).toShort)
    @inline def bitsTo(i: Int, n: Int)(value: Short) = {
      val m = (0xFFFF >>> (16-n)) << i
      new Bitx16( ((S & (0xFFFF - m)) | ((value << i) & m)).toShort )
    }

    @inline def asBytes = new Bytex2(S)
  }

  final class Bitx32(val I: Int) extends AnyVal {
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
    @inline def bit0To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000001) else (I & 0xFFFFFFFE))
    @inline def bit1To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000002) else (I & 0xFFFFFFFD))
    @inline def bit2To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000004) else (I & 0xFFFFFFFB))
    @inline def bit3To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000008) else (I & 0xFFFFFFF7))
    @inline def bit4To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000010) else (I & 0xFFFFFFEF))
    @inline def bit5To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000020) else (I & 0xFFFFFFDF))
    @inline def bit6To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000040) else (I & 0xFFFFFFBF))
    @inline def bit7To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000080) else (I & 0xFFFFFF7F))
    @inline def bit8To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000100) else (I & 0xFFFFFEFF))
    @inline def bit9To(b: Boolean)  = new Bitx32(if (b) (I | 0x00000200) else (I & 0xFFFFFDFF))
    @inline def bit10To(b: Boolean) = new Bitx32(if (b) (I | 0x00000400) else (I & 0xFFFFFBFF))
    @inline def bit11To(b: Boolean) = new Bitx32(if (b) (I | 0x00000800) else (I & 0xFFFFF7FF))
    @inline def bit12To(b: Boolean) = new Bitx32(if (b) (I | 0x00001000) else (I & 0xFFFFEFFF))
    @inline def bit13To(b: Boolean) = new Bitx32(if (b) (I | 0x00002000) else (I & 0xFFFFDFFF))
    @inline def bit14To(b: Boolean) = new Bitx32(if (b) (I | 0x00004000) else (I & 0xFFFFBFFF))
    @inline def bit15To(b: Boolean) = new Bitx32(if (b) (I | 0x00008000) else (I & 0xFFFF7FFF))
    @inline def bit16To(b: Boolean) = new Bitx32(if (b) (I | 0x00010000) else (I & 0xFFFEFFFF))
    @inline def bit17To(b: Boolean) = new Bitx32(if (b) (I | 0x00020000) else (I & 0xFFFDFFFF))
    @inline def bit18To(b: Boolean) = new Bitx32(if (b) (I | 0x00040000) else (I & 0xFFFBFFFF))
    @inline def bit19To(b: Boolean) = new Bitx32(if (b) (I | 0x00080000) else (I & 0xFFF7FFFF))
    @inline def bit20To(b: Boolean) = new Bitx32(if (b) (I | 0x00100000) else (I & 0xFFEFFFFF))
    @inline def bit21To(b: Boolean) = new Bitx32(if (b) (I | 0x00200000) else (I & 0xFFDFFFFF))
    @inline def bit22To(b: Boolean) = new Bitx32(if (b) (I | 0x00400000) else (I & 0xFFBFFFFF))
    @inline def bit23To(b: Boolean) = new Bitx32(if (b) (I | 0x00800000) else (I & 0xFF7FFFFF))
    @inline def bit24To(b: Boolean) = new Bitx32(if (b) (I | 0x01000000) else (I & 0xFEFFFFFF))
    @inline def bit25To(b: Boolean) = new Bitx32(if (b) (I | 0x02000000) else (I & 0xFDFFFFFF))
    @inline def bit26To(b: Boolean) = new Bitx32(if (b) (I | 0x04000000) else (I & 0xFBFFFFFF))
    @inline def bit27To(b: Boolean) = new Bitx32(if (b) (I | 0x08000000) else (I & 0xF7FFFFFF))
    @inline def bit28To(b: Boolean) = new Bitx32(if (b) (I | 0x10000000) else (I & 0xEFFFFFFF))
    @inline def bit29To(b: Boolean) = new Bitx32(if (b) (I | 0x20000000) else (I & 0xDFFFFFFF))
    @inline def bit30To(b: Boolean) = new Bitx32(if (b) (I | 0x40000000) else (I & 0xBFFFFFFF))
    @inline def bit31To(b: Boolean) = new Bitx32(if (b) (I | 0x80000000) else (I & 0x7FFFFFFF))
    @inline def bitTo(i: Int)(b: Boolean) = new Bitx32(if (b) I | (1<<i) else I & (0xFFFFFFFF - (1<<i)))
    @inline def bitsTo(i: Int, n: Int)(value: Int) = {
      val m = (-1 >>> (32-n)) << i
      new Bitx32( (I & (-1 - m)) | ((value << i) & m) )
    }

    @inline def asBytes = new Bytex4(I)
    @inline def asShorts = new Shortx2(I)
    @inline def asChars = new Charx2(I)
    @inline def F = java.lang.Float.intBitsToFloat(I) 
  }

  final class Bitx64(val L: Long) extends AnyVal {    
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

    @inline def bit0To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000001L) else (L & 0xFFFFFFFFFFFFFFFEL))
    @inline def bit1To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000002L) else (L & 0xFFFFFFFFFFFFFFFDL))
    @inline def bit2To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000004L) else (L & 0xFFFFFFFFFFFFFFFBL))
    @inline def bit3To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000008L) else (L & 0xFFFFFFFFFFFFFFF7L))
    @inline def bit4To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000010L) else (L & 0xFFFFFFFFFFFFFFEFL))
    @inline def bit5To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000020L) else (L & 0xFFFFFFFFFFFFFFDFL))
    @inline def bit6To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000040L) else (L & 0xFFFFFFFFFFFFFFBFL))
    @inline def bit7To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000080L) else (L & 0xFFFFFFFFFFFFFF7FL))
    @inline def bit8To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000100L) else (L & 0xFFFFFFFFFFFFFEFFL))
    @inline def bit9To(b: Boolean)  = new Bitx64(if (b) (L | 0x0000000000000200L) else (L & 0xFFFFFFFFFFFFFDFFL))
    @inline def bit10To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000000400L) else (L & 0xFFFFFFFFFFFFFBFFL))
    @inline def bit11To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000000800L) else (L & 0xFFFFFFFFFFFFF7FFL))
    @inline def bit12To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000001000L) else (L & 0xFFFFFFFFFFFFEFFFL))
    @inline def bit13To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000002000L) else (L & 0xFFFFFFFFFFFFDFFFL))
    @inline def bit14To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000004000L) else (L & 0xFFFFFFFFFFFFBFFFL))
    @inline def bit15To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000008000L) else (L & 0xFFFFFFFFFFFF7FFFL))
    @inline def bit16To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000010000L) else (L & 0xFFFFFFFFFFFEFFFFL))
    @inline def bit17To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000020000L) else (L & 0xFFFFFFFFFFFDFFFFL))
    @inline def bit18To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000040000L) else (L & 0xFFFFFFFFFFFBFFFFL))
    @inline def bit19To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000080000L) else (L & 0xFFFFFFFFFFF7FFFFL))
    @inline def bit20To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000100000L) else (L & 0xFFFFFFFFFFEFFFFFL))
    @inline def bit21To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000200000L) else (L & 0xFFFFFFFFFFDFFFFFL))
    @inline def bit22To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000400000L) else (L & 0xFFFFFFFFFFBFFFFFL))
    @inline def bit23To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000000800000L) else (L & 0xFFFFFFFFFF7FFFFFL))
    @inline def bit24To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000001000000L) else (L & 0xFFFFFFFFFEFFFFFFL))
    @inline def bit25To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000002000000L) else (L & 0xFFFFFFFFFDFFFFFFL))
    @inline def bit26To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000004000000L) else (L & 0xFFFFFFFFFBFFFFFFL))
    @inline def bit27To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000008000000L) else (L & 0xFFFFFFFFF7FFFFFFL))
    @inline def bit28To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000010000000L) else (L & 0xFFFFFFFFEFFFFFFFL))
    @inline def bit29To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000020000000L) else (L & 0xFFFFFFFFDFFFFFFFL))
    @inline def bit30To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000040000000L) else (L & 0xFFFFFFFFBFFFFFFFL))
    @inline def bit31To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000080000000L) else (L & 0xFFFFFFFF7FFFFFFFL))
    @inline def bit32To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000100000000L) else (L & 0xFFFFFFFEFFFFFFFFL))
    @inline def bit33To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000200000000L) else (L & 0xFFFFFFFDFFFFFFFFL))
    @inline def bit34To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000400000000L) else (L & 0xFFFFFFFBFFFFFFFFL))
    @inline def bit35To(b: Boolean) = new Bitx64(if (b) (L | 0x0000000800000000L) else (L & 0xFFFFFFF7FFFFFFFFL))
    @inline def bit36To(b: Boolean) = new Bitx64(if (b) (L | 0x0000001000000000L) else (L & 0xFFFFFFEFFFFFFFFFL))
    @inline def bit37To(b: Boolean) = new Bitx64(if (b) (L | 0x0000002000000000L) else (L & 0xFFFFFFDFFFFFFFFFL))
    @inline def bit38To(b: Boolean) = new Bitx64(if (b) (L | 0x0000004000000000L) else (L & 0xFFFFFFBFFFFFFFFFL))
    @inline def bit39To(b: Boolean) = new Bitx64(if (b) (L | 0x0000008000000000L) else (L & 0xFFFFFF7FFFFFFFFFL))
    @inline def bit40To(b: Boolean) = new Bitx64(if (b) (L | 0x0000010000000000L) else (L & 0xFFFFFEFFFFFFFFFFL))
    @inline def bit41To(b: Boolean) = new Bitx64(if (b) (L | 0x0000020000000000L) else (L & 0xFFFFFDFFFFFFFFFFL))
    @inline def bit42To(b: Boolean) = new Bitx64(if (b) (L | 0x0000040000000000L) else (L & 0xFFFFFBFFFFFFFFFFL))
    @inline def bit43To(b: Boolean) = new Bitx64(if (b) (L | 0x0000080000000000L) else (L & 0xFFFFF7FFFFFFFFFFL))
    @inline def bit44To(b: Boolean) = new Bitx64(if (b) (L | 0x0000100000000000L) else (L & 0xFFFFEFFFFFFFFFFFL))
    @inline def bit45To(b: Boolean) = new Bitx64(if (b) (L | 0x0000200000000000L) else (L & 0xFFFFDFFFFFFFFFFFL))
    @inline def bit46To(b: Boolean) = new Bitx64(if (b) (L | 0x0000400000000000L) else (L & 0xFFFFBFFFFFFFFFFFL))
    @inline def bit47To(b: Boolean) = new Bitx64(if (b) (L | 0x0000800000000000L) else (L & 0xFFFF7FFFFFFFFFFFL))
    @inline def bit48To(b: Boolean) = new Bitx64(if (b) (L | 0x0001000000000000L) else (L & 0xFFFEFFFFFFFFFFFFL))
    @inline def bit49To(b: Boolean) = new Bitx64(if (b) (L | 0x0002000000000000L) else (L & 0xFFFDFFFFFFFFFFFFL))
    @inline def bit50To(b: Boolean) = new Bitx64(if (b) (L | 0x0004000000000000L) else (L & 0xFFFBFFFFFFFFFFFFL))
    @inline def bit51To(b: Boolean) = new Bitx64(if (b) (L | 0x0008000000000000L) else (L & 0xFFF7FFFFFFFFFFFFL))
    @inline def bit52To(b: Boolean) = new Bitx64(if (b) (L | 0x0010000000000000L) else (L & 0xFFEFFFFFFFFFFFFFL))
    @inline def bit53To(b: Boolean) = new Bitx64(if (b) (L | 0x0020000000000000L) else (L & 0xFFDFFFFFFFFFFFFFL))
    @inline def bit54To(b: Boolean) = new Bitx64(if (b) (L | 0x0040000000000000L) else (L & 0xFFBFFFFFFFFFFFFFL))
    @inline def bit55To(b: Boolean) = new Bitx64(if (b) (L | 0x0080000000000000L) else (L & 0xFF7FFFFFFFFFFFFFL))
    @inline def bit56To(b: Boolean) = new Bitx64(if (b) (L | 0x0100000000000000L) else (L & 0xFEFFFFFFFFFFFFFFL))
    @inline def bit57To(b: Boolean) = new Bitx64(if (b) (L | 0x0200000000000000L) else (L & 0xFDFFFFFFFFFFFFFFL))
    @inline def bit58To(b: Boolean) = new Bitx64(if (b) (L | 0x0400000000000000L) else (L & 0xFBFFFFFFFFFFFFFFL))
    @inline def bit59To(b: Boolean) = new Bitx64(if (b) (L | 0x0800000000000000L) else (L & 0xF7FFFFFFFFFFFFFFL))
    @inline def bit60To(b: Boolean) = new Bitx64(if (b) (L | 0x1000000000000000L) else (L & 0xEFFFFFFFFFFFFFFFL))
    @inline def bit61To(b: Boolean) = new Bitx64(if (b) (L | 0x2000000000000000L) else (L & 0xDFFFFFFFFFFFFFFFL))
    @inline def bit62To(b: Boolean) = new Bitx64(if (b) (L | 0x4000000000000000L) else (L & 0xBFFFFFFFFFFFFFFFL))
    @inline def bit63To(b: Boolean) = new Bitx64(if (b) (L | 0x8000000000000000L) else (L & 0x7FFFFFFFFFFFFFFFL))
    @inline def bitTo(i: Int)(b: Boolean) = new Bitx64(if (b) L | (1L<<i) else L & (-1L - (1L<<i)))
    @inline def bitsTo(i: Int, n: Int)(value: Long) = {
      val m = (-1L >>> (64-n)) << i
      new Bitx64( (L & (-1L - m)) | ((value << i) & m) )
    }

    @inline def asBytes = new Bytex8(L)
    @inline def asShorts = new Shortx4(L)
    @inline def asChars = new Charx4(L)
    @inline def asInts = new Intx2(L)
    @inline def asFloats = new Floatx2(L)
    @inline def D = java.lang.Double.longBitsToDouble(L)
  }

  object Bits {
    def apply(
      b0: Boolean, b1: Boolean, b2: Boolean, b3: Boolean,
      b4: Boolean, b5: Boolean, b6: Boolean, b7: Boolean
    ) = new Bitx8({ 
      (if (b0) 0x1 else 0) | (if (b1) 0x2 else 0) | (if (b2) 0x4 else 0) | (if (b3) 0x8 else 0) |
      (if (b4) 0x10 else 0) | (if (b5) 0x20 else 0) | (if (b6) 0x40 else 0) | (if (b7) 0x80 else 0)
    }.toByte)
    def apply(
      b0: Boolean, b1: Boolean, b2: Boolean, b3: Boolean,
      b4: Boolean, b5: Boolean, b6: Boolean, b7: Boolean,
      b8: Boolean, b9: Boolean, b10: Boolean, b11: Boolean,
      b12: Boolean, b13: Boolean, b14: Boolean, b15: Boolean
    ) = new Bitx16({
      (if (b0) 0x1 else 0) | (if (b1) 0x2 else 0) | (if (b2) 0x4 else 0) | (if (b3) 0x8 else 0) |
      (if (b4) 0x10 else 0) | (if (b5) 0x20 else 0) | (if (b6) 0x40 else 0) | (if (b7) 0x80 else 0) |
      (if (b8) 0x100 else 0) | (if (b9) 0x200 else 0) | (if (b10) 0x400 else 0) | (if (b11) 0x800 else 0) |
      (if (b12) 0x1000 else 0) | (if (b13) 0x2000 else 0) | (if (b14) 0x4000 else 0) | (if (b15) 0x8000 else 0)
    }.toShort)
    def apply (
      b0: Boolean, b1: Boolean, b2: Boolean, b3: Boolean,
      b4: Boolean, b5: Boolean, b6: Boolean, b7: Boolean,
      b8: Boolean, b9: Boolean, b10: Boolean, b11: Boolean,
      b12: Boolean, b13: Boolean, b14: Boolean, b15: Boolean,
      b16: Boolean, b17: Boolean, b18: Boolean, b19: Boolean,
      b20: Boolean, b21: Boolean, b22: Boolean, b23: Boolean,
      b24: Boolean, b25: Boolean, b26: Boolean, b27: Boolean,
      b28: Boolean, b29: Boolean, b30: Boolean, b31: Boolean
    ) = new Bitx32(
      (if (b0) 0x1 else 0) | (if (b1) 0x2 else 0) | (if (b2) 0x4 else 0) | (if (b3) 0x8 else 0) |
      (if (b4) 0x10 else 0) | (if (b5) 0x20 else 0) | (if (b6) 0x40 else 0) | (if (b7) 0x80 else 0) |
      (if (b8) 0x100 else 0) | (if (b9) 0x200 else 0) | (if (b10) 0x400 else 0) | (if (b11) 0x800 else 0) |
      (if (b12) 0x1000 else 0) | (if (b13) 0x2000 else 0) | (if (b14) 0x4000 else 0) | (if (b15) 0x8000 else 0) |
      (if (b16) 0x10000 else 0) | (if (b17) 0x20000 else 0) | (if (b18) 0x40000 else 0) | (if (b19) 0x80000 else 0) |
      (if (b20) 0x100000 else 0) | (if (b21) 0x200000 else 0) | (if (b22) 0x400000 else 0) | (if (b23) 0x800000 else 0) |
      (if (b24) 0x1000000 else 0) | (if (b25) 0x2000000 else 0) | (if (b26) 0x4000000 else 0) | (if (b27) 0x8000000 else 0) |
      (if (b28) 0x10000000 else 0) | (if (b29) 0x20000000 else 0) | (if (b30) 0x40000000 else 0) | (if (b31) 0x80000000 else 0)
    )
    def apply(
      b0: Boolean, b1: Boolean, b2: Boolean, b3: Boolean,
      b4: Boolean, b5: Boolean, b6: Boolean, b7: Boolean,
      b8: Boolean, b9: Boolean, b10: Boolean, b11: Boolean,
      b12: Boolean, b13: Boolean, b14: Boolean, b15: Boolean,
      b16: Boolean, b17: Boolean, b18: Boolean, b19: Boolean,
      b20: Boolean, b21: Boolean, b22: Boolean, b23: Boolean,
      b24: Boolean, b25: Boolean, b26: Boolean, b27: Boolean,
      b28: Boolean, b29: Boolean, b30: Boolean, b31: Boolean,
      b32: Boolean, b33: Boolean, b34: Boolean, b35: Boolean,
      b36: Boolean, b37: Boolean, b38: Boolean, b39: Boolean,
      b40: Boolean, b41: Boolean, b42: Boolean, b43: Boolean,
      b44: Boolean, b45: Boolean, b46: Boolean, b47: Boolean,
      b48: Boolean, b49: Boolean, b50: Boolean, b51: Boolean,
      b52: Boolean, b53: Boolean, b54: Boolean, b55: Boolean,
      b56: Boolean, b57: Boolean, b58: Boolean, b59: Boolean,
      b60: Boolean, b61: Boolean, b62: Boolean, b63: Boolean
    ) = new Bitx64(
      (if (b0) 0x1L else 0L) | (if (b1) 0x2L else 0L) |
      (if (b2) 0x4L else 0L) | (if (b3) 0x8L else 0L) |
      (if (b4) 0x10L else 0L) | (if (b5) 0x20L else 0L) |
      (if (b6) 0x40L else 0L) | (if (b7) 0x80L else 0L) |
      (if (b8) 0x100L else 0L) | (if (b9) 0x200L else 0L) |
      (if (b10) 0x400L else 0L) | (if (b11) 0x800L else 0L) |
      (if (b12) 0x1000L else 0L) | (if (b13) 0x2000L else 0L) |
      (if (b14) 0x4000L else 0L) | (if (b15) 0x8000L else 0L) |
      (if (b16) 0x10000L else 0L) | (if (b17) 0x20000L else 0L) |
      (if (b18) 0x40000L else 0L) | (if (b19) 0x80000L else 0L) |
      (if (b20) 0x100000L else 0L) | (if (b21) 0x200000L else 0L) |
      (if (b22) 0x400000L else 0L) | (if (b23) 0x800000L else 0L) |
      (if (b24) 0x1000000L else 0L) | (if (b25) 0x2000000L else 0L) |
      (if (b26) 0x4000000L else 0L) | (if (b27) 0x8000000L else 0L) |
      (if (b28) 0x10000000L else 0L) | (if (b29) 0x20000000L else 0L) |
      (if (b30) 0x40000000L else 0L) | (if (b31) 0x80000000L else 0L) |
      (if (b32) 0x100000000L else 0L) | (if (b33) 0x200000000L else 0L) |
      (if (b34) 0x400000000L else 0L) | (if (b35) 0x800000000L else 0L) |
      (if (b36) 0x1000000000L else 0L) | (if (b37) 0x2000000000L else 0L) |
      (if (b38) 0x4000000000L else 0L) | (if (b39) 0x8000000000L else 0L) |
      (if (b40) 0x10000000000L else 0L) | (if (b41) 0x20000000000L else 0L) |
      (if (b42) 0x40000000000L else 0L) | (if (b43) 0x80000000000L else 0L) |
      (if (b44) 0x100000000000L else 0L) | (if (b45) 0x200000000000L else 0L) |
      (if (b46) 0x400000000000L else 0L) | (if (b47) 0x800000000000L else 0L) |
      (if (b48) 0x1000000000000L else 0L) | (if (b49) 0x2000000000000L else 0L) |
      (if (b50) 0x4000000000000L else 0L) | (if (b51) 0x8000000000000L else 0L) |
      (if (b52) 0x10000000000000L else 0L) | (if (b53) 0x20000000000000L else 0L) |
      (if (b54) 0x40000000000000L else 0L) | (if (b55) 0x80000000000000L else 0L) |
      (if (b56) 0x100000000000000L else 0L) | (if (b57) 0x200000000000000L else 0L) |
      (if (b58) 0x400000000000000L else 0L) | (if (b59) 0x800000000000000L else 0L) |
      (if (b60) 0x1000000000000000L else 0L) | (if (b61) 0x2000000000000000L else 0L) |
      (if (b62) 0x4000000000000000L else 0L) | (if (b63) 0x8000000000000000L else 0L)
    )
  }

  final class Bytex2(val S: Short) extends AnyVal {
    @inline def b0 = (S & 0xFF).toByte
    @inline def b1 = ((S&0xFFFF)>>8).toByte
    @inline def b0To(b: Byte) = new Bytex2(((S&0xFF00)|(b&0xFF)).toShort)
    @inline def b1To(b: Byte) = new Bytex2(((S&0xFF)|((b&0xFF) << 8)).toShort)
    @inline def swapB = new Bytex2((((S&0xFF00)>>8) | ((S&0xFF) << 8)).toShort)

    @inline def asBits = new Bitx16(S)
    @inline def C = S.toChar
  }

  final class Bytex4(val I: Int) extends AnyVal {
    @inline def b0 =  (I & 0xFF)            .toByte
    @inline def b1 = ((I & 0xFF00)   >>   8).toByte
    @inline def b2 = ((I & 0xFF0000) >>  16).toByte
    @inline def b3 =  (I             >>> 24).toByte
    @inline def b0To(b: Byte) = new Bytex4((I & 0xFFFFFF00) | (b & 0xFF))
    @inline def b1To(b: Byte) = new Bytex4((I & 0xFFFF00FF) | ((b & 0xFF) << 8))
    @inline def b2To(b: Byte) = new Bytex4((I & 0xFF00FFFF) | ((b & 0xFF) << 16))
    @inline def b3To(b: Byte) = new Bytex4((I & 0x00FFFFFF) | ((b & 0xFF) << 24))
    @inline def rotrB = new Bytex4((I >>> 8) | (I << 24))
    @inline def rotlB = new Bytex4((I >>> 24) | (I << 8))
    @inline def swapBB = new Bytex4(((I & 0xFF00FF00) >>> 8) | ((I & 0x00FF00FF) << 8))
    @inline def reverseB = new Bytex4(((I&0xFF000000) >>> 24) | ((I&0xFF0000) >> 8) | ((I&0xFF00) << 8) | ((I&0xFF) << 24))

    @inline def asBits = new Bitx32(I)
    @inline def asShorts = new Shortx2(I)
    @inline def asChars = new Charx2(I)
    @inline def F = java.lang.Float.intBitsToFloat(I)
  }

  final class Bytex8(val L: Long) extends AnyVal {
    @inline def b0 =  (L & 0xFF)                     .toByte
    @inline def b1 = ((L & 0xFF00)            >>   8).toByte
    @inline def b2 = ((L & 0xFF0000)          >>  16).toByte
    @inline def b3 = ((L & 0xFF000000L)       >>  24).toByte
    @inline def b4 = ((L & 0xFF00000000L)     >>  32).toByte
    @inline def b5 = ((L & 0xFF0000000000L)   >>  40).toByte
    @inline def b6 = ((L & 0xFF000000000000L) >>  48).toByte
    @inline def b7 = ( L                      >>> 56).toByte
    @inline def b0To(b: Byte) = new Bytex8((L&0xFFFFFFFFFFFFFF00L) |  (b&0xFF))
    @inline def b1To(b: Byte) = new Bytex8((L&0xFFFFFFFFFFFF00FFL) | ((b&0xFF) << 8))
    @inline def b2To(b: Byte) = new Bytex8((L&0xFFFFFFFFFF00FFFFL) | ((b&0xFF) << 16))
    @inline def b3To(b: Byte) = new Bytex8((L&0xFFFFFFFF00FFFFFFL) | ((b&0xFF).toLong << 24))
    @inline def b4To(b: Byte) = new Bytex8((L&0xFFFFFF00FFFFFFFFL) | ((b&0xFF).toLong << 32))
    @inline def b5To(b: Byte) = new Bytex8((L&0xFFFF00FFFFFFFFFFL) | ((b&0xFF).toLong << 40))
    @inline def b6To(b: Byte) = new Bytex8((L&0xFF00FFFFFFFFFFFFL) | ((b&0xFF).toLong << 48))
    @inline def b7To(b: Byte) = new Bytex8((L&0x00FFFFFFFFFFFFFFL) | ((b&0xFF).toLong << 56))
    @inline def rotrB = new Bytex8((L >>> 8) | (L << 56))
    @inline def rotlB = new Bytex8((L >>> 56) | (L << 8))
    @inline def swapBBBB = new Bytex8(((L&0xFF00FF00FF00FF00L) >>> 8) | ((L&0x00FF00FF00FF00FFL) << 8))
    @inline def reverseB = {
      val m = swapBBBB.L
      new Bytex8((m>>>48) | ((m&0xFFFF00000000L)>>16) | ((m&0xFFFF0000L) << 16) | (m << 48))
    }

    @inline def asBits = new Bitx64(L)
    @inline def asShorts = new Shortx4(L)
    @inline def asChars = new Charx4(L)
    @inline def asInts = new Intx2(L)
    @inline def asFloats = new Floatx2(L)
    @inline def D = java.lang.Double.longBitsToDouble(L)
  }

  object Bytes {
    def apply(b0: Byte, b1: Byte) = new Bytex2(((b0 & 0xFF) | ((b1 & 0xFF) << 8 )).toShort)
    def apply(b0: Byte, b1: Byte, b2: Byte, b3: Byte) =
      new Bytex4((b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | (b3 << 24))
    def apply(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte) =
      new Bytex8(
        (b0.toLong & 0xFF) | ((b1.toLong & 0xFF) << 8) | ((b2.toLong & 0xFF) << 16) | ((b3.toLong & 0xFF) << 24) |
        ((b4.toLong & 0xFF) << 32) | ((b5.toLong & 0xFF) << 40) | ((b6.toLong & 0xFF) << 48) | (b7.toLong << 56)
    )
  }

  final class Shortx2(val I: Int) extends AnyVal {
    @inline def s0 = (I & 0xFFFF).toShort
    @inline def s1 = (I >>> 16).toShort
    @inline def s0To(s: Short) = new Shortx2((I&0xFFFF0000) | (s&0xFFFF))
    @inline def s1To(s: Short) = new Shortx2((I&0xFFFF) | ((s&0xFFFF) << 16))
    @inline def swapS = new Shortx2((I >>> 16) | (I << 16))

    @inline def asBits = new Bitx32(I)
    @inline def asBytes = new Bytex4(I)
    @inline def asChars = new Charx2(I)
    @inline def F = java.lang.Float.intBitsToFloat(I)
  }

  final class Shortx4(val L: Long) extends AnyVal {
    @inline def s0 =  (L & 0xFFFF)                 .toShort
    @inline def s1 = ((L & 0xFFFF0000L)     >>  16).toShort
    @inline def s2 = ((L & 0xFFFF00000000L) >>  32).toShort
    @inline def s3 = ( L                    >>> 48).toShort
    @inline def s0To(s: Short) = new Shortx4((L & 0xFFFFFFFFFFFF0000L) | (s&0xFFFF))
    @inline def s1To(s: Short) = new Shortx4((L & 0xFFFFFFFF0000FFFFL) | ((s&0xFFFF).toLong << 16))
    @inline def s2To(s: Short) = new Shortx4((L & 0xFFFF0000FFFFFFFFL) | ((s&0xFFFF).toLong << 32))
    @inline def s3To(s: Short) = new Shortx4((L & 0x0000FFFFFFFFFFFFL) | (s.toLong << 48))
    @inline def rotrS = new Shortx4((L >>> 16) | (L << 48))
    @inline def rotlS = new Shortx4((L >>> 48) | (L << 16))
    @inline def swapSS = new Shortx4(((L&0xFFFF0000FFFF0000L) >>> 16) | ((L&0x0000FFFF0000FFFFL) << 16))
    @inline def reverseS = new Shortx4((L >>> 48) | ((L&0xFFFF00000000L) >> 16) | ((L&0xFFFF0000L) << 16) | (L << 48))

    @inline def asBits = new Bitx64(L)
    @inline def asBytes = new Bytex8(L)
    @inline def asChars = new Charx4(L)
    @inline def asInts = new Intx2(L)
    @inline def asFloats = new Floatx2(L)
    @inline def D = java.lang.Double.longBitsToDouble(L)
  }

  object Shorts {
    def apply(s0: Short, s1: Short) = new Shortx2((s0 & 0xFFFF) | (s1 << 16))
    def apply(s0: Short, s1: Short, s2: Short, s3: Short) =
      new Shortx4((s0.toLong & 0xFFFF) | ((s1.toLong & 0xFFFF) << 16) | ((s2.toLong & 0xFFFF) << 32) | (s2.toLong << 48))
  }

  final class Charx2(val I: Int) extends AnyVal {
    @inline def c0 = I.toChar
    @inline def c1 = (I >>> 16).toChar
    @inline def c0To(c: Char) = new Charx2((I&0xFFFF0000) | c)
    @inline def c1To(c: Char) = new Charx2((I&0xFFFF) | (c << 16))
    @inline def swapC = new Charx2((I >>> 16) | (I << 16))

    @inline def asBits = new Bitx32(I)
    @inline def asBytes = new Bytex4(I)
    @inline def asShorts = new Shortx2(I)
    @inline def F = java.lang.Float.intBitsToFloat(I)
  }

  final class Charx4(val L: Long) extends AnyVal {
    @inline def c0 =  (L & 0xFFFF)                 .toChar
    @inline def c1 = ((L & 0xFFFF0000L)     >>  16).toChar
    @inline def c2 = ((L & 0xFFFF00000000L) >>  32).toChar
    @inline def c3 = ( L                    >>> 48).toChar
    @inline def c0To(c: Char) = new Charx4((L & 0xFFFFFFFFFFFF0000L) | c)
    @inline def c1To(c: Char) = new Charx4((L & 0xFFFFFFFF0000FFFFL) | (c.toLong << 16))
    @inline def c2To(c: Char) = new Charx4((L & 0xFFFF0000FFFFFFFFL) | (c.toLong << 32))
    @inline def c3To(c: Char) = new Charx4((L & 0x0000FFFFFFFFFFFFL) | (c.toLong << 48))
    @inline def rotrC = new Charx4((L >>> 16) | (L << 48))
    @inline def rotlC = new Charx4((L >>> 48) | (L << 16))
    @inline def swapCC = new Charx4(((L&0xFFFF0000FFFF0000L) >>> 16) | ((L&0x0000FFFF0000FFFFL) << 16))
    @inline def reverseC = new Charx4((L >>> 48) | ((L&0xFFFF00000000L) >> 16) | ((L&0xFFFF0000L) << 16) | (L << 48))

    @inline def asBits = new Bitx64(L)
    @inline def asBytes = new Bytex8(L)
    @inline def asShorts = new Shortx4(L)
    @inline def asInts = new Intx2(L)
    @inline def asFloats = new Floatx2(L)
    @inline def D = java.lang.Double.longBitsToDouble(L)
  }

  object Chars {
    def apply(c0: Char, c1: Char) = new Charx2((c0 & 0xFFFF) | (c1 << 16))
    def apply(c0: Char, c1: Char, c2: Char, c3: Char) =
      new Charx4((c0.toLong & 0xFFFF) | ((c1.toLong & 0xFFFF) << 16) | ((c2.toLong & 0xFFFF) << 32) | (c2.toLong << 48))
  }

  final class Intx2(val L: Long) extends AnyVal {
    @inline def i0 = (L & 0xFFFFFFFFL).toInt
    @inline def i1 = (L >>> 32).toInt
    @inline def i0To(i: Int) = new Intx2((L&0xFFFFFFFF00000000L) | (i&0xFFFFFFFFL))
    @inline def i1To(i: Int) = new Intx2((L&0xFFFFFFFFL) | (i.toLong<<32))
    @inline def swapI = new Intx2((L >>> 32) | (L << 32))

    @inline def asBits = new Bitx64(L)
    @inline def asBytes = new Bytex8(L)
    @inline def asShorts = new Shortx4(L)
    @inline def asChars = new Charx4(L)
    @inline def asFloats = new Floatx2(L)
    @inline def D = java.lang.Double.longBitsToDouble(L)
  }

  object Ints {
    def apply(i0: Int, i1: Int) = new Intx2((i0 & 0xFFFFFFFFL) | (i0.toLong << 32))
  }

  final class Floatx2(val L: Long) extends AnyVal {
    @inline def f0 = java.lang.Float.intBitsToFloat((L & 0xFFFFFFFFL).toInt)
    @inline def f1 = java.lang.Float.intBitsToFloat((L >>> 32).toInt)
    @inline def f0To(f: Float) = new Floatx2((L & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL))
    @inline def f1To(f: Float) = new Floatx2((L & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f).toLong << 32))
    @inline def swapF = new Floatx2((L >>> 32) | (L << 32))

    @inline def asBits = new Bitx64(L)
    @inline def asBytes = new Bytex8(L)
    @inline def asShorts = new Shortx4(L)
    @inline def asChars = new Charx4(L)
    @inline def asInts = new Intx2(L)
    @inline def D = java.lang.Double.longBitsToDouble(L)
  }

  object Floats {
    def apply(f0: Float, f1: Float) = new Floatx2(
      (java.lang.Float.floatToRawIntBits(f0) & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f1).toLong << 32)
    )
  }
}

package object packed {
  implicit final class ByteCanPack(private val b: Byte) extends AnyVal {
    @inline def asBits = new Bitx8(b)
    @inline def <>(b2: Byte) = Bytes(b, b2)
    @inline def bincat(b2: Byte) = Bytes(b, b2)
  }
  
  implicit final class ShortCanPack(private val s: Short) extends AnyVal {
    @inline def asBits = new Bitx16(s)
    @inline def asBytes = new Bytex2(s)
    @inline def <>(s2: Short) = Shorts(s, s2)
    @inline def bincat(s2: Short) = Shorts(s, s2)
  }
  
  implicit final class CharCanPack(private val c: Char) extends AnyVal {
    @inline def asBits = new Bitx16(c.toShort)
    @inline def asBytes = new Bytex2(c.toShort)
    @inline def <>(c2: Char) = Chars(c, c2)
    @inline def bincat(c2: Char) = Chars(c, c2)
  }
  
  implicit final class IntCanPack(private val i: Int) extends AnyVal {
    @inline def asBits = new Bitx32(i)
    @inline def asBytes = new Bytex4(i)
    @inline def asShorts = new Shortx2(i)
    @inline def asChars = new Charx2(i)
    @inline def <>(i2: Int) = Ints(i, i2)
    @inline def bincat(i2: Int) = Ints(i, i2)
  }
  
  implicit final class FloatCanPack(private val f: Float) extends AnyVal {
    @inline def asBits = new Bitx32(java.lang.Float.floatToRawIntBits(f))
    @inline def asBytes = new Bytex4(java.lang.Float.floatToRawIntBits(f))
    @inline def asShorts = new Shortx2(java.lang.Float.floatToRawIntBits(f))
    @inline def asChars = new Charx2(java.lang.Float.floatToRawIntBits(f))
    @inline def <>(f2: Float) = Floats(f, f2)
    @inline def bincat(f2: Float) = Floats(f, f2)
  }
  
  implicit final class LongCanPack(private val l: Long) extends AnyVal {
    @inline def asBits = new Bitx64(l)
    @inline def asBytes = new Bytex8(l)
    @inline def asShorts = new Shortx4(l)
    @inline def asChars = new Charx4(l)
    @inline def asInts = new Intx2(l)
    @inline def asFloats = new Floatx2(l)
  }
  
  implicit final class DoubleCanPack(private val d: Double) extends AnyVal {
    @inline def asBits = new Bitx64(java.lang.Double.doubleToRawLongBits(d))
    @inline def asBytes = new Bytex8(java.lang.Double.doubleToRawLongBits(d))
    @inline def asShorts = new Shortx4(java.lang.Double.doubleToRawLongBits(d))
    @inline def asChars = new Charx4(java.lang.Double.doubleToRawLongBits(d))
    @inline def asInts = new Intx2(java.lang.Double.doubleToRawLongBits(d))
    @inline def asFloats = new Floatx2(java.lang.Double.doubleToRawLongBits(d))
  }
}
