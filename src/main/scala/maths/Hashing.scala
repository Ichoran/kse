// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015-2016 by Rex Kerr and Calico Life Sciences
// Contains code ported from xxHash C source (by Yann Collet, "Cyan5973")
//   See https://github.com/Cyan4973/xxHash
// Contains code ported from MurmurHash C++ source (by Austin Appleby)
//   See https://github.com/aappleby/smhasher

package kse.maths

import java.lang.Integer.{rotateLeft => rotl32, rotateRight => rotr32 }
import java.lang.Long.{rotateLeft => rotl64, rotateRight => rotr64 }
import java.nio.{ByteBuffer, ByteOrder}

package hashing {

  trait IncrementalHash {
    def begin(): this.type
    def append(bb: ByteBuffer): this.type
    def resultAs[A](implicit tag: scala.reflect.ClassTag[A]): Option[A]
  }

  trait FullHash32 {
    def hash32(bb: ByteBuffer, seed: Int): Int
    final def hash32(bb: ByteBuffer): Int = hash32(bb, 0)
  }

  trait Hash32 extends FullHash32 with IncrementalHash {
    def hash32(bb: ByteBuffer, seed: Int): Int = begin(seed).result(bb)
    def begin(): this.type = begin(0)
    def begin(seed: Int): this.type
    def append(bb: ByteBuffer): this.type
    def result(bb: ByteBuffer): Int
    def result(): Int
    def resultAs[A](implicit tag: scala.reflect.ClassTag[A]): Option[A] = {
      tag.unapply(0: Int) match {
        case Some(_) => tag.unapply(result())
        case None    => None
      }
    }
  }


  trait FullHash64 {
    def hash64(bb: ByteBuffer, seed: Long): Long
    final def hash64(bb: ByteBuffer): Long = hash64(bb, 0L)
  }

  trait Hash64 extends FullHash64 with IncrementalHash {
    def hash64(bb: ByteBuffer, seed: Long): Long = begin(seed).result(bb)
    def begin(): this.type = begin(0L)
    def begin(seed: Long): this.type
    def append(bb: ByteBuffer): this.type
    def result(bb: ByteBuffer): Long
    def result(): Long
    def resultAs[A](implicit tag: scala.reflect.ClassTag[A]): Option[A] = {
      tag.unapply(0: Long) match {
        case Some(_) => tag.unapply(result())
        case None    => None
      }
    }
  }


  trait FullHash128 {
    def hash128(bb: ByteBuffer, seed0: Long, seed1: Long): Array[Long]
    final def hash128(bb: ByteBuffer): Array[Long] = hash128(bb, 0L, 0L)
  }

  trait Hash128 extends FullHash128 with IncrementalHash {
    def hash128(bb: ByteBuffer, seed0: Long, seed1: Long): Array[Long] = begin(seed0, seed1).results(bb)
    def begin(): this.type = begin(0L, 0L)
    def begin(seed0: Long, seed1: Long): this.type
    def append(bb: ByteBuffer): this.type
    final def results(bb: ByteBuffer): Array[Long] = { val a = new Array[Long](2); resultInto(bb, a, 0) }
    final def results(): Array[Long] = { val a = new Array[Long](2); resultInto(a, 0) }
    def resultInto(bb: ByteBuffer, target: Array[Long], index: Int): Array[Long]
    def resultInto(target: Array[Long], index: Int): Array[Long]
    def resultAs[A](implicit tag: scala.reflect.ClassTag[A]): Option[A] = {
      tag.unapply(Hash128.emptyArrayLong).flatMap{ _ => tag.unapply(results()) }
    }
  }
  object Hash128 {
    private[hashing] val emptyArrayLong = new Array[Long](0)
  }


  final class XxHash32(initialSeed: Int) extends Hash32 {
    import XX.{Prime32_1, Prime32_2, Prime32_3, Prime32_4, Prime32_5}
    private[this] var v1: Int = 0
    private[this] var v2: Int = 0
    private[this] var v3: Int = 0
    private[this] var v4: Int = 0
    private[this] var v5: Int = 0
    private[this] var hadBlock: Boolean = false
    private[this] var myBuffer: ByteBuffer = null
    begin(initialSeed)
    def this() = this(0)
    def begin(seed: Int): this.type = {
      v1 = seed + Prime32_1 + Prime32_2
      v2 = seed + Prime32_2
      v3 = seed
      v4 = seed - Prime32_1
      v5 = 0
      hadBlock = false
      if (myBuffer ne null) myBuffer.clear
      this
    }
    def appendBy16(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      var x1 = v1
      var x2 = v2
      var x3 = v3
      var x4 = v4
      if (bb.remaining >= 16) {
        hadBlock = true
        v5 += (bb.remaining & 0xFFFFFFF0)
      }
      while (bb.remaining >= 16) {
        x1 = rotl32(x1 + bb.getInt * Prime32_2, 13) * Prime32_1
        x2 = rotl32(x2 + bb.getInt * Prime32_2, 13) * Prime32_1
        x3 = rotl32(x3 + bb.getInt * Prime32_2, 13) * Prime32_1
        x4 = rotl32(x4 + bb.getInt * Prime32_2, 13) * Prime32_1
      }
      v1 = x1
      v2 = x2
      v3 = x3
      v4 = x4
      this        
    }
    def appendIx4(one: Int, two: Int, three: Int, four: Int): this.type = {
      v1 = rotl32(v1 +   one * Prime32_2, 13) * Prime32_1
      v2 = rotl32(v2 +   two * Prime32_2, 13) * Prime32_1
      v3 = rotl32(v3 + three * Prime32_2, 13) * Prime32_1
      v4 = rotl32(v4 +  four * Prime32_2, 13) * Prime32_1
      v5 += 16
      hadBlock = true
      this
    }
    def counting(extra: Int): this.type = {
      v1 = if (!hadBlock) v3 + Prime32_5 else rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
      v1 += v5 + extra
      this
    }
    def trailing(one: Int): this.type = {
      v1 = rotl32(v1 + one * Prime32_3, 17) * Prime32_4
      this
    }
    def trailing(quarter: Byte): this.type = {
      v1 = rotl32(v1 + (quarter&0xFF) * Prime32_5, 11) * Prime32_1
      this      
    }
    def complete(): Int = {
      var h32 = v1
      h32 ^= h32 >>> 15
      h32 *= Prime32_2
      h32 ^= h32 >>> 13
      h32 *= Prime32_3
      h32 ^ (h32 >>> 16)      
    }
    def append(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if ((myBuffer ne null) && (myBuffer.position > 0)) {
        while (myBuffer.position <= 12 && bb.remaining >= 4) myBuffer.putInt(bb.getInt)
        while (myBuffer.position < 16 && bb.remaining >= 1) myBuffer.put(bb.get)
        if (myBuffer.position == 16) {
          myBuffer.flip
          appendIx4(myBuffer.getInt, myBuffer.getInt, myBuffer.getInt, myBuffer.getInt)
          myBuffer.clear
        }
      }
      if (bb.remaining >= 16) appendBy16(bb)
      if (bb.remaining > 0) {
        if (myBuffer eq null) {
          myBuffer = ByteBuffer.allocate(16)
          myBuffer.order(ByteOrder.LITTLE_ENDIAN)
        }
        while (bb.remaining >= 4) myBuffer.putInt(bb.getInt)
        while (bb.remaining >= 1) myBuffer.put(bb.get)
      }
      this
    }
    def result(bb: ByteBuffer): Int = {
      val terminal =
        if ((myBuffer ne null) && (myBuffer.position > 0)) {
          append(bb)
          myBuffer.flip
          if (myBuffer.remaining == 16) appendIx4(myBuffer.getInt, myBuffer.getInt, myBuffer.getInt, myBuffer.getInt)
          myBuffer
        }
        else {
          if (bb.remaining >= 16) appendBy16(bb)
          else bb.order(ByteOrder.LITTLE_ENDIAN)
          bb
        }
      counting(terminal.remaining)
      while (terminal.remaining >= 4) trailing(terminal.getInt)
      while (terminal.remaining >= 1) trailing(terminal.get)
      if (terminal eq myBuffer) myBuffer.clear
      complete()
    }
    def result(): Int = {
      if ((myBuffer ne null) && myBuffer.hasRemaining) {
        counting(myBuffer.remaining)
        while (myBuffer.remaining >= 4) trailing(myBuffer.getInt)
        while (myBuffer.remaining >= 1) trailing(myBuffer.get)
        myBuffer.clear
        complete()
      }
      else {
        counting(0)
        complete()
      }
    }
  }


  final class XxHash64(initialSeed: Long) extends Hash64 {
    import XX.{Prime64_1, Prime64_2, Prime64_3, Prime64_4, Prime64_5}
    private[this] var v1: Long = 0
    private[this] var v2: Long = 0
    private[this] var v3: Long = 0
    private[this] var v4: Long = 0
    private[this] var v5: Long = 0
    private[this] var hadBlock: Boolean = false
    private[this] var myBuffer: ByteBuffer = null
    begin(initialSeed)
    def this() = this(0)
    def begin(seed: Long): this.type = {
      v1 = seed + Prime64_1 + Prime64_2
      v2 = seed + Prime64_2
      v3 = seed
      v4 = seed - Prime64_1
      v5 = 0
      hadBlock = false
      if (myBuffer ne null) myBuffer.clear
      this
    }
    def appendBy32(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      var x1 = v1
      var x2 = v2
      var x3 = v3
      var x4 = v4
      if (bb.remaining >= 32) {
        hadBlock = true
        v5 += (bb.remaining & 0xFFFFFFE0)
      }
      while (bb.remaining >= 32) {
        x1 = rotl64(x1 + bb.getLong * Prime64_2, 31) * Prime64_1
        x2 = rotl64(x2 + bb.getLong * Prime64_2, 31) * Prime64_1
        x3 = rotl64(x3 + bb.getLong * Prime64_2, 31) * Prime64_1
        x4 = rotl64(x4 + bb.getLong * Prime64_2, 31) * Prime64_1
      }
      v1 = x1
      v2 = x2
      v3 = x3
      v4 = x4
      this        
    }
    def appendLx4(one: Long, two: Long, three: Long, four: Long): this.type = {
      v1 = rotl64(v1 +   one * Prime64_2, 31) * Prime64_1
      v2 = rotl64(v2 +   two * Prime64_2, 31) * Prime64_1
      v3 = rotl64(v3 + three * Prime64_2, 31) * Prime64_1
      v4 = rotl64(v4 +  four * Prime64_2, 31) * Prime64_1
      v5 += 32
      hadBlock = true
      this
    }
    def counting(extra: Int): this.type = {
      v1 =
        if (!hadBlock) v3 + Prime64_5
        else {
          var x = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18)
          x ^= rotl64(v1 * Prime64_2, 31) * Prime64_1
          x = x*Prime64_1 + Prime64_4
          x ^= rotl64(v2 * Prime64_2, 31) * Prime64_1
          x = x*Prime64_1 + Prime64_4
          x ^= rotl64(v3 * Prime64_2, 31) * Prime64_1
          x = x*Prime64_1 + Prime64_4
          x ^= rotl64(v4 * Prime64_2, 31) * Prime64_1
          x*Prime64_1 + Prime64_4
        }
      v1 += v5 + extra
      this
    }
    def trailing(one: Long): this.type = {
      v1 = rotl64(v1 ^ (rotl64(one * Prime64_2, 31) * Prime64_1), 27)*Prime64_1 + Prime64_4
      this
    }
    def trailing(one: Int): this.type = {
      v1 = rotl64(v1 ^ ((one & 0xFFFFFFFFL) * Prime64_1), 23) * Prime64_2 + Prime64_3
      this
    }
    def trailing(quarter: Byte): this.type = {
      v1 = rotl64(v1 ^ ((quarter & 0xFF) * Prime64_5), 11) * Prime64_1
      this      
    }
    def complete(): Long = {
      var h64 = v1
      h64 ^= h64 >>> 33
      h64 *= Prime64_2
      h64 ^= h64 >>> 29
      h64 *= Prime64_3
      h64 ^ (h64 >>> 32)      
    }
    def append(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if ((myBuffer ne null) && (myBuffer.position > 0)) {
        while (myBuffer.position <= 24 && bb.remaining >= 8) myBuffer.putLong(bb.getLong)
        while (myBuffer.position < 32 && bb.remaining >= 1) myBuffer.put(bb.get)
        if (myBuffer.position == 32) {
          myBuffer.flip
          appendLx4(myBuffer.getLong, myBuffer.getLong, myBuffer.getLong, myBuffer.getLong)
          myBuffer.clear
        }
      }
      if (bb.remaining >= 32) appendBy32(bb)
      if (bb.remaining > 0) {
        if (myBuffer eq null) {
          myBuffer = ByteBuffer.allocate(32)
          myBuffer.order(ByteOrder.LITTLE_ENDIAN)
        }
        while (bb.remaining >= 8) myBuffer.putLong(bb.getLong)
        while (bb.remaining >= 1) myBuffer.put(bb.get)
      }
      this
    }
    def result(bb: ByteBuffer): Long = {
      val terminal =
        if ((myBuffer ne null) && (myBuffer.position > 0)) {
          append(bb)
          myBuffer.flip
          if (myBuffer.remaining == 36) appendLx4(myBuffer.getLong, myBuffer.getLong, myBuffer.getLong, myBuffer.getLong)
          myBuffer
        }
        else {
          if (bb.remaining >= 32) appendBy32(bb)
          else bb.order(ByteOrder.LITTLE_ENDIAN)
          bb
        }
      counting(terminal.remaining)
      while (terminal.remaining >= 8) trailing(terminal.getLong)
      if (terminal.remaining >= 4) trailing(terminal.getInt)
      while (terminal.remaining >= 1) trailing(terminal.get)
      if (terminal eq myBuffer) myBuffer.clear
      complete()
    }
    def result(): Long = {
      if ((myBuffer ne null) && myBuffer.hasRemaining) {
        counting(myBuffer.remaining)
        while (myBuffer.remaining >= 8) trailing(myBuffer.getLong)
        if (myBuffer.remaining >= 4) trailing(myBuffer.getInt)
        while (myBuffer.remaining >= 1) trailing(myBuffer.get)
        myBuffer.clear
        complete()
      }
      else {
        counting(0)
        complete()
      }
    }
  }


  object XX extends FullHash32 with FullHash64 {
    final val Prime32_1 = 0x9e3779b1 // 2654435761
    final val Prime32_2 = 0x85ebca77 // 2246822519
    final val Prime32_3 = 0xc2b2ae3d // 3266489917
    final val Prime32_4 = 0x27d4eb2f //  668265263
    final val Prime32_5 = 0x165667b1 //  374761393
    final val Prime64_1 = 0x9e3779b185ebca87L // 11400714785074694791L
    final val Prime64_2 = 0xc2b2ae3d27d4eb4fL // 14029467366897019727L
    final val Prime64_3 = 0x165667b19e3779f9L //  1609587929392839161L
    final val Prime64_4 = 0x85ebca77c2b2ae63L //  9650029242287828579L
    final val Prime64_5 = 0x27d4eb2f165667c5L //  2870177450012600261L

    def hash32(a: Array[Byte], seed: Int, i0: Int = 0, iN: Int = Int.MaxValue): Int = {
      val iM = math.min(a.length, iN)
      var i = i0
      var h32 =
        if (i0 > iM - 16) seed + Prime32_5
        else {
          var v1 = seed + Prime32_1 + Prime32_2
          var v2 = seed + Prime32_2
          var v3 = seed
          var v4 = seed - Prime32_1
          do {
            v1 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
            v1 = rotl32(v1, 13)
            v1 *= Prime32_1
            i += 4
            v2 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
            v2 = rotl32(v2, 13)
            v2 *= Prime32_1
            i += 4
            v3 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
            v3 = rotl32(v3, 13)
            v3 *= Prime32_1
            i += 4
            v4 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
            v4 = rotl32(v4, 13)
            v4 *= Prime32_1
            i += 4
          } while (i <= iM - 16);
          rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
        }
      h32 += (iM - i0)
      while (i <= iM - 4) {
        h32 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_3
        h32 = rotl32(h32, 17) * Prime32_4
        i += 4
      }
      while (i < iM) {
        h32 += (a(i) & 0xFF) * Prime32_5
        h32 = rotl32(h32, 11) * Prime32_1
        i += 1
      }
      h32 ^= h32 >>> 15
      h32 *= Prime32_2
      h32 ^= h32 >>> 13
      h32 *= Prime32_3
      h32 ^ (h32 >>> 16)
    }

    def hash32(bb: ByteBuffer, seed: Int): Int = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      val len = bb.remaining
      var h32 =
        if (bb.remaining < 16) seed + Prime32_5
        else {
          var v1 = seed + Prime32_1 + Prime32_2
          var v2 = seed + Prime32_2
          var v3 = seed
          var v4 = seed - Prime32_1
          do {
            v1 += bb.getInt * Prime32_2
            v1 = rotl32(v1, 13)
            v1 *= Prime32_1
            v2 += bb.getInt * Prime32_2
            v2 = rotl32(v2, 13)
            v2 *= Prime32_1
            v3 += bb.getInt * Prime32_2
            v3 = rotl32(v3, 13)
            v3 *= Prime32_1
            v4 += bb.getInt * Prime32_2
            v4 = rotl32(v4, 13)
            v4 *= Prime32_1
          } while (bb.remaining >= 16);
          rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
        }
      h32 += len
      while (bb.remaining >= 4) {
        h32 += bb.getInt * Prime32_3
        h32 = rotl32(h32, 17) * Prime32_4
      }
      while (bb.hasRemaining) {
        h32 += (bb.get & 0xFF) * Prime32_5
        h32 = rotl32(h32, 11) * Prime32_1
      }
      h32 ^= h32 >>> 15
      h32 *= Prime32_2
      h32 ^= h32 >>> 13
      h32 *= Prime32_3
      h32 ^ (h32 >>> 16)
    }

    def hash64(bb: ByteBuffer, seed: Long): Long = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      val len = bb.remaining
      var h64 =
        if (bb.remaining < 32) seed + Prime64_5
        else {
          var v1 = seed + Prime64_1 + Prime64_2
          var v2 = seed + Prime64_2
          var v3 = seed
          var v4 = seed - Prime64_1
          do {
            v1 += bb.getLong * Prime64_2
            v1 = rotl64(v1, 31)
            v1 *= Prime64_1
            v2 += bb.getLong * Prime64_2
            v2 = rotl64(v2, 31)
            v2 *= Prime64_1
            v3 += bb.getLong * Prime64_2
            v3 = rotl64(v3, 31)
            v3 *= Prime64_1
            v4 += bb.getLong * Prime64_2
            v4 = rotl64(v4, 31)
            v4 *= Prime64_1
          } while (bb.remaining >= 32);
          var x = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18)
          x ^= rotl64(v1 * Prime64_2, 31) * Prime64_1
          x = x*Prime64_1 + Prime64_4
          x ^= rotl64(v2 * Prime64_2, 31) * Prime64_1
          x = x*Prime64_1 + Prime64_4
          x ^= rotl64(v3 * Prime64_2, 31) * Prime64_1
          x = x*Prime64_1 + Prime64_4
          x ^= rotl64(v4 * Prime64_2, 31) * Prime64_1
          x*Prime64_1 + Prime64_4
        }
      h64 += len
      while (bb.remaining >= 8) {
        h64 ^= rotl64(bb.getLong * Prime64_2, 31) * Prime64_1
        h64 = rotl64(h64, 27)*Prime64_1 + Prime64_4
      }
      if (bb.remaining >= 4) {
        h64 ^= (bb.getInt & 0xFFFFFFFFL) * Prime64_1
        h64 = rotl64(h64, 23) * Prime64_2 + Prime64_3
      }
      while (bb.hasRemaining) {
        h64 ^= (bb.get & 0xFF) * Prime64_5
        h64 = rotl64(h64, 11) * Prime64_1
      }
      h64 ^= h64 >>> 33
      h64 *= Prime64_2
      h64 ^= h64 >>> 29
      h64 *= Prime64_3
      h64 ^ (h64 >>> 32)      
    }
    def hash64(ab: Array[Byte], seed: Int, i0: Int = 0, iN: Int = Int.MaxValue): Long = hash64(ByteBuffer.wrap(ab, i0, math.min(iN, ab.length)), seed)
  }

  /// Austin Appleby's MurmurHash3, commit 92cf370 -- x86 32 bit algorithm
  final class Murmur32 extends Hash32 {
    private[this] var state = 0
    private[this] var n = 0
    private[this] var partial = 0
    private[this] var partialN = 0
    def appendI(i: Int): this.type = {
      n += 4
      val x = state ^ (0x1B873593 * rotl32(i * 0xCC9E2D51, 15))
      state = (5 * rotl32(x, 13)) + 0xE6546B64
      this
    }
    def appendLastI(i: Int, bytes: Int): this.type = {
      n += (bytes&3)
      state = state ^ (0x1B873593 * rotl32(i * 0xCC9E2D51, 15))
      this
    }
    private[this] def finalizer() {
      val x = state ^ n
      val y = 0x85EBCA6B * (x ^ (x >>> 16))
      val z = 0xC2B2AE35 * (y ^ (y >>> 13))
      state = z ^ (z >>> 16)
    }
    def begin(seed: Int): this.type = { state = seed; n = 0; partial = 0; partialN = 0; this }
    def append(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if (partialN > 0) {
        while (partialN < 4 && bb.hasRemaining) {
          partial |= (bb.get & 0xFF) << (partialN*8)
          partialN += 1
        }
        if (partialN == 4) {
          appendI(partial)
          partialN = 0
          partial = 0
        }
      }
      while (bb.remaining >= 4) appendI(bb.getInt)
      while (bb.hasRemaining) {
        partial |= (bb.get & 0xFF) << (partialN*8)
        partialN += 1
      }
      this
    }
    def result(bb: ByteBuffer): Int = {
      append(bb)
      result()
    }
    def result(): Int = {
      if (partialN > 0) {
        appendLastI(partial, partialN)
        partialN = 0
        partial = 0
      }
      finalizer()
      n = 0
      state
    }
  }

  final class Murmur128 extends Hash64 with Hash128 with IncrementalHash {
    private[this] var state0, state1 = 0L
    private[this] var partial0, partial1 = 0L
    private[this] var partialN = 0
    private[this] var n = 0
    override def begin(): this.type = begin(0L, 0L)
    def begin(seed: Long): this.type = begin(seed, 0L)
    def begin(seed0: Long, seed1: Long): this.type = {
      state0 = seed0
      state1 = seed1
      partial0 = 0
      partial1 = 0
      partialN = 0
      n = 0
      this
    }
    def appendLx2(la: Long, lb: Long) = {
      n += 16
      val x0 = state0 ^ (0x4CF5AD432745937FL * rotl64(la * 0x87C37B91114253D5L, 31))
      state0 = ((rotl64(x0, 27) + state1) * 5) + 0x52DCE729
      val x1 = state1 ^ (0x87C37B91114253D5L * rotl64(lb * 0x4CF5AD432745937FL, 33))
      state1 = ((rotl64(x1, 31) + state0) * 5) + 0x38495AB5
      this
    }
    def appendLastLx2(la: Long, lb: Long, bytes: Int): this.type = {
      val m = bytes & 0xF
      n += m
      if (m > 8) state1 = state1 ^ (0x87C37B91114253D5L * rotl64(lb * 0x4CF5AD432745937FL, 33))
      state0 = state0 ^ (0x4CF5AD432745937FL * rotl64(la * 0x87C37B91114253D5L, 31))
      this
    }
    private[this] def mixer(l: Long): Long = {
      val x = 0xFF51AFD7ED558CCDL * (l ^ (l >>> 33))
      val y = 0xC4CEB9FE1A85EC53L * (x ^ (x >>> 33))
      y ^ (y >>> 33)
    }
    private[this] def finalizer() {
      state0 ^= n
      state1 ^= n
      state0 += state1
      state1 += state0
      state0 = mixer(state0)
      state1 = mixer(state1)
      state0 += state1
      state1 += state0
    }
    def append(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if (partialN > 0) {
        while (bb.hasRemaining && partialN < 8) {
          partial0 |= ((bb.get & 0xFFL) << (partialN*8))
          partialN += 1
        }
        while (bb.hasRemaining && partialN < 16) {
          partial1 |= ((bb.get & 0xFFL) << ((partialN - 8) * 8))
          partialN += 1
        }
        if (partialN == 16) {
          appendLx2(partial0, partial1)
          partial0 = 0
          partial1 = 0
          partialN = 0
        }
      }
      while (bb.remaining >= 16) appendLx2(bb.getLong, bb.getLong)
      while (bb.hasRemaining && partialN < 8) {
        partial0 |= ((bb.get & 0xFFL) << (partialN*8))
        partialN += 1        
      }
      while (bb.hasRemaining && partialN < 16) {
        partial1 |= ((bb.get & 0xFFL) << ((partialN - 8) * 8))
        partialN += 1
      }
      this
    }
    def result(bb: ByteBuffer): Long = append(bb).result()
    def result(): Long = {
      if (partialN > 0) {
        appendLastLx2(partial0, partial1, partialN)
        partial0 = 0
        partial1 = 0
        partialN = 0
      }
      finalizer()
      state0
    }
    def resultInto(bb: ByteBuffer, target: Array[Long], index: Int): Array[Long] =
      append(bb).resultInto(target, index)
    def resultInto(target: Array[Long], index: Int): Array[Long] = {
      if (partialN > 0) {
        appendLastLx2(partial0, partial1, partialN)
        partial0 = 0
        partial1 = 0
        partialN = 0
      }
      finalizer()
      target(index) = state0
      target(index+1) = state1
      target
    }
    override def resultAs[A](implicit tag: scala.reflect.ClassTag[A]): Option[A] = {
      tag.unapply(0L) match {
        case Some(_) => tag.unapply(result())
        case None    => tag.unapply(Hash128.emptyArrayLong) match {
          case Some(_) => tag.unapply(results())
          case None    => None
        }
      }
    }
  }


  final class SimpleSum32 extends Hash32 {
    private[this] var sum = 0
    private[this] var partial = 0
    private[this] var partialN = 0
    def begin(seed: Int): this.type = { sum = seed; partial = 0; partialN = 0; this }
    def append(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if (partialN > 0) {
        while (partialN < 4 && bb.hasRemaining) {
          partial |= (bb.get & 0xFF) << (partialN*8)
          partialN += 1
        }
        if (partialN == 4) {
          sum += partial
          partialN = 0
          partial = 0
        }
      }
      while (bb.remaining >= 4) sum += bb.getInt
      while (bb.hasRemaining) {
        partial |= (bb.get & 0xFF) << (partialN*8)
        partialN += 1
      }
      this
    }
    def result(bb: ByteBuffer): Int = {
      append(bb)
      result()
    }
    def result(): Int = {
      if (partialN > 0) {
        sum += partial
        partial = 0
        partialN = 0        
      }
      sum
    }
  }

  final class SimpleSum64 extends Hash64 {
    private[this] var sum = 0L
    private[this] var partial = 0L
    private[this] var partialN = 0
    def begin(seed: Long): this.type = { sum = seed; partial = 0; partialN = 0; this }
    def append(bb: ByteBuffer): this.type = {
      bb.order(ByteOrder.LITTLE_ENDIAN)
      if (partialN > 0) {
        while (partialN < 8 && bb.hasRemaining) {
          partial |= (bb.get & 0xFFL) << (partialN*8)
          partialN += 1
        }
        if (partialN == 8) {
          sum += partial
          partialN = 0
          partial = 0
        }
      }
      while (bb.remaining >= 8) sum += bb.getLong
      while (bb.hasRemaining) {
        partial |= (bb.get & 0xFFL) << (partialN*8)
        partialN += 1
      }
      this
    }
    def result(bb: ByteBuffer): Long = {
      append(bb)
      result()
    }
    def result(): Long = {
      if (partialN > 0) {
        sum += partial
        partialN = 0
        partial = 0
      }
      sum
    }
  }
}
