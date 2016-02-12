// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr
// Contains code ported from xxHash C source (by Yann Collet, "Cyan5973")
//   See https://github.com/Cyan4973/xxHash

package kse.maths

import java.lang.Integer.{rotateLeft => rotl32, rotateRight => rotr32 }
import java.lang.Long.{rotateLeft => rotl64, rotateRight => rotr64 }
import java.nio.{ByteBuffer, ByteOrder}

package hashing {
  trait Hash32 { def hash32(bb: ByteBuffer, seed: Int): Int }
  trait Hash64 { def hash64(bb: ByteBuffer, seed: Long): Long }
  class XxHash32(initialSeed: Int) {
    import XX.{Prime32_1, Prime32_2, Prime32_3, Prime32_4, Prime32_5}
    private[this] var v1: Int = 0
    private[this] var v2: Int = 0
    private[this] var v3: Int = 0
    private[this] var v4: Int = 0
    private[this] var v5: Int = 0
    private[this] var hadBlock: Boolean = false
    begin(initialSeed)
    def this() = this(0)
    def begin(seed: Int): this.type = {
      v1 = seed + Prime32_1 + Prime32_2
      v2 = seed + Prime32_2
      v3 = seed
      v4 = seed - Prime32_1
      v5 = 0
      hadBlock = false
      this
    }
    def apply(bb: ByteBuffer): this.type = {
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
    def apply(one: Int, two: Int, three: Int, four: Int): this.type = {
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
    def result(bb: ByteBuffer): Int = {
      if (bb.remaining >= 16) apply(bb) else bb.order(ByteOrder.LITTLE_ENDIAN)
      counting(bb.remaining)
      while (bb.remaining >= 4) trailing(bb.getInt)
      while (bb.remaining >= 1) trailing(bb.get)
      complete()
    }
  }
  class XxHash64(initialSeed: Long) {
    import XX.{Prime64_1, Prime64_2, Prime64_3, Prime64_4, Prime64_5}
    private[this] var v1: Long = 0
    private[this] var v2: Long = 0
    private[this] var v3: Long = 0
    private[this] var v4: Long = 0
    private[this] var v5: Long = 0
    private[this] var hadBlock: Boolean = false
    begin(initialSeed)
    def this() = this(0)
    def begin(seed: Long): this.type = {
      v1 = seed + Prime64_1 + Prime64_2
      v2 = seed + Prime64_2
      v3 = seed
      v4 = seed - Prime64_1
      v5 = 0
      hadBlock = false
      this
    }
    def apply(bb: ByteBuffer): this.type = {
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
    def apply(one: Long, two: Long, three: Long, four: Long): this.type = {
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
    def result(bb: ByteBuffer): Long = {
      if (bb.remaining >= 32) apply(bb) else bb.order(ByteOrder.LITTLE_ENDIAN)
      counting(bb.remaining)
      while (bb.remaining >= 8) trailing(bb.getLong)
      if (bb.remaining >= 4) trailing(bb.getInt)
      while (bb.remaining >= 1) trailing(bb.get)
      complete()
    }
  }
  object XX extends Hash32 with Hash64 {
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
}
