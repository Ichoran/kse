package kse.tests

import kse.coll.packed._

object Test_Packed extends Test_Kse {
  def test_byte = {
    (false).inByte.B == 0 && (true).inByte.B == 1 &&
    (0 to 255).map(_.toByte).forall{ b =>
      val x = b.inByte
      x.Z == x.bit0 && x.B == b &&
      x.bit0 == x.bit(0) && x.bit1 == x.bit(1) && x.bit2 == x.bit(2) && x.bit3 == x.bit(3) && x.bit4 == x.bit(4) && x.bit5 == x.bit(5) && x.bit6 == x.bit(6) && x.bit7 == x.bit(7) &&
      x.bit0 == (x.bits(0,1) != 0) && x.bit1 == (x.bits(1,1) != 0) && x.bit2 == (x.bits(2,1) != 0) && x.bit3 == (x.bits(3,1) != 0) && x.bit4 == (x.bits(4,1) != 0) && x.bit5 == (x.bits(5,1) != 0) && x.bit6 == (x.bits(6,1) != 0) && x.bit7 == (x.bits(7,1) != 0) &&
      (x.bits(0,8) & 0xFF) == ((b & 0xFF) >> 0) && (x.bits(1,7) & 0xFF) == ((b & 0xFF) >> 1) && (x.bits(2,6) & 0xFF) == ((b & 0xFF) >> 2) && (x.bits(3,5) & 0xFF) == ((b & 0xFF) >> 3) && (x.bits(4,4) & 0xFF) == ((b & 0xFF) >> 4) && (x.bits(5,3) & 0xFF) == ((b & 0xFF) >> 5) && (x.bits(6,2) & 0xFF) == ((b & 0xFF) >> 6) && (x.bits(7,1) & 0xFF) == ((b & 0xFF) >> 7) &&      x.bit0Is(true).B == (b | 1) && x.bit1Is(true).B == (b | 2) && x.bit2Is(true).B == (b | 4) && x.bit3Is(true).B == (b | 8) && x.bit4Is(true).B == (b | 16) && x.bit5Is(true).B == (b | 32) && x.bit6Is(true).B == (b | 64) && (x.bit7Is(true).B & 0xFF) == ((b & 0xFF) | 128) &&
      (x.bit0Is(false).B & 0xFF) == (b & 254) && (x.bit1Is(false).B & 0xFF) == (b & 253) && (x.bit2Is(false).B & 0xFF) == (b & 251) && (x.bit3Is(false).B & 0xFF) == (b & 247) && (x.bit4Is(false).B & 0xFF) == (b & 239) && (x.bit5Is(false).B & 0xFF) == (b & 223) && (x.bit6Is(false).B & 0xFF) == (b & 191) && (x.bit7Is(false).B & 0xFF) == (b & 127) &&
      x.bit0Is(true).B == x.bitIs(0)(true).B && x.bit1Is(true).B == x.bitIs(1)(true).B && x.bit2Is(true).B == x.bitIs(2)(true).B && x.bit3Is(true).B == x.bitIs(3)(true).B && x.bit4Is(true).B == x.bitIs(4)(true).B && x.bit5Is(true).B == x.bitIs(5)(true).B && x.bit6Is(true).B == x.bitIs(6)(true).B && x.bit7Is(true).B == x.bitIs(7)(true).B &&
      x.bit0Is(false).B == x.bitIs(0)(false).B && x.bit1Is(false).B == x.bitIs(1)(false).B && x.bit2Is(false).B == x.bitIs(2)(false).B && x.bit3Is(false).B == x.bitIs(3)(false).B && x.bit4Is(false).B == x.bitIs(4)(false).B && x.bit5Is(false).B == x.bitIs(5)(false).B && x.bit6Is(false).B == x.bitIs(6)(false).B && x.bit7Is(false).B == x.bitIs(7)(false).B &&
      (0 to 15).forall{ y =>
        x.bit0Is((y&1) != 0).bit1Is((y&2) != 0).bit2Is((y&4) != 0).bit3Is((y&8) != 0).B == x.bitsAre(0, 4)(y.toByte).B &&
        x.bit2Is((y&1) != 0).bit3Is((y&2) != 0).bit4Is((y&4) != 0).bit5Is((y&8) != 0).B == x.bitsAre(2, 4)(y.toByte).B &&
        x.bit4Is((y&1) != 0).bit5Is((y&2) != 0).bit6Is((y&4) != 0).bit7Is((y&8) != 0).B == x.bitsAre(4, 4)(y.toByte).B
      }
    }
  }
  
  def test_short = {
    (false).inShort.S == 0 && (true).inShort.S == 1 &&
    (0 to 255).map(_.toByte).forall(x => x.inShort.S == (x&0xFF)) &&
    (0 to 65535).map(_.toChar).forall(x => x.inShort.C == x) &&
    (0 to 65535).map(_.toShort).forall{ s =>
      val x = s.inShort
      x.Z == x.bit0 && x.B == x.b0 && x.S == s && x.C == s.toChar &&
      (x.b0 packBB x.b1).S == s && (0: Short).inShort.b0(x.b0).b1(x.b1).S == s &&
      x.swapB.S == { val t = x.b1; x.b1(x.b0).b0(t).S } &&
      (x.b0 & 0xFF) == (s & 0xFF) && (x.b1 & 0xFF) == ((s >>> 8) & 0xFF) &&
      x.b1(0).S == (x.b0 & 0xFF) && (x.b1(-1).S & 0xFFFF) == (0xFF00 | (x.b0 & 0xFF)) &&
      x.bit0 == x.bit(0) && x.bit1 == x.bit(1) && x.bit2 == x.bit(2) && x.bit3 == x.bit(3) && x.bit4 == x.bit(4) && x.bit5 == x.bit(5) && x.bit6 == x.bit(6) && x.bit7 == x.bit(7) &&
      x.bit8 == x.bit(8) && x.bit9 == x.bit(9) && x.bit10 == x.bit(10) && x.bit11 == x.bit(11) && x.bit12 == x.bit(12) && x.bit13 == x.bit(13) && x.bit14 == x.bit(14) && x.bit15 == x.bit(15) &&
      (0 to 15).forall(i => x.bit(i) == (x.bits(i,1) != 0) && (x.bits(i, 16-i) & 0xFFFF) == ((s & 0xFFFF) >>> i) && x.bit(i) == ((s & (1 << i)) != 0)) &&
      List(false, true).forall{ c =>
        val line = Array.tabulate(16){ i => 1 << i }
        val notch = Array.tabulate(16){ i => 0xFFFF - (1 << i) }
        x.bit0Is(c).S == x.bitIs(0)(c).S && x.bit1Is(c).S == x.bitIs(1)(c).S && x.bit2Is(c).S == x.bitIs(2)(c).S && x.bit3Is(c).S == x.bitIs(3)(c).S &&
        x.bit4Is(c).S == x.bitIs(4)(c).S && x.bit5Is(c).S == x.bitIs(5)(c).S && x.bit6Is(c).S == x.bitIs(6)(c).S && x.bit7Is(c).S == x.bitIs(7)(c).S &&
        x.bit8Is(c).S == x.bitIs(8)(c).S && x.bit9Is(c).S == x.bitIs(9)(c).S && x.bit10Is(c).S == x.bitIs(10)(c).S && x.bit11Is(c).S == x.bitIs(11)(c).S &&
        x.bit12Is(c).S == x.bitIs(12)(c).S && x.bit13Is(c).S == x.bitIs(13)(c).S && x.bit14Is(c).S == x.bitIs(14)(c).S && x.bit15Is(c).S == x.bitIs(15)(c).S &&
        (0 to 15).forall{ i => 
          (x.bitIs(i)(c).S & 0xFFFF) == (if (c) ((s & 0xFFFF) | line(i)) else (s & notch(i))) &&
          x.bitIs(i)(c).S == x.bitsAre(i,1)(if (c) 1 else 0).S
        }
      } &&
      (0 to 15).forall{ y => (0 to 12 by 2).forall{ k =>
        x.bitIs(k)((y&1) != 0).bitIs(k+1)((y&2) != 0).bitIs(k+2)((y&4) != 0).bitIs(k+3)((y&8) != 0).S == x.bitsAre(k, 4)(y.toShort).S
      }}
    }
  }
  
  def test_int = {
    val rng = new scala.util.Random(8917561)
    val xs = List(
      Int.MinValue, Int.MinValue+1, Int.MaxValue, Int.MaxValue-1, 0, 0xFFFFFFFF,
      0xFFFF0000, 0x0000FFFF, 0xFF00FF00, 0x00FF00FF, 0xF0F0F0F0, 0x0F0F0F0F,
      0xFFFFFF00, 0xFFFF00FF, 0xFF00FFFF, 0x00FFFFFF, 0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF
    ) ++ List.fill(2048)(rng.nextInt)
    
    false.inInt.I == 0 && true.inInt.I == 1 &&
    (0 to 255).map(_.toByte).forall(x => x.inInt.I == (x&0xFF)) &&
    (0 to 65535).map(_.toChar).forall(x => x.inInt.I == x) &&
    (0 to 65535).map(_.toShort).forall{ x => x.inInt.I == (x&0xFFFF) } &&
    xs.forall{ i =>
      val x = i.inInt
      x.Z == x.bit0 && x.B == x.b0 && x.S == x.s0 && x.C == x.c0 && x.I == i &&
      (x.s0 & 0xFFFF) == (i & 0xFFFF) && (x.s1 & 0xFFFF) == (i >>> 16) &&
      x.b0 == x.s0.inShort.b0 && x.b1 == x.s0.inShort.b1 && x.b2 == x.s1.inShort.b0 && x.b3 == x.s1.inShort.b1 &&
      x.c0 == x.s0.toChar && x.c1 == x.s1.toChar &&
      x.F.inInt.I == i && x.f0.inInt.I == i &&
      (x.s0 packSS x.s1).I == i && 0.inInt.s0(x.s0).s1(x.s1).I == i &&
      (x.c0 packCC x.c1).I == i && 0.inInt.c0(x.c0).c1(x.c1).I == i &&
      x.b0.packBBBB(x.b1, x.b2, x.b3).I == i && 0.inInt.b0(x.b0).b1(x.b1).b2(x.b2).b3(x.b3).I == i &&
      x.swapS.I == x.swapC.I &&
      x.swapS.I == { val t = x.s1; x.s1(x.s0).s0(t).I } &&
      x.swapBB.I == x.s0(x.s0.inShort.swapB.S).s1(x.s1.inShort.swapB.S).I &&
      x.reverseB.I == { val t = x.s1; x.s1(x.s0.inShort.swapB.S).s0(t.inShort.swapB.S).I } &&
      x.rotlB.I == { val b0 = x.b0; val b1 = x.b1; val b2 = x.b2; x.b0(x.b3).b1(b0).b2(b1).b3(b2).I } &&
      x.rotrB.I == { val b3 = x.b3; val b2 = x.b2; val b1 = x.b1; x.b3(x.b0).b2(b3).b1(b2).b0(b1).I } &&
      x.bit0 == x.bit(0) && x.bit1 == x.bit(1) && x.bit2 == x.bit(2) && x.bit3 == x.bit(3) && x.bit4 == x.bit(4) && x.bit5 == x.bit(5) && x.bit6 == x.bit(6) && x.bit7 == x.bit(7) &&
      x.bit8 == x.bit(8) && x.bit9 == x.bit(9) && x.bit10 == x.bit(10) && x.bit11 == x.bit(11) && x.bit12 == x.bit(12) && x.bit13 == x.bit(13) && x.bit14 == x.bit(14) && x.bit15 == x.bit(15) &&
      x.bit16 == x.bit(16) && x.bit17 == x.bit(17) && x.bit18 == x.bit(18) && x.bit19 == x.bit(19) && x.bit20 == x.bit(20) && x.bit21 == x.bit(21) && x.bit22 == x.bit(22) && x.bit23 == x.bit(23) &&
      x.bit24 == x.bit(24) && x.bit25 == x.bit(25) && x.bit26 == x.bit(26) && x.bit27 == x.bit(27) && x.bit28 == x.bit(28) && x.bit29 == x.bit(29) && x.bit30 == x.bit(30) && x.bit31 == x.bit(31) &&
      (0 to 31).forall(j => x.bit(j) == (x.bits(j,1) != 0) && x.bits(j, 32-j) == (i >>> j) && x.bit(j) == ((i & (1 << j)) != 0)) &&
      List(false, true).forall{ c =>
        val line = Array.tabulate(32){ i => 1 << i }
        val notch = Array.tabulate(32){ i => 0xFFFFFFFF - (1 << i) }
        x.bit0Is(c).I == x.bitIs(0)(c).I && x.bit1Is(c).I == x.bitIs(1)(c).I && x.bit2Is(c).I == x.bitIs(2)(c).I && x.bit3Is(c).I == x.bitIs(3)(c).I &&
        x.bit4Is(c).I == x.bitIs(4)(c).I && x.bit5Is(c).I == x.bitIs(5)(c).I && x.bit6Is(c).I == x.bitIs(6)(c).I && x.bit7Is(c).I == x.bitIs(7)(c).I &&
        x.bit8Is(c).I == x.bitIs(8)(c).I && x.bit9Is(c).I == x.bitIs(9)(c).I && x.bit10Is(c).I == x.bitIs(10)(c).I && x.bit11Is(c).I == x.bitIs(11)(c).I &&
        x.bit12Is(c).I == x.bitIs(12)(c).I && x.bit13Is(c).I == x.bitIs(13)(c).I && x.bit14Is(c).I == x.bitIs(14)(c).I && x.bit15Is(c).I == x.bitIs(15)(c).I &&
        x.bit16Is(c).I == x.bitIs(16)(c).I && x.bit17Is(c).I == x.bitIs(17)(c).I && x.bit18Is(c).I == x.bitIs(18)(c).I && x.bit19Is(c).I == x.bitIs(19)(c).I &&
        x.bit20Is(c).I == x.bitIs(20)(c).I && x.bit21Is(c).I == x.bitIs(21)(c).I && x.bit22Is(c).I == x.bitIs(22)(c).I && x.bit23Is(c).I == x.bitIs(23)(c).I &&
        x.bit24Is(c).I == x.bitIs(24)(c).I && x.bit25Is(c).I == x.bitIs(25)(c).I && x.bit26Is(c).I == x.bitIs(26)(c).I && x.bit27Is(c).I == x.bitIs(27)(c).I &&
        x.bit28Is(c).I == x.bitIs(28)(c).I && x.bit29Is(c).I == x.bitIs(29)(c).I && x.bit30Is(c).I == x.bitIs(30)(c).I && x.bit31Is(c).I == x.bitIs(31)(c).I &&
        (0 to 31).forall{ j =>
          x.bitIs(j)(c).I == (if (c) i | line(j) else i & notch(j)) &&
          x.bitIs(j)(c).I == x.bitsAre(j,1)(if (c) 1 else 0).I
        }
      } &&
      (0 to 15).forall{ y => (0 to 28 by 2).forall{ k =>
        x.bitIs(k)((y&1) != 0).bitIs(k+1)((y&2) != 0).bitIs(k+2)((y&4) != 0).bitIs(k+3)((y&8) != 0).I == x.bitsAre(k, 4)(y).I
      }}
    } &&
    List.fill(2048)(rng.nextFloat).forall{ f => if (f.isNaN) { f.inInt.F.isNaN && 0.inInt.f0(f).f0.isNaN } else { f.inInt.F == f && 0.inInt.f0(f).f0 == f } }
  }
  
  def test_long = {
    val rng = new scala.util.Random(438975628901287L)
    val xs = List(
      Long.MinValue, Long.MinValue+1, Int.MinValue-1, Int.MinValue, Int.MinValue+1, Int.MaxValue-1, Int.MaxValue, Int.MaxValue+1, Long.MaxValue-1, Long.MaxValue+1, 0, 0xFFFFFFFFFFFFFFFFL,
      0xFFFFFFFF00000000L, 0x00000000FFFFFFFFL, 0xFFFF0000FFFF0000L, 0x0000FFFF0000FFFFL, 0xFF00FF00FF00FF00L, 0x00FF00FF00FF00FFL, 0xF0F0F0F0F0F0F0F0L, 0x0F0F0F0F0F0F0F0FL,
      0xFF00000000000000L, 0x00FF000000000000L, 0x0000FF0000000000L, 0x000000FF00000000L, 0x00000000FF000000L, 0x0000000000FF0000L, 0x000000000000FF00L, 0x00000000000000FFL,
      0x00FFFFFFFFFFFFFFL, 0xFF00FFFFFFFFFFFFL, 0xFFFF00FFFFFFFFFFL, 0xFFFFFF00FFFFFFFFL, 0xFFFFFFFF00FFFFFFL, 0xFFFFFFFFFF00FFFFL, 0xFFFFFFFFFFFF00FFL, 0xFFFFFFFFFFFFFF00L
    ) ++ List.fill(4096)(rng.nextLong)
    val ixs = List(
      Int.MinValue, Int.MinValue+1, Int.MaxValue, Int.MaxValue-1, 0, 0xFFFFFFFF,
      0xFFFF0000, 0x0000FFFF, 0xFF00FF00, 0x00FF00FF, 0xF0F0F0F0, 0x0F0F0F0F,
      0xFFFFFF00, 0xFFFF00FF, 0xFF00FFFF, 0x00FFFFFF, 0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF
    ) ++ List.fill(2048)(rng.nextInt)
    
    false.inLong.L == 0 && true.inLong.L == 1 &&
    (0 to 255).map(_.toByte).forall(x => x.inLong.L == (x&0xFF)) &&
    (0 to 65535).map(_.toChar).forall(x => x.inLong.L == x) &&
    (0 to 65535).map(_.toShort).forall{ x => x.inLong.L == (x&0xFFFF) } &&
    ixs.forall{ x => x.inLong.L == (x & 0xFFFFFFFFL) } &&
    xs.forall{ l =>
      val x = l.inLong
      x.Z == x.bit0 && x.B == x.b0 && x.S == x.s0 && x.C == x.c0 && x.I == x.i0 && x.L == l &&
      (x.i0 & 0xFFFFFFFFL) == (l & 0xFFFFFFFFL) && (x.i1 & 0xFFFFFFFFL) == (l >>> 32) &&
      x.s0 == x.i0.inInt.s0 && x.s1 == x.i0.inInt.s1 && x.s2 == x.i1.inInt.s0 && x.s3 == x.i1.inInt.s1 &&
      x.c0 == x.i0.inInt.c0 && x.c1 == x.i0.inInt.c1 && x.c2 == x.i1.inInt.c0 && x.c3 == x.i1.inInt.c1 &&
      x.b0 == x.i0.inInt.b0 && x.b1 == x.i0.inInt.b1 && x.b2 == x.i0.inInt.b2 && x.b3 == x.i0.inInt.b3 && x.b4 == x.i1.inInt.b0 && x.b5 == x.i1.inInt.b1 && x.b6 == x.i1.inInt.b2 && x.b7 == x.i1.inInt.b3 &&
      x.F.inInt.I == x.i0 && x.f0.inInt.I == x.i0 && x.f1.inInt.I == x.i1 && x.D.inLong.L == l && x.d0.inLong.L == l &&
      (x.i0 packII x.i1).L == l && 0L.inLong.i0(x.i0).i1(x.i1).L == l &&
      (x.i0.inInt.F packFF x.i1.inInt.F).L == l && 0L.inLong.f0(x.i0.inInt.F).f1(x.i1.inInt.F).L == l &&
      x.c0.packCCCC(x.c1, x.c2, x.c3).L == l && 0L.inLong.c0(x.c0).c1(x.c1).c2(x.c2).c3(x.c3).L == l &&
      x.s0.packSSSS(x.s1, x.s2, x.s3).L == l && 0L.inLong.s0(x.s0).s1(x.s1).s2(x.s2).s3(x.s3).L == l &&
      x.b0.packB8(x.b1, x.b2, x.b3, x.b4, x.b5, x.b6, x.b7).L == l && 0L.inLong.b0(x.b0).b1(x.b1).b2(x.b2).b3(x.b3).b4(x.b4).b5(x.b5).b6(x.b6).b7(x.b7).L == l &&
      x.swapI.L == x.swapF.L && x.swapSS.L == x.swapCC.L && x.rotlS.L == x.rotlC.L && x.rotrS.L == x.rotrC.L && x.reverseS == x.reverseC &&
      x.swapI.L == { val t = x.i0; x.i0(x.i1).i1(t).L } &&
      x.swapSS.L == x.i0(x.i0.inInt.swapS.I).i1(x.i1.inInt.swapS.I).L &&
      x.reverseS.L == { val t = x.i0; x.i0(x.i1.inInt.swapS.I).i1(t.inInt.swapS.I).L } &&
      x.rotlS.L == { val s0 = x.s0; val s1 = x.s1; val s2 = x.s2; x.s0(x.s3).s1(s0).s2(s1).s3(s2).L } &&
      x.rotrS.L == { val s3 = x.s3; val s2 = x.s2; val s1 = x.s1; x.s3(x.s0).s2(s3).s1(s2).s0(s1).L } &&
      x.swapBBBB.L == x.i0(x.i0.inInt.swapBB.I).i1(x.i1.inInt.swapBB.I).L &&
      x.reverseB.L == { val t = x.i0; x.i0(x.i1.inInt.reverseB.I).i1(t.inInt.reverseB.I).L } &&
      x.rotlB.L == { val b0 = x.b0; val b1 = x.b1; val b2 = x.b2; val b3 = x.b3; val b4 = x.b4; val b5 = x.b5; val b6 = x.b6; x.b0(x.b7).b1(b0).b2(b1).b3(b2).b4(b3).b5(b4).b6(b5).b7(b6).L } &&
      x.rotrB.L == { val b7 = x.b7; val b6 = x.b6; val b5 = x.b5; val b4 = x.b4; val b3 = x.b3; val b2 = x.b2; val b1 = x.b1; x.b7(x.b0).b6(b7).b5(b6).b4(b5).b3(b4).b2(b3).b1(b2).b0(b1).L } &&
      x.bit0 == x.bit(0) && x.bit1 == x.bit(1) && x.bit2 == x.bit(2) && x.bit3 == x.bit(3) && x.bit4 == x.bit(4) && x.bit5 == x.bit(5) && x.bit6 == x.bit(6) && x.bit7 == x.bit(7) &&
      x.bit8 == x.bit(8) && x.bit9 == x.bit(9) && x.bit10 == x.bit(10) && x.bit11 == x.bit(11) && x.bit12 == x.bit(12) && x.bit13 == x.bit(13) && x.bit14 == x.bit(14) && x.bit15 == x.bit(15) &&
      x.bit16 == x.bit(16) && x.bit17 == x.bit(17) && x.bit18 == x.bit(18) && x.bit19 == x.bit(19) && x.bit20 == x.bit(20) && x.bit21 == x.bit(21) && x.bit22 == x.bit(22) && x.bit23 == x.bit(23) &&
      x.bit24 == x.bit(24) && x.bit25 == x.bit(25) && x.bit26 == x.bit(26) && x.bit27 == x.bit(27) && x.bit28 == x.bit(28) && x.bit29 == x.bit(29) && x.bit30 == x.bit(30) && x.bit31 == x.bit(31) &&
      x.bit32 == x.bit(32) && x.bit33 == x.bit(33) && x.bit34 == x.bit(34) && x.bit35 == x.bit(35) && x.bit36 == x.bit(36) && x.bit37 == x.bit(37) && x.bit38 == x.bit(38) && x.bit39 == x.bit(39) &&
      x.bit40 == x.bit(40) && x.bit41 == x.bit(41) && x.bit42 == x.bit(42) && x.bit43 == x.bit(43) && x.bit44 == x.bit(44) && x.bit45 == x.bit(45) && x.bit46 == x.bit(46) && x.bit47 == x.bit(47) &&
      x.bit48 == x.bit(48) && x.bit49 == x.bit(49) && x.bit50 == x.bit(50) && x.bit51 == x.bit(51) && x.bit52 == x.bit(52) && x.bit53 == x.bit(53) && x.bit54 == x.bit(54) && x.bit55 == x.bit(55) &&
      x.bit56 == x.bit(56) && x.bit57 == x.bit(57) && x.bit58 == x.bit(58) && x.bit59 == x.bit(59) && x.bit60 == x.bit(60) && x.bit61 == x.bit(61) && x.bit62 == x.bit(62) && x.bit63 == x.bit(63) &&
      (0 to 63).forall(j => x.bit(j) == (x.bits(j,1) != 0) && x.bits(j, 64-j) == (l >>> j) && x.bit(j) == ((l & (1L << j)) != 0)) &&
      List(false, true).forall{ c =>
        val line = Array.tabulate(64){ i => 1L << i }
        val notch = Array.tabulate(64){ i => 0xFFFFFFFFFFFFFFFFL - (1L << i) }
        x.bit0Is(c).L == x.bitIs(0)(c).L && x.bit1Is(c).L == x.bitIs(1)(c).L && x.bit2Is(c).L == x.bitIs(2)(c).L && x.bit3Is(c).L == x.bitIs(3)(c).L &&
        x.bit4Is(c).L == x.bitIs(4)(c).L && x.bit5Is(c).L == x.bitIs(5)(c).L && x.bit6Is(c).L == x.bitIs(6)(c).L && x.bit7Is(c).L == x.bitIs(7)(c).L &&
        x.bit8Is(c).L == x.bitIs(8)(c).L && x.bit9Is(c).L == x.bitIs(9)(c).L && x.bit10Is(c).L == x.bitIs(10)(c).L && x.bit11Is(c).L == x.bitIs(11)(c).L &&
        x.bit12Is(c).L == x.bitIs(12)(c).L && x.bit13Is(c).L == x.bitIs(13)(c).L && x.bit14Is(c).L == x.bitIs(14)(c).L && x.bit15Is(c).L == x.bitIs(15)(c).L &&
        x.bit16Is(c).L == x.bitIs(16)(c).L && x.bit17Is(c).L == x.bitIs(17)(c).L && x.bit18Is(c).L == x.bitIs(18)(c).L && x.bit19Is(c).L == x.bitIs(19)(c).L &&
        x.bit20Is(c).L == x.bitIs(20)(c).L && x.bit21Is(c).L == x.bitIs(21)(c).L && x.bit22Is(c).L == x.bitIs(22)(c).L && x.bit23Is(c).L == x.bitIs(23)(c).L &&
        x.bit24Is(c).L == x.bitIs(24)(c).L && x.bit25Is(c).L == x.bitIs(25)(c).L && x.bit26Is(c).L == x.bitIs(26)(c).L && x.bit27Is(c).L == x.bitIs(27)(c).L &&
        x.bit28Is(c).L == x.bitIs(28)(c).L && x.bit29Is(c).L == x.bitIs(29)(c).L && x.bit30Is(c).L == x.bitIs(30)(c).L && x.bit31Is(c).L == x.bitIs(31)(c).L &&
        x.bit32Is(c).L == x.bitIs(32)(c).L && x.bit33Is(c).L == x.bitIs(33)(c).L && x.bit34Is(c).L == x.bitIs(34)(c).L && x.bit35Is(c).L == x.bitIs(35)(c).L &&
        x.bit36Is(c).L == x.bitIs(36)(c).L && x.bit37Is(c).L == x.bitIs(37)(c).L && x.bit38Is(c).L == x.bitIs(38)(c).L && x.bit39Is(c).L == x.bitIs(39)(c).L &&
        x.bit40Is(c).L == x.bitIs(40)(c).L && x.bit41Is(c).L == x.bitIs(41)(c).L && x.bit42Is(c).L == x.bitIs(42)(c).L && x.bit43Is(c).L == x.bitIs(43)(c).L &&
        x.bit44Is(c).L == x.bitIs(44)(c).L && x.bit45Is(c).L == x.bitIs(45)(c).L && x.bit46Is(c).L == x.bitIs(46)(c).L && x.bit47Is(c).L == x.bitIs(47)(c).L &&
        x.bit48Is(c).L == x.bitIs(48)(c).L && x.bit49Is(c).L == x.bitIs(49)(c).L && x.bit50Is(c).L == x.bitIs(50)(c).L && x.bit51Is(c).L == x.bitIs(51)(c).L &&
        x.bit52Is(c).L == x.bitIs(52)(c).L && x.bit53Is(c).L == x.bitIs(53)(c).L && x.bit54Is(c).L == x.bitIs(54)(c).L && x.bit55Is(c).L == x.bitIs(55)(c).L &&
        x.bit56Is(c).L == x.bitIs(56)(c).L && x.bit57Is(c).L == x.bitIs(57)(c).L && x.bit58Is(c).L == x.bitIs(58)(c).L && x.bit59Is(c).L == x.bitIs(59)(c).L &&
        x.bit60Is(c).L == x.bitIs(60)(c).L && x.bit61Is(c).L == x.bitIs(61)(c).L && x.bit62Is(c).L == x.bitIs(62)(c).L && x.bit63Is(c).L == x.bitIs(63)(c).L &&
        (0 to 63).forall{ j =>
          x.bitIs(j)(c).L == (if (c) l | line(j) else l & notch(j)) &&
          x.bitIs(j)(c).L == x.bitsAre(j,1)(if (c) 1 else 0).L
        }
      } &&
      (0 to 15).forall{ y => (0 to 60 by 2).forall{ k =>
        x.bitIs(k)((y&1) != 0).bitIs(k+1)((y&2) != 0).bitIs(k+2)((y&4) != 0).bitIs(k+3)((y&8) != 0).L == x.bitsAre(k, 4)(y).L
      }}
    } &&
    List.fill(4096)(rng.nextFloat).forall{ f => 
      f.inLong.i1 == 0 && 0L.inLong.f0(f).i1 == 0 && 0L.inLong.f1(f).i0 == 0 && 
      (if (f.isNaN) { f.inLong.F.isNaN && 0L.inLong.f0(f).f0.isNaN && 0L.inLong.f1(f).f1.isNaN } else { f.inLong.F == f && 0L.inLong.f0(f).f0 == f && 0L.inLong.f1(f).f1 == f })
    } &&
    List.fill(4096)(rng.nextDouble).forall{ d =>
      if (d.isNaN) { d.inLong.D.isNaN && 0L.inLong.d0(d).d0.isNaN } else { d.inLong.D == d && 0L.inLong.d0(d).d0 == d }
    }
  }

  def main(args: Array[String]) { typicalMain(args) }
}
