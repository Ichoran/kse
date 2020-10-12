// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2020 Rex Kerr and Calico Labs.


package kse.eio
package cursor

import java.io._

import scala.collection.mutable.{ArrayBuffer, TreeMap}

import kse.maths._

final class ByteCursor private[eio] (source: Either[Array[Byte], RandomAccessFile]) {
  import ByteCursor._
  private[this] var allocated: Long = 0L
  private[this] var active: Chunk = source match { case Left(a) => Chunk(a, 0L); case _ => null }
  private[this] val access: RandomAccessFile = source match { case Right(raf) => raf; case _ => null }
  private[this] val cache: TreeMap[Long, Chunk] = {
    val t = TreeMap.empty[Long, Chunk]
    if (active ne null) {
      t(active.offset) = active
      allocated += active.buffer.length
    }
    t
  }

  private[this] var index: Long = 0L

  val length = if (access eq null) active.buffer.length.toLong else access.length

  private[this] def load(l: Long, size: Int, direction: Int): Chunk = {
    val a = new Array[Byte](size)
    access.seek(l)
    val n = access.read(a)
    if (n != a.length) throw new IOException(s"Could only read $n of ${a.length} bytes")
    Chunk(a, l, direction)
  }

  private[this] def uncheckedLoadFromFile(l: Long, left: Option[Chunk], right: Option[Chunk]) {
    // Load a cache that contains position l between `left` and `right`, without error checking
    val i0 = left match  { case Some(before) => before.end;   case _ => 0L }
    val iN = right match { case Some(after)  => after.offset; case _ => length }
    val gap = iN - i0
    val sizeL = left match { 
      case Some(before) if before.direction == 1 => 2 * before.buffer.length min MaxChunkSize
      case _ => ChunkSize
    }
    val sizeR = right match {
      case Some(after) if after.direction == -1 => 2 * after.buffer.length min MaxChunkSize
      case _ => ChunkSize
    }
    val dL = l - i0
    val dR = iN - l
    if (dL > sizeL && dR > sizeR && dL - Lookahead >= i0 + ChunkSize && dR + Lookahead <= iN + ChunkSize) {
      // Can't reach anything, unattached
      val i = ((l - Lookahead) & BoundaryMask) max i0
      active = load(i, size = ChunkSize, direction = 0)
    }
    else if (sizeL + sizeR >= gap && gap <= MaxChunkSize) {
      // Can reach everything, attached on both sides
      active = load(i0, size = gap.toInt, direction = 0)
    }
    else {
      // Either attached to the right or the left--need to figure out which
      val lFrac = dL / sizeL.toDouble
      val rFrac = dR / sizeR.toDouble
      if (dL < Lookahead + ChunkSize || lFrac <= rFrac) {
        // Attach on left
        var n = sizeL
        if (l + Lookahead > i0 + n) n = (l + Lookahead - i0).toInt
        if (gap - n < ChunkSize) n = (gap - ChunkSize).toInt
        val small = n & BoundaryMask
        if (small != 0) n = ((n + ~BoundaryMask) & BoundaryMask).toInt
        if (i0 + n > iN) n = (iN - i0).toInt
        active = load(i0, size = n, direction = 1)
      }
      else {
        // Attach on right
        var n = sizeR
        if (l - Lookahead < iN - n) n = (iN - (l - Lookahead)).toInt
        if (gap - n < ChunkSize) n = (gap - ChunkSize).toInt
        val i = ((iN - n) & BoundaryMask) max i0
        active = load(i, size = (iN - i).toInt, direction = -1)
      }
    }
  }

  private[this] def uncheckedChunkActive(l: Long, left: Option[Chunk], right: Option[Chunk]) {
    // Loads the active chunk into the cache, clearing out space as needed, but either adjacent cache if they exist
    allocated += active.buffer.length
    var hopeful = cache.nonEmpty
    while (hopeful && allocated > MaxChunk) {
      val first = cache.headOption.map(_._2)
      val last = cache.lastOption.map(_._2)
      (first, last) match {
        case (Some(a), Some(b)) =>
          val removable =
            if (b.end < l) a
            else if (a.offset > l) b
            else if (l - a.end > b.offset - l) a
            else b
          if ((left.exists(removable eq _)) || (right.exists(removable eq _))) hopeful = false
          else {
            allocated -= removable.buffer.length
            cache -= allocated
          }
        case _ => hopeful = false
      }
    }
    cache(active.offset) = active
  }

  private[this] def activate(l: Long): Chunk = {
    val m = l.clip(0, length)
    if (((active eq null) || !(active contains m)) && (access ne null)) {
      val left = cache.to(m).lastOption.map(_._2)
      left match {
        case Some(before) if before contains m =>
          // Already have this cached; just make it active
          active = before
        case _ =>
          // Need to read new data from file and cache it
        val right = cache.from(m).headOption.map(_._2)
        uncheckedLoadFromFile(m, left, right)
        uncheckedChunkActive(m, left, right)
      }
    }
    active
  }

  def toStart() { index = 0 }
  def toEnd() { index = length }
  def to(l: Long) { index = l.clip(-1, length) }
  def move(delta: Long) { 
    val l = index + delta
    index =
      if (l > length) length
      else if (l < 0) {
        if (l < 0) 0L else length
      }
      else l
  }
  def position: Long = index

  def hasNextChunk = index < length
  def hasPrevChunk = index > 0
  def nextChunk = { 
    activate(index)
    index = active.end
    active
  }
  def prevChunk = {
    activate(index)
    if (index == active.offset) activate(index - 1)
    index = active.offset
    active
  }

  def hasNextByte: Boolean = nextByteCount > 0
  def hasPrevByte: Boolean = prevByteCount > 0
  def nextByte: Byte = {
    activate(index)
    val ans = active(index)
    index += 1
    ans
  }
  def prevByte: Byte = {
    index -= 1
    activate(index)
    active(index)
  }
  def nextByteCount: Long = length - index
  def prevByteCount: Long = index
  def nextBytesInto(a: Array[Byte], n: Int, offset: Int = 0): Int = {
    val space = a.length - offset
    val remaining = nextByteCount
    val m = ((n min space).toLong min remaining).toInt
    if (m <= 0) return 0
    var i = offset
    var need = m
    while (need > 0) {
      activate(index)
      val j = (index - active.offset).toInt
      val available = active.buffer.length - j
      val count = available min need
      System.arraycopy(active.buffer, j, a, i, count)
      i += count
      index += count
      need -= count
    }
    m
  }
  def prevBytesInto(a: Array[Byte], n: Int, offset: Int = 0): Int = {
    val space = a.length - offset
    val remaining = prevByteCount
    val m = ((n min space).toLong min remaining).toInt
    if (m <= 0) return 0
    var i = offset + m
    var need = m
    while (need > 0) {
      activate(index - 1)
      val j = (index - active.offset).toInt
      val available = j
      val count = available min need
      System.arraycopy(active.buffer, j - count, a, i - count, count)
      i -= count
      index -= count
      need -= count
    }
    m
  }
  def nextBytes(n: Int): Array[Byte] = {
    val m = (n.toLong min nextByteCount).toInt
    val a = new Array[Byte](m)
    nextBytesInto(a, m)
    a
  }
  def prevBytes(n: Int): Array[Byte] = {
    val m = (n.toLong min prevByteCount).toInt
    val a = new Array[Byte](m)
    prevBytesInto(a, m)
    a
  }

  // Move `index` to the start of a string.
  private def toStringStart(): Unit = {
    if (index <= 0) index = 0
    else if (index >= length) index = length
    else {
      var i = -1
      while (index > 0 && i < 0) {
        activate(index - 1)
        i = active.prevNL((index - active.offset).toInt - 1)
        index = active.offset + i
      }
      index += 1
    }
  }

  private def stringFromBuffer(i0: Int, iN: Int, buffer: Array[Byte]): String = {
    val i = i0 max 0
    var j = iN min buffer.length
    if (j > i && buffer(j-1) == '\n') {
      j -= 1
      if (j > i && buffer(j-1) == '\r') j -= 1
    }
    if (i >= j) ""
    else new String(buffer, i, j - i)
  }

  private def bufferToStart(buffers: TreeMap[Long, Chunk]): TreeMap[Long, Chunk] = 
    if (buffers.isEmpty || buffers.head._2.offset <= 0) {
      index = 0
      buffers
    }
    else {
      activate(buffers.head._2.offset - 1)
      buffers(active.offset) = active
      val i = active.prevNL(active.buffer.length - 1)
      if (i >= 0) {
        index = active.offset + i + 1
        buffers
      }
      else {
        bufferToStart(buffers)
      }
    }

  private def bufferToEnd(buffers: TreeMap[Long, Chunk]): TreeMap[Long, Chunk] =
    if (buffers.isEmpty || buffers.last._2.end >= length) {
      index = length
      buffers
    }
    else {
      activate(buffers.last._2.end)
      buffers(active.offset) = active
      val i = active.nextNL(0)
      if (i < active.buffer.length) {
        index = active.offset + i + 1
        buffers
      }
      else bufferToEnd(buffers)
    }

  private def stringFromBuffers(start: Long, end: Long, buffers: TreeMap[Long, Chunk]): String = {
    val flat = new Array[Byte](((end - start) min BiggestArray).toInt)
    val e = start + flat.length
    var s = start
    var k = 0
    val ib = buffers.iterator
    while (ib.hasNext) {
      val (_, c) = ib.next
      val l = s max c.offset
      val r = e min c.end
      if (r > l) {
        val i = (l - c.offset).toInt
        val n = (r - l).toInt
        System.arraycopy(c.buffer, i, flat, k, n)
        k += n
      }
      s = c.end
    }
    if (k < flat.length) {
      val listing = buffers.iterator.map{ case (_, b) => s"  ${b.offset}...${b.end}" }.mkString("\n")
      throw new Exception(s"Could not span from $start to $end using buffers:\n$listing")
    }
    buffers.clear()
    stringFromBuffer(0, flat.length, flat)
  }

  private def expandToString(movingRight: Boolean): String =
    if (index >= length) null
    else {
      activate(index)
      val i = (index - active.offset).toInt
      val iL = active.prevNL(i - 1)
      val iR = active.nextNL(i)
      val missingLeft = iL < 0 && active.offset > 0
      val missingRight = iR >= active.buffer.length && active.end < length
      if (missingLeft || missingRight) {
        val buffers = TreeMap.empty[Long, Chunk]
        buffers(active.offset) = active
        val a = active  // Store in case we change who is active
        val (start, end) = {
          if (movingRight) (
            if (missingLeft)  { bufferToStart(buffers); index } else a.offset + iL + 1,
            if (missingRight) { bufferToEnd(buffers);   index } else a.offset + iR + 1
          )
          else (
            if (missingRight) { bufferToEnd(buffers);   index } else a.offset + iR + 1,           
            if (missingLeft)  { bufferToStart(buffers); index } else a.offset + iL + 1
          ).swap
        }
        index = if (movingRight) end else start   // Often this will already be true, but we want to be sure in every case
        stringFromBuffers(start.clip(0, length), end.clip(0, length), buffers)
      }
      else {
        index = active.offset + (if (movingRight) (iR + 1) min active.buffer.length else iL + 1)
        stringFromBuffer(iL + 1, iR + 1, active.buffer)
      }
    }


  /** Checks whether there is another string ahead of the current position in the buffer.
    * 
    * This is always the case when the current position is not at the end.  Note that if
    * you are anywhere inside a string, it is considered to be the "next" string, as long
    * as you're not past its terminating \n.
    */
  def hasNext: Boolean = index < length

  /** Returns the next string if there is one, or None if there is not. */
  def nextOption(): Option[String] = Option(nextOrNull)

  /** Returns the next string if there is one, or throws an exception if there is not. */
  def next(): String = nextOrNull match {
    case null => throw new NoSuchElementException("No next String")
    case x    => x
  }

  /** Returns the next string if there is one, or null if there is not. */
  def nextOrNull(): String =
    if (!hasNext) null
    else expandToString(movingRight = true)

  /** Prepare to read the previous string, returning `true` if it exists.
    *
    * Like `hasNext`, but changes the index to point to the beginning of the "next"
    */
  def preparePrev(): Boolean = {
    toStringStart()
    index > 0
  }
  def prevOption() = Option(prevOrNull)

  
  def prevOrNull = {
    toStringStart()
    if (index <= 0) null
    else {
      index -= 1
      expandToString(movingRight = false)
    }
  }
}
object ByteCursor {
  final val Lookahead = 2048
  final val ChunkSize = 8192
  final val MaxChunkSize = 1024*8192
  final val MaxChunk = 8192*8192
  final val BoundaryMask = ~(1024L - 1)
  final val BiggestArray = ((1L << 31) - 16).toInt    // Biggest is actually probably - 2, but this is okay

  final case class Chunk(buffer: Array[Byte], offset: Long, direction: Int = 0) {
    def end: Long = offset + buffer.length
    def contains(l: Long) = l >= offset && (l - offset) < buffer.length
    def apply(index: Long) = buffer((index - offset).toInt)
    def head = buffer(0)
    def last = buffer(buffer.length - 1)
    def nextNL(from: Int): Int =
      if (from >= buffer.length) buffer.length
      else if (buffer(from) == '\n') from
      else nextNL(from + 1)
    def prevNL(from: Int): Int =
      if (from < 0) -1
      else if (buffer(from) == '\n') from
      else prevNL(from - 1)
  }

  def test(path: String) = new ByteCursor(Right(new RandomAccessFile(path, "r")))
}
