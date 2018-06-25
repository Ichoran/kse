// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2018 Rex Kerr and Calico Labs.

package kse.alien

import java.lang.{Process, ProcessBuilder}
import java.nio.{ByteBuffer, ByteOrder}
import java.time._
import java.util.concurrent._

import scala.annotation.tailrec
import scala.util._
import scala.util.control.NonFatal
import scala.util.control.Breaks._

import kse.typecheck._
import kse.flow._
import kse.coll._
import kse.maths._

sealed trait Exec {
  def running: Boolean
  def elapsed: Duration
  def exitCode: Option[Int]
  def maxLifespan(total: Duration): Unit
  def maxLifeMore(additional: Duration): Unit
  def printRaw(message: Array[Byte]): Boolean
  def printRaw(message: ByteBuffer): Boolean
  def println(message: String): Boolean
  def printlns(messages: Array[String]): Boolean
  def viewOutput: Array[String]
  def viewErrors: Array[String]
  def viewOutputRaw: Array[Byte]
  def viewErrorsRaw: Array[Byte]
  def retrieveOutput(): Array[String]
  def retrieveErrors(): Array[String]
  def retrieveOutputRaw(): Array[Byte]
  def retrieveErrorsRaw(): Array[Byte]
  def threaded: ExecThread
  def waitOrKill(duration: Duration): Option[Int]
}

final class ExecThread(args: Array[String], mergeErrors: Boolean = false, pollingMs: Int = 100) extends Thread with Exec {
  private[this] val pollEvery = pollingMs.clip(2, 1000)
  private[this] var inbuf: Array[Array[Byte]] = new Array[Array[Byte]](0)
  private[this] var inbuf_i: Int = 0
  private[this] var errbuf: Array[Array[Byte]] = new Array[Array[Byte]](0)
  private[this] var errbuf_i: Int = 0
  private[this] var errbuf_nl: Boolean = false
  private[this] var outbuf: Array[Array[Byte]] = new Array[Array[Byte]](0)
  private[this] var outbuf_i: Int = 0
  private[this] var outbuf_nl: Boolean = false
  private[this] var buffer: Array[Byte] = new Array[Byte](8192)

  private[this] val myFinishFlag = new atomic.AtomicBoolean(false)
  private[this] val myStartTime = LocalDateTime.now
  private[this] val myStopTime: atomic.AtomicReference[Option[LocalDateTime]] = new atomic.AtomicReference(None)
  private[this] val myDieTime: atomic.AtomicReference[Option[LocalDateTime]] = new atomic.AtomicReference(None)
  private[this] val deathAttempts = new atomic.AtomicInteger(0)
  private[this] val myExitCode: atomic.AtomicReference[Option[Int]] = new atomic.AtomicReference(None)

  val process: Process = (new ProcessBuilder(args: _*)).start()

  def running = !myFinishFlag.get()
  def elapsed = Duration.between(myStartTime, myStopTime.get().getOrElse(LocalDateTime.now))
  def exitCode = myExitCode.get()

  def maxLifespan(total: Duration) {
    if (running) {
      myDieTime.set(Some(myStartTime plus total))
    }
  }

  def maxLifeMore(additional: Duration) {
    if (running) {
      myDieTime.set(Some(LocalDateTime.now plus additional))
    }
  }

  def printRaw(message: Array[Byte]) = synchronized {
    false  // TODO--implement!!!
  }

  def printRaw(message: ByteBuffer) = {
    val msg = new Array[Byte](message.remaining)
    message.get(msg)
    printRaw(msg)
  }

  def println(message: String) = {
    val bytes = message.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val msg = java.util.Arrays.copyOf(bytes, bytes.length + 1)
    msg(msg.length - 1) = '\n'.toByte
    printRaw(msg)
  }

  def printlns(messages: Array[String]) = {
    val byteses = messages.map(_.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val msg = new Array[Byte](byteses.map(_.length).sum + byteses.length)
    var i = 0
    var n = 0
    while (i < byteses.length) {
      System.arraycopy(byteses(i), 0, msg, n, byteses(i).length)
      n += byteses(i).length
      msg(n) = '\n'.toByte
      n += 1
      i += 1
    }
    printRaw(msg)
  }

  private[this] def viewThing(nl: Boolean, len: Int, thing: Array[Array[Byte]], trailing: Option[Mu[(Int, Int)]] = None): Array[String] = {
    if (len == 0) {
      trailing match {
        case Some(m) => m.value = Exec.emptyPosition
        case _ =>
      }
      return Exec.emptyStrings
    }

    var n = 0
    var i = 0
    var i0 = 0
    var j0 = 0
    while (i < len) {
      val ti = thing(i)
      var j = 0
      while (j < ti.length) {
        val c = ti(j)
        if (c == '\n') {i0 = i; j0 = j; n += 1 }
        j += 1
      }
      i += 1
    }
    // WARNING: Must NOT return early past this point UNLESS you put `originally` back if need be!
    val originally =
      if (myFinishFlag.get() && (i0+1 < len || j0+2 < thing(len-1).length || (j0+2 == thing(len-1).length && thing(len-1)(j0+1) != '\r'))) {
        // This is a horrible hack, but it works :(
        val saved = thing(len-1)
        thing(len-1) = java.util.Arrays.copyOf(thing(len-1), thing(len-1).length+1)
        thing(len-1)(thing(len-1).length-1) = '\n'
        n += 1
        saved
      }
      else null
    val strings = new Array[String](n)
    var buffer = Exec.emptyBytes
    i0 = 0
    j0 = if (nl && thing(0)(0) == '\r') 1 else 0
    var m = 0
    while (m < n) {
      var i = i0
      var j = j0
      var k = 0
      var seek = true
      while (seek && i < len) {
        val ti = thing(i)
        while (j < ti.length && ti(j) != '\n') { j += 1; k += 1 }
        if (j >= ti.length) {
          i += 1
          j = 0
        }
        else seek = false
      }
      if (i == i0 || (i == i0+1 && j == 0)) {
        if (i == i0 && j == j0) strings(m) = ""
        else strings(m) = new String(thing(i0), j0, (if (i == i0) j else thing(i0).length) - j0, java.nio.charset.StandardCharsets.UTF_8)
      }
      else {
        if (buffer.length < k) buffer = new Array[Byte](if (k - buffer.length > (k >>> 2)) k else buffer.length + (k >>> 2))
        var bn = thing(i0).length - j0
        System.arraycopy(thing(i0), j0, buffer, 0, bn)
        var ii = i0 + 1
        while (ii < i) {
          System.arraycopy(thing(ii), 0, buffer, bn, thing(ii).length)
          bn += thing(ii).length
          ii += 1
        }
        if (j > 0) {
          System.arraycopy(thing(i), 0, buffer, bn, j)
          bn += j
        }
        strings(m) = new String(buffer, 0, bn, java.nio.charset.StandardCharsets.UTF_8)
      }
      if (i < len) {
        j += 1
        if (j >= thing(i).length) {
          i += 1
          j = 0
        }
        if (i < len && j < thing(i).length && thing(i)(j) == '\r') {
          j += 1
          if (j >= thing(i).length) {
            i +=1
            j = 0
          }
        }
      }
      i0 = i
      j0 = j
      m += 1
    }
    if (originally != null) {
      // End of horrible but effective hack
      thing(len-1) = originally
      if (i0 == len-1 && j0 >= originally.length) {
        i0 += 1
        j0 = 0
      }
    }
    trailing match {
      case Some(m) => m.value = (i0, j0)
      case _ =>
    }
    strings
  }

  private[this] def viewThingRaw(nl: Boolean, len: Int, thing: Array[Array[Byte]]): Array[Byte] = {
    var n = 0
    var i = 0
    while (i < len) { n += thing(i).length; i += 1 }
    if (nl && thing(0)(0) == '\r') n -= 1
    if (n == 0) return Exec.emptyBytes
    val result = new Array[Byte](n)
    i = 0
    n = 0
    while (i < len) {
      val skip = if (i==0 && nl && thing(0)(0) == '\r') 1 else 0
      if (skip < thing(i).length) System.arraycopy(thing(i), skip, result, n, thing(i).length - skip)
      n += thing(i).length - skip
      i += 1
    }
    result
  }

  private[this] def retrieveThing(nl: Boolean, len: Int, thing: Array[Array[Byte]], setNL: Boolean => Unit, setLen: Int => Unit): Array[String] = {
    val where = Mu((0, 0))
    val result = viewThing(nl, len, thing, Some(where))
    if (result.nonEmpty) {
      val lastnl =
        if (where.value._2 > 0) thing(where.value._1)(where.value._2 - 1) == '\n'
        else thing(where.value._1 - 1).last == '\n'
      if (where.value._1 >= len) {
        var i = 0; while (i < len) { thing(i) = null; i += 1 }
        setLen(0)
      }
      else {
        var i = 0
        var j = where.value._1
        while (j < len) {
          thing(i) = thing(j)
          i += 1
          j += 1
        }
        j = 0
        while (i <= len) {
          thing(i) = null
          i += 1
          j += 1
        }
        if (where.value._2 > 0) thing(0) = java.util.Arrays.copyOfRange(thing(0), where.value._2, thing(0).length)
        setLen(len - j)
      }
    }
    result
  }

  private[this] def retrieveThingRaw(nl: Boolean, len: Int, thing: Array[Array[Byte]], setNL: Boolean => Unit, setLen: Int => Unit): Array[Byte] = {
    val result = viewThingRaw(nl, len, thing)
    if (result.nonEmpty) {
      var i = 0; while (i < len) { thing(i) = null; i += 1 }
      setNL(result.last == '\n')
      setLen(0)
    }
    result
  }

  def viewOutput = synchronized { viewThing(outbuf_nl, outbuf_i, outbuf) }

  def viewErrors = synchronized { viewThing(outbuf_nl, errbuf_i, errbuf) }

  def viewOutputRaw = synchronized { viewThingRaw(outbuf_nl, outbuf_i, outbuf) }

  def viewErrorsRaw = synchronized { viewThingRaw(outbuf_nl, errbuf_i, errbuf) }

  def retrieveOutput() = synchronized { retrieveThing(outbuf_nl, outbuf_i, outbuf, nl => { outbuf_nl = nl}, i => { outbuf_i = i }) }

  def retrieveErrors() = synchronized { retrieveThing(outbuf_nl, errbuf_i, errbuf, nl => { errbuf_nl = nl}, i => { errbuf_i = i }) }

  def retrieveOutputRaw() = synchronized { retrieveThingRaw(outbuf_nl, outbuf_i, outbuf, nl => { outbuf_nl = nl}, i => { outbuf_i = i }) }

  def retrieveErrorsRaw() = synchronized { retrieveThingRaw(errbuf_nl, errbuf_i, errbuf, nl => { errbuf_nl = nl}, i => { errbuf_i = i }) }

  def threaded = this

  def waitOrKill(duration: java.time.Duration): Option[Int] = {
    if (Thread.currentThread eq this)
      throw new Exception(s"Thread $this cannot wait for itself in waitOrKill!")

    exitCode match {
      case x: Some[_] => return x
      case _ =>
    }

    val timesup = LocalDateTime.now plus duration
    while (LocalDateTime.now isBefore timesup) {
      Thread.sleep(10)
      exitCode match {
        case x: Some[_] => return x
        case _ =>
      }
    }
    maxLifeMore(Duration.ZERO)
    join()
    exitCode
  }

  private[this] def compactInPlace(bufs: Array[Array[Byte]]): Int = {
    var j = 0
    var seek = true
    while (seek && j < bufs.length) {
      if (bufs(j) == null) seek = false
      else if (j+1 < bufs.length) {
        if (bufs(j) == null) { seek = false; j -= 2 }
        else if (bufs(j).length < bufs(j+1).length) seek = false
        else if (bufs(j).length > 3*bufs(j+1).length) { j += 1; seek = false }
        else j += 1
      }
      else j += 1
    }
    if (j < 0 || j >= bufs.length-3) j = 0
    var i = j
    var ii = j
    while (i < bufs.length) {
      if (i+1 < bufs.length) {
        if (bufs(i) != null) {
          if (bufs(i+1) != null) {
            bufs(ii) = java.util.Arrays.copyOf(bufs(i), bufs(i).length + bufs(i+1).length)
            System.arraycopy(bufs(i+1), 0, bufs(ii), bufs(i).length, bufs(i+1).length)
          }
          else bufs(ii) = bufs(i)
        }
        else bufs(ii) = bufs(i+1)
        i += 2
      }
      else {
        bufs(ii) = bufs(i)
        i += 1
      }
      ii += 1
    }
    i = ii
    while (i < bufs.length) { bufs(i) = null; i += 1 }
    while (ii > 0 && bufs(ii-1) == null) ii -= 1
    ii
  }

  private[this] def bufferedStore(stream: java.io.InputStream, len0: Int, bufs0: Array[Array[Byte]], setLen: Int => Unit, setBufs: Array[Array[Byte]] => Unit) {
    val n = stream.available
    if (n <= 0) return
    if (n > buffer.length) {
      buffer = new Array[Byte](n max (buffer.length + (n >> 2)))
    }
    var len = len0
    var bufs = bufs0
    val m = stream.read(buffer, 0, n)
    if (m <= 0) return
    if (bufs.length < 1) bufs = new Array[Array[Byte]](1)
    else if (len > 0 && bufs(len-1).length >= 1024 && bufs.length < 1024) bufs = java.util.Arrays.copyOf(bufs, bufs.length + bufs.length/2 + 2)
    if (bufs.length >= 1024 && len == bufs.length && bufs(len-1).length >= 1024) len = compactInPlace(bufs)
    if (len < bufs.length && (len == 0 || bufs(len-1).length >= 1024)) {
      bufs(len) = java.util.Arrays.copyOf(buffer, n)
      len += 1
      if (bufs ne bufs0) setBufs(bufs)
      if (len != len0) setLen(len)
    }
    else {
      val appended = java.util.Arrays.copyOf(bufs(len-1), bufs(len-1).length + n)
      System.arraycopy(buffer, 0, appended, bufs(len-1).length, n)
      bufs(len-1) = appended
    }
  }

  def update() { synchronized {
    if (process.getErrorStream.available > 0) bufferedStore(process.getErrorStream, errbuf_i, errbuf, i => { errbuf_i = i }, bufs => { errbuf = bufs })
    if (process.getInputStream.available > 0) bufferedStore(process.getInputStream, outbuf_i, outbuf, i => { outbuf_i = i }, bufs => { outbuf = bufs })
    if (myDieTime.get.exists(_ isBefore LocalDateTime.now)) {
      if (deathAttempts.getAndIncrement() == 0) {
        process.destroy()
        myDieTime.set(Some(LocalDateTime.now plus Duration.ofSeconds(3)))
      }
      else {
        process.destroyForcibly()
        myDieTime.set(Some(LocalDateTime.now plus Duration.ofSeconds(1)))
      }
    }
  } }

  def finish() { 
    if (!myFinishFlag.get()) synchronized {
      myStopTime.set(Some(LocalDateTime.now))
      myExitCode.set(Some(process.exitValue))
      var i = 0; while (i < inbuf_i) { inbuf(i) = null; i += 1 }; inbuf_i = 0
      while (process.getErrorStream.available > 0) bufferedStore(process.getErrorStream, errbuf_i, errbuf, i => { errbuf_i = i }, bufs => { errbuf = bufs })
      while (process.getInputStream.available > 0) bufferedStore(process.getInputStream, outbuf_i, outbuf, i => { outbuf_i = i }, bufs => { outbuf = bufs })
      myFinishFlag.set(true)
    }
  }

  override def run() {
    while (process.isAlive()) {
      update()
      process.waitFor(pollEvery, TimeUnit.MILLISECONDS)
    }
    finish()
  }
}
object Exec {
  val emptyBytes = new Array[Byte](0)
  val emptyStrings = new Array[String](0)
  val emptyPosition = (0, 0)
  def apply(args: Array[String], mergeErrors: Boolean = false, pollingMs: Int = 10): Exec = {
    val thread = new ExecThread(args, mergeErrors, pollingMs)
    thread.start()
    thread
  }
  def apply(args: String*): Exec = apply(args.toArray)
}
