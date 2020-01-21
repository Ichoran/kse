// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015 Rex Kerr, UCSF, and Calico Labs.

package kse

import scala.annotation.tailrec
import scala.util._
import scala.util.control.NonFatal
import scala.util.control.Breaks._

import kse.typecheck._
import kse.flow._
import kse.coll._
import kse.maths._
import kse.maths.hashing._

import kse.jsonal._

package object eio {
  import java.io._
  import java.nio._
  import java.nio.file.{Path, Files, FileSystem, FileSystems, Paths, StandardCopyOption}
  import java.nio.file.attribute.FileTime
  
  import java.time._

  import java.util.zip._
  
  implicit class ConvertSafelyFromByte(private val underlying: Byte) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asShort = underlying.toShort
    def asUShort = (underlying & 0xFF).toShort
    def asInt = underlying.toInt
    def asUInt = (underlying & 0xFF)
    def asLong = underlying.toLong
    def asULong = (underlying & 0xFF).toLong
    def asFloat = underlying.toFloat
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromShort(private val underlying: Short) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asByte(implicit oops: Oops) = if (underlying < Byte.MinValue || underlying > Byte.MaxValue) OOPS else underlying.toByte
    def asUByte(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFF) OOPS else underlying.toByte
    def asInt = underlying.toInt
    def asUInt = (underlying & 0xFFFF)
    def asLong = underlying.toLong
    def asULong = (underlying & 0xFFFF).toLong
    def asFloat = underlying.toFloat
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromInt(private val underlying: Int) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asByte(implicit oops: Oops) = if (underlying < Byte.MinValue || underlying > Byte.MaxValue) OOPS else underlying.toByte
    def asUByte(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFF) OOPS else underlying.toByte
    def asShort(implicit oops: Oops) = if (underlying < Short.MinValue || underlying > Short.MaxValue) OOPS else underlying.toShort
    def asUShort(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFFFF) OOPS else underlying.toShort
    def asLong = underlying.toLong
    def asULong = (underlying & 0xFFFFFFFFL)
    def asFloat(implicit oops: Oops) = { val f = underlying.toFloat; if (f.toInt != underlying) OOPS else f }
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromLong(private val underlying: Long) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asByte(implicit oops: Oops) = if (underlying < Byte.MinValue || underlying > Byte.MaxValue) OOPS else underlying.toByte
    def asUByte(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFF) OOPS else underlying.toByte
    def asShort(implicit oops: Oops) = if (underlying < Short.MinValue || underlying > Short.MaxValue) OOPS else underlying.toShort
    def asUShort(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFFFF) OOPS else underlying.toShort
    def asInt(implicit oops: Oops) = if (underlying < Int.MinValue || underlying > Int.MaxValue) OOPS else underlying.toInt
    def asUInt(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFFFFFFFFL) OOPS else underlying.toInt
    def asFloat(implicit oops: Oops) = { val f = underlying.toFloat; if (f.toLong != underlying) OOPS else f }
    def asDouble(implicit oops: Oops) = { val d = underlying.toDouble; if (d.toLong != underlying) OOPS else d }
  }
  
  implicit class ConvertSafelyFromFloat(private val underlying: Float) extends AnyVal {
    def asByte(implicit oops: Oops) = { val b = math.rint(underlying).toByte; if (b.toFloat != underlying) OOPS else b }
    def asUByte(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFF) != underlying) OOPS else (i & 0xFF).toByte }
    def asShort(implicit oops: Oops) = { val s = math.rint(underlying).toShort; if (s.toFloat != underlying) OOPS else s }
    def asUShort(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFFFF) != underlying) OOPS else (i & 0xFFFF).toShort }
    def asInt(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if (i.toFloat != underlying) OOPS else i }
    def asUInt(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if ((l & 0xFFFFFFFFL) != underlying) OOPS else (l & 0xFFFFFFFFL).toInt }
    def asLong(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if (l.toFloat != underlying) OOPS else l }
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromDouble(private val underlying: Double) extends AnyVal {
    def asByte(implicit oops: Oops) = { val b = math.rint(underlying).toByte; if (b.toDouble != underlying) OOPS else b }
    def asUByte(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFF) != underlying) OOPS else (i & 0xFF).toByte }
    def asShort(implicit oops: Oops) = { val s = math.rint(underlying).toShort; if (s.toDouble != underlying) OOPS else s }
    def asUShort(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFFFF) != underlying) OOPS else (i & 0xFFFF).toShort }
    def asInt(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if (i.toDouble != underlying) OOPS else i }
    def asUInt(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if ((l & 0xFFFFFFFFL) != underlying) OOPS else (l & 0xFFFFFFFFL).toInt }
    def asLong(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if (l.toDouble != underlying) OOPS else l }
    def asFloat(implicit oops: Oops) = { val f = underlying.toFloat; if (f.toDouble != underlying) OOPS else f }
  }

  class DoubleAsTime private[eio] (val time: Double) extends AnyVal {
    def <(that: DoubleAsTime) = time < that.time
    def <=(that: DoubleAsTime) = time <= that.time
    def >=(that: DoubleAsTime) = time >= that.time
    def >(that: DoubleAsTime) = time > that.time
    def abs = if (time < 0) new DoubleAsTime(-time) else this
    def max(that: DoubleAsTime) =
      if      (time <  that.time) that
      else if (time >  that.time) this
      else if (time == that.time) this
      else if (time.isNaN) this
      else that
    def min(that: DoubleAsTime) =
      if      (time <  that.time) this
      else if (time >  that.time) that
      else if (time == that.time) this
      else if (time.isNaN) this
      else that
    def unary_- = new DoubleAsTime(-time)
    def +(that: DoubleAsTime) = new DoubleAsTime(time + that.time)
    def -(that: DoubleAsTime) = new DoubleAsTime(time - that.time)
    def *(factor: Double) = DoubleAsTime.apply(factor * time)
    def /(factor: Double) = DoubleAsTime.apply(time / factor)
    def timeFn(f: Double => Double) = DoubleAsTime.apply(f(time))
    def +(that: Duration) = new DoubleAsTime(time + that.getSeconds + that.getNano/1e9)
    def -(that: Duration) = new DoubleAsTime(time - (that.getSeconds.toDouble + that.getNano/1e9))
    def roundMillis = new DoubleAsTime(math.rint(time * 1e3) / 1e3)
    def roundSecs = new DoubleAsTime(math.rint(time))
    def roundMins = new DoubleAsTime(math.rint(time / 60) * 60)
    def roundHours = new DoubleAsTime(math.rint(time / 3600) * 3600)
    def roundDays = new DoubleAsTime(math.rint(time / 86400) * 86400)
    def toNanos = math.rint(time * 1e9).toLong
    def toMillis = math.rint(time * 1e3).toLong
    def toSecs = math.rint(time).toLong
    def toMins = math.rint(time / 60).toLong
    def toHours = math.rint(time / 3600).toLong
    def toDays = math.rint(time / 86400).toLong
    def toDuration = {
      val t = math.abs(time)
      if (t < 9e9) Duration.ofNanos(math.rint(time * 1e9).toLong)
      else if (t < 9e15) Duration.ofMillis(math.rint(time * 1e3).toLong)
      else if (t < 9e18) Duration.ofSeconds(math.rint(time).toLong)
      else if (t < 54e19) Duration.ofMinutes(math.rint(time/60).toLong)
      else if (t < 324e20) Duration.ofHours(math.rint(time/3600).toLong)
      else Duration.ofDays(math.rint(time/86400).toLong)
    }
    def epochInstant = {
      val millis = time * 1e3
      if (math.abs(millis) < Long.MaxValue) Instant.ofEpochMilli(math.rint(millis).toLong)
      else if (time > DoubleAsTime.InstantMaxEpochSeconds) Instant.MAX
      else if (time < DoubleAsTime.InstantMinEpochSeconds) Instant.MIN
      else Instant.ofEpochSecond(math.rint(time).toLong)
    }
    def epochFileTime =
      if (math.abs(time) < 9e15) FileTime.fromMillis(math.rint(time * 1e3).toLong)
      else FileTime.from(math.rint(time).toLong, java.util.concurrent.TimeUnit.SECONDS)
    def +(when: Instant)        = when plus this.toDuration
    def +(when: LocalDateTime)  = when plus this.toDuration
    def +(when: ZonedDateTime)  = when plus this.toDuration
    def +(when: OffsetDateTime) = when plus this.toDuration
    def +(when: FileTime): FileTime = this.+((new FileTimeCanDoThings(when)).epoch).epochFileTime
    override def toString = time.toString + " sec"
    def =~=(that: DoubleAsTime) = {
      val t = math.max(math.abs(time), math.abs(time))
      if (t < 11259e2)                         time == that.time
      else if (t < 11259e5)   math.rint(time*1e6)   == math.rint(that.time*1e6)
      else if (t < 11259e8)   math.rint(time*1e3)   == math.rint(that.time*1e3)
      else if (t < 11259e11)  math.rint(time)       == math.rint(that.time)
      else if (t < 67554e12)  math.rint(time/60)    == math.rint(that.time/60)
      else if (t < 405324e13) math.rint(time/3600)  == math.rint(that.time/3600)
      else                    math.rint(time/86400) == math.rint(that.time/86400)
    }
  }
  object DoubleAsTime {
    val InstantMaxEpochSeconds = Instant.MAX.getEpochSecond
    val InstantMinEpochSeconds = Instant.MIN.getEpochSecond

    implicit val canonicalTimeOrdering: Ordering[DoubleAsTime] = new Ordering[DoubleAsTime] {
      def compare(a: DoubleAsTime, b: DoubleAsTime) =
        if (a.=~=(b)) 0 else java.lang.Double.compare(a.time, b.time)
    }

    def apply(time: Double) = new DoubleAsTime(math.rint(time*1e9)/1e9)
  }
  implicit class DoubleCanMultiplyAndBeTime(val value: Double) extends AnyVal {
    def asTime = new DoubleAsTime(value)
    def asDuration = (new DoubleAsTime(value)).toDuration
    def *(that: DoubleAsTime) = new DoubleAsTime(value * that.time)
  }
  implicit class LongCanMultiplyDuration(val value: Long) extends AnyVal {
    def *(that: Duration) = that.multipliedBy(value)
  }
  implicit class LongSecondsAsDoubleTime(val value: Long) extends AnyVal {
    def sec = new DoubleAsTime(value.toDouble)
    def secs = new DoubleAsTime(value.toDouble)
  }
  implicit class LongMillisAsDoubleTime(val value: Long) extends AnyVal {
    def milli = new DoubleAsTime(value.toDouble / 1e3)
    def millis = new DoubleAsTime(value.toDouble / 1e3)
  }
  implicit class LongNanosAsDoubleTime(val value: Long) extends AnyVal {
    def nano = new DoubleAsTime(value.toDouble / 1e9)
    def nanos = new DoubleAsTime(value.toDouble / 1e9)
  }
  implicit class LongMinutesAsDoubleTime(val value: Long) extends AnyVal {
    def minute = new DoubleAsTime(value.toDouble * 60)
    def minutes = new DoubleAsTime(value.toDouble * 60)
  }
  implicit class LongHoursAsDoubleTime(val value: Long) extends AnyVal {
    def hour = new DoubleAsTime(value.toDouble * 3600)
    def hours = new DoubleAsTime(value.toDouble * 3600)
  }
  implicit class LongDaysAsDoubleTime(val value: Long) extends AnyVal {
    def day = new DoubleAsTime(value.toDouble * 86400)
    def days = new DoubleAsTime(value.toDouble * 86400)
  }
  implicit class LongCanDispatchAsDuration(private val value: Long) extends AnyVal {
    def duration = new DurationDispatcher(value)
  }
  class DurationDispatcher(private val value: Long) extends AnyVal {
    def nano = Duration.ofNanos(value)
    def nanos = Duration.ofNanos(value)
    def milli = Duration.ofMillis(value)
    def millis = Duration.ofMillis(value)
    def sec = Duration.ofSeconds(value)
    def secs = Duration.ofSeconds(value)
    def min = Duration.ofMinutes(value)
    def mins = Duration.ofMinutes(value)
    def hour = Duration.ofHours(value)
    def hours = Duration.ofHours(value)
    def day = Duration.ofDays(value)
    def days = Duration.ofDays(value)
  }
  implicit class DurationCanDoThings(private val duration: Duration) extends AnyVal {
    def <(that: Duration) = duration.compareTo(that) < 0
    def <=(that: Duration) = duration.compareTo(that) <= 0
    def >=(that: Duration) = duration.compareTo(that) >= 0
    def >(that: Duration) = duration.compareTo(that) > 0
    def abs = if (duration.getSeconds < 0) duration.negated else duration
    def max(that: Duration) = if (duration.compareTo(that) < 0) that else duration
    def min(that: Duration) = if (duration.compareTo(that) > 0) that else duration
    def +(that: Duration) = duration plus that
    def -(that: Duration) = duration minus that
    def unary_- = duration.negated
    def *(factor: Long) = duration multipliedBy factor
    def +(that: DoubleAsTime) = new DoubleAsTime(that.time + duration.getSeconds.toDouble + duration.getNano/1e9)
    def -(that: DoubleAsTime) = new DoubleAsTime(duration.getSeconds.toDouble + duration.getNano/1e9 - that.time)
    def toDouble = new DoubleAsTime(duration.getSeconds.toDouble + duration.getNano/1e9)
    def roundMillis = {
      val s = duration.getSeconds
      val n = duration.getNano
      val r = n % 1000000
      if (r == 0) this
      else {
        val m = if (r < 500000) n - r else if (r > 500000) (n - r) + 1000000 else if (s >= 0) n+500000 else n-500000
        if (m < 1000000000) duration.withNanos(m)
        else Duration.ofSeconds(s+1)
      }
    }
    def roundSeconds = {
      val n = duration.getNano
      if (n == 0) duration
      else if (n < 500000000) duration.withNanos(0)
      else {
        val s = duration.getSeconds
        if (n > 500000000) Duration.ofSeconds(s)
        else if (s < 0) duration.withNanos(0)
        else Duration.ofSeconds(s + 1)
      }
    }
    def +(when: Instant       ): Instant        = when plus duration
    def +(when: LocalDateTime ): LocalDateTime  = when plus duration
    def +(when: ZonedDateTime ): ZonedDateTime  = when plus duration
    def +(when: OffsetDateTime): OffsetDateTime = when plus duration
    def +(when: FileTime): FileTime = FileTime from when.toInstant.plus(duration)
  }
  implicit class InstantCanDoThings(private val instant: Instant) extends AnyVal {
    def <(when: Instant) = instant isBefore when
    def <=(when: Instant) = !(when isBefore instant)
    def >=(when: Instant) = !(instant isBefore when)
    def >(when: Instant) = when isBefore instant
    def min(when: Instant) = if (when isBefore instant) when else instant
    def max(when: Instant) = if (when isAfter instant) when else instant
    def +(that: Duration) = instant plus that
    def -(that: Duration) = instant minus that
    def +(that: DoubleAsTime) = instant plus that.toDuration
    def -(that: DoubleAsTime) = instant minus that.toDuration
    def -(when: Instant) = Duration.between(when, instant)
    def roundMillis = {
      val n = instant.getNano % 1000000
      if (n == 0) instant
      else instant.plusNanos(if (n < 500000) -n else 1000000 - n)
    }
    def roundSeconds = {
      val n = instant.getNano
      if (n == 0) instant
      else instant.plusNanos(if (n < 500000000) -n else 1000000000 - n)
    }
    def epoch = new DoubleAsTime(instant.getEpochSecond + instant.getNano/1e9)
    def local = instant.atZone(ZoneId.systemDefault).toLocalDateTime
    def utc = instant.atOffset(ZoneOffset.UTC)
    def zoned = instant.atZone(ZoneId.systemDefault)
    def filetime = FileTime from instant
  }
  implicit class LocalDateTimeCanDoThings(private val datetime: LocalDateTime) extends AnyVal {
    def <(when: LocalDateTime) = datetime isBefore when
    def <=(when: LocalDateTime) = !(when isBefore datetime)
    def >=(when: LocalDateTime) = !(datetime isBefore when)
    def >(when: LocalDateTime) = when isBefore datetime
    def min(when: LocalDateTime) = if (when isBefore datetime) when else datetime
    def max(when: LocalDateTime) = if (when isAfter datetime) when else datetime
    def +(that: Duration) = datetime plus that
    def -(that: Duration) = datetime minus that
    def +(that: DoubleAsTime) = datetime plus that.toDuration
    def -(that: DoubleAsTime) = datetime minus that.toDuration
    def -(when: LocalDateTime) = Duration.between(when, datetime)
    def roundMillis = {
      val n = datetime.getNano % 1000000
      if (n == 0) datetime
      else datetime.plusNanos(if (n < 500000) -n else 1000000 - n)
    }
    def roundSeconds = {
      val n = datetime.getNano
      if (n == 0) datetime
      else datetime.plusNanos(if (n < 500000000) -n else 1000000000 - n)
    }
    def epoch = new DoubleAsTime(datetime.atZone(ZoneId.systemDefault).toInstant.getEpochSecond + datetime.getNano/1e9)
    def utc = datetime.atZone(ZoneId.systemDefault).toOffsetDateTime.withOffsetSameInstant(ZoneOffset.UTC)
    def zoned = datetime.atZone(ZoneId.systemDefault)
    def instant = datetime.atZone(ZoneId.systemDefault).toInstant
    def filetime = FileTime from datetime.atZone(ZoneId.systemDefault).toInstant
  }
  implicit class ZonedDateTimeCanDoThings(private val datetime: ZonedDateTime) extends AnyVal {
    def <(when: ZonedDateTime) = datetime isBefore when
    def <=(when: ZonedDateTime) = !(when isBefore datetime)
    def >=(when: ZonedDateTime) = !(datetime isBefore when)
    def >(when: ZonedDateTime) = when isBefore datetime
    def min(when: ZonedDateTime) = if (when isBefore datetime) when else datetime
    def max(when: ZonedDateTime) = if (when isAfter datetime) when else datetime
    def +(that: Duration) = datetime plus that
    def -(that: Duration) = datetime minus that
    def +(that: DoubleAsTime) = datetime plus that.toDuration
    def -(that: DoubleAsTime) = datetime minus that.toDuration
    def -(when: ZonedDateTime) = Duration.between(when, datetime)
    def roundMillis = {
      val n = datetime.getNano % 1000000
      if (n == 0) datetime
      else datetime.plusNanos(if (n < 500000) -n else 1000000 - n)
    }
    def roundSeconds = {
      val n = datetime.getNano
      if (n == 0) datetime
      else datetime.plusNanos(if (n < 500000000) -n else 1000000000 - n)
    }
    def epoch = new DoubleAsTime(datetime.toEpochSecond + datetime.getNano/1e9)
    def utc = datetime.toOffsetDateTime.withOffsetSameInstant(ZoneOffset.UTC)
    def local = datetime.withZoneSameInstant(ZoneId.systemDefault).toLocalDateTime
    def instant = datetime.toInstant
    def filetime = FileTime from datetime.toInstant
  }
  implicit class OffsetDateTimeCanDoThings(private val datetime: OffsetDateTime) extends AnyVal {
    def <(when: OffsetDateTime) = datetime isBefore when
    def <=(when: OffsetDateTime) = !(when isBefore datetime)
    def >=(when: OffsetDateTime) = !(datetime isBefore when)
    def >(when: OffsetDateTime) = when isBefore datetime
    def min(when: OffsetDateTime) = if (when isBefore datetime) when else datetime
    def max(when: OffsetDateTime) = if (when isAfter datetime) when else datetime
    def +(that: Duration) = datetime plus that
    def -(that: Duration) = datetime minus that
    def +(that: DoubleAsTime) = datetime plus that.toDuration
    def -(that: DoubleAsTime) = datetime minus that.toDuration
    def -(when: OffsetDateTime) = Duration.between(when, datetime)
    def roundMillis = {
      val n = datetime.getNano % 1000000
      if (n == 0) datetime
      else datetime.plusNanos(if (n < 500000) -n else 1000000 - n)
    }
    def roundSeconds = {
      val n = datetime.getNano
      if (n == 0) datetime
      else datetime.plusNanos(if (n < 500000000) -n else 1000000000 - n)
    }
    def epoch = new DoubleAsTime(datetime.toEpochSecond + datetime.getNano/1e9)
    def utc = datetime.withOffsetSameInstant(ZoneOffset.UTC)
    def zoned = datetime.toZonedDateTime.withZoneSameInstant(ZoneId.systemDefault)
    def local = datetime.toInstant.atZone(ZoneId.systemDefault).toLocalDateTime
    def instant = datetime.toInstant
    def filetime = FileTime from datetime.toInstant
  }
  implicit class FileTimeCanDoThings(private val filetime: FileTime) extends AnyVal {
    def <(when: FileTime) = filetime.compareTo(when) < 0
    def <=(when: FileTime) = filetime.compareTo(when) <= 0
    def >=(when: FileTime) = filetime.compareTo(when) >= 0
    def >(when: FileTime) = filetime.compareTo(when) > 0
    def min(when: FileTime) = if (filetime.compareTo(when) > 0) when else filetime
    def max(when: FileTime) = if (filetime.compareTo(when) < 0) when else filetime
    def +(that: Duration) = FileTime from (filetime.toInstant plus that)
    def -(that: Duration) = FileTime from (filetime.toInstant minus that)
    def +(that: DoubleAsTime) = FileTime from (filetime.toInstant plus that.toDuration)
    def -(that: DoubleAsTime) = FileTime from (filetime.toInstant minus that.toDuration)
    def -(when: FileTime) = Duration.between(when.toInstant, filetime.toInstant)
    def roundMillis = {
      val i = filetime.toInstant
      if (i == Instant.MAX || i == Instant.MIN) filetime
      else {
        val i2 = (new InstantCanDoThings(i)).roundMillis
        if (i == i2) filetime
        else FileTime from i2
      }
    }
    def roundSeconds = {
      val i = filetime.toInstant
      if (i == Instant.MAX || i == Instant.MIN) filetime
      else {
        val i2 = (new InstantCanDoThings(i)).roundSeconds
        if (i == i2) filetime
        else FileTime from i2
      }
    }
    def epoch = {
      val i = filetime.toInstant
      new DoubleAsTime(
        if (i == Instant.MAX || i == Instant.MIN) filetime.to(java.util.concurrent.TimeUnit.SECONDS).toDouble
        else i.getEpochSecond + i.getNano/1e9
      )
    }
    def utc = filetime.toInstant.atOffset(ZoneOffset.UTC)
    def zoned = filetime.toInstant.atZone(ZoneId.systemDefault)
    def local = filetime.toInstant.atZone(ZoneId.systemDefault).toLocalDateTime
    def instant = filetime.toInstant
  }

  private val quotes_not_needed_regex_string = """[A-Za-z0-9_/\.\-~]+"""
  private val single_escapes_regex_string    = """[A-Za-z0-9_/\.\-~ ]+"""
  implicit class QuoteStringsForCommandLine(private val underlying: String) extends AnyVal {
    def bashquoted: String = 
      if (underlying.length == 0) "''"
      else if (underlying matches quotes_not_needed_regex_string) underlying
      else if (underlying matches single_escapes_regex_string) {
        val sb = new java.lang.StringBuilder
        var i = 0
        while (i < underlying.length) {
          val j = underlying.indexOf(' ', i)
          if (j < 0) {
            sb append underlying.substring(i)
            i = underlying.length
          }
          else if (j > i) {
            sb append underlying.substring(i, j)
            sb append """\ """
            i = j+1
          }
          else {
            sb append """\ """
            i += 1
          }
        }
        sb.toString
      }
      else {
        val sb = new java.lang.StringBuilder
        var i = 0
        while (i < underlying.length) {
          val j = underlying.indexOf('\'', i)
          if (j < 0) {
            sb append '\''
            sb append underlying.substring(i)
            sb append '\''
            i = underlying.length
          }
          else if (j > i) {
            sb append '\''
            sb append underlying.substring(i, j)
            sb append """'\'"""
            i = j+1
          }
          else {
            sb append """\'"""
            i += 1
          }
        }
        sb.toString
      }
  }
  

  object \: {
    def unapply(af: Array[File]): Option[(File, Array[File])] = {
      if (af.length == 0) None
      else if (af.length == 1) unapply(af(0)).map{ case (f, g) => (f, Array(g)) }
      else {
        val p0 = af(0).getPath
        var fid = p0.length
        var i = 1
        while (i < af.length && fid > 0) {
          var j = 0
          var p = af(i).getPath
          while (j < fid && j < p.length && p(j) == p0(j)) j += 1
          fid = j
          i += 1
        }
        if (fid == 0) None
        else {
          val k = p0.lastIndexOf(File.separatorChar,fid)
          if (k < 0) None
          else Some( (new File(p0.substring(0, k+1)), af.map(f => new File(f.getPath.substring(k+1)))) )
        }
      }
    }
    def unapply(f: File): Option[(File, File)] = {
      val p = f.getParentFile
      if (p == null) None else Some((p,f))
    }
  }
  
  object % {
    def unapply(f: File): Option[(File, String)] = {
      val n = f.getName
      val i = n.lastIndexOf('.')
      var pre = i-1
      while (pre >= 0 && n(pre) == '.') pre -= 1
      if (pre < 0 || i+1 >= n.length) None
      else {
        val p = f.getParentFile
        val nf = if (p != null) new File(p, n.substring(0, pre+1)) else new File(n.substring(0, pre+1))
        Some((nf, n.substring(i+1)))
      }
    }
  }
  
  implicit class StringAsFile(private val underlying: String) extends AnyVal {
    def file = new File(underlying)
    def \:(parent: File) = new File(parent, underlying)
  }

  implicit class StringAsPath(private val underlying: String) extends AnyVal {
    def path = FileSystems.getDefault.getPath(underlying)
    def pathOption = try { Some(FileSystems.getDefault.getPath(underlying)) } catch { case ipe: file.InvalidPathException => None }
    def grabPath(implicit oops: Oops): Path = try { FileSystems.getDefault.getPath(underlying) } catch { case ipe: file.InvalidPathException => OOPS }
  }

  implicit class BytesAsStrings(private val underlying: Array[Byte]) extends AnyVal {
    def utf8 = new String(underlying, java.nio.charset.StandardCharsets.UTF_8)
    def ascii = new String(underlying, java.nio.charset.StandardCharsets.US_ASCII)
    def rawString = new String(underlying, java.nio.charset.StandardCharsets.ISO_8859_1)
    def iso8859_1 = new String(underlying, java.nio.charset.StandardCharsets.ISO_8859_1)
  }
  
  implicit class ZipEntryProperPaths(private val underlying: ZipEntry) extends AnyVal {
    def name = {
      val n = underlying.getName
      val i = n.indexOf('/')
      val j = n.indexOf('\\')
      if (i < 0 && j > 0) n.replace('\\', '/') else n
    }
    def file = {
      val n = name
      if (File.separatorChar == '/') new File(n) else new File(n.replace('/',File.separatorChar))
    }
  }

  implicit class InputStreamShouldDoThis(private val underlying: java.io.InputStream) extends AnyVal {
    def bigGulp(limit: Long): Ok[String, List[Array[Byte]]] = {
      if (limit < 0) return No("Negative size limit")
      if (limit == 0) return Yes(Nil)
      try {
        val pieces = List.newBuilder[Array[Byte]]
        var current = new Array[Byte](underlying.available max (32L min limit).toInt)
        var n = 0
        var total = 0L
        var complete = false
        while (!complete) {
          val limitNow = ((limit - total) min (current.length - n)).toInt max 1
          val i = underlying.read(current, n, limitNow)
          if (i < 0) complete = true
          else {
            total += i
            if (total > limit) return No(s"Could not gulp entire stream: too big (> $limit bytes)")
            if (i == current.length - n) {
              if (n == 0 && total == i) {
                val b = underlying.read()
                if (b < 0) {
                  n = current.length
                  complete = true
                }
                else {
                  total += 1
                  if (total > limit) return No(s"Could not gulp entire stream: too big (> $limit bytes)")
                  pieces += current
                  current = new Array[Byte](4*current.length min 262144)
                  current(0) = b.toByte
                  n = 1
                }
              }
              else {
                pieces += current
                current = new Array[Byte](4*current.length min 262144)
                n = 0
              }
            }
            else n += i
          } 
        }
        if (n > 0) {
          pieces += (if (n == current.length) current else java.util.Arrays.copyOf(current, n))
        }
        Yes(pieces.result())
      }
      catch {
        case t if NonFatal(t) => No(s"Error while consuming InputStream:\n${t.explain(128)}")
      }
      finally { underlying.close }
    }

    def gulp(): Ok[String, Array[Byte]] = bigGulp(Int.MaxValue - 1).map{
      case Nil        => new Array[Byte](0)
      case one :: Nil => one
      case lots       =>
        val combined = new Array[Byte](lots.map(_.length).sum)
        var n = 0
        lots.foreach{ one =>
          System.arraycopy(one, 0, combined, n, one.length)
          n += one.length
        }
        combined
    }

    def reader() = new BufferedReader(new InputStreamReader(underlying))

    def slurp(): Ok[String, Vector[String]] = safe {
      val reader = new BufferedReader(new java.io.InputStreamReader(underlying))
      val vb = Vector.newBuilder[String]
      var continue = true
      while (continue) {
        val line = reader.readLine
        if (line eq null) continue = false
        else vb += line
      }
      vb.result()
    }.mapNo(e => s"Error consuming InputStream:\n${e.explain()}")

    def walker(size: Int = 8192): Walker[Array[Byte]] = new InputStreamStepper(underlying, size)
  }

  implicit class FileShouldDoThis(private val underlying: java.io.File) extends AnyVal {
    def %(ext: String) = {
      val f0 = underlying match {
        case fi % _ => fi
        case _ => underlying
      }
      if (ext == null || ext == "") f0
      else {
        val p0 = f0.getParentFile
        val name = f0.getName + "." + ext
        if (p0 == null) new File(name) else new File(p0, name)
      }
    }
    
    def \:(parent: File) = {
      if (underlying.isAbsolute) new File(parent, underlying.getName)
      else new File(parent, underlying.getPath)
    }
    
    def canon = underlying.getCanonicalFile
    def parent = Option(underlying.getParentFile)
    
    def path = underlying.toPath
    def pathName = underlying.getPath
    def zipname: String = {
      if (underlying.isAbsolute) {
        var f = underlying
        var names = List.empty[String]
        while (f != null) {
          val p = f.getParentFile
          names = f.getName :: names
          f = p
        }
        names match {
          case first :: rest => rest.mkString("/")
          case _ => ""
        }
      }
      else {
        if (File.separatorChar == '/') pathName else pathName.replace(File.separatorChar, '/')
      }
    }
    
    def name = underlying.getName
    def nameFn(f: String => String) = {
      val p = underlying.getParentFile
      if (p == null) new File(f(underlying.getName)) else new File(p, f(underlying.getName))
    }
    
    def ext = underlying match { case _ % x => x; case _ => "" }
    def extFn(f: String => String) = this.%(f(ext))
    
    def base = underlying match { case f % _ => f.getName; case _ => underlying.getName }
    def baseFn(f: String => String) = underlying match {
      case p \: b % x => new File(p, f(b.getName) + "." + x)
      case p \: b => new File(p, f(b.getName))
      case b % x => new File(f(b.getName) + "." + x)
      case _ => new File(f(underlying.getName))
    }

    def files = underlying.listFiles match {
      case null => FileShouldDoThis.emptyFileArray
      case x    => x
    }

    def t: FileTime = FileTime fromMillis underlying.lastModified
    def t_=(ft: FileTime) {
      underlying.setLastModified(ft.toMillis)
    }
    
    def relativize(absolutes: Array[File]): Ok[Vector[String], Array[File]] = {
      val af = try { underlying.getAbsoluteFile } catch { case t if NonFatal(t) => return No(Vector("Could not find absolute form of root " + underlying.getPath)) }
      val cf = try { af.getCanonicalFile } catch { case t if NonFatal(t) => return No(Vector("Could not find canonical form of root " + af.getPath)) }
      val ap = af.getPath
      val cp = cf.getPath
      val wrongs = Vector.newBuilder[String]
      val rights = Array.newBuilder[File]
      def clip(h: File, xp: String): Option[File] = {
        val hp = h.getPath
        if (hp startsWith xp) {
          val n = if (xp.length < hp.length && hp(xp.length) == File.separatorChar) xp.length+1 else xp.length
          if (hp.length < n+1) None
          else Some(new File(hp.substring(n)))
        }
        else None
      }
      absolutes.foreach{ g =>
        okay[String]{ fail =>
          val ag = try { g.getAbsoluteFile } catch { case t if NonFatal(t) => fail("Could not find absolute form of " + g.getPath) }
          clip(ag, ap).getOrElse {
            val cg = try { ag.getCanonicalFile } catch { case t if NonFatal(t) => fail("Could not find canonical form of " + ag.getPath) }
            clip(cg, cp).getOrElse{ fail(s"$cp is not a root for $cg") }
          }
        } match {
          case Yes(x) => rights += x
          case No(e) => wrongs += e
        }
      }
      val w = wrongs.result()
      if (w.nonEmpty) No(w) else Yes(rights.result())
    }
    
    def gulp: Ok[String, Array[Byte]] = okay[String]{ fail =>
      if (underlying.isDirectory) fail(s"${underlying.getPath} is a directory")
      val sz = try { underlying.length } catch { case t if NonFatal(t) => fail(s"Could not read length of ${underlying.getPath}") }
      if (sz >= Int.MaxValue) fail(s"${underlying.getPath} is too big")
      try {
        val buf = try { new Array[Byte](sz.toInt) } catch { case oome: OutOfMemoryError => fail(s"Not enough memory to read ${underlying.getPath}") }
        val fis = new FileInputStream(underlying)
        try {
          var i = 0
          var ret = 0
          var zeros = 0
          while (i < buf.length && ret != -1 && zeros < 4) {
            ret = fis.read(buf, i, math.min(262144, buf.length - i))
            if (ret < 0) zeros += 1
            else {
              zeros = 0
              i += ret
            }
          }
          buf
        }
        catch { case t if NonFatal(t) => fail(s"Error while reading ${underlying.getPath}") }
        finally { try { fis.close } catch { case t if NonFatal(t) => } }
      }
      catch { case t if NonFatal(t) => fail(s"Could not read ${underlying.getPath}") }
    }
    
    def slurp: Ok[String, Vector[String]] = okay[String]{ fail =>
      val src = try { scala.io.Source.fromFile(underlying) } catch { case t if NonFatal(t) => fail(s"Could not open ${underlying.getPath}") }
      try {
        try { src.getLines.toVector }
        catch { case t if NonFatal(t) => fail(s"Error while reading ${underlying.getPath}") }
      }
      catch { case oome: OutOfMemoryError => fail(s"Out of memory reading ${underlying.getPath}") }
      finally { try { src.close } catch { case t if NonFatal(t) => } }
    }

    private[this] def myHashWith(
      h0: SimpleIncrementalHash, h1: SimpleIncrementalHash, hmore: Seq[SimpleIncrementalHash]
    ): Ok[String, Array[Any]] =
      okay[String]{ fail =>
        if (underlying.isDirectory) fail(s"${underlying.getPath} is a directory")
        var sz = try { underlying.length } catch { case t if NonFatal(t) => fail(s"Could not read length of ${underlying.getPath}") }
        val h = new Array[SimpleIncrementalHash](hmore.length + (if (h1 eq null) 0 else 1) + (if (h0 eq null) 0 else 1))
        var i = 0
        if (h0 ne null) { h(i) = h0.begin(); i += 1 }
        if (h1 ne null) { h(i) = h1.begin(); i += 1 }
        hmore.foreach{ hi => h(i) = hi.begin(); i += 1 }
        try {
          val buf = new Array[Byte](math.min(sz, 262144).toInt)
          val bb = java.nio.ByteBuffer wrap buf
          val fis = new FileInputStream(underlying)
          try {
            var n = 0
            var zeros = 0
            while (n >= 0 && sz >= 0 && zeros < 4) {
              n = fis.read(buf, 0, buf.length)
              if (n == 0) zeros += 1
              else if (n > 0) {
                zeros = 0
                sz -= n
                i = 0
                while (i < h.length) {
                  bb.position(0).limit(n)
                  h(i) append bb
                  i += 1
                }
              }
            }
          }
          catch { case t if NonFatal(t) => fail(s"Error while reading ${underlying.getPath}:\n${t.toString}") }
          finally { try { fis.close } catch { case t if NonFatal(t) => } }
        }
        catch { case t if NonFatal(t) => fail(s"Could not read ${underlying.getPath} because:\n${t.toString}") }
        h.map{ hi => 
          hi.resultAs[Any] match {
            case Some(x) => x
            case None    =>
              fail(s"Could not get final hash value from hasher of type ${hi.getClass.getName} on ${underlying.getPath}")
          }
        }
      }

    def hashWith(h: maths.hashing.Hash32, h2: maths.hashing.Hash32, more: maths.hashing.Hash32*): Ok[String, Array[Int]] =
      myHashWith(h, h2, more).map{_.map{ case i: Int => i; case _ => throw new Exception("Wrong hash type") }}

    def hashWith(h: maths.hashing.Hash32): Ok[String, Int] = hashWith(h, null).map(_.head)

    def hashWith(h: maths.hashing.Hash64, h2: maths.hashing.Hash64, more: maths.hashing.Hash64*): Ok[String, Array[Long]] =
      myHashWith(h, h2, more).map{_.map{ case l: Long => l; case _ => throw new Exception("Wrong hash type") }}

    def hashWith(h: maths.hashing.Hash64): Ok[String, Long] = hashWith(h, null).map(_.head)
    
    def walk(act: FileWalker, log: FileLogger, sizeLimit: Int = Int.MaxValue) {
      val fw = act match { case fwi: FileWalkImpl => fwi }
      
      val seen = new collection.mutable.AnyRefMap[File, Unit]()
      val unit: Unit = ()
      var pending = underlying :: Nil
      
      def isZip(s: String) = s == ".zip" || s == ".jar"
      
      def ziply(zis: ZipInputStream, inzes: List[ZipEntry]) {
        var ze = zis.getNextEntry
        while (ze != null) {
          val zes = ze :: inzes
          fw.zes = zes
          val stance = fw.picker
          val myName = fw.file.getPath + "//" + zes.map(_.getName).mkString("//")
          var buf: Array[Byte] = null
          var wlk: Walker[Array[Byte]] = null
          var consumed = false
          
          stance match {
            case _: Selected =>
              val sz = ze.getSize
              var oversize = sz > sizeLimit
              if (sz < 0) {
                consumed = true
                val bufs = Vector.newBuilder[Array[Byte]]
                var n = 0
                var go = true
                wlk = new InputStreamStepper(zis, 8192)
                while (n < sizeLimit && go) {
                  go = wlk.step{ b => n += b.length; bufs += b }
                }
                if (!go) {
                  wlk = null
                  buf = new Array[Byte](n)
                  var i = 0
                  bufs.result().foreach{ a => 
                    java.lang.System.arraycopy(a, 0, buf, i, a.length)
                    i += a.length
                  }
                }
                else {
                  oversize = true
                  wlk = bufs.result().iterator.walker ++ wlk
                }
              }
              else if (sz < sizeLimit) {
                consumed = true
                buf = new Array[Byte](sz.toInt)
                var i = 0
                var zeros = 0
                while (i < buf.length && zeros < 4) {
                  val k = zis.read(buf, i, buf.length - i)
                  if (k > 0) {
                    zeros = 0
                    i += k
                  }
                  else zeros += 1
                }
                if (zeros >= 4) buf = null
              }
              fw match {
                case fsw: FileWalkOnStreams if oversize =>
                  consumed = true
                  fsw.stream = if (wlk == null) zis else (new SteppedByteArrayInputStream(wlk))
                  fsw.streamOp
                  if (wlk != null) wlk = null
                case fbw: FileWalkOnBuffers if buf != null =>
                  fbw.buffer = buf
                  fbw.bufOp
                case fsw: FileWalkOnStreams if (!consumed || buf != null || wlk != null) =>
                  fsw.stream = if (!consumed) zis else if (buf == null) new ByteArrayInputStream(buf) else new SteppedByteArrayInputStream(wlk)
                  consumed = true
                  fsw.streamOp
                  if (wlk != null) wlk = null
                case fbw: FileWalkOnBuffers =>
                  log(myName, "Could not read zip entry into buffer", "buffer missing")
                case fsw: FileWalkOnStreams =>
                  log(myName, "Could not read zip entry into stream", "already consumed")
                case _ =>
                  fw.listOp
              }
            case _ =>
          }
          stance match {
            case _: Recursed if isZip(ze.getName.takeRight(4).toLowerCase) =>
              try {
                if (buf == null && consumed) log(myName, "Unable to buffer entry for both reading and recursing", "both recursing into and processing archive raw without adequate buffering")
                else if (buf != null) {
                  val bais = new ByteArrayInputStream(buf)
                  val zis2 = new ZipInputStream(bais)
                  ziply(zis2, zes)
                }
                else try { ziply(new ZipInputStream(zis), zes) } finally { zis.closeEntry }
              }
              catch { 
                case soe: StackOverflowError => log(myName, "Stack overflow while recursing", exceptionAsString(soe))
                case oome: OutOfMemoryError => log(myName, "Not enough memory to recurse", exceptionAsString(oome))
                case t if NonFatal(t) => log(myName, "Could not recurse", exceptionAsString(t))
              }
            case _ =>
          }
          
          ze = zis.getNextEntry
        }
      }
      
      @tailrec def inner() {
        pending match {
          case f :: rest =>
            pending = rest
            val exists = try { f.exists } catch { case t if NonFatal(t) => false }
            if (exists) safeOption(f.getCanonicalFile) match {
              case Some(cf) if (!(seen contains cf)) =>
                seen += cf -> unit
                fw.file = if (fw.canonized) cf else f
                fw.zes = Nil
                val p = try { fw.picker } catch { case t if NonFatal(t) => log(fw.file.getPath, "Error selecting during walk", exceptionAsString(t)); Reject }
                val dir = try { fw.file.isDirectory } catch { case t if NonFatal(t) => false }
                if (p.selected) {
                  if (dir) fw.listOp
                  else {
                    fw match {
                      case fbsw: FileWalkOnBuffers with FileWalkOnStreams =>
                        if (fw.file.length < sizeLimit) {
                          (new FileShouldDoThis(fw.file)).gulp match {
                            case Yes(b) => fbsw.buffer = b; fbsw.bufOp; fbsw.buffer = null
                            case No(n) => log(fw.file.getPath, "Failed to buffer file", n)
                          }
                        }
                        else {
                          val fis = new FileInputStream(fw.file)
                          try { fbsw.stream = fis; fbsw.streamOp }
                          catch { case t if NonFatal(t) => log(fw.file.getPath, "Failed to stream file", exceptionAsString(t)) }
                          finally { fis.close; fbsw.stream = null }
                        }
                      case fsw: FileWalkOnStreams =>
                        val fis = new FileInputStream(fw.file)
                        try { fsw.stream = fis; fsw.streamOp }
                        catch { case t if NonFatal(t) => log(fw.file.getPath, "Failed to stream file", exceptionAsString(t)) }
                        finally { fis.close; fsw.stream = null }
                      case fbw: FileWalkOnBuffers =>
                        (new FileShouldDoThis(fw.file)).gulp match {
                          case Yes(b) => fbw.buffer = b; fbw.bufOp; fbw.buffer = null
                          case No(n) => log(fw.file.getPath, "Failed to buffer file", n)
                        }
                      case _ => fw.listOp
                    }
                  }
                }
                if (p.recursed) {
                  if (dir) {
                    val children = try { fw.file.listFiles.sortBy(_.getName) } catch { case t if NonFatal(t) => log("Could not read files in "+fw.file.getName); Array[File]() }
                    var i = children.length
                    while (i > 0) {
                      i -= 1
                      pending = children(i) :: pending
                    }
                  }
                  else if (isZip(fw.file.getName.takeRight(4).toLowerCase)) {
                    try {
                      val fis = new FileInputStream(fw.file)
                      try {
                        val zis = new ZipInputStream(fis)
                        ziply(zis, Nil)
                        zis.close
                      }
                      catch { case t if NonFatal(t) => log("Could not recurse into file "+fw.file.getPath+" because "+exceptionAsString(t)) }
                      finally { fis.close }
                    }
                    catch { case t if NonFatal(t) => log("Could not recurse into file "+fw.file.getPath+" because "+exceptionAsString(t)) }
                  }
                  else log("Do not know how to recurse into " + fw.file.getPath)
                }
              case _ =>
            }
            inner()
          case Nil =>
        }
      }
      
      inner()
    }
    
    def tree(
      pick: FileWalk => Stance = Pick.files,
      canonize: Boolean = false
    ): Ok[Vector[String], Array[File]] = {
      val picked = Array.newBuilder[File]
      val log = FileLogger.vector
      walk(FileWalk.listed(pick, canonize)(picked += _.file), log)
      val v = log.result()
      if (v.nonEmpty) No(v) else Yes(picked.result())
    }

    def createParents() {
      var p = canon.getParentFile
      var ps = List.empty[File]
      while (p != null && !p.exists) { ps = p :: ps; p = p.getParentFile }
      ps.foreach(pi =>
        if (!pi.mkdir) throw new IOException("Unable to create parent directory " + pi.getPath)
      )
    }
    
    def copyTo(target: File) {
      val fis = new java.io.FileInputStream(underlying)
      try {
        val fos = new java.io.FileOutputStream(target)
        try { fos.getChannel.transferFrom(fis.getChannel, 0, Long.MaxValue ) }
        finally { fos.close }
      }
      finally { fis.close }
    }
    
    def slurpAll(p: String => Boolean, inZip: Boolean = false): Ok[Ok[Throwable,(Map[String,Vector[String]], Map[String,Vector[String]])], Map[String, Vector[String]]] = {
      val slurped = Vector.newBuilder[(String, Vector[String])]
      val errors = FileLogger.map
      safe{
        val pickAll = if (inZip) Pick.leaves else Pick.files
        val select: (FileWalk => Stance) = (fw) => pickAll(fw) match {
          case sel: Selected =>
            val keep = if (fw.zes.isEmpty) p(fw.file.getPath) else p(fw.file.getPath + fw.zes.mkString("//","//",""))
            if (keep) sel else Reject // No select + recurse allowed in this API
          case x => x
        }
        walk(FileWalk(select, true)(_ => ()){ fb =>
            slurped += ((fb.name, scala.io.Source.fromInputStream(new ByteArrayInputStream(fb.buffer)).getLines.toVector))
          }{ fis =>
            slurped += ((fis.name, scala.io.Source.fromInputStream(fis.stream).getLines.toVector))
          },
          errors,
          1048576 // 1M buffer should be plenty for efficiently grabbing small files
        )
        val em = errors.result
        if (em.isEmpty) Yes(slurped.result.toMap) else No((em, slurped.result.toMap))
      } match {
        case No(t) => No(No(t))
        case Yes(No(x)) => No(Yes(x))
        case Yes(Yes(y)) => Yes(y)
      }
    }
  }
  object FileShouldDoThis {
    val emptyFileArray = new Array[File](0)
  }
  

  implicit class PathShouldDoThis(private val underlying: Path) extends AnyVal {
    import PathShouldDoThis._

    def name = underlying.getFileName.toString
    def nameFn(f: String => String) = underlying resolveSibling f(underlying.getFileName.toString)
    def ext = {
      val n = underlying.getFileName.toString
      val i = n.lastIndexOf('.')
      if (i < 1) "" else n.substring(i+1)
    }
    def extFn(f: String => String) = {
      val n = underlying.getFileName.toString
      val i = n.lastIndexOf('.')
      val e = if (i < 1) "" else n.substring(i+1)
      val x = f(e)
      if (x == e) underlying
      else if (i < 1) underlying resolveSibling n+"."+x
      else if (x.isEmpty) underlying resolveSibling n.substring(0, i)
      else underlying resolveSibling n.substring(0, i+1)+x
    }
    def base = {
      val n = underlying.getFileName.toString
      val i = n.lastIndexOf('.')
      if (i < 1) n else n.substring(0, i)
    }
    def baseFn(f: String => String) = {
      val n = underlying.getFileName.toString
      val i = n.lastIndexOf('.')
      val b = if (i < 1) n else n.substring(0, i)
      val x = f(b)
      if (b == x) underlying
      else if (i < 1) underlying resolveSibling x
      else underlying resolveSibling x+n.substring(i)
    }
    def parentName = underlying.getParent match { case null => ""; case p => p.getFileName.toString }
    def namesIterator = Iterator.iterate(underlying)(_.getParent).takeWhile(_ != null).map(_.getFileName.toString)
    def pathsIterator = Iterator.iterate(underlying)(_.getParent).takeWhile(_ != null)   
    def parentOption = Option(underlying.getParent)
    def real = underlying.toRealPath()
    def file = underlying.toFile

    def /(that: String) = underlying resolve that
    def /(that: Path) = underlying resolve that
    def `..` = underlying.getParent match { case null => underlying; case p => p }
    def sib(that: String) = underlying resolveSibling that
    def sib(that: Path) = underlying resolveSibling that
    def reroot(oldRoot: Path, newRoot: Path): Option[Path] =
      if (underlying startsWith oldRoot) Some(newRoot resolve oldRoot.relativize(underlying))
      else None
    def reroot(roots: (Path, Path)): Option[Path] = reroot(roots._1, roots._2)
    def prune(child: Path): Option[Path] =
      if (child startsWith underlying) Some(underlying relativize child)
      else None

    def exists = Files exists underlying
    def isDirectory = Files isDirectory underlying
    def isSymbolic = Files isSymbolicLink underlying
    def size = Files size underlying

    def safely = new PathShouldSafelyDoThis(underlying)

    def t: FileTime = Files getLastModifiedTime underlying
    def t_=(ft: FileTime) {
      Files.setLastModifiedTime(underlying, ft)
    }
    def mkdir() = Files createDirectory underlying
    def delete() = Files delete underlying
    def touch() {
      if (Files exists underlying) Files.setLastModifiedTime(underlying, FileTime from Instant.now)
      else Files.write(underlying, new Array[Byte](0))
    }

    def paths =
      if (!(Files exists underlying)) emptyPathArray
      else if (!(Files isDirectory underlying)) emptyPathArray
      else safe{ val list = Files.list(underlying); val ans = list.toArray(i => new Array[Path](i)); list.close; ans }.yesOr(_ => emptyPathArray)

    def slurp: Ok[String, Vector[String]] = safe{
      val s = Files lines underlying
      val vb = Vector.newBuilder[String]
      s.forEach(vb += _)
      s.close
      vb.result
    }.mapNo(_.explain())

    def gulp: Ok[String, Array[Byte]] = safe{
      Files readAllBytes underlying
    }.mapNo(_.explain())

    def copyTo(to: Path) {
      Files.copy(underlying, to, StandardCopyOption.REPLACE_EXISTING)
    }

    def moveTo(to: Path) {
      Files.move(underlying, to, StandardCopyOption.REPLACE_EXISTING)
    }

    def atomicCopy(to: Path) {
      val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
      to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
      Files.copy(underlying, temp, StandardCopyOption.REPLACE_EXISTING)
      Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    }

    def atomicMove(to: Path) {
      val up = to.getParent
      if (up != null) {
        if (!Files.exists(up)) Files.createDirectories(up)
      }
      if (up != null && Files.getFileStore(underlying) == Files.getFileStore(up))
        Files.move(underlying, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      else {        
        val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
        to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
        Files.copy(underlying, temp, StandardCopyOption.REPLACE_EXISTING)
        Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
        Files.delete(underlying)
      }
    }

    def atomicZipCopy(to: Path, compression: Option[Int] = None, maxDirectoryDepth: Int = 10) {
      val temp = to.resolveSibling(to.getFileName.toString + ".atomic")
      to.getParent.tap{ gp => if (gp ne null) { if (!Files.exists(gp)) Files.createDirectories(gp) }}
      val zos = new ZipOutputStream(new FileOutputStream(temp.toFile))
      compression.foreach(zos.setLevel)
      if (Files.isDirectory(underlying)) {
        val base = underlying.getParent.pipe{ fp => if (fp eq null) "".file.toPath else fp }
        def recurse(current: Path, maxDepth: Int) {
          val stable = (new PathShouldDoThis(current)).paths
          val (directories, files) = stable.sortBy(_.getFileName.toString).partition(x => Files.isDirectory(x))
          files.foreach{ fi =>
            val rel = base relativize fi
            val ze = new ZipEntry(rel.toString)
            ze.setLastModifiedTime(Files.getLastModifiedTime(fi))
            zos.putNextEntry(ze)
            Files.copy(fi, zos)
            zos.closeEntry
          }
          if (maxDepth > 1) directories.foreach(d => recurse(d, maxDepth-1))
        }
        recurse(underlying, maxDirectoryDepth)
      }
      else {
        val ze = new ZipEntry(underlying.getFileName.toString)
        ze.setLastModifiedTime(Files.getLastModifiedTime(underlying))
        zos.putNextEntry(ze)
        Files.copy(underlying, zos)
        zos.closeEntry
      }
      zos.close
      Files.move(temp, to, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    }

    def recursively = new RootedRecursion(underlying, underlying)

    def recurseIn(inside: Path) =
      if (underlying startsWith inside) new RootedRecursion(inside, underlying)
      else throw new IOException(s"Trying recursive operation in $inside but started outside at $underlying")
  }
  object PathShouldDoThis {
    val emptyPathArray = new Array[Path](0)
    val doNothingHook: Path => Unit = _ => ()

    class RootedRecursion(val root: Path, val origin: Path) {
      def delete(hook: Path => Unit = doNothingHook) = recursiveDelete(origin, root, hook)
      def atomicDelete(hook: Path => Unit = doNothingHook) = atomicRecursiveDelete(origin, root, hook)
    }

    private[PathShouldDoThis] def recursiveDelete(f: Path, root: Path, hook: Path => Unit = _ => ()) {
      if (f startsWith root) {
        if (Files.isDirectory(f) && !Files.isSymbolicLink(f)) {
          (new PathShouldDoThis(f)).paths.foreach(fi => recursiveDelete(fi, root, hook))
        }
        hook(f)
        Files delete f
      }
      else throw new IOException(s"Tried to delete $f but escaped root path $root")
    }

    private[PathShouldDoThis] def atomicRecursiveDelete(f: Path, root: Path, hook: Path => Unit = _ => ()) {
      if (f startsWith root) {
        val name = f.getFileName.toString
        val i = name.lastIndexOf('.')
        val delext = if (i > 0) name.substring(i+1) else ""
        val delnum = 
          if (!delext.startsWith("deleted")) None
          else if (delext.length > "deleted".length + 8) None
          else if (delext.length > "deleted".length) {
            val more = delext.substring("deleted".length)
            if (!more.forall(_.isDigit)) None
            else safe{ more.toInt }.toOption
          }
          else Some(1)
        val delname = delnum match {
          case Some(n) => name.substring(0, i) + ".deleted" + (n+1).toString
          case _ => name + ".deleted"
        }
        val del = f.resolveSibling(delname).normalize
        if (Files exists del) atomicRecursiveDelete(del, root, hook)
        Files.move(f, del, StandardCopyOption.ATOMIC_MOVE)
        val drp = del.toRealPath()
        recursiveDelete(drp, drp, hook)
      }
      else throw new IOException(s"Tried to delete $f but escaped root path $root")
    }
  }

  class PathShouldSafelyDoThis private[eio] (private val underlying: Path) extends AnyVal {
    def exists =
      try { Files exists underlying }
      catch { case e if NonFatal(e) => false }
    def isDirectory =
      try { Files isDirectory underlying }
      catch { case e if NonFatal(e) => false }
    def isSymbolic =
      try { Files isSymbolicLink underlying }
      catch { case e if NonFatal(e) => false }
    def size =
      try { 
        if (Files exists underlying) Files size underlying
        else -1L
      }
      catch { case e if NonFatal(e) => -1L }
  }


  implicit class ConvenientFileOutput(private val underlying: TraversableOnce[String]) extends AnyVal {
    def toFile(f: File, lineEnding: String = null) {
      val p = new java.io.PrintWriter(f)
      try { if (lineEnding == null) underlying.foreach(p.println) else underlying.foreach(x => p.print(x + lineEnding)) } finally { p.close() }
    }
    /** Atomically replaces the file.
      *
      * It is guaranteed not to be corrupted as long as this operation is not run concurrently.
      *
      * Returns `true` if the update is successful (whether anything changed on disk or not)
      */
    def atomicallyReplace(f: File, lineEnding: String = null): Ok[String, Unit] = {
      if (f.exists && underlying.isTraversableAgain) {
        f.slurp match {
          case Yes(lines) =>
            val i = lines.iterator
            if (underlying.forall(line => i.hasNext && i.next == line)) return Ok.UnitYes   // Didn't change anything
          case _ =>
        }
      }
      val fnew = new File(f.getPath + ".atomic-new")
      if (fnew.exists) return No("New file for writing, "+fnew.getPath+", already exists")
      safe{ toFile(fnew, lineEnding) } match {
        case No(t) => return No("Could not write file "+fnew.getPath+": "+t.getClass.getName)
        case _ =>
      }
      val fexisted = 
        if (f.exists) {
          val fold = new File(f.getPath + ".atomic-old")
          if (fold.exists) return No("Wrote new file but could not move old version because "+fold.getPath+" already exists")
          safe{ f renameTo fold } match {
            case Yes(false) => return No("For unknown reason, could not move old version to "+fold.getPath)
            case No(t) => return No("Could not move old version to "+fold.getPath+" because of "+exceptionAsString(t))
            case _ =>
          }
          Some(fold)
        }
        else None
      safe{ fnew renameTo f } match {
        case Yes(false) => return No("For unknown reason, could not move new version to "+f.getPath)
        case No(t) => return No("Could not move new version to "+f.getPath+" because of "+exceptionAsString(t))
        case _ =>
      }
      safe{ fexisted.foreach(_.delete) }
      Ok.UnitYes
    }
  }
  
  implicit class ConvenientPathOutput(private val underlying: TraversableOnce[String]) extends AnyVal {
    def toFile(f: Path, lineEnding: String = null) {
      val p = new java.io.PrintWriter(Files newOutputStream f)
      try { if (lineEnding == null) underlying.foreach(p.println) else underlying.foreach(x => p.print(x + lineEnding)) } finally { p.close() }
    }
    /** Atomically replaces the file.
      *
      * It is guaranteed not to be corrupted as long as this operation is not run concurrently.
      *
      * Returns `true` there were any changes, `false` if not
      */
    def atomicallyReplace(f: Path, lineEnding: String = null): Ok[String, Boolean] = {
      val replacing = Files exists f
      if (replacing && underlying.isTraversableAgain) {
        f.slurp match {
          case Yes(lines) =>
            val i = lines.iterator
            if (underlying.forall(line => i.hasNext && i.next == line)) return Yes(false)   // Didn't change anything
          case _ =>
        }
      }
      val fnew = f resolveSibling f.getFileName.toString + ".atomic-new"
      if (Files exists fnew) return No(s"New file for writing, $fnew, already exists")
      safe {
        if (!replacing || underlying.isTraversableAgain) toFile(fnew, lineEnding)
        else f.slurp match {
          case Yes(lines) =>
            var identical = true
            val i = lines.iterator
            val p = new java.io.PrintWriter(Files newOutputStream fnew)
            try {
              if (lineEnding == null) underlying.foreach{ line =>
                identical = identical && i.hasNext && i.next == line
                p.println(line)
              }
              else underlying.foreach{ line =>
                identical = identical && i.hasNext && i.next == line
                p.print(line + lineEnding)
              }
            }
            finally { p.close() }
            if (identical) return safe { Files delete fnew }.mapNo(_.explain()).map(_ => false)
          case _ => toFile(fnew, lineEnding)
        }
      } match {
        case No(t) => return No(s"Could not write file $fnew: ${t.getClass.getName}")
        case _ =>
      }
      val fexisted = 
        if (replacing) {
          val fold = f resolveSibling f.getFileName.toString + ".atomic-old"
          if (Files exists fold) return No(s"Wrote new file but could not move old version because $fold already exists")
          safe{ Files.move(f, fold) } match {
            case No(t) => return No(s"Could not move old version to $fold because of ${exceptionAsString(t)}")
            case _ =>
          }
          Some(fold)
        }
        else None
      safe{ Files.move(fnew, f) } match {
        case No(t) => return No(s"Could not move new version to $f because of ${exceptionAsString(t)}")
        case _ =>
      }
      safe{ fexisted.foreach(Files delete _) }
      Yes(true)
    }
  }
  
  private[eio] def exceptionAsString(t: Throwable) = t.getClass.getName + ": " + Option(t.getMessage).getOrElse("") + "; " + t.getStackTrace.take(2).mkString("; ")
}

package eio {
  import java.io._
  import java.util.zip._
  
  /** Note: the minimum chunk size is 256 */
  class InputStreamStepper(is: InputStream, chunkSize: Int) extends Walker[Array[Byte]] {
    private var buf = new Array[Byte](math.max(256, chunkSize))
    private var isEmpty = false
    def step(f: Array[Byte] => Unit) = {
      if (isEmpty) false
      else {
        var i, k = 0
        var zeros = 0
        while (i < buf.length && zeros < 4 && k >= 0) {
          k = is.read(buf, i, buf.length - i)
          if (k > 0) {
            zeros = 0
            i += k
          }
          else zeros += 1
        }
        if (k < 0) isEmpty = true
        if (i <= 0) false
        else {
          f(java.util.Arrays.copyOf(buf,i))
          true
        }
      }
    }
  }
  
  class SteppedByteArrayInputStream(walker: Walker[Array[Byte]]) extends InputStream {
    private[this] var working: Array[Byte] = null
    private[this] var taken: Int = 0
    private[this] var exhausted = false
    private[this] def prepared(): Boolean = {
      if (!exhausted) {
        if (working == null || taken >= working.length) {
          taken = 0
          exhausted = !walker.step(working = _)
          prepared()
        }
        else true
      }
      else { working = null; false }
    }
    override def available() = if (exhausted || working == null) 0 else (working.length - taken)
    override def close() {}
    override def mark(readlimit: Int) {}
    override def markSupported = false
    override def read(): Int = { if (prepared()) { var ans = working(taken) & 0xFF; taken += 1; ans } else -1 }
    override def read(b: Array[Byte]): Int = read(b, 0, b.length)
    override def read(b: Array[Byte], off: Int, len: Int): Int = if (exhausted) -1 else if (len <= 0) 0 else {
      var m = len
      var o = off
      while (prepared()) {
        val k = math.min(working.length - taken, m)
        System.arraycopy(working, taken, b, o, k)
        o += k
        taken += k
        m -= k
        if (m <= 0) return len
      }
      if (len == m) -1 else len - m
    }
    override def reset() {}
    override def skip(n: Long): Long = if (exhausted) -1 else if (n <= 0) 0 else {
      var m = n
      while (prepared()) {
        if (m < working.length - taken) {
          taken = m.toInt
          return n
        }
        else m -= (working.length - taken)
      }
      if (m >= n) -1 else n - m
    }
  }
  
  
  sealed trait Stance { def selected: Boolean = false; def recursed: Boolean = false }
  sealed trait Recursed extends Stance { override def recursed = true }
  sealed trait Selected extends Stance { override def selected = true }
  case object Reject extends Stance
  case object Recurse extends Recursed { def &(s: Select.type) = RecurseSelect }
  case object Select extends Selected { def &(r: Recurse.type) = RecurseSelect }
  case object RecurseSelect extends Recursed with Selected {}
  
  object Pick {
    val files: FileWalk => Stance = fw => {
      if (fw.zes.nonEmpty) Reject
      else {
        // (Some versions of?) Windows has an infuriating habit of making root directories hidden.  Ugly hack to fix this.
        if (fw.file.isHidden && (File.separatorChar != '\\' || { val af = fw.file.getAbsoluteFile; af.getParentFile != null })) Reject
        else if (fw.file.isDirectory) Recurse
        else Select
      }
    }
    
    val leaves: FileWalk => Stance = fw => fw.zes match {
      case Nil =>
        files(fw) match {
          case Select if fw.file.getName.toLowerCase.endsWith("zip") => Recurse
          case x => x
        }
        case ze :: more =>
          if (ze.isDirectory) Reject
          else if (ze.getName.toLowerCase.endsWith("zip")) Recurse
          else Select
    }
    
    def toDepth(depth: Int): FileWalk => Stance = fw => leaves(fw) match {
      case Recurse if fw.zes.lengthCompare(depth) <= 0 && fw.zes.headOption.exists(_.getName.toLowerCase.endsWith("zip")) => Select
      case x => x
    }
    
    def filesNamed(p: String => Boolean): FileWalk => Stance = fw => files(fw) match {
      case Select if (!p(fw.file.getName)) => Reject
      case x => x
    }
    def leavesNamed(p: String => Boolean): FileWalk => Stance = fw => leaves(fw) match {
      case Select if (!p(fw.zes.headOption.map(_.getName).getOrElse(fw.file.getName))) => Reject
      case x => x
    }
    
    def filesFiltered(p: File => Boolean): FileWalk => Stance = fw => files(fw) match {
      case Select if (!p(fw.file)) => Reject
      case x => x
    }
  }
  
  trait FileLogger extends (String => Unit) {
    protected def explain(where: String, what: String, reason: String) = s"$what in $where because $reason"
    def apply(where: String, what: String, reason: String) { apply(explain(where, what, reason)) }
    def apply(s: String): Unit
  }
  trait FileLogsTo[A] extends FileLogger {
    def result(): A
  }
  object FileLogger {
    def vector: FileLogsTo[Vector[String]] = new FileLogsTo[Vector[String]] {
      private[this] val myLog = Vector.newBuilder[String]
      def apply(s: String) { myLog += s }
      def result() = myLog.result()
    }
    def map: FileLogsTo[Map[String, Vector[String]]] = new FileLogsTo[Map[String, Vector[String]]] {
      private type Vb = collection.mutable.Builder[String, Vector[String]]
      private[this] val myLog = new collection.mutable.AnyRefMap[String, Vb]
      def apply(s: String) { myLog.getOrElseUpdate("", Vector.newBuilder[String]) += s }
      override def apply(where: String, what: String, reason: String) { myLog.getOrElseUpdate(where, Vector.newBuilder[String]) += explain(where, what, reason) }
      def result() = {
        val mb = Map.newBuilder[String, Vector[String]]
        myLog.foreach{ case (k,v) => mb += ((k, v.result())) }
        myLog.clear()
        mb.result()
      }
    }
  }
  
  sealed trait FileWalker { def name: String }
  sealed abstract class FileWalk extends FileWalker {
    def file: File
    def zes: List[ZipEntry]
    def name: String = if (zes.isEmpty) file.getPath else file.getPath + zes.mkString("//","//","")
  }
  sealed trait FileBufferWalk extends FileWalk {
    def buffer: Array[Byte]
  }
  sealed trait FileStreamWalk extends FileWalk {
    def stream: InputStream
  }
  private[eio] sealed abstract class FileWalkImpl extends FileWalk {
    var file: File = null
    var zes: List[ZipEntry] = Nil
    def picker: Stance
    def canonized: Boolean
    def listOp: Unit
  }
  private[eio] sealed trait FileWalkOnBuffers extends FileWalkImpl with FileBufferWalk {
    var buffer: Array[Byte] = null
    def bufOp: Unit
  }
  private [eio] sealed trait FileWalkOnStreams extends FileWalkImpl with FileStreamWalk {
    var stream: InputStream = null
    def streamOp: Unit
  }
  object FileWalk {
    def listed(pick: FileWalk => Stance, canonize: Boolean = false)(f: FileWalk => Unit): FileWalker = new FileWalkImpl {
      def picker = pick(this)
      def canonized = canonize
      def listOp { f(this) }
    }
      
    def blocked(pick: FileWalk => Stance, canonize: Boolean = false)(fdir: FileWalk => Unit)(fbuf: FileBufferWalk => Unit): FileWalker = new FileWalkOnBuffers {
      def picker = pick(this)
      def canonized = canonize
      def listOp { fdir(this) }
      def bufOp { fbuf(this) }
    }
    
    def streamed(pick: FileWalk => Stance, canonize: Boolean = false)(fdir: FileWalk => Unit)(fis: FileStreamWalk => Unit): FileWalker = new FileWalkOnStreams {
      def picker = pick(this)
      def canonized = canonize
      def listOp { fdir(this) }
      def streamOp{ fis(this) }
    }
    
    def apply(pick: FileWalk => Stance, canonize: Boolean = false)(fdir: FileWalk => Unit)(fbuf: FileBufferWalk => Unit)(fis: FileStreamWalk => Unit): FileWalker = new FileWalkOnBuffers with FileWalkOnStreams {
      def picker = pick(this)
      def canonized = canonize
      def listOp { fdir(this) }
      def bufOp { fbuf(this) }
      def streamOp { fis(this) }
    }    
  }

  object Args {
    class OptionSource(val options: Array[String]) {
      val ops = collection.mutable.AnyRefMap[String, List[String]]()
      options.reverse.foreach(o => (if (o.startsWith("--")) o.drop(2) else o.drop(1)) fn { s =>
        val i = s.indexOf('=')
        if (i < 0) ops += s -> ("" :: ops.getOrElse(s, Nil))
        else {
          val k = s.take(i)
          ops += k -> (s.drop(i+1) :: ops.getOrElse(k, Nil))
        }
      })
      def has(s: String): Boolean = {
        ops.get(s) match {
          case Some(x :: more) => if (more.isEmpty) ops -= s else ops += s -> more; true
          case _ => false
        }
      }
      def get(s: String): Option[String] = {
        ops.get(s).flatMap{ _ match {
          case x :: more => if (more.isEmpty) ops -= s else ops += s -> more; Some(x)
          case _ => None
        }}
      }
      def peek = ops.toArray.flatMap{ case (k,v) => v.map(x => if (x.nonEmpty) "-" + k + "=" + x else "-" + k) }.sorted
      def isEmpty = ops.forall{ case (k,v) => v.isEmpty }
    }
    
    def apply(args: Array[String]): (Array[String], OptionSource) = {
      val i = args.indexWhere(_ == "--")
      if (i < 0) args.partition(a => !a.startsWith("-")) _2Fn ( x => new OptionSource(x) )
      else args.take(i).partition(a => !a.startsWith("-")) eachFn (_ ++ args.drop(i+1), x => new OptionSource(x))
    }
  }

  object Text {
    def wrapLine(line: String, width: Int, wrapIndicator: String = ""): List[String] = {
      val w = math.max(width, 1)
      val hw = w/2
      if (line.length == 0) Nil
      else if (line.length <= w) line :: Nil
      else if (w == 1) line.map(c => c.toString).toList
      else {
        var parens, brackets, braces = 0L
        var quotes = false
        var white = false
        val score = new Array[Short](line.length)
        var i = 0
        while (i < line.length) {
          line.charAt(i) match {
            case '(' => parens += 1
            case '[' => brackets += 1
            case '{' => braces += 1
            case '}' => braces = math.max(0, braces-1)
            case ']' => brackets = math.max(0, brackets-1)
            case ')' => parens = math.max(0, parens-1)
            case '"' => quotes = !quotes
            case c => white = c.isWhitespace
          }
          score(i) = ((if (white) 0 else 1) + (if (quotes) 2 else 0) + 4*(parens.toLong + brackets + braces)).clip(0, Short.MaxValue).toShort
          i += 1
        }
        var x = 0
        val cuts = List.newBuilder[Int]
        while (x < line.length) {
          var i = math.min(x + w - 1, line.length - 1)
          if (i < line.length - 1) {
            i -= math.min(wrapIndicator.length, hw)
            if (score(i) != 0) {
              var sc: Double = score(i)
              var ix = i
              i -= 1
              while (i > x + hw) {
                score(i) match {
                  case 0 => ix = i; i = x  // Early termination, we can't do better than this
                  case s if s + 1e-3*(ix - i) < sc => ix = i; sc = s
                  case _ =>
                }
                i -= 1
              }
              i = ix
            }
          }
          cuts += i
          x = i+1
        }
        x = 0
        cuts.result().map{ i => val ans = line.substring(x, i+1); x = i+1; if (i+1 < line.length) ans + wrapIndicator else ans }
      }
    }

    def block(label: String, content: Seq[String], lmargin: Int, rmargin: Int = 79, wrapIndicator: String = "", mergeShort: String = null): Seq[String] = {
      if (content.length == 0) {
        if (label.nonEmpty) content :+ label
        else content
      }
      else {
        val iL = lmargin max 0
        val iR = iL max rmargin
        val N = 1 + iR - iL
        val cN = content.map(_.length).sum
        if ((content.length == 1 && cN <= N) || (mergeShort != null && cN + (content.length-1)*mergeShort.length <= N)) {
          val single = (if (content.length == 1) content else content.take(0) :+ content.mkString(mergeShort))
          if (label.isEmpty) single.map(x => " "*iL + x)
          else if (label.length < iL) single.map(x => label + " "*(iL - label.length) + x)
          else label +: single.map(x => " "*iL + x)
        }
        else {
          val multi = content.flatMap{ line => 
            if (line.length <= N) line :: Nil
            else wrapLine(line, N, wrapIndicator)
          }
          val whites = " "*iL
          if (label.isEmpty || label.length < iL) (label + " "*(iL - label.length) + multi.head) +: multi.tail.map(x => whites + x)
          else label +: multi.map(x => whites + x)
        }
      }
    }

    def deblock(
      lines: Seq[String], line0: Int,
      lmargin: Int,
      wrapIndicator: String = "", mergeShort: String = null,
      reportNextIndex: Int => Unit = _ => ()
    ): Ok[String, (String, Seq[String])] = {
      val lit = (lines.iterator drop line0).buffered
      if (!lit.hasNext) return No("Empty input")
      if (lit.head.isEmpty) return No("Input line empty")
      if (lit.head.charAt(0).isWhitespace) return No("Empty key")
      if (lmargin <= 0) return No("Blocks must have positive margin")

      val keyAlone = (lit.head.length < lmargin || lit.head(lmargin - 1) != ' ')
      var key = if (keyAlone) lit.next.trim else ""

      val ls = lit.map{ x =>
        val (pre, post) = x.splitAt(lmargin)
        if (key.nonEmpty && (pre.isEmpty || pre(0) != ' ')) null
        else {
          if (key.isEmpty) key = pre.trim
          else {
            var i = 0
            while (i < pre.length && pre(i) == ' ') i += 1
            if (i < pre.length) return No(s"Indentation wrong depth (expected $lmargin)\n$pre\n${" "*i}\n")            
          }
          post
        }
      }.takeWhile(_ != null).toArray

      val lb = lines.genericBuilder[String]

      if (ls.length == 1 && mergeShort != null && mergeShort.nonEmpty) ls(0).split(mergeShort).foreach(lb += _)
      else if (wrapIndicator.isEmpty) ls.foreach(lb += _)
      else {
        var i = 0
        while (i < ls.length) {
          if (ls(i).endsWith(wrapIndicator)) {
            var j = i + 1
            while (j < ls.length && ls(j).endsWith(wrapIndicator)) j += 1
            if (j >= ls.length) return No("Last line of block has a wrap indicator\n"+ls.last+"\n"+(" "*(ls.length-1))+"^\n")
            var sb = new StringBuilder
            while (i < j) {
              val l = ls(i)
              sb ++= l.dropRight(wrapIndicator.length)
              i += 1
            }
            sb ++= ls(j)
            lb += sb.result()
            i = j+1
          }
          else {
            lb += ls(i)
            i += 1
          }
        }
      }

      reportNextIndex(line0 + ls.length + (if (keyAlone) 1 else 0))
      Yes(key -> lb.result())
    }
  }
}
