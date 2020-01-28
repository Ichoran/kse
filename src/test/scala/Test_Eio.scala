package kse.tests

import kse.eio._

import java.time._
import java.nio.file.attribute.FileTime
import java.util.concurrent.TimeUnit

import kse.flow._

object Test_Eio extends Test_Kse {
  def test_durations(): Boolean = {
    5.duration.nanos  == Duration.ofNanos(  5) &&
    6.duration.millis == Duration.ofMillis( 6) &&
    7.duration.secs   == Duration.ofSeconds(7) &&
    8.duration.mins   == Duration.ofMinutes(8) &&
    9.duration.hours  == Duration.ofHours(  9) &&
    4.duration.days   == Duration.ofDays(   4)
  }

  def test_double_durations(): Boolean = {
    5.nanos.  toDuration == Duration.ofNanos(  5) &&
    6.millis. toDuration == Duration.ofMillis( 6) &&
    7.secs.   toDuration == Duration.ofSeconds(7) &&
    8.minutes.toDuration == Duration.ofMinutes(8) &&
    9.hours.  toDuration == Duration.ofHours(  9) &&
    4.days.   toDuration == Duration.ofDays(   4)
  }

  def test_double_time_operations(): Boolean = {
    15.nanos < 16.nanos && !(15.nanos < 15.nanos) && !(15.nanos < (-30).nanos) &&
    15.nanos <= 16.nanos && 15.nanos <= 15.nanos && !(15.nanos <= (-30).nanos) &&
    !(15.nanos > 16.nanos) && !(15.nanos > 15.nanos) && 15.nanos > (-30).nanos &&
    !(15.nanos >= 16.nanos) && 15.nanos >= 15.nanos && 15.nanos >= (-30).nanos &&
    -(1851.nanos) =~= (-1851).nanos &&
    (-1851).nanos.abs =~= 1851.nanos &&
    (15.millis max 1851815.nanos) =~= 15.millis &&
    (1851815.nanos max 15.millis) =~= 15.millis &&
    (15.millis min 1851815.nanos) =~= 1851815.nanos &&
    (1851815.nanos min 15.millis) =~= 1851815.nanos &&
    5.nanos * 1000000 =~= 5.millis &&
    3600000 * 14.millis =~= 14.hours &&
    10.secs + 5.millis =~= 10005.millis &&
    10.secs + Duration.ofMillis(5) =~= 10005.millis &&
    10.secs / 8 =~= 1250.millis &&
    10.secs - 5.millis =~= 9995.millis &&
    10.secs - Duration.ofMillis(-5) =~= 10005.millis &&
    10.secs.timeFn(t => 2*t + 15.5) =~= 35500.millis &&
    0.017499.asTime.roundMillis =~= 17.millis && 0.017499.asTime.floorMillis =~= 17.millis &&
    0.01750001.asTime.roundMillis =~= 18.millis && 0.01750001.asTime.floorMillis =~= 17.millis &&
    0.0175.asTime.roundMillis =~= 18.millis &&
    (-0.017499).asTime.roundMillis =~= -17.millis && (-0.017499).asTime.floorMillis =~= -18.millis &&
    (-0.01750001).asTime.roundMillis =~= -18.millis &&
    (-0.0175).asTime.roundMillis =~= -18.millis &&
    17.5.asTime.roundSecs =~= 18.secs && 17.5.asTime.floorSecs =~= 17.secs &&
    1050.asTime.roundMins =~= 18.minutes && 1050.asTime.floorMins =~= 17.minutes &&
    63000.asTime.roundHours =~= 18.hours && 63000.asTime.floorHours =~= 17.hours &&
    1512000.asTime.roundDays =~= 18.days && 1512000.asTime.floorDays =~= 17.days &&
    11.millis.toNanos == 11000000L &&
    11.secs.toMillis == 11000L &&
    11.minutes.toSecs == 660L &&
    11.hours.toMins == 660L &&
    11.days.toHours == 264L &&
    11.nanos.toDays == 0L  &&
    (15.minutes + 4044.millis).toString == "904.044 sec" &&
    List(11.minutes, 51.nanos, -198519.secs, 1.day).sorted == List(-198519.secs, 51.nanos, 11.minutes, 1.day)
  }

  def test_double_time_with_java_time(): Boolean = {
    val d = Duration.ofDays(14) plus Duration.ofHours(21) plus Duration.ofMinutes(4) plus Duration.ofSeconds(41) plus Duration.ofMillis(115) plus Duration.ofNanos(815891)
    val i = Instant.now
    val l = LocalDateTime.now
    val z = ZonedDateTime.now
    val o = OffsetDateTime.now
    val f = FileTime.from(i)
    val t = 2951235918.115.asTime
    val dt = 15.minutes

    d.toDouble.toDuration == d &&
    i.epoch.epochInstant == i &&
    f.epoch.epochFileTime == f &&
    t.toDuration.toDouble == t &&
    t.epochInstant.epoch == t &&
    t.epochFileTime.epoch == t &&
    Duration.between(i, i + dt) == Duration.ofMinutes(15) &&
    Duration.between(l, l + dt) == Duration.ofMinutes(15) &&
    Duration.between(z, z + dt) == Duration.ofMinutes(15) &&
    Duration.between(o, o + dt) == Duration.ofMinutes(15) &&
    (f + dt).toMillis - f.toMillis == 15*60*1000
  }

  def test_duration(): Boolean = {
    val dt = Duration.ofMinutes(15)
    val i = Instant.now
    val l = LocalDateTime.now
    val z = ZonedDateTime.now
    val o = OffsetDateTime.now
    val f = FileTime.from(i)
    Duration.ofNanos(15) < Duration.ofNanos(16) && !(Duration.ofNanos(15) < Duration.ofNanos(15)) && !(Duration.ofNanos(15) < Duration.ofNanos(-30)) &&
    Duration.ofNanos(15) <= Duration.ofNanos(16) && Duration.ofNanos(15) <= Duration.ofNanos(15) && !(Duration.ofNanos(15) <= Duration.ofNanos(-30)) &&
    !(Duration.ofNanos(15) > Duration.ofNanos(16)) && !(Duration.ofNanos(15) > Duration.ofNanos(15)) && Duration.ofNanos(15) > Duration.ofNanos(-30) &&
    !(Duration.ofNanos(15) >= Duration.ofNanos(16)) && Duration.ofNanos(15) >= Duration.ofNanos(15) && Duration.ofNanos(15) >= Duration.ofNanos(-30) &&
    Duration.ofDays(-5).abs == Duration.ofDays(5) &&
    Duration.ofSeconds(3).abs == Duration.ofSeconds(3) &&
    Duration.ofNanos(17684382L).roundMillis =?= Duration.ofMillis(18) &&
    Duration.ofNanos(17684382L).floorMillis =?= Duration.ofMillis(17) &&
    Duration.ofMillis(1582).roundSeconds =?= Duration.ofSeconds(2) &&
    Duration.ofMillis(1582).floorSeconds =?= Duration.ofSeconds(1) &&
    (Duration.ofMillis(1512) min Duration.ofSeconds(2)) == Duration.ofMillis(1512) &&
    (Duration.ofMillis(1512) max Duration.ofSeconds(2)) == Duration.ofSeconds(2) &&
    (Duration.ofSeconds(2) min Duration.ofMillis(1512)) == Duration.ofMillis(1512) &&
    (Duration.ofSeconds(2) max Duration.ofMillis(1512)) == Duration.ofSeconds(2) &&
    Duration.ofSeconds(5) + Duration.ofMillis(1818) == Duration.ofNanos(6818000000L) &&
    Duration.ofSeconds(5) - Duration.ofMillis(1818) == Duration.ofNanos(3182000000L) &&
    Duration.ofMillis(18) * 5 == Duration.ofMillis(90) &&
    5 * Duration.ofMillis(18) == Duration.ofMillis(90) &&
    Duration.between(i, i + dt) == dt &&
    Duration.between(l, l + dt) == dt &&
    Duration.between(z, z + dt) == dt &&
    Duration.between(o, o + dt) == dt &&
    (f + dt).toMillis - f.toMillis == 15*60*1000
  }

  def test_instant(): Boolean = {
    val d = Duration.ofMillis(1581)
    val t = 1.58131.asTime
    val i = Instant.now
    val l = LocalDateTime.now
    val o = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    val z = ZonedDateTime.now
    val f = FileTime from i
    val i2 = Instant.now
    (i + d) - i == d &&
    (i + t) - i == t.toDuration &&
    (i.roundMillis + t).roundMillis - i.roundMillis == d && (i.floorMillis + t).floorMillis - i.floorMillis =?= d &&
    (i.roundSeconds + d).roundSeconds =?= i.roundSeconds.plus(Duration ofSeconds 2) &&
    (i.floorSeconds + d).floorSeconds =?= i.floorSeconds.plus(Duration ofSeconds 1) &&
    i < i+d && !(i < i) && !(i+t < i) &&
    i <= i+d && i <= i && !(i+t <= i) &&
    !(i > i+d) && !(i > i) && i+t > i &&
    !(i >= i+d) && i >= i && i+t >= i &&
    (i min i+d) == i && (i+d min i) == i &&
    (i max i+d) == i+d && (i+d max i) == i+d &&
    i2.epoch >= i.epoch &&
    l >= i.local && i2.local >= l &&
    o >= i.utc && i2.utc >= o &&
    z >= i.zoned && i2.zoned >= z &&
    f >= i.filetime && i2.filetime >= f
  }

  def test_localdatetime(): Boolean = {
    val d = Duration.ofMillis(1581)
    val t = 1.58131.asTime
    val l = LocalDateTime.now
    val i = Instant.now
    val o = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    val z = ZonedDateTime.now
    val f = FileTime from i
    val l2 = LocalDateTime.now
    (l + d) - l == d &&
    (l + t) - l == t.toDuration &&
    (l.roundMillis + t).roundMillis - l.roundMillis == d && (l.floorMillis + t).floorMillis - l.floorMillis == d &&
    (l.roundSeconds + d).roundSeconds =?= l.roundSeconds.plus(Duration ofSeconds 2) &&
    (l.floorSeconds + d).floorSeconds =?= l.floorSeconds.plus(Duration ofSeconds 1) &&
    l < l+d && !(l < l) && !(l+t < l) &&
    l <= l+d && l <= l && !(l+t <= l) &&
    !(l > l+d) && !(l > l) && l+t > l &&
    !(l >= l+d) && l >= l && l+t >= l &&
    (l min l+d) == l && (l+d min l) == l &&
    (l max l+d) == l+d && (l+d max l) == l+d &&
    l2.epoch >= l.epoch &&
    i >= l.instant && l2.instant >= i &&
    o >= l.utc && l2.utc >= o &&
    z >= l.zoned && l2.zoned >= z &&
    f >= l.filetime && l2.filetime >= f
  }

  def test_zoneddatetime(): Boolean = {
    val d = Duration.ofMillis(1581)
    val t = 1.58131.asTime
    val z = ZonedDateTime.now
    val l = LocalDateTime.now
    val i = Instant.now
    val o = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    val f = FileTime from i
    val z2 = ZonedDateTime.now
    (z + d) - z == d &&
    (z + t) - z == t.toDuration &&
    (z.roundMillis + t).roundMillis - z.roundMillis == d && (z.floorMillis + t).floorMillis - z.floorMillis == d &&
    (z.roundSeconds + d).roundSeconds =?= z.roundSeconds.plus(Duration ofSeconds 2) &&
    (z.floorSeconds + d).floorSeconds =?= z.floorSeconds.plus(Duration ofSeconds 1) &&
    z < z+d && !(z < z) && !(z+t < z) &&
    z <= z+d && z <= z && !(z+t <= z) &&
    !(z > z+d) && !(z > z) && z+t > z &&
    !(z >= z+d) && z >= z && z+t >= z &&
    (z min z+d) == z && (z+d min z) == z &&
    (z max z+d) == z+d && (z+d max z) == z+d &&
    z2.epoch >= z.epoch &&
    i >= z.instant && z2.instant >= i &&
    o >= z.utc && z2.utc >= o &&
    l >= z.local && z2.local >= l &&
    f >= z.filetime && z2.filetime >= f
  }

  def test_offsetdatetime(): Boolean = {
    val d = Duration.ofMillis(1581)
    val t = 1.58131.asTime
    val o = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    val z = ZonedDateTime.now
    val l = LocalDateTime.now
    val i = Instant.now
    val f = FileTime from i
    val o2 = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    (o + d) - o == d &&
    (o + t) - o == t.toDuration &&
    (o.roundMillis + t).roundMillis - o.roundMillis == d && (o.floorMillis + t).floorMillis - o.floorMillis == d &&
    (o.roundSeconds + d).roundSeconds =?= o.roundSeconds.plus(Duration ofSeconds 2) &&
    (o.floorSeconds + d).floorSeconds =?= o.floorSeconds.plus(Duration ofSeconds 1) &&
    o < o+d && !(o < o) && !(o+t < o) &&
    o <= o+d && o <= o && !(o+t <= o) &&
    !(o > o+d) && !(o > o) && o+t > o &&
    !(o >= o+d) && o >= o && o+t >= o &&
    (o min o+d) == o && (o+d min o) == o &&
    (o max o+d) == o+d && (o+d max o) == o+d &&
    o2.epoch >= o.epoch &&
    i >= o.instant && o2.instant >= i &&
    z >= o.zoned && o2.zoned >= z &&
    l >= o.local && o2.local >= l &&
    f >= o.filetime && o2.filetime >= f
  }

  def test_filetime(): Boolean = {
    val d = Duration.ofMillis(1581)
    val t = 1.58131.asTime
    val f = FileTime from Instant.now
    val z = ZonedDateTime.now
    val l = LocalDateTime.now
    val i = Instant.now
    val o = OffsetDateTime.now.withOffsetSameInstant(ZoneOffset.UTC)
    val f2 = FileTime from Instant.now
    (f + d) - f == d &&
    (f + t) - f == t.toDuration &&
    (f.roundMillis + t).roundMillis - f.roundMillis == d && (f.floorMillis + t).floorMillis - f.floorMillis == d &&
    (f.roundSeconds + d).roundSeconds.to(TimeUnit.SECONDS) =?= f.roundSeconds.to(TimeUnit.SECONDS) + 2 &&
    (f.floorSeconds + d).floorSeconds.to(TimeUnit.SECONDS) =?= f.floorSeconds.to(TimeUnit.SECONDS) + 1 &&
    f < f+d && !(f < f) && !(f+t < f) &&
    f <= f+d && f <= f && !(f+t <= f) &&
    !(f > f+d) && !(f > f) && f+t > f &&
    !(f >= f+d) && f >= f && f+t >= f &&
    (f min f+d) == f && (f+d min f) == f &&
    (f max f+d) == f+d && (f+d max f) == f+d &&
    f2.epoch >= f.epoch &&
    i >= f.instant && f2.instant >= i &&
    o >= f.utc && f2.utc >= o &&
    l >= f.local && f2.local >= l &&
    z >= f.zoned && f2.zoned >= z
  }

  def main(args: Array[String]) { typicalMain(args) }
}

class Test_Eio_from_JUnit {
  @org.junit.Test
  def test() { Test_Eio.main(Array()) }
}
