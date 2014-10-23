package kse.tests

import kse.flow._
import kse.coll._

object Test_Mopt extends Test_Kse {
  def test_lowlevel = {
    val m = Mopt.empty[Boolean]
    !m.ok && m.copy.on.ok && !m.off.ok && m.on.ok && (m := true).value &&
    !m.copy.value(false).value && m.copy.ok && m.value &&
    !{ m.value = false; m }.value && !m.clear.ok
  }

  def test_gets = {
    val m = Mopt.empty[Int]
    probably{ implicit oops => m.grab }.isEmpty && probably{ implicit oops => m.copy.:=(5).grab } == Some(5) &&
    m.getOr(2) == 2 && m.copy.:=(5).getOr(2) == 5 &&
    m.getOrSet(2) == 2 && m.getOrSet(5) == 2 &&
    m.get == 2 && (safe{ m.clear.get } match { case No(nse: NoSuchElementException) => true; case _ => false })
  }

  def test_highlevel = {
    val m = Mopt.empty[Float].value(1f)
    var i = 0
    m.xform(_ * 2).value == 1f && { m.tap(_ => i += 1).value == i+1 } && !m.exists(_ < 10) &&
    m.on.xform(_ * 2).value == 2f && { m.tap(_ => i += 1).value == i+1 } && m.exists(_ < 10) &&
    m.reject(_ > 10).ok && !m.reject(_ < 10).ok && m.getOr(0f) == 0
  }

  def test_object = {
    val m = Mopt.empty[Char].value('m')
    m.toString == "<_>" && m.hashCode == m.copy.value('x').hashCode &&
    m == m.copy.value('x') && m.copy != m.on &&
    m.toString == "<m>" && m.hashCode == m.value.hashCode &&
    m == m.copy && m != m.copy.value('x') && m != m.copy.off
  }

  def test_conversion = {
    val m = Mopt.empty[Long].value(1)
    m.toOption == None && m.toOk == Ok.UnitNo &&
    m.on.toOption == Some(1L) && m.toOk == Yes(1L) && m == Mopt(Option(1L)) &&
    Option(1L).toMopt == m
  }

  def test_specialization = {
    val mu = Mopt.empty[Unit]
    val mb = Mopt.empty[Byte]
    val ms = Mopt.empty[Short]
    val mc = Mopt.empty[Char]
    val mi = Mopt.empty[Int]
    val ml = Mopt.empty[Long]
    val mf = Mopt.empty[Float]
    val md = Mopt.empty[Double]
    val mo = Mopt.empty[String]
    !mu.ok && mu.copy == mu && mu.on.copy.ok && !mu.clear.ok && mu.isInstanceOf[Mopt.MoptUnit] &&
    !mb.ok && mb.copy == mb && mb.on.copy.ok && mb.value == 0 && !mb.clear.ok && mb.isInstanceOf[Mopt.MoptByte] &&
    !ms.ok && ms.copy == ms && ms.on.copy.ok && ms.value == 0 && !ms.clear.ok && ms.isInstanceOf[Mopt.MoptShort] &&
    !mc.ok && mc.copy == mc && mc.on.copy.ok && mc.value == 0 && !mc.clear.ok && mc.isInstanceOf[Mopt.MoptChar] &&
    !mi.ok && mi.copy == mi && mi.on.copy.ok && mi.value == 0 && !mi.clear.ok && mi.isInstanceOf[Mopt.MoptInt] &&
    !ml.ok && ml.copy == ml && ml.on.copy.ok && ml.value == 0 && !ml.clear.ok && ml.isInstanceOf[Mopt.MoptLong] &&
    !mf.ok && mf.copy == mf && mf.on.copy.ok && mf.value.isNaN && !mf.clear.ok && mf.isInstanceOf[Mopt.MoptFloat] &&
    !md.ok && md.copy == md && md.on.copy.ok && md.value.isNaN && !md.clear.ok && md.isInstanceOf[Mopt.MoptDouble] &&
    !mo.ok && mo.copy == mo && mo.:=("").copy.ok && mo.value == "" && !mo.clear.ok && mo.isInstanceOf[Mopt.MoptAny[_]]
  }

  def test_creation = {
    Mopt(()).ok &&
    Mopt(2: Byte).exists(_ == 2) &&
    Mopt(3: Short).exists(_ == 3) &&
    Mopt('m').exists(_ == 'm') &&
    Mopt(4).exists(_ == 4) &&
    Mopt(5L).exists(_ == 5) &&
    Mopt(6f).exists(_ == 6) &&
    Mopt(7d).exists(_ == 7) &&
    Mopt("fish").exists(_ == "fish") &&
    !Mopt(None: Option[Unit]).ok && Mopt(Some(()): Option[Unit]).ok && Option(()).toMopt.ok &&
    !Mopt(None: Option[Byte]).ok && Mopt(Option(8: Byte)).exists(_ == 8) && Option(8: Byte).toMopt.exists(_ == 8) &&
    !Mopt(None: Option[Short]).ok && Mopt(Option(9: Short)).exists(_ == 9) && Option(9: Short).toMopt.exists(_ == 9) &&
    !Mopt(None: Option[Char]).ok && Mopt(Option('p')).exists(_ == 'p') && Option('p').toMopt.exists(_ == 'p') &&
    !Mopt(None: Option[Int]).ok && Mopt(Option(10)).exists(_ == 10) && Option(10).toMopt.exists(_ == 10) &&
    !Mopt(None: Option[Long]).ok && Mopt(Option(11L)).exists(_ == 11) && Option(11L).toMopt.exists(_ == 11) &&
    !Mopt(None: Option[Float]).ok && Mopt(Option(12f)).exists(_ == 12) && Option(12f).toMopt.exists(_ == 12) &&
    !Mopt(None: Option[Double]).ok && Mopt(Option(13d)).exists(_ == 13) && Option(13d).toMopt.exists(_ == 13) &&
    !Mopt(None: Option[String]).ok && Mopt(Option("fish")).exists(_ == "fish") && Option("fish").toMopt.exists(_ == "fish")
  }

  def main(args: Array[String]) { typicalMain(args) }
}

