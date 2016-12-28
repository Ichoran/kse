// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr

package kse.maths
package stats

import scala.math._

import kse.flow._

import kse.maths.stochastic._

object Bootstrap {
  def simple(data: Array[Double], n: Int, r: Prng): (Double, Est) =
    if (data.length == 0) (0, Est(0, 0, 0))
    else if (data.length == 1) (data(0), Est(1, data(0), 0))
    else {
      val e = new EstM
      e ++= data
      val expected = e.mean
      e.reset
      val ee = new EstM
      var i = 0
      while (i < n) {
        ee.reset
        var j = 0
        while (j < data.length) {
          ee += data(r % data.length)
          j += 1
        }
        e += ee.mean
        i += 1
      }
      (expected, e.immutable)
    }
  def simple(data: Array[Double], n: Int): (Double, Est) = simple(data, n, new ShiftMix64())
  def simple(data: Array[Double], r: Prng): (Double, Est) = simple(data, 1000, r)
  def simple(data: Array[Double]): (Double, Est) = simple(data, 1000, new ShiftMix64())

  def simple(data: Array[Float], n: Int = 1000, r: Prng = new ShiftMix64()): (Double, Est) =
    if (data.length == 0) (0.0, Est(0, 0, 0))
    else if (data.length == 1) (data(0).toDouble, Est(1, data(0), 0))
    else {
      val e = new EstM
      e ++= data
      val expected = e.mean
      e.reset
      val ee = new EstM
      var i = 0
      while (i < n) {
        ee.reset
        var j = 0
        while (j < data.length) {
          ee += data(r % data.length)
          j += 1
        }
        e += ee.mean
        i += 1
      }
      (expected, e.immutable)
    }
  def simple(data: Array[Float], n: Int): (Double, Est) = simple(data, n, new ShiftMix64())
  def simple(data: Array[Float], r: Prng): (Double, Est) = simple(data, 1000, r) 
  def simple(data: Array[Float]): (Double, Est) = simple(data, 1000, new ShiftMix64())

  def categorized(which: Array[Int], data: Array[Double], n: Int, r: Prng): (Array[Double], Array[Est]) = {
    ???
  }

  def pooled(datas: Array[Array[Double]], n: Int, r: Prng): (Double, Est) = {
    val e = new EstM
    var i = 0
    while (i < datas.length) {
      e ++= datas(i)
      i += 1
    }
    if (e.n <= 1 || datas.length <= 1) (e.mean, e.immutable)
    else {
      val count = e.n
      val expected = e.mean
      e.reset
      val ee = new EstM
      if (count < 20L*datas.length) {
        // Direct computation
        var m = 0
        while (m < n) {
          ee.reset
          i = 0
          while (i < datas.length) {
            ee ++= datas(r % datas.length)
            i += 1
          }
          e += ee.mean
          m += 1
        }
      }
      else {
        // More efficient to make sub-estimates and add those
        val ees = new Array[Est](datas.length)
        i = 0
        while (i < datas.length) {
          e ++= datas(i)
          ees(i) = e.immutable
          e.reset  // e was empty when we entered, so leave it empty when we're done
          i += 1
        }
        var m = 0
        while (m < n) {
          ee.reset
          i = 0
          while (i <  ees.length) {
            ee ++= ees(r % ees.length)
            i += 1
          }
          e += ee.mean
          m += 1
        }
      }
      (expected, e.immutable)
    }
  }
  def pooled(datas: Array[Array[Double]], n: Int): (Double, Est) = pooled(datas, n, new ShiftMix64())
  def pooled(datas: Array[Array[Double]], r: Prng): (Double, Est) = pooled(datas, 1000, r)
  def pooled(datas: Array[Array[Double]]): (Double, Est) = pooled(datas, 1000, new ShiftMix64())

  def pooled(datas: Array[Array[Float]], n: Int = 1000, r: Prng = new ShiftMix64()): (Double, Est) = {
    val e = new EstM
    var i = 0
    while (i < datas.length) {
      var j = 0
      while (j < datas(i).length) {
        e += datas(i)(j)
        j += 1
      }
      i += 1
    }
    if (e.n <= 1 || datas.length <= 1) (e.mean, e.immutable)
    else {
      val expected = e.mean
      e.reset
      val ee = new EstM
      if (e.n < 20L*datas.length) {
        // Direct computation
        var m = 0
        while (m < n) {
          ee.reset
          i = 0
          while (i < datas.length) {
            ee ++= datas(r % datas.length)
            i += 1
          }
          e += ee.mean
          m += 1
        }
      }
      else {
        // More efficient to make sub-estimates and add those
        val ees = new Array[Est](datas.length)
        i = 0
        while (i < datas.length) {
          e ++= datas(i)
          ees(i) = e.immutable
          e.reset  // e was empty when we entered, so leave it empty when we're done
          i += 1
        }
        var m = 0
        while (m < n) {
          ee.reset
          i = 0
          while (i <  ees.length) {
            ee ++= ees(r % ees.length)
            i += 1
          }
          e += ee.mean
          m += 1
        }
      }
      (expected, e.immutable)
    }
  }
  def pooled(datas: Array[Array[Float]], n: Int): (Double, Est) = pooled(datas, n, new ShiftMix64())
  def pooled(datas: Array[Array[Float]], r: Prng): (Double, Est) = pooled(datas, 1000, r)
  def pooled(datas: Array[Array[Float]]): (Double, Est) = pooled(datas, 1000, new ShiftMix64())
}
