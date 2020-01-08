// This file is distributed under the BSD 3-clause license
// Copyright 2019, 2020 Rex Kerr and Calico Life Sciences

package kse.proc

import java.time.{Duration, Instant}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Queue
import scala.util.control.NonFatal

import kse.flow._
import kse.maths._
import kse.maths.stats._
import kse.maths.fits._

/** Acts handles running multiple instances of Act, either in serial or in parallel,
  * optionally gathering timing information in order to stop in time.
  */