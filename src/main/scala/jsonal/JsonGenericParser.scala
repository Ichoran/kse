// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

import kse.flow._

object JsonGenericParser {
  private[jsonal] val smallPowersOfTen = Array.tabulate(23)(i => s"1e$i".toDouble)

  private[jsonal] val myRightNull = Yes(kse.jsonal.Json.Null)
  private[jsonal] val myRightTrue = Yes(kse.jsonal.Json.Bool.True)
  private[jsonal] val myRightFalse = Yes(kse.jsonal.Json.Bool.False)

  private[jsonal] val wouldNotFitInDouble = JastError("Text number would not fit in a Double")
}
