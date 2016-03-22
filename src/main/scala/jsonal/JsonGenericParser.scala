// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal

object JsonGenericParser {
  private[jsonal] val smallPowersOfTen = Array.tabulate(23)(i => s"1e$i".toDouble)

  private[jsonal] val myRightNull: Either[JastError, kse.jsonal.Json.Null] = Right(kse.jsonal.Json.Null)
  private[jsonal] val myRightTrue: Either[JastError, kse.jsonal.Json.Bool] = Right(kse.jsonal.Json.Bool.True)
  private[jsonal] val myRightFalse: Either[JastError, kse.jsonal.Json.Bool] = Right(kse.jsonal.Json.Bool.False)

  private[jsonal] val wouldNotFitInDouble: JastError = JastError("Text number would not fit in a Double")
}
