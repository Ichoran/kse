// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Labs.

package kse.maths

import scala.math._

object LinAlg {
  /* Solves the tridiagonal matrix equation Mx = b where M is tri-diagonal.
   * Input must be packed as follows:
   * {{{
   *    0  m00 m01  b0   // First 4 entries in input
   *   m10 m11 m12  b1   // Next 4
   *   m21 m22 m23  b2   // ...
   *    ...
   * m(n-1)n mnn 0  bn   // Last 4
   * }}}
   * That is, the array is interpreted in blocks of 4, with the last slot being
   * right-hand side of the equation, the second slot the diagonal, the first
   * slot the row under the diagonal (or 0 where it does not exist), and the second
   * to last slot the row over the diagonal.
   *
   * If a solution is impossible, `Double.NaN` will be returned in those
   * indices where the problem cannot be avoided.  Degenerate solutions are
   * also not presently handled.  The algorithm expects values to be centered
   * around 1; if they are not, renormalization may be necessary.
   *
   * The algorithm is O(n) where n is the number of rows and takes on the order
   * of 20 ns per row on a fast modern processor.
   */
  def solveTriDiagonal(augTriDiag: Array[Double]): Array[Double] = {
    if ((augTriDiag.length) % 4 != 0) throw new IllegalArgumentException("solveTriDiagonal only works on arrays of lengths a multiple of 4")
    val work = augTriDiag.clone
    val ans = new Array[Double](work.length / 4)
    solveTriDiagonalImpl(work, ans, 0, ans.length);
    ans
  }
  private def solveTriDiagonalImpl(work: Array[Double], ans: Array[Double], i0: Int, iN: Int) {
    /*
     * Note: Mathematica solves LinearSolve[{{2, 7, 0, 0}, {1, 4, 3, 0}, {0, 5, 3, 9}, {0, 0, 6, 7}}, {1, 2, 3, 4}]
     * as {10/27, 1/27, 40/81, 4/27}
     * which should match solveTriDiagonal(Array[Double](0,2,7, 1, 1,4,3, 2, 5,3,9, 3, 6,7,0, 4))
     */
    if (i0 >= iN) return;
    if (work(4*i0).abs > NumericConstants.EpsDouble100x) {
      ans(i0) = Double.NaN
      var i = i0
      while (true) {
        i += 1
        if (i >= iN) return
        if (work(4*i).abs <= NumericConstants.EpsDouble100x && work(4*i-2) <= NumericConstants.EpsDouble100x) {
          solveTriDiagonalImpl(work, ans, i, iN)
          return
        }
        ans(i) = Double.NaN
      }
    }
    var i = i0
    var diag = work(4*i+1)
    var upper = work(4*i+2)
    var aug = work(4*i+3)
    // Forward pass, Gaussian elimination
    while (i+1 < iN) {
      i += 1
      val toclear = work(4*i)
      if (abs(toclear) > NumericConstants.EpsDouble100x) {
        work(4*i) = 0
        val fix = toclear/diag
        diag = work(4*i + 1) - upper*fix
        work(4*i+1) = diag
        upper = work(4*i + 2)
        aug = work(4*i + 3) - aug*fix
        work(4*i+3) = aug
      }
      else {
        diag = work(4*i+1)
        upper = work(4*i+2)
        aug = work(4*i+3)
      }
    }
    // Backward pass, Gaussian elimination
    if (abs(upper) > NumericConstants.EpsDouble100x) aug = Double.NaN
    else aug = aug/diag
    ans(i) = aug
    while (i > i0) {
      i -= 1
      val toclear = work(4*i + 2)
      if (abs(toclear) > NumericConstants.EpsDouble100x) aug = (work(4*i+3) - toclear*aug)/work(4*i+1)
      else aug = work(4*i+3)/work(4*i+1)
      ans(i) = aug
    }
  }
}
