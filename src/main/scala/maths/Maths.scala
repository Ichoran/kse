// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse

package maths {
  /** Useful constants.  Computed using Mathematica 8.0 or Wolfram Alpha. */
  object NumericConstants {
    // Common constants involving Pi or roots
    final val OneOverPi = 0.31830988618379067154
    final val OverSqrtTwoPi = 0.39894228040143267794
    final val TwoOverPi = 0.63661977236758134308
    final val OverSqrtTwo = 0.7071067811865475244
    final val SqrtTwo = 1.4142135623730950488
    final val PiOverTwo = 1.5707963267948966192
    final val SqrtTwoPi = 2.5066282746310005024

    // Less common constants involving Pi
    final val LnTwoPi = 1.8378770664093454836
    final val HalfLnTwoPi = 0.91893853320467274178
    final val OverSqrtEight = 0.3535533905932737622
    final val QuarterSqrtPi = 0.44311346272637900682

    // Logarithmic constants
    final val OverLnTwo = 1.4426950408889634079
    final val LnTwo = 0.69314718055994530942
    final val LnHalf = -0.69314718055994530942

    // Constants for statistical distributions; bidirectional conversions chosen so they multiply to 1.
    final val GaussianFWHMToSigma = 0.7413011092528010  // Conversion constant from Mathematica 8: 1/(2*InverseErf[1/2]*Sqrt[2])
    final val GaussianSigmaToFWHM = 1.3489795003921635  // Conversion constant from Mathematica 8: 2*InverseErf[1/2]*Sqrt[2]
    final val Gaussian90PctToSigma = 0.30397841595588447291   // One sigma is this fraction of the central 90% of a Gaussian distribution.    

    // Constants for numeric approximation
    final val GammaTwentyTwo = 5.109094217170944e19
    final val LanczosDoubleG = 6.0246800407767295837         // Magic value not from Mathematica

    // Constants involving machine precision
    final val TiniestDouble = 2.2250738585072014e-308       // NOT subnormal, that's Double.MinPositiveValue
    final val SqrtTiniestDouble = 1.4916681462400413e-154
    final val OverSqrtTiniestDouble = 6.7039039649712985e153
    final val EpsDouble10x = 2.220446049250313e-15
    final val EpsDouble100x = 2.220446049250313E-14
  }
}

package object maths {
  import scala.math._
  import NumericConstants._

  // Common numeric functions that are missing
  @inline final def log2(d: Double) = log(d) * OverLnTwo
  @inline final def entropy(d: Double) = d * log(d) * OverLnTwo

  implicit class EnrichedShortMaths(private val value: Short) extends AnyVal {
    @inline final def clip(lo: Short, hi: Short) = max(lo, min(hi, value)).toShort
  }

  implicit class EnrichedIntMaths(private val value: Int) extends AnyVal {
    @inline final def clip(lo: Int, hi: Int) = max(lo, min(hi, value))
  }

  implicit class EnrichedLongMaths(private val value: Long) extends AnyVal {
    @inline final def clip(lo: Long, hi: Long) = max(lo, min(hi, value))
  }

  implicit class EnrichedFloatMaths(private val value: Float) extends AnyVal {
    @inline final def sq = value*value
    @inline final def sign = scala.math.signum(value)
    @inline final def ulp = scala.math.ulp(value)
    @inline final def nan = java.lang.Float.isNaN(value)
    @inline final def inf = java.lang.Float.isInfinite(value)
    @inline final def finite = !java.lang.Float.isNaN(value) && !java.lang.Float.isInfinite(value)
    @inline final def bits = java.lang.Float.floatToRawIntBits(value)
    @inline final def clip(lo: Float, hi: Float) = max(lo, min(hi, value))
  }

  implicit class EnrichedDoubleMaths(private val value: Double) extends AnyVal {
    @inline final def sq = value*value
    @inline final def sqrt = scala.math.sqrt(value)
    @inline final def log = scala.math.log(value)
    @inline final def log2 = maths.log2(value)
    @inline final def log10 = scala.math.log10(value)
    @inline final def entropy = maths.entropy(value)

    @inline final def gamma = maths.gamma(value)
    @inline final def lnGamma = maths.lnGamma(value)
    @inline final def erf = maths.erf(value)
    @inline final def erfc = maths.erfc(value)
    @inline final def erfInv = maths.erfInv(value)
    @inline final def ervcInv = maths.erfcInv(value)

    @inline final def sign = scala.math.signum(value)
    @inline final def ulp = scala.math.ulp(value)
    @inline final def nan = java.lang.Double.isNaN(value)
    @inline final def inf = java.lang.Double.isInfinite(value)
    @inline final def finite = !java.lang.Double.isNaN(value) && !java.lang.Double.isInfinite(value)
    @inline final def bits = java.lang.Double.doubleToRawLongBits(value)
    @inline final def clip(lo: Double, hi: Double) = max(lo, min(hi, value))
  }

  // Functions useful in computing statistical distributions
  // Gamma functions and their ilk (including complete beta)
  // Common operation when using Lanczos rational function approximation for gamma
  private final def lanczosLogGTerm(x: Double) = (x-0.5)*log(x*0.36787944117144232160 + 2.0324162060519644573)
  // "Exact" (double precision) Lanczos rational function for gamma
  // Values taken from Boost 1.53, at 20 digits of precision for g value 6.024680040776729583740234375
  // Takes ~20 ns on 3.33 GHz Intel Xeon X5680
  private final def lanczosApproximationRatio(x: Double) = {
    (56906521.9134715639 + x*(
      103794043.1163445452 + x*(
        86363131.28813859146 + x*(
          43338889.32467613834 + x*(
            14605578.08768506808 + x*(
              3481712.154980645909 + x*(
                601859.6171681098787 + x*(
                  75999.29304014542650 + x*(
                    6955.999602515376140 + x*(
                      449.9445569063168119 + x*(
                        19.51992788247617483 + x*(
                          0.5098416655656676188 + x*0.006061842346248906526))))))))))))/(
      x*(
        39916800 + x*(
          120543840 + x*(
            150917976 + x*(
              105258076 + x*(
                45995730 + x*(
                  13339535 + x*(
                    2637558 + x*(
                      357423 + x*(
                        32670 + x*(
                          1925 + x*(
                            66 + x)))))))))))   // Coefficients from Mathematica 8: CoefficientList[Product[(x + i), {i, 1, 11}], x]
    )
  }
  // Takes ~60 ns on 3.33GHz Intel Xeon X5680
  final def lnGamma(z: Double): Double = lanczosLogGTerm(z) + log(lanczosApproximationRatio(z))
  // Takes ~15-30 ns for integer z <= 21, 20-70 ns for integer 22 <= z <= 60, 110 ns for real z > 0, 210 ns for z < 0  (3.33GHz Intel Xeon X5680)
  final def gamma(z: Double): Double = {
    if (z > 0) {
      if (z <= 60.5 && abs(z-rint(z)) < 100*ulp(z)) {
        val n = math.round(z).toInt
        if (n <= 21) {
          var p = 1L
          var i = 2
          while (i < n) {
            p *= i
            i += 1
          }
          p.toDouble
        }
        else {
          var q = GammaTwentyTwo
          var i = 23
          while (i < n) {
            q *= i
            i += 1
          }
          q
        }
      }
      else exp(lanczosLogGTerm(z))*lanczosApproximationRatio(z)
    }
    else {
      val d = sin(Pi * z)
      if (d==0) Double.NaN else -Pi/(z*d*exp(lanczosLogGTerm(1-z))*lanczosApproximationRatio(1-z))
    }
  }
  final def lnGammaRat(z: Double, w: Double): Double = lanczosLogGTerm(z) - lanczosLogGTerm(w) + log(lanczosApproximationRatio(z)/lanczosApproximationRatio(w))
  final def gammaRat(z: Double, w: Double): Double = exp(lanczosLogGTerm(z) - lanczosLogGTerm(w))*lanczosApproximationRatio(z)/lanczosApproximationRatio(w)
  // lnBeta is lnGamma(a) + lnGamma(b) - lnGamma(a+b) but we'll take it apart to calculate more efficiently
  // Takes ~150 ns on a 3.33GHz Intel Xeon X5680
  final def lnBeta(a: Double, b: Double): Double = if (a < b) lnBeta(b,a) else if (a <= 0 || b <= 0) Double.NaN else {
    val c = a+b
    lanczosLogGTerm(a) + lanczosLogGTerm(b) - lanczosLogGTerm(c) + 
    log(lanczosApproximationRatio(a)*lanczosApproximationRatio(b)/lanczosApproximationRatio(c))
  }
  // beta is gamma(a+b)/(gamma(a)*gamma(b)) but we'll take it apart to calculate more efficiently
  // Takes 40-110 ns for small integer a,b, 200 ns for general case (large integer or real) (3.33GHz Intel Xeon X5680)
  final def beta(a: Double, b: Double): Double = if (a < b) beta(b,a) else if (a <= 0 || b <= 0) Double.NaN else {
    val c = a+b
    if (b < 40.5 && c < 1024.5 && abs(a-rint(a)) + abs(b-rint(b)) < 100*ulp(c)) {
      var n = round(c).toInt
      var m = round(b).toInt
      var p = 1.0
      var q = 1.0
      while (m >= 1) {
        p *= n
        q *= m
        m -= 1
        n -= 1
      }
      q/p
    }
    else {
      exp(lanczosLogGTerm(a)*lanczosLogGTerm(b)/lanczosLogGTerm(c)) *
      lanczosApproximationRatio(a)*lanczosApproximationRatio(b)/lanczosApproximationRatio(c)
    }
  }

  // Reasonably high-quality error/inverse error functions for general use
  // Based on Applied Statistics 37:477-484 (1988), alg. AS241
  // Takes ~45 ns on a 3.33 GHz Intel Xeon X5680
  def icdfNormal(p: Double) = {
    val h = p-0.5
    val x = 0.180625 - h*h
    if (x>=0) {
      h*
      (((((((2.5090809287301226727e3*x + 3.3430575583588128105e4
            )*x + 6.7265770927008700853e4
           )*x + 4.5921953931549871457e4
          )*x + 1.3731693765509461125e4
         )*x + 1.9715909503065514427e3
        )*x + 1.3314166789178437745e2
       )*x + 3.3871328727963666080e0
      ) /
      (((((((5.2264952788528545610e3*x + 2.8729085735721942674e4
            )*x + 3.9307895800092710610e4
           )*x + 2.1213794301586595867e4
          )*x + 5.3941960214247511077e3
         )*x + 6.8718700749205790830e2
        )*x + 4.2313330701600911252e1
       )*x + 1.0e0
      )
    }
    else {
      val hh = (if (h<=0) -1.0 else 1.0)
      val y = (if (h<=0) p else 1.0-p)
      val z = math.sqrt(-math.log(y))
      if (z<=5.0) {
        val x = z - 1.6
        hh*
        (((((((7.74545014278341407640e-4*x + 2.27238449892691845833e-2
              )*x + 2.41780725177450611770e-1
             )*x + 1.27045825245236838258e0
            )*x + 3.64784832476320460504e0
           )*x + 5.76949722146069140550e0
          )*x + 4.63033784615654529590e0
         )*x + 1.42343711074968357734e0
        ) /
        (((((((1.05075007164441684324e-9*x + 5.47593808499534494600e-4
              )*x + 1.51986665636164571966e-2
             )*x + 1.48103976427480074590e-1
            )*x + 6.89767334985100004550e-1
           )*x + 1.67638483018380384940e0
          )*x + 2.05319162663775882187e0
         )*x + 1.0
        )
      }
      else {
        val x = z - 5.0
        hh*
        (((((((2.01033439929228813265e-7*x + 2.71155556874348757815e-5
              )*x + 1.24266094738807843860e-3
             )*x + 2.65321895265761230930e-2
            )*x + 2.96560571828504891230e-1
           )*x + 1.78482653991729133580e0
          )*x + 5.46378491116411436990e0
         )*x + 6.65790464350110377720e0
        ) /
        (((((((2.04426310338993978564e-15*x + 1.42151175831644588870e-7
              )*x + 1.84631831751005468180e-5
             )*x + 7.86869131145613259100e-4
            )*x + 1.48753612908506148525e-2
           )*x + 1.36929880922735805310e-1
          )*x + 5.99832206555887937690e-1
         )*x + 1.0
        );
      }
    }
  }
  def erfInv(x: Double) = OverSqrtTwo*icdfNormal(0.5+0.5*x)
  def erfcInv(x: Double) = OverSqrtTwo*icdfNormal(1.0-0.5*x)
  // Piecewise rational function approximation of CDF for Normal distribution (courtesy of Mathematica 7)
  // Should be full double precision
  // Takes ~100ns on a 3.33 GHz Intel Xeon X5680 for moderate values
  def cdfNormal(y: Double) = {
    if (y > 8.3) 1.0 else if (y < - 38.5) 0.0 else {
      val x = if (y<0) -y else y
      val f = {
        if (x < 3) math.exp(
          -0.5*x*x -
          (((((((-3.6271830621274548308e-6*x - 6.2054577195631746255e-5
                )*x + 0.0020555154846807655013
               )*x + 0.032099345474574417685
              )*x + 0.21504119632351847003
             )*x + 0.73055326515392090713
            )*x + 1.3812898842892215850
           )*x + 0.69314718055994526146
          ) /
          (((((((-5.8186829446354815108e-7*x - 2.2135273033157240657e-5
                )*x + 3.6576165145176352643e-4
               )*x + 0.0094667294072793799548
              )*x + 0.078740088812851505927
             )*x + 0.34723234319509102797
            )*x + 0.84167596702197143827
           )*x + 1.0
          )
        )
        else if (x < 16) (math.exp( -0.5*x*x ) *
          ((((((0.00118089255719362346624*x + 0.0136334301130162766315
               )*x + 0.086474160844062169269
              )*x + 0.33993667920309143168
             )*x + 0.86339167691367313008
            )*x + 1.3345326346191572297
           )*x + 1
          ) /
          (((((((0.0029600586715196076372*x + 0.034173941597530707646
                )*x + 0.21971862448906668587
               )*x + 0.88626919617829879773
              )*x + 2.3750320592403537542
             )*x + 4.1290652702771203918
            )*x + 4.2651316245967753927
           )*x + 1.9999244808870340017
          )
        )
        else {
          val f0 = math.exp(-0.5*x*x)*OverSqrtTwoPi
          val z = 1/(x*x)
          var g, sum = 1/x
          var i = -1
          while (i >= -20) { g *= i*z; sum += g; i -= 2 }
          f0 * sum
        }
      }
      if (y>0) 1.0-f else f
    }
  }
  def erf(x: Double) = 2.0*cdfNormal(SqrtTwo*x)-1.0
  def erfc(x: Double) = -2.0*cdfNormal(-SqrtTwo*x)
  // Student's T test distribution functions (special case of incomplete regularized beta)
  // Approximations from Hill, Comm. ACM, Algorithm 395 & 396, v13 pp 617-620 (1970)
  // Takes no more than about 180 ns on a 3.33 GHz Intel Xeon X5680 (df = 18)
  def cdfStudentT(df: Long, t0: Double): Double = {
    val t = abs(t0)
    val p = {
      if (df == 1) 0.5*(1 - TwoOverPi*atan(t))
      else {
        val y = t*t/df
        if (df >= 20) {
          val dg = df - 0.5
          val b = 48*dg*dg
          val z = if (y > 1e-6) dg*log(1+y) else dg*y
          cdfNormal( -sqrt(z)*(((((-0.4*z - 3.3)*z - 24.0)*z - 85.5)/(0.8*z*z+100+b) + z + 3)/b + 1) )
        }
        else {
          val iy1 = 1/(1+y)
          val cs = if (df < 4) 1.0 else nestCosS(df.toInt, iy1, 1.0)
          0.5*max(0 , 1 - (if ((df&1)==0) sqrt(y/(1+y))*cs else { var yrt = sqrt(y); TwoOverPi*(atan(yrt) + yrt*iy1*cs) }))
        }
      }
    }
    if (t0 < 0) p else 1-p
  }
  @annotation.tailrec private def nestCosS(n: Int, ib: Double, x: Double): Double = if (n<4) x else nestCosS(n-2, ib, 1 + (x*ib*(n-3))/(n-2))

  // Takes about 350 ns on a 3.33 GHz Intel Xeon X5680 (df = 12)
  def icdfStudentT(df: Long, p0: Double): Double = {
    val p = if (p0 > 0.5) 2*(1-p0) else 2*p0
    val t = {
      if (df < 2) 1.0/tan(p*PiOverTwo)
      else if (df == 2) sqrt(2/(p*(2-p)) - 2)
      else {
        val dg = df - 0.5
        val idg = 1/dg
        val b = 48*dg*dg
        val ib = 1/b
        val c = ((20700*idg*ib - 98)*idg-16)*idg + 96.36
        val d = ((94.5/(b+c)-3)*ib+1)*sqrt(idg*PiOverTwo)*df
        val y = math.pow(d*p, 2.0/df)
        val z = {
          if (y > 0.05 + idg) {
            val in = icdfNormal(p*0.5)
            val insq = in*in
            val e = if (df < 5) c + 0.3*(df - 4.5)*(in+0.6) else c
            val f = (((0.05*d*in-5)*in-7)*in-2)*in + b + e
            val g = (((((0.4*insq + 6.3)*insq + 36)*insq + 94.5)/f - insq - 3)*ib + 1)*in
            val h = idg*g*g
            (if (h > 0.002) exp(h)-1 else 0.5*h*h+h)
          }
          else ((1/(((df + 6)/(df*y) - 0.089*d - 0.822)*(df+2)*3) + 0.5/(df+4))*y-1)*(df+1)/(df+2.0) + 1/y
        }
        sqrt(df*z)
      }
    }
    if (p0>0.5) t else -t
  }
  
  // Regularized incomplete gamma functions and chi squared distributions
  // $\gamma (s,x) = \frac{1}{\Gamma (s)} \cdot \int_{0}^{x} t^{s-1} e^{-t} dt$
  // Using standard form found in Cuyt & Peterson's "Handbook of Continued Fractions for Special Functions"
  // unless x is small so the series form should do better.  Assumes s>0,x>0.
  // A better split could be found for s,x >> 1000
  private final def igammaLowerTaylorTerm(s: Double, x: Double): Double = {
    var taylor = 1.0/s;
    var sum = taylor;
    var denom = 1.0+s
    while (taylor > 100*ulp(sum)) {
      taylor *= x/denom
      sum += taylor
      denom += 1.0
    }
    sum
  }
  private final def igammaUpperContFracTerm(s: Double, x: Double): Double = {
    var cont = x + 1.0 - s
    var lentzC = OverSqrtTiniestDouble
    var lentzD = (if (abs(cont) < SqrtTiniestDouble) OverSqrtTiniestDouble else 1.0/cont)
    var factor = 2.0
    var prod = lentzD
    var i = 1
    while (abs(factor-1) > EpsDouble100x) {
      val a = i*(s-i)
      cont += 2.0
      lentzC = cont + a/lentzC
      if (abs(lentzC) < SqrtTiniestDouble) lentzC = SqrtTiniestDouble*signum(lentzC)
      lentzD = cont + a*lentzD
      if (abs(lentzD) < SqrtTiniestDouble) lentzD = OverSqrtTiniestDouble*signum(lentzD) else lentzD = 1.0/lentzD
      factor = lentzC*lentzD
      prod *= factor
      i += 1
    }
    prod
  }
  private final def igammaRegShapeApprox(s: Double, x: Double) = exp(-x + s*log(x) - lnGamma(s))
  // Takes about 300 ns on a 3.33 GHz Intel Xeon X5680
  def igammaRegL(s: Double, x: Double) = if (x < s+1) igammaLowerTaylorTerm(s,x)*igammaRegShapeApprox(s,x) else 1.0 - igammaUpperContFracTerm(s,x)*igammaRegShapeApprox(s,x)
  // Takes about 300 ns on a 3.33 GHz Intel Xeon X5680
  def igammaRegU(s: Double, x: Double) = if (x < s+1) 1.0 - igammaLowerTaylorTerm(s,x)*igammaRegShapeApprox(s,x) else igammaUpperContFracTerm(s,x)*igammaRegShapeApprox(s,x)
  // Runtime equal to igammaRegL
  def cdfChiSq(df: Double, chisq: Double) = igammaRegL(0.5*df, 0.5*chisq)
  
  // Incomplete beta functions and F distribution based on DiDonato & Morris, ACM Trans Math Soft v18 pp360-373 (1992)
  // Additional inspiration taken from bratio.f90 by DD & M, and Boost 1.53 implementation and documentation also based on DD & M
  private def ddmMethodBPSER(a: Double, b: Double, x: Double) = {
    var nu = a
    var de = 1.0
    var term = EpsDouble100x
    var sum = 1.0
    var j = 0
    while (abs(term) >= sum*EpsDouble100x) {
      j += 1
      nu *= (j-b)*x
      de *= j
      term = nu/(de*(a+j))
      sum += term
    }
    exp(a * log(x) - lnBeta(a, b))*sum/a
  }
  private def ddmMethodBUP(a: Double, b: Double, x: Double, n: Int) = {
    var term = exp(a*log(x) + b*log(1-x) - lnBeta(a,b))/a
    var sum = term
    var j = 1
    val ab1 = a+b-1
    val earliable = (if (b <= 1) 1 else ceil((b-1)*x/(1-x) - a).toInt)
    while (j < n && (j <= earliable || sum*EpsDouble100x < term)) {
      term *= (ab1+j)*x/(a+j)
      sum += term
      j += 1
    }
    sum
  }
  // Only gives about 9 digits accuracy
  private def ddmMethodBGRAT(a: Double, b: Double, x: Double, w: Double = 0.0) = {
    val t = a + 0.5*(b-1)
    val lx = log(x)
    val u = -t*lx
    val lh = -u + b*log(u) - lnGamma(b)
    val m = exp(lh - b*log(t) + lnGammaRat(a+b,a))
    val ew = abs(w/m)*EpsDouble100x
    var p = new Array[Double](8); p(0) = 1
    val i4tsq = 1/(4*t*t)
    val lx2sq = 0.25*lx*lx
    var j = igammaRegU(b, u)*exp(-lh)
    var term = j
    var sum = term
    var n = 0
    val ub = u + b
    var q = b-1
    var g = 1.0
    while (max(ew,sum*EpsDouble100x) < abs(term)) {
      j = i4tsq*(g*(ub+(2*n+1)) + (b+2*n)*(b+2*n+1)*j)
      g *= lx2sq
      n += 1
      q /= 2*n*(2*n+1)
      var m = 1
      var s = 0.0
      var r = 1.0
      while (m < n) {
        r *= (m*b-n)/(2*m*(2*m+1))
        s += r*p(n-m)
        m += 1
      }
      if (n >= p.length) { val pp = new Array[Double](p.length*2); System.arraycopy(p,0,pp,0,p.length); p = pp }
      p(n) = q + s/n
      term = p(n)*j
      sum += term
    }
    m * sum
  }
  private def ddmMethodBFRAC(a: Double, b: Double, x: Double) = {
    val lam1 = 1 + a - (a+b)*x
    val ia = 1/a
    var p = 1.0
    var an = 0.0
    var bn = 1.0
    var an1 = 1.0
    var bn1 = lam1/(1 + ia)
    var r = (1 + ia)/lam1
    var n = 1
    while (n != 0) {
      val w = n*(b - n)*x
      val ia2n1 = 1/(a + (2*n - 1))
      val e = a*ia2n1
      val alph = (p*(p + b*ia)*e*e)*(w*x)
      if (alph <= 0) n = 0
      else {
        p = 1 + n*ia
        val bet = n + w*ia2n1 + (p/(1+ia*(2*n+1)))*(lam1 + n*(2 - x))
        val aa = alph*an + bet*an1; an = an1; an1 = aa
        val bb = alph*bn + bet*bn1; bn = bn1; bn1 = bb
        val r0 = r
        val ibn1 = 1/bn1
        r = an1*ibn1
        if (abs(r-r0) <= EpsDouble100x*r) n = 0
        else {
          an *= ibn1
          an1 = r
          bn *= ibn1
          bn1 = 1
          n += 1
        }
      }
    }
    r * exp(a*log(x) + b*log(1-x) - lnBeta(a,b))
  }
  // Incomplete regularized beta.  At least 9 digits of accuracy almost everywhere.
  // ~1000 ns for most values, except for large a,b with x near a/(a+b), which takes ~2000*log10((a+b)/1000) ns (all on a 3.33 GHz Intel Xeon X5680)
  def ibetaReg(a: Double, b: Double)(x: Double): Double = {
    if (a <= 0 || b <= 0) return Double.NaN
    val y = 1-x
    if (x <= 0 || y <= 0) return (if (min(x,y) > -EpsDouble100x) { if (x < 0.5) 0 else 1 } else Double.NaN)
    val abm = min(a,b)
    val abM = max(a,b)
    lazy val lxa = a*log(x)
    lazy val lbxa = a*log(x*b)
    if (abm < 1) {
      if (x > 0.5) 1 - ibetaReg(b, a)(1-x)
      else if (abM <= 1) {
        if (a >= min(0.2,b) || (lxa <= -0.1053605156578263 /* log(0.9) */)) ddmMethodBPSER(a, b, x)
        else if (x >= 0.3) 1 - ddmMethodBPSER(b, a, 1-x)
        else { val w = ddmMethodBUP(b, a, 1-x, 20); 1 - (w + ddmMethodBGRAT(b + 20, a, 1-x, w)) }
      }
      else if (b <= 1) ddmMethodBPSER(a, b, x)
      else {
        if (x >= 0.3) 1 - ddmMethodBPSER(b, a, 1-x)
        else if (x < 0.1 && lbxa <= -0.35667494393873238 /* log(0.7) */) ddmMethodBPSER(a, b, x)
        else { val (n,w) = (if (b<=15) (20, ddmMethodBUP(b, a, 1-x, 20)) else (0, 0.0)); 1 - (w + ddmMethodBGRAT(b + n, a, 1-x, w)) }
      }
    }
    else if (x*(a+b) > a) 1 - ibetaReg(b, a)(1-x)
    else if (b >= 40) ddmMethodBFRAC(a, b, x)
    else {
      val m = ceil(b).toInt - 1
      if (b*x < 0.7) ddmMethodBPSER(a, b, x)
      else if (x <= 0.7) ddmMethodBUP(b-m, a, 1-x, m) + ddmMethodBPSER(a, b-m, x)
      else {
        val w = ddmMethodBUP(b-m, a, 1-x, m)
        val (n,v) = (if (a<=15) (20, ddmMethodBUP(a, b-m, x,20)) else (0, 0.0))
        w + v + ddmMethodBGRAT(a+n, b-m, x, w+v)
      }
    }
  }
  // F distribution from  incomplete regularized beta
  def cdfFDistribution(F: Double)(n: Int, m: Int) = ibetaReg(0.5*n, 0.5*m)(n*F/(n*F+m))
}
