// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-2014 Rex Kerr, HHMI/JFRC, and UCSF.

package kse

package tpck {
  // What are these even for???
  trait ImplicitSearch0[Z] {}
  trait ImplicitSearch1[A, Z] {}
  trait ImplicitSearch2[A, B, Z] {}
  trait ImplicitSearch3[A, B, C, Z] {}
  trait ImplicitSearch4[A, B, C, D, Z] {}
  trait ImplicitValue1[@specialized A, Z] extends ImplicitSearch1[A, Z] { def valueA: A }
  trait ImplicitValue2[A, B, Z] extends ImplicitSearch2[A, B, Z] { def valueA: A; def valueB: B }
  trait ImplicitValue3[A, B, C, Z] extends ImplicitSearch3[A, B, C, Z] { def valueA: A; def valueB: B; def valueC: C }
  trait ImplicitValue4[A, B, C, D, Z] extends ImplicitSearch4[A, B, C, D, Z] { def valueA: A; def valueB: B; def valueC: C; def valueD: D }
}

/** This package provides some useful traits to help the type system correctly search for implicits or handle type unions. */
package object tpck {
  /** A canonical contravariant parameterized type. */
  trait Contra[-A] {}
  /** Allows type unions using contravariance.
    * Example:
    * {{{ def f[T: Union2[Int, String]#Check](t: T) = t match { case i: Int => i; case s: String => s.length } }}}
    * Up to 5-way unions are predefined (Union2 to Union5).
    */
  type Union2[A,B] = { type Check[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B]] }
  /** 3-way type union.
    * Example: 
    * {{{ def f[T: Union3[Int, String, Float]#Check](t: T) = t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt } }}}
    */
  type Union3[A,B,C] = { type Check[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B] with Contra[C]] }
  /** 4-way type union.
    * Example: 
    * {{{
    * def f[T: Union4[Int, String, Float, Char]#Check](t: T) =
    *   t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt; case c: Char => if (c.isDigit) c-'0' else -1 }
    * }}}
    */
  type Union4[A,B,C,D] = { type Check[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B] with Contra[C] with Contra[D]] }
  /** 5-way type union.
    * Example: 
    * {{{
    * def f[T: Union5[Int, String, Float, Char, Unit]#Check](t: T) =
    *   t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt; case c: Char => if (c.isDigit) c-'0' else -1; case _: Unit => 0 }
    * }}}
    */
  type Union5[A,B,C,D,E] = { type Check[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B] with Contra[C] with Contra[D] with Contra[E]] }

  /** Referential equality on Any; probably doesn't work right for value classes */
  def refEq[A](x: A, y: Any) = y match {
    case Unit | Boolean | Byte | Short | Char | Int | Long | Float | Double => y == x
    case _ => x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]
  }
}