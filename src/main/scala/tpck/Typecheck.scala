// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-2015 Rex Kerr, HHMI Janelia, UCSF, and Calico Labs.

package kse

/** This package provides traits and types to help the type system correctly search for implicits or handle type unions. */
package typecheck {
  /** Implicitly provides a value parameterized by type `Z` */
  trait ImplicitValue[@specialized A, Z] { def value: A }
}

/** This package provides traits and types to help the type system correctly search for implicits or handle type unions. */
package object typecheck {
  
  /** A canonical contravariant parameterized type. */
  trait Contra[-A] {}
  
  /** Allows type unions using contravariance.
    * Example:
    * {{{ def f[T: Union2[Int, String]#Apply](t: T) = t match { case i: Int => i; case s: String => s.length } }}}
    * Up to 5-way unions are predefined (Union2 to Union5).
    */
  type Union2[A,B] = { type Apply[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B]] }
  
  /** 3-way type union.
    * Example: 
    * {{{ def f[T: Union3[Int, String, Float]#Apply](t: T) = t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt } }}}
    */
  type Union3[A,B,C] = { type Apply[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B] with Contra[C]] }
  
  /** 4-way type union.
    * Example: 
    * {{{
    * def f[T: Union4[Int, String, Float, Char]#Apply](t: T) =
    *   t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt; case c: Char => if (c.isDigit) c-'0' else -1 }
    * }}}
    */
  type Union4[A,B,C,D] = { type Apply[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B] with Contra[C] with Contra[D]] }
  
  /** 5-way type union.
    * Example: 
    * {{{
    * def f[T: Union5[Int, String, Float, Char, Unit]#Apply](t: T) =
    *   t match { case i: Int => i; case s: String => s.length; case f: Float => math.ceil(f).toInt; case c: Char => if (c.isDigit) c-'0' else -1; case _: Unit => 0 }
    * }}}
    */
  type Union5[A,B,C,D,E] = { type Apply[Z] = Contra[Contra[Z]] <:< Contra[Contra[A] with Contra[B] with Contra[C] with Contra[D] with Contra[E]] }

}
