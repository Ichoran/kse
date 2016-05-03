// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015 Rex Kerr, UCSF, and Calico Labs.

package kse.flow

import scala.language.experimental.macros

/** Contains implementations of control flow macros. */
object ControlFlowMacroImpl {
  import scala.reflect.macros.blackbox.Context
  
  private def rename[C <: Context](context: C)(tree: context.universe.Tree, name: context.universe.TermName, rename: context.universe.TermName) = {
    import context.universe._
    (new Transformer {
      override def transform(t: Tree) = t match {
        case Ident(x) if x == name => Ident(rename)
        case _ => super.transform(t)
      }
    }).transform(tree)
  }
    
  def cFor[A](c: Context)(zero: c.Tree)(p: c.Tree)(next: c.Tree)(f: c.Tree) = {
    import c.universe._
    
    val i = TermName(c.freshName("i$"))
    
    var valdefs = List(q"var $i = $zero")
    
    def inlineFn(tree: c.Tree): c.Tree = tree match {
      case Function(List(param), body) => rename[c.type](c)(body, param.name, i)
      case Block(Nil, last) => inlineFn(last)
      case _ =>
        val lf = TermName(c.freshName("lf$"))
        valdefs = q"val $lf = $tree" :: valdefs
        q"$lf($i)"
    }
    
    var newp = inlineFn(p)
    var newnext = inlineFn(next)
    var newf = inlineFn(f)
    
    c.untypecheck(q"""
    {
      ..${valdefs.reverse}
      while ($newp) {
        { $newf }
        $i = { $newnext }
      }
    }
    """)
  }
  
  def aFor[A](c: Context)(array: c.Tree)(f: c.Tree) = {
    import c.universe._
    
    val a = TermName(c.freshName("a$"))
    val i = TermName(c.freshName("i$"))
    val x = TermName(c.freshName("x$"))
    
    var valdefs = List(q"val $a = $array", q"var $i = 0")
    
    def inlineFn(tree: c.Tree): c.Tree = tree match {
      case Function(List(paramA, paramInt), body) =>
        val bodyPrime = rename[c.type](c)(body, paramA.name, x)
        rename[c.type](c)(bodyPrime, paramInt.name, i)
      case Block(Nil, last) => inlineFn(last)
      case _ =>
        val lf = TermName(c.freshName("lf$"))
        valdefs = valdefs :+ q"val $lf = $tree"
        q"$lf($a, $i)"
    }
    
    val body = inlineFn(f)
    
    c.untypecheck(q"""
    {
      ..$valdefs
      while ($i < $a.length) {
        val $x = $a.apply($i)
        $body
        $i += 1
      }
    }
    """)
  }
  
  def nFor(c: Context)(count: c.Tree)(f: c.Tree) = {
    import c.universe._
    
    val i = TermName(c.freshName("i$"))
    val n = TermName(c.freshName("n$"))
    
    var valdefs = List(q"val $n = $count", q"var $i = 0")
    
    def inlineFn(tree: c.Tree): c.Tree = tree match {
      case Function(List(param), body) => rename[c.type](c)(body, param.name, i)
      case Block(Nil, last) => inlineFn(last)
      case _ =>
        val lf = TermName(c.freshName("lf$"))
        valdefs = valdefs :+ q"val $lf = $tree"
        q"$lf($i)"
    }
    
    val body = inlineFn(f)
    c.untypecheck(q"""
    {
      ..$valdefs
      while ($i < $n) {
        $body
        $i += 1
      }
    }
    """)
  }
  
  def iFor[A](c: Context)(iterator: c.Tree)(f: c.Tree) = {
    import c.universe._
    
    val i = TermName(c.freshName("i$"))
    val x = TermName(c.freshName("x$"))
    
    var valdefs = List(q"val $i = $iterator")
    
    def inlineFn(tree: c.Tree): c.Tree = tree match {
      case Function(List(param), body) => rename[c.type](c)(body, param.name, x)
      case Block(Nil, last) => inlineFn(last)
      case _ =>
        val lf = TermName(c.freshName("lf$"))
        valdefs = valdefs :+ q"val $lf = $tree"
        q"$lf($x)"
    }
    
    val body = inlineFn(f)
    c.untypecheck(q"""
    {
      ..$valdefs
      while ($i.hasNext) {
        val $x = $i.next
        $body
      }
    }
    """)
  }

  def keepYesRetNo(c: Context) = {
    import c.universe._

    c.untypecheck(q"""
      if (${c.prefix}.isOk) ${c.prefix}.yes
      else return ${c.prefix}.asInstanceOf[No[${c.prefix.tree.tpe.typeArgs.head}]]
    """)
  }

  def keepYesRetNoMap(c: Context)(f: c.Tree) = {
    import c.universe._

    c.untypecheck(q"""
      ${c.prefix} match {
        case Yes(y) => y
        case No(n) => return No($f(n))
      }
    """)
  }
}
