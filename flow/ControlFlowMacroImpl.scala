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
      case Function(List(param), body) => println(s"Function $param $body"); rename[c.type](c)(body, param.name, i)
      case Block(Nil, last) => println("Block"); inlineFn(last)
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
}
