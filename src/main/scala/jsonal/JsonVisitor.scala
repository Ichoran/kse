// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal


/** This trait defines a visitor pattern for JSON entities.  Classes
  * which implement this trait can walk through a JSON entity automatically
  * and have JSON-type-specific `visit` methods called as the JSON entity
  * is traversed.
  *
  * This functionality is useful, for example, in creating prettyprinters.
  */
trait JsonVisitor {
  /** This method is called first when visiting a new JSON entity. */
  def begin: this.type

  /** This method is called each time a JSON null is visited. */
  def visitNull: this.type

  /** This method is called each time a JSON true or false is visited.  The value is in `truth`. */
  def visit(truth: Boolean): this.type

  /** This method is called each time a JSON string is visited.
    * The string is contained in `text`.  To encode this, one can
    * use the `Json.Str.encodeJsonString`method.
    */
  def visit(text: String): this.type

  /** This method is called each time a JSON number is visited. */
  def visit(num: Json.Num): this.type

  /** This method is called after a `Json.Arr.Dbl`, `Json.Arr.All`, or `Json.Obj` are
    * visited for the first time.  If it returns `true`, the children are visited using
    * the appropriate methods.  If `false`, no children will be visited.
    *
    * Note--this method will be called exactly once for each JSON collection, and it will
    * be called immediately after the corresponding `visit` method.
    *
    * Example: If you wanted to enter only objects of size 3, you might define
    * {{{
    * private[this] var inNext = false
    * def goIn = { val go = inNext; inNext = false; go }
    * def visit(o: Json.Obj): this.type = { if (o.size == 3) inNext = true; this }
    * }}}
    */
  def goIn: Boolean

  /** This method is called each time a JSON array of doubles is visited.
    * It is called before any of the contents are visited.
    */
  def visit(jad: Json.Arr.Dbl): this.type

  /** After a `Json.Arr.Dbl` is visited and `goIn` returns true, this method will
    * be called for each index of the just-visited `Json.Arr.Dbl`.
    */
  def visitDblIndex(index: Int, value: Double): this.type

  /** If and only if all the indices of a `Json.Arr.Dbl` were visited, this method will
    * be called to indicate that the double array has been completed.
    */
  def outOfDblArr: this.type

  /** This method is called each time a JSON array of JSON values is visited.
    * It is called before any of the contents are visited.
    */
  def visit(jaa: Json.Arr.All): this.type

  /** After a `Json.Arr.All` is visited and `goIn` returns true, this method will
    * be called to declare the index of the array for the next visit.  After this
    * method is called, the appropriate other `visit` method will be called.  It
    * is the responsibility of implementing classes to cache the index information
    * if it is to inform what happens when the content at that index is visited.
    */
  def nextIndex(index: Int): this.type

  /** If and only if all the indices of a `Json.Arr.All` were visited, this method
    * will be called to indicate that the array of JSON values has been completed.
    */
  def outOfAllArr: this.type

  /** This method is called each time a JSON object is visited.  It is called before
    * any of the contents are visited.
    */
  def visit(obj: Json.Obj): this.type

  /** After a `Json.Obj` is visited and `goIn` returns true, this method will be
    * called to declare the key before visiting each value (with the appropriate
    * `visit` method).  It is the responsibility of the implementing classes to
    * cache the key information if it is to inform what happens when the corresponding
    * value is visited.
    */
  def nextKey(index: Int, key: String): this.type

  /** If and only if all the entries of a `Json.Obj` were visited, this method
    * will be called to indicate that the JSON object has been completed.
    */
  def outOfObj: this.type

  /** After all elements of a JSON entity have been visited, this method will be called
    * to indicate the completion of the visitation run.
    */
  def finish: this.type

  private def traverseInto(aa: Json.Arr.All) {
    val vs = aa.values
    var i = 0
    while (i < vs.length) {
      nextIndex(i)
      traverseInto(vs(i))
      i += 1
    }
  }

  private def traverseInto(da: Json.Arr.Dbl) {
    val ds = da.doubles
    var i = 0
    while (i < ds.length) {
      visitDblIndex(i, ds(i))
      i += 1
    }
  }

  private def traverseInto(obj: Json.Obj) {
    var i = 0
    obj.foreach{ (k,v) =>
      nextKey(i,k)
      i += 1
      traverseInto(v)
    }
  }

  private def traverseInto(j: Json) {
    j match {
      case s: Json.Str => visit(s.text)
      case n: Json.Num => visit(n)
      case o: Json.Obj => visit(o); if (goIn) { traverseInto(o); outOfObj }
      case aa: Json.Arr.All => visit(aa); if (goIn) { traverseInto(aa); outOfAllArr }
      case da: Json.Arr.Dbl => visit(da); if (goIn) { traverseInto(da); outOfDblArr }
      case b: Json.Bool => visit(b.value)
      case n: Json.Null => visitNull
    }    
  }

  /** Runs through the JSON entity `j` by calling the methods on `JsonVisitor`. */
  def traverse(j: Json) {
    begin
    traverseInto(j)
    finish
  }
}


/** This class visits every component of a JSON entity, but
  * does nothing.  Subclasses are expected to override the
  * do-nothing methods with desired functionality.
  */
abstract class AbstractJsonVisitor extends JsonVisitor {
  def begin: this.type = this
  def visitNull: this.type = this
  def visit(truth: Boolean): this.type = this
  def visit(text: String): this.type = this
  def visit(num: Json.Num): this.type = this
  def goIn: Boolean = true
  def visit(jad: Json.Arr.Dbl): this.type = this
  def visitDblIndex(index: Int, value: Double): this.type = this
  def outOfDblArr: this.type = this
  def visit(jaa: Json.Arr.All): this.type = this
  def nextIndex(index: Int): this.type = this
  def outOfAllArr: this.type = this
  def visit(obj: Json.Obj): this.type = this
  def nextKey(index: Int, key: String): this.type = this
  def outOfObj: this.type = this
  def finish: this.type = this
}

