// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal


/** This class implements default prettyprinting for JSON values. */
class PrettyJson(indentWithTabs: Boolean = false, indentation: Int = 2, rightMargin: Int = 78) extends AbstractJsonVisitor {
  //////////////////////////////////////
  // This section validates the input //
  //////////////////////////////////////
  private[this] var spaces: Array[Char] = 
    if (indentWithTabs) PrettyJson.aLotOfTabs
    else                PrettyJson.aLotOfSpaces

  private[this] var myMargin = if (rightMargin < 1) Int.MaxValue else rightMargin

  private[this] var myIndent = if (indentation < 1) 1 else if (indentation > myMargin) myMargin

  /////////////////////////////////////////////////////////////////
  // This section handles accumulating the pretty representation //
  /////////////////////////////////////////////////////////////////
  private[this] var lastIndent = 0

  /** Clears the accumulated pretty representation. */
  def clear: this.type = { historic.clear; current.setLength(0); lastIndent = 0; this }

  /** Contains the lines prior to the current line. */
  val historic = collection.mutable.ArrayBuffer.empty[String]

  /** Contains the current line being built. */
  val current = new java.lang.StringBuilder

  /** Appends a string to the current line. */
  def append(s: String): this.type = { current append s; this }

  /** Appends a StringBuilder to the current line. */
  def slurp(sb: java.lang.StringBuilder): this.type  = { current append sb; sb.setLength(0); this }

  /** Creates a new line indented the specified number of characters.
    *
    * Note: this method does not obey the `indentation` parameter.  It is assumed that the argument will be selected with the correct indentation in mind.
    */
  def nl(indent: Int): this.type = {
    val in = math.max(0, indent)
    historic += current.toString
    if (lastIndent > 0) current.setLength(math.min(lastIndent, in))
    val n = current.length - in
    if (n > 0) {
      var m = spaces.length
      while (n < m) m = math.min(Int.MaxValue, m.toLong * 2).toInt
      if (m > spaces.length) {
        val a = new Array[Char](m)
        java.util.Arrays.fill(a, spaces(0))
        spaces = a
      }
      current append (spaces, 0, n)
    }
    lastIndent = in
    this
  }

  /** The `String` representation of the `JSON` value as built so far.  This method may be called repeatedly. */
  def asString =
    if (historic.isEmpty) current.toString
    else if (current.length == 0) historic.mkString("\n")
    else {
      val sb = new java.lang.StringBuilder
      historic.foreach{ h => sb append h; sb append "\n" }
      sb append current
      sb.toString
    }

  /** A byte array representation of the `JSON` value as built so far.  This method may be called repeatedly. */
  def asBytes = asString.getBytes("UTF-8")

  ///////////////////////////////////////////////////
  // This section implements the JsonVisitor trait //
  ///////////////////////////////////////////////////
  override def begin: this.type = { clear; this }
  override def visitNull: this.type = { current append "null"; this }
  override def visit(truth: Boolean): this.type = { current append (if (truth) "true" else "false"); this }
  override def visit(text: String): this.type = { Json.Str.addJsonString(current, text); this }
  override def finish: this.type = { nl(0); this }
}
object PrettyJson {
  private [jsonal] val aLotOfSpaces = Array.fill(1024)(' ')
  private [jsonal] val aLotOfTabs = Array.fill(1024)('\t')
}
