// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016 Rex Kerr and Calico Life Sciences.

package kse.jsonal


/** This class implements default prettyprinting for JSON values. */
class PrettyJson(indentWithTabs: Boolean = false, indentation: Int = 2, rightMargin: Int = 78) extends JsonVisitor {
  //////////////////////////////////////
  // This section validates the input //
  //////////////////////////////////////
  private[this] var spaces: Array[Char] = 
    if (indentWithTabs) PrettyJson.aLotOfTabs
    else                PrettyJson.aLotOfSpaces

  private[this] def ensureSpaces(n: Int) {
    var m = spaces.length
    while (n > m) m = math.min(Int.MaxValue, m.toLong * 2).toInt
    if (m > spaces.length) {
      val a = new Array[Char](m)
      java.util.Arrays.fill(a, spaces(0))
      spaces = a
    }
  }

  private[this] var myMargin = if (rightMargin < 1) Int.MaxValue else rightMargin

  private[this] var myIndent = if (indentation < 1) 1 else if (indentation > myMargin) myMargin else indentation

  private[this] var myOne = { ensureSpaces(myIndent); new String(spaces, 0, myIndent) }

  /////////////////////////////////////////////////////////////////
  // This section handles accumulating the pretty representation //
  /////////////////////////////////////////////////////////////////
  private[this] var lastIndent = 0
  private[this] var nextIndent = 0
  private[this] var wrapAt = new Array[Long](6)
  private[this] var keyed: Boolean = false
  var wrapN = 0

  private[this] def ensureWrapSpace {
    if (wrapN >= wrapAt.length)
      wrapAt = java.util.Arrays.copyOf(wrapAt, (wrapAt.length | (wrapAt.length << 1)) & 0x7FFFFFFE)
  }

  /** Clears the accumulated pretty representation. */
  def clear: this.type = { historic.clear; current.setLength(0); lastIndent = 0; nextIndent = 0; wrapN = 0; this }

  /** Contains the lines prior to the current line. */
  val historic = collection.mutable.ArrayBuffer.empty[String]

  /** Contains the current line being built. */
  val current = new java.lang.StringBuilder

  private[this] def markOpeningIfKeyed: this.type = {
    if (keyed) {
      ensureWrapSpace
      val w = current.length | 0x80000000L | ((nextIndent-1).toLong << 32)
      while (wrapN > 0 && wrapAt(wrapN-1) > w) wrapN -= 1
      wrapAt(wrapN) = w
      wrapN += 1
      keyed = false
    }
    this
  }

  private[this] def markComma: this.type = {
    val w = current.length | 0x80000000L | ((nextIndent-1).toLong << 32)
    while (wrapN > 0 && wrapAt(wrapN-1) > w) wrapN -= 1
    if (wrapN > 0 && ((wrapAt(wrapN-1) ^ w) & 0xFFFFFFFF80000000L) == 0) wrapAt(wrapN-1) = w
    else {
      ensureWrapSpace
      wrapAt(wrapN) = w
      wrapN += 1
    }
    this
  }

  /** Appends an opening brace to the current line. */
  def lbrace: this.type = {
    nextIndent += myIndent
    current append "{ "
    markOpeningIfKeyed
  }

  /** Appends a closing brace to the current line. */
  def rbrace: this.type = {
    nextIndent -= myIndent
    if (current.length >= myMargin - 2) tryWrap()
    if (current.length == nextIndent) current append "}"
    else if (current.length >= myMargin - 2) {
      nl(nextIndent)
      current append "}"
    }
    else current append " }"
    this
  }

  /** Appends an opening bracket to the current line. */
  def lbracket: this.type = {
    nextIndent += myIndent
    current append "[ "
    markOpeningIfKeyed
  }

  /** Appends a closing bracket to the current line. */
  def rbracket: this.type = {
    nextIndent -= myIndent
    if (current.length >= myMargin - 2) tryWrap()
    if (current.length == nextIndent) current append "]"
    else if (current.length >= myMargin - 2) {
      nl(nextIndent)
      current append "]"
    }
    else current append " ]"
    this
  }

  /** Appends a comma to the current line. */
  def comma: this.type = {
    keyed = false
    if (current.length >= myMargin - 1) {
      current append ", "
      tryWrap()
      if (nextIndent < lastIndent) {
        nl(nextIndent)
        wrapN = 0
      }
      else if (current.length > lastIndent) markComma
    }
    else if (nextIndent < lastIndent) {
      current append ","
      nl(nextIndent)
      wrapN = 0
    }
    else {
      current append ", "
      markComma
    }
    this
  }

  /** Appends a colon to the current line. */
  def colon: this.type = {
    current append ": "
    keyed = true
    this
  }

  /** Appends a string to the current line. */
  def append(s: String): this.type = {
    current append s
    this
  }

  /** Appends a JSON value to the current line (as a single line). */
  def append(j: Json): this.type = {
    j.jsonString(current)
    this
  }

  /** Appends a StringBuilder to the current line. */
  def slurp(sb: java.lang.StringBuilder): this.type  = {
    current append sb
    sb.setLength(0)
    this
  }

  protected def tryWrap(): this.type = {
    if (wrapN == 0) nl(nextIndent)
    else {
      val w = wrapAt(0)
      val i = (((w >>> 31) + 1) >>> 1).toInt
      val n = (w & 0x7FFFFFFFL).toInt
      if (current.length > n) {
        val shift = i - n
        historic += current.substring(0, n)
        val blank =
          if (i - lastIndent <= 0) ""
          else if (i - lastIndent == myIndent) myOne
          else { ensureSpaces(i - lastIndent); new String(spaces, 0, i - lastIndent) }
        //println(f"Switching because $i $n $w%016x ${current.length}")
        //println(current.toString)
        if (blank.nonEmpty) current.replace(lastIndent, n, blank)
        else current.delete(lastIndent, n)
        //println(current.toString)
        lastIndent = i
        var j = 1
        while (j < wrapN) {
          wrapAt(j-1) = wrapAt(j) + shift
          j += 1
        }
        wrapN -= 1
        if (current.length >= myMargin) return tryWrap()
      }
    }
    this
  }

  /** Creates a new line indented the specified number of characters.
    *
    * Note: this method does not obey the `indentation` parameter.  It is assumed that the argument will be selected with the correct indentation in mind.
    */
  def nl(indent: Int): this.type = {
    val in = math.max(0, indent)
    historic += current.toString
    if (lastIndent > 0) current.setLength(math.min(lastIndent, in))
    else current.setLength(0)
    val n = in - current.length
    if (n > 0) {
      ensureSpaces(n)
      current append (spaces, 0, n)
    }
    lastIndent = in
    wrapN = 0
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
  private[this] var wasEmpty: Boolean = false
  private[this] var mySb = new java.lang.StringBuilder

  def begin: this.type = { clear; this }
  def visitNull: this.type = { append("null"); this }
  def visit(truth: Boolean): this.type = { append(if (truth) "true" else "false"); this }
  def visit(text: String): this.type = { mySb setLength 0; Json.Str.addJsonString(mySb, text); slurp(mySb); this }
  def visit(num: Json.Num): this.type = { append(num.toString); this }
  def goIn: Boolean = if (wasEmpty) { wasEmpty = false; false } else true
  def visit(jad: Json.Arr.Dbl): this.type = {
    if (jad.size > 0) lbracket
    else { append("[]"); wasEmpty = true }
    this
  }
  def visitDblIndex(index: Int, value: Double): this.type = {
    if (index > 0) comma
    append(Json.Num(value))
    this
  }
  def outOfDblArr: this.type = rbracket
  def visit(jaa: Json.Arr.All): this.type = {
    if (jaa.size > 0) lbracket
    else { append("[]"); wasEmpty = true }
    this
  }
  def nextIndex(index: Int): this.type = { if (index > 0) comma; this }
  def outOfAllArr: this.type = rbracket
  def visit(obj: Json.Obj): this.type = { 
    if (obj.size > 0) lbrace
    else { append("{}"); wasEmpty = true }
    this
  }
  def nextKey(index: Int, key: String): this.type = {
    if (index > 0) comma
    mySb setLength 0
    Json.Str.addJsonString(mySb, key)
    slurp(mySb)
    colon
    this
  }
  def outOfObj: this.type = rbrace
  def finish: this.type = this
}
object PrettyJson {
  private [jsonal] val aLotOfSpaces = Array.fill(1024)(' ')
  private [jsonal] val aLotOfTabs = Array.fill(1024)('\t')
}

class PrettyContextJson(indentWithTabs: Boolean = false, indentation: Int = 2, rightMargin: Int = 78)
extends PrettyJson(indentWithTabs, indentation, rightMargin) {
  protected var context: List[Any] = Nil   // Note - ONLY put `String` or `Int` in here!!  (Need union types!)
  override def nextIndex(index: Int): this.type = {
    if (index == 0) context = index :: context
    else context = index :: context.tail
    super.nextIndex(index)
  }
  override def outOfAllArr: this.type = {
    context = context.tail
    super.outOfAllArr
  }
  override def nextKey(index: Int, key: String): this.type = {
    if (index == 0) context = key :: context
    else context = key :: context.tail
    super.nextKey(index, key)
  }
  override def outOfObj: this.type = {
    context = context.tail
    super.outOfObj
  }
}

class PrettyNumberJson(contextFormatter: List[Any] => PrettyNumberJson.Formatter = PrettyNumberJson.defaultContext, rightMargin: Int = 129)
extends PrettyContextJson(false, 2, rightMargin) {
  private[this] var fmt: PrettyNumberJson.Formatter = null
  override def visit(num: Json.Num): this.type = { append(contextFormatter(context) format num); this }
  override def visit(jad: Json.Arr.Dbl): this.type = {
    fmt = contextFormatter(context)
    super.visit(jad)
  }
  override def visitDblIndex(index: Int, value: Double): this.type = {
    if (index > 0) comma
    append(fmt format value)
    this
  }
}
object PrettyNumberJson {
  private[this] val eNumberFormatStrings = (0 to 18).map(i => s"%.${i}e")
  private[this] val fNumberFormatStrings = (0 to 21).map(i => s"%.${i}f")
  private[this] val powersOfTen = (0 to 308).map(i => s"1e$i".toDouble)
  private[this] val reciprocalPowersOfTen = (0 to 323).map(i => s"1e-$i".toDouble)
  trait Formatter {
    def format(d: Double): String
    def format(num: Json.Num): String
  }
  final case class FormatTo(absolutePrecisionDigit: Int, relativePrecisionDigits: Int) extends Formatter {
    private[this] val tooTiny = ("5e-" + math.max(0, math.min(324, absolutePrecisionDigit+1))).toDouble
    def format(d: Double): String = {
      if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) return "null"
      val ad = math.abs(d)
      val zero = if (d < 0) 1 else 0
      if (ad < tooTiny) return "0"
      if (ad >= 1e-3 && ad < 1e7) {
        val first =
          if (ad >= 10) {
            if (ad >= 1e4) {
              if (ad >= 1e5) {
                if (ad >= 1e6) 6
                else 5
              }
              else 4
            }
            else if (ad >= 1e2) {
              if (ad >= 1e3) 3
              else 2
            }
            else 1
          }
          else if (ad >= 1e-1) {
            if (ad >= 1) 0
            else -1
          }
          else if (ad >= 1e-2) -2
          else -3
        val f = math.min(21, math.max(0, math.min(relativePrecisionDigits - first, absolutePrecisionDigit)))
        val s = fNumberFormatStrings(f).format(d)
        if (f > 0 && s.charAt(s.length-1) == '0') {
          var i = s.length - 2
          while (i > zero && s.charAt(i) == '0') i -= 1
          if (i > zero && s.charAt(i) == '.') i -= 1
          s.substring(0, i+1)
        }
        else s
      }
      else {
        val first =
          if (ad > 1) {
            var k = 8
            var inc = 2
            while (k + inc < powersOfTen.length && ad >= powersOfTen(k+inc)) { k += inc; inc = inc*2 }
            var l = math.min(k + inc, powersOfTen.length - 1)
            while (l-k > 1) {
              var x = (l+k)/2
              if (ad >= powersOfTen(x)) k = x
              else l = x
            }
            if (ad >= powersOfTen(l)) l
            else k
          }
          else {
            var k = 4
            var inc = 2
            while (k + inc < reciprocalPowersOfTen.length && ad < reciprocalPowersOfTen(k+inc)) { k += inc; inc = inc*2 }
            var l = math.min(k + inc, reciprocalPowersOfTen.length - 1)
            while (l-k > 1) {
              var x = (l+k)/2
              if (ad < reciprocalPowersOfTen(x)) k = x
              else l = x
            }
            if (ad >= reciprocalPowersOfTen(k)) -k
            else -l
          }
        val e = math.min(18, math.max(0, math.min(relativePrecisionDigits, first + absolutePrecisionDigit)))
        val s = eNumberFormatStrings(e).format(d)
        val i = s.lastIndexOf('e')
        var j = i-1
        if (j > zero && s.charAt(j) == '0') {
          while (j > zero && s.charAt(j) == '0') j -= 1
          if (j > zero && s.charAt(j) == '.') j -= 1
          s.substring(0, j+1) + s.substring(i)
        }
        else s
      }
    }
      
    def format(jn: Json.Num): String = {
      if (jn.isLong) jn.long.toString
      else if (jn.isDouble) format(jn.double)
      else jn.toString
    }
  }
  def defaultContext = (xs: List[Any]) => defaultFormatter
  val defaultFormatter: Formatter = new Formatter {
    def format(d: Double): String =
      if (java.lang.Double.isNaN(d) || java.lang.Double.isInfinite(d)) "null"
      else d.toString
    def format(num: Json.Num): String = num.toString
  }
}
