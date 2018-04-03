package kse.eio

import kse.flow._

/** Serializes and deserializes something-separated-values */
abstract class Xsv(val separators: String, val newline: String) {
  val quotables = {
    var i = 0
    while (separators.charAt(i) < '\n') i += 1
    var j = i
    while (separators.charAt(j) < '\r') j += 1
    var k = i
    while (separators.charAt(k) < '"') k += 1
    if (k == 0) "\n\r\"" + separators
    else if (i == separators.length) separators + "\n\r\""
    else {
      val sb = new java.lang.StringBuilder
      var z = 0
      if (separators.charAt(i) != '\n') {
        if (i > 0) sb.append(separators, 0, i)
        sb append '\n'
        z = i
      }
      if (separators.charAt(j) != '\r') {
        if (j > z) sb.append(separators, z, j)
        sb append '\r'
        z = j
      }
      if (separators.charAt(k) != '"') {
        if (k > z) sb.append(separators, z, k)
        sb append '"'
        z = k
      }
      if (z < separators.length) sb.append(separators, z, separators.length)
      sb.toString
    }
  }

  def isSeparator(c: Char) = CharSetDelim.sortedContains(separators, c)
  def needsQuote(c: Char) = CharSetDelim.sortedContains(quotables, c)

  def decode(s: String): Ok[Xsv.ErrorLocation, Array[Array[String]]] = {
    // aab stores complete lines
    val aab = Array.newBuilder[Array[String]]
    var aabn = 0

    // ab stores complete tokens
    val ab = Array.newBuilder[String]
    var abn = 0

    // sb builds up tokens if they can't be clipped out directly; always leave it empty after use!
    val sb = new java.lang.StringBuilder

    var i = 0
    while (i < s.length) {
      // When entering, we are always at the _beginning_ of a token!
      var c = s.charAt(i)
      // We will specially handle newlines at the end, as needed
      // After this conditional:
      //  - c must contain the separator character _after_ the token or 0 if no character
      //  - i must point after the character held by c, or at s.length if string exhausted
      if (c == '"') {
        val start = i
        var z = i+1               // First character of quoted block
        var j = s.indexOf('"', z) // Position of end quote
        var incomplete = true
        while (incomplete && j >= 0) {
          if (j+1 < s.length && s.charAt(j+1) == '"') {
            // Double quote (quote escape) case
            sb.append(s, z, j+1)
            z = j + 2
            j = s.indexOf('"', z)
          }
          else {
            // Single quote case; done now
            if (j > z) sb.append(s, z, j)
            incomplete = false
          }
        }
        if (incomplete) return No(Xsv.ErrorLocation(aabn + 1, start + 1))
        abn += 1
        ab += sb.toString
        sb.setLength(0)
        i = j+1
        if (i == s.length) c = 0
        else {
          c = s.charAt(i)
          if (!needsQuote(c)) return No(Xsv.ErrorLocation(aabn+1, i+1))
          i += 1
        }
      }
      else {
        var j = i
        while (j < s.length && { c = s.charAt(j); !needsQuote(c) }) j += 1
        if (j < s.length && j > i && c == '"') return No(Xsv.ErrorLocation(aabn+1, j+1))
        ab += (if (i == j) "" else s.substring(i, j))
        abn += 1
        if (j == s.length) { i = j; c = 0 }
        else i = j + 1
      }
      // Check for newline; store line if so.
      if (c == '\n' || c == '\r') {
        // Newline!  Store the current line.
        aab += ab.result
        aabn += 1
        ab.clear
        abn = 0
        if (c == '\r' && i < s.length && s.charAt(i) == '\n') i += 1  // Handle CRLF
      }
    }
    // If there was no newline, we still keep whichever tokens we found on the current line
    if (abn > 0) aab += ab.result
    Yes(aab.result)
  }

  def encode(lines: Array[Array[String]]): String = {
    val b = new java.lang.StringBuffer
    var i = 0
    val sep = separators.charAt(0)
    while (i < lines.length) {
      val line = lines(i)
      var j = 0
      while (j < line.length) {
        val tok = line(j)
        var k = 0
        while (k < tok.length && !needsQuote(tok.charAt(k))) k += 1
        if (k == tok.length) b append tok
        else {
          b append '"'
          var z = 0
          var qi = tok.indexOf('"', k)
          while (qi >= 0) {
            b.append(tok, z, qi+1)
            b append '"'
            z = qi + 1
            qi = tok.indexOf('"', z)
          }
          if (z < tok.length) b.append(tok, z, tok.length)
          b append '"'
        }
        j += 1
        if (j < line.length) b append sep 
      }
      i += 1
      b append newline
    }
    b.toString
  }
}

object Xsv {
  val emptyArrayOfStrings = new Array[String](0)

  case class ErrorLocation(line: Int, char: Int) {}
}


object Csv extends Xsv(",", "\n") {}


object Tsv extends Xsv("\t", "\n") {}
