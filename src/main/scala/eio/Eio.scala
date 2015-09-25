// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2015 Rex Kerr, UCSF, and Calico Labs.

package kse

import scala.annotation.tailrec
import scala.util._
import scala.util.control.NonFatal
import scala.util.control.Breaks._

import kse.typecheck._
import kse.flow._
import kse.coll._
import kse.maths._

package object eio {
  import java.io._
  import java.nio._
  import java.util.zip._
  
  implicit class ConvertSafelyFromByte(private val underlying: Byte) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asShort = underlying.toShort
    def asUShort = (underlying & 0xFF).toShort
    def asInt = underlying.toInt
    def asUInt = (underlying & 0xFF)
    def asLong = underlying.toLong
    def asULong = (underlying & 0xFF).toLong
    def asFloat = underlying.toFloat
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromShort(private val underlying: Short) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asByte(implicit oops: Oops) = if (underlying < Byte.MinValue || underlying > Byte.MaxValue) OOPS else underlying.toByte
    def asUByte(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFF) OOPS else underlying.toByte
    def asInt = underlying.toInt
    def asUInt = (underlying & 0xFFFF)
    def asLong = underlying.toLong
    def asULong = (underlying & 0xFFFF).toLong
    def asFloat = underlying.toFloat
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromInt(private val underlying: Int) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asByte(implicit oops: Oops) = if (underlying < Byte.MinValue || underlying > Byte.MaxValue) OOPS else underlying.toByte
    def asUByte(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFF) OOPS else underlying.toByte
    def asShort(implicit oops: Oops) = if (underlying < Short.MinValue || underlying > Short.MaxValue) OOPS else underlying.toShort
    def asUShort(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFFFF) OOPS else underlying.toShort
    def asLong = underlying.toLong
    def asULong = (underlying & 0xFFFFFFFFL)
    def asFloat(implicit oops: Oops) = { val f = underlying.toFloat; if (f.toInt != underlying) OOPS else f }
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromLong(private val underlying: Long) extends AnyVal {
    def asU(implicit oops: Oops) = if (underlying < 0) OOPS else underlying
    def asByte(implicit oops: Oops) = if (underlying < Byte.MinValue || underlying > Byte.MaxValue) OOPS else underlying.toByte
    def asUByte(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFF) OOPS else underlying.toByte
    def asShort(implicit oops: Oops) = if (underlying < Short.MinValue || underlying > Short.MaxValue) OOPS else underlying.toShort
    def asUShort(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFFFF) OOPS else underlying.toShort
    def asInt(implicit oops: Oops) = if (underlying < Int.MinValue || underlying > Int.MaxValue) OOPS else underlying.toInt
    def asUInt(implicit oops: Oops) = if (underlying < 0 || underlying > 0xFFFFFFFFL) OOPS else underlying.toInt
    def asFloat(implicit oops: Oops) = { val f = underlying.toFloat; if (f.toLong != underlying) OOPS else f }
    def asDouble(implicit oops: Oops) = { val d = underlying.toDouble; if (d.toLong != underlying) OOPS else d }
  }
  
  implicit class ConvertSafelyFromFloat(private val underlying: Float) extends AnyVal {
    def asByte(implicit oops: Oops) = { val b = math.rint(underlying).toByte; if (b.toFloat != underlying) OOPS else b }
    def asUByte(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFF) != underlying) OOPS else (i & 0xFF).toByte }
    def asShort(implicit oops: Oops) = { val s = math.rint(underlying).toShort; if (s.toFloat != underlying) OOPS else s }
    def asUShort(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFFFF) != underlying) OOPS else (i & 0xFFFF).toShort }
    def asInt(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if (i.toFloat != underlying) OOPS else i }
    def asUInt(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if ((l & 0xFFFFFFFFL) != underlying) OOPS else (l & 0xFFFFFFFFL).toInt }
    def asLong(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if (l.toFloat != underlying) OOPS else l }
    def asDouble = underlying.toDouble
  }
  
  implicit class ConvertSafelyFromDouble(private val underlying: Double) extends AnyVal {
    def asByte(implicit oops: Oops) = { val b = math.rint(underlying).toByte; if (b.toDouble != underlying) OOPS else b }
    def asUByte(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFF) != underlying) OOPS else (i & 0xFF).toByte }
    def asShort(implicit oops: Oops) = { val s = math.rint(underlying).toShort; if (s.toDouble != underlying) OOPS else s }
    def asUShort(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if ((i & 0xFFFF) != underlying) OOPS else (i & 0xFFFF).toShort }
    def asInt(implicit oops: Oops) = { val i = math.rint(underlying).toInt; if (i.toDouble != underlying) OOPS else i }
    def asUInt(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if ((l & 0xFFFFFFFFL) != underlying) OOPS else (l & 0xFFFFFFFFL).toInt }
    def asLong(implicit oops: Oops) = { val l = math.rint(underlying).toLong; if (l.toDouble != underlying) OOPS else l }
    def asFloat(implicit oops: Oops) = { val f = underlying.toFloat; if (f.toDouble != underlying) OOPS else f }
  }
  

  object \: {
    def unapply(af: Array[File]): Option[(File, Array[File])] = {
      if (af.length == 0) None
      else if (af.length == 1) unapply(af(0)).map{ case (f, g) => (f, Array(g)) }
      else {
        val p0 = af(0).getPath
        var fid = p0.length
        var i = 1
        while (i < af.length && fid > 0) {
          var j = 0
          var p = af(i).getPath
          while (j < fid && j < p.length && p(j) == p0(j)) j += 1
          fid = j
          i += 1
        }
        if (fid == 0) None
        else {
          val k = p0.lastIndexOf(File.separatorChar,fid)
          if (k < 0) None
          else Some( (new File(p0.substring(0, k+1)), af.map(f => new File(f.getPath.substring(k+1)))) )
        }
      }
    }
    def unapply(f: File): Option[(File, File)] = {
      val p = f.getParentFile
      if (p == null) None else Some((p,f))
    }
  }
  
  object % {
    def unapply(f: File): Option[(File, String)] = {
      val n = f.getName
      val i = n.lastIndexOf('.')
      var pre = i-1
      while (pre >= 0 && n(pre) == '.') pre -= 1
      if (pre <= 0 || i+1 >= n.length) None
      else {
        val p = f.getParentFile
        val nf = if (p != null) new File(p, n.substring(0, pre+1)) else new File(n.substring(0, pre+1))
        Some((nf, n.substring(i+1)))
      }
    }
  }
  
  implicit class StringAsFile(private val underlying: String) extends AnyVal {
    def file = new File(underlying)
    def \:(parent: File) = new File(parent, underlying)
  }
  
  implicit class ZipEntryProperPaths(private val underlying: ZipEntry) extends AnyVal {
    def name = {
      val n = underlying.getName
      val i = n.indexOf('/')
      val j = n.indexOf('\\')
      if (i < 0 && j > 0) n.replace('\\', '/') else n
    }
    def file = {
      val n = name
      if (File.separatorChar == '/') new File(n) else new File(n.replace('/',File.separatorChar))
    }
  }

  implicit class FileShouldDoThis(private val underlying: java.io.File) extends AnyVal {
    def %(ext: String) = {
      val f0 = underlying match {
        case fi % _ => fi
        case _ => underlying
      }
      if (ext == null) f0
      else {
        val p0 = f0.getParentFile
        val name = f0.getName + "." + ext
        if (p0 == null) new File(name) else new File(p0, name)
      }
    }
    
    def \:(parent: File) = {
      if (underlying.isAbsolute) new File(parent, underlying.getName)
      else new File(parent, underlying.getPath)
    }
    
    def canon = underlying.getCanonicalFile
    def parent = Option(underlying.getParentFile)
    
    def path = underlying.getPath
    def zipname = {
      if (underlying.isAbsolute) {
        var f = underlying
        var names = List.empty[String]
        while (f != null) {
          val p = f.getParentFile
          names = f.getName :: names
          f = p
        }
        names match {
          case first :: rest => rest.mkString("/")
          case _ => ""
        }
      }
      else {
        if (File.separatorChar == '/') path else path.replace(File.separatorChar, '/')
      }
    }
    
    def name = underlying.getName
    def nameFn(f: String => String) = {
      val p = underlying.getParentFile
      if (p == null) new File(f(underlying.getName)) else new File(p, f(underlying.getName))
    }
    
    def ext = underlying match { case _ % x => x; case _ => "" }
    def extFn(f: String => String) = this.%(f(ext))
    
    def base = underlying match { case f % _ => f.getName; case _ => underlying.getName }
    def baseFn(f: String => String) = underlying match {
      case p \: b % x => new File(p, f(b.getName) + "." + x)
      case p \: b => new File(p, f(b.getName))
      case b % x => new File(f(b.getName) + "." + x)
      case _ => new File(f(underlying.getName))
    }
    
    def relativize(absolutes: Array[File]): Ok[Vector[String], Array[File]] = {
      val af = try { underlying.getAbsoluteFile } catch { case t if NonFatal(t) => return No(Vector("Could not find absolute form of root " + underlying.getPath)) }
      val cf = try { af.getCanonicalFile } catch { case t if NonFatal(t) => return No(Vector("Could not find canonical form of root " + af.getPath)) }
      val ap = af.getPath
      val cp = cf.getPath
      val wrongs = Vector.newBuilder[String]
      val rights = Array.newBuilder[File]
      def clip(h: File, xp: String): Option[File] = {
        val hp = h.getPath
        if (hp startsWith xp) {
          val n = if (xp.length < hp.length && hp(xp.length) == File.separatorChar) xp.length+1 else xp.length
          if (hp.length < n+1) None
          else Some(new File(hp.substring(n)))
        }
        else None
      }
      absolutes.foreach{ g =>
        okay[String]{ fail =>
          val ag = try { g.getAbsoluteFile } catch { case t if NonFatal(t) => fail("Could not find absolute form of " + g.getPath) }
          clip(ag, ap).getOrElse {
            val cg = try { ag.getCanonicalFile } catch { case t if NonFatal(t) => fail("Could not find canonical form of " + ag.getPath) }
            clip(cg, cp).getOrElse{ fail(s"$cp is not a root for $cg") }
          }
        } match {
          case Yes(x) => rights += x
          case No(e) => wrongs += e
        }
      }
      val w = wrongs.result()
      if (w.nonEmpty) No(w) else Yes(rights.result())
    }
    
    def gulp: Ok[String, Array[Byte]] = okay[String]{ fail =>
      if (underlying.isDirectory) fail(s"${underlying.getPath} is a directory")
      val sz = try { underlying.length } catch { case t if NonFatal(t) => fail(s"Could not read length of ${underlying.getPath}") }
      if (sz >= Int.MaxValue) fail(s"${underlying.getPath} is too big")
      try {
        val buf = try { new Array[Byte](sz.toInt) } catch { case oome: OutOfMemoryError => fail(s"Not enough memory to read ${underlying.getPath}") }
        val fis = new FileInputStream(underlying)
        try {
          var i = 0
          var ret = 0
          var zeros = 0
          while (i < buf.length && ret != -1 && zeros < 4) {
            ret = fis.read(buf, i, math.min(262144, buf.length - i))
            if (ret < 0) zeros += 1
            else {
              zeros = 0
              i += ret
            }
          }
          buf
        }
        catch { case t if NonFatal(t) => fail(s"Error while reading ${underlying.getPath}") }
        finally { try { fis.close } catch { case t if NonFatal(t) => } }
      }
      catch { case t if NonFatal(t) => fail(s"Could not read ${underlying.getPath}") }
    }
    
    def slurp: Ok[String, Vector[String]] = okay[String]{ fail =>
      val src = try { scala.io.Source.fromFile(underlying) } catch { case t if NonFatal(t) => fail(s"Could not open ${underlying.getPath}") }
      try {
        try { src.getLines.toVector }
        catch { case t if NonFatal(t) => fail(s"Error while reading ${underlying.getPath}") }
      }
      catch { case oome: OutOfMemoryError => fail(s"Out of memory reading ${underlying.getPath}") }
      finally { try { src.close } catch { case t if NonFatal(t) => } }
    }
    
    def walk(act: FileWalker, log: FileLogger, sizeLimit: Int = Int.MaxValue) {
      val fw = act match { case fwi: FileWalkImpl => fwi }
      
      val seen = new collection.mutable.AnyRefMap[File, Unit]()
      val unit: Unit = ()
      var pending = underlying :: Nil
      
      def isZip(s: String) = s == ".zip" || s == ".jar"
      
      def ziply(zis: ZipInputStream, inzes: List[ZipEntry]) {
        var ze = zis.getNextEntry
        while (ze != null) {
          val zes = ze :: inzes
          fw.zes = zes
          val stance = fw.picker
          val myName = fw.file.getPath + "//" + zes.map(_.getName).mkString("//")
          var buf: Array[Byte] = null
          var stp: Stepper[Array[Byte]] = null
          var consumed = false
          
          stance match {
            case _: Selected =>
              val sz = ze.getSize
              var oversize = sz > sizeLimit
              if (sz < 0) {
                consumed = true
                val bufs = Vector.newBuilder[Array[Byte]]
                var n = 0
                var go = true
                stp = new InputStreamStepper(zis, 8192)
                while (n < sizeLimit && go) {
                  go = stp.step{ b => n += b.length; bufs += b }
                }
                if (!go) {
                  stp = null
                  buf = new Array[Byte](n)
                  var i = 0
                  bufs.result().foreach{ a => 
                    java.lang.System.arraycopy(a, 0, buf, i, a.length)
                    i += a.length
                  }
                }
                else {
                  oversize = true
                  stp = bufs.result().iterator.stepper ++ stp
                }
              }
              else if (sz < sizeLimit) {
                consumed = true
                buf = new Array[Byte](sz.toInt)
                var i = 0
                var zeros = 0
                while (i < buf.length && zeros < 4) {
                  val k = zis.read(buf, i, buf.length - i)
                  if (k > 0) {
                    zeros = 0
                    i += k
                  }
                  else zeros += 1
                }
                if (zeros >= 4) buf = null
              }
              fw match {
                case fsw: FileWalkOnStreams if oversize =>
                  consumed = true
                  fsw.stream = if (stp == null) zis else (new SteppedByteArrayInputStream(stp))
                  fsw.streamOp
                  if (stp != null) stp = null
                case fbw: FileWalkOnBuffers if buf != null =>
                  fbw.buffer = buf
                  fbw.bufOp
                case fsw: FileWalkOnStreams if (!consumed || buf != null || stp != null) =>
                  fsw.stream = if (!consumed) zis else if (buf == null) new ByteArrayInputStream(buf) else new SteppedByteArrayInputStream(stp)
                  consumed = true
                  fsw.streamOp
                  if (stp != null) stp = null
                case fbw: FileWalkOnBuffers =>
                  log(myName, "Could not read zip entry into buffer", "buffer missing")
                case fsw: FileWalkOnStreams =>
                  log(myName, "Could not read zip entry into stream", "already consumed")
                case _ =>
                  fw.listOp
              }
            case _ =>
          }
          stance match {
            case _: Recursed if isZip(ze.getName.takeRight(4).toLowerCase) =>
              try {
                if (buf == null && consumed) log(myName, "Unable to buffer entry for both reading and recursing", "both recursing into and processing archive raw without adequate buffering")
                else if (buf != null) {
                  val bais = new ByteArrayInputStream(buf)
                  val zis2 = new ZipInputStream(bais)
                  ziply(zis2, zes)
                }
                else try { ziply(new ZipInputStream(zis), zes) } finally { zis.closeEntry }
              }
              catch { 
                case soe: StackOverflowError => log(myName, "Stack overflow while recursing", exceptionAsString(soe))
                case oome: OutOfMemoryError => log(myName, "Not enough memory to recurse", exceptionAsString(oome))
                case t if NonFatal(t) => log(myName, "Could not recurse", exceptionAsString(t))
              }
            case _ =>
          }
          
          ze = zis.getNextEntry
        }
      }
      
      @tailrec def inner() {
        pending match {
          case f :: rest =>
            pending = rest
            val exists = try { f.exists } catch { case t if NonFatal(t) => false }
            if (exists) safeOption(f.getCanonicalFile) match {
              case Some(cf) if (!(seen contains cf)) =>
                seen += cf -> unit
                fw.file = if (fw.canonized) cf else f
                fw.zes = Nil
                val p = try { fw.picker } catch { case t if NonFatal(t) => log(fw.file.getPath, "Error selecting during walk", exceptionAsString(t)); Reject }
                val dir = try { fw.file.isDirectory } catch { case t if NonFatal(t) => false }
                if (p.selected) {
                  if (dir) fw.listOp
                  else {
                    fw match {
                      case fbsw: FileWalkOnBuffers with FileWalkOnStreams =>
                        if (fw.file.length < sizeLimit) {
                          (new FileShouldDoThis(fw.file)).gulp match {
                            case Yes(b) => fbsw.buffer = b; fbsw.bufOp; fbsw.buffer = null
                            case No(n) => log(fw.file.getPath, "Failed to buffer file", n)
                          }
                        }
                        else {
                          val fis = new FileInputStream(fw.file)
                          try { fbsw.stream = fis; fbsw.streamOp }
                          catch { case t if NonFatal(t) => log(fw.file.getPath, "Failed to stream file", exceptionAsString(t)) }
                          finally { fis.close; fbsw.stream = null }
                        }
                      case fsw: FileWalkOnStreams =>
                        val fis = new FileInputStream(fw.file)
                        try { fsw.stream = fis; fsw.streamOp }
                        catch { case t if NonFatal(t) => log(fw.file.getPath, "Failed to stream file", exceptionAsString(t)) }
                        finally { fis.close; fsw.stream = null }
                      case fbw: FileWalkOnBuffers =>
                        (new FileShouldDoThis(fw.file)).gulp match {
                          case Yes(b) => fbw.buffer = b; fbw.bufOp; fbw.buffer = null
                          case No(n) => log(fw.file.getPath, "Failed to buffer file", n)
                        }
                      case _ => fw.listOp
                    }
                  }
                }
                if (p.recursed) {
                  if (dir) {
                    val children = try { fw.file.listFiles.sortBy(_.getName) } catch { case t if NonFatal(t) => log("Could not read files in "+fw.file.getName); Array[File]() }
                    var i = children.length
                    while (i > 0) {
                      i -= 1
                      pending = children(i) :: pending
                    }
                  }
                  else if (isZip(fw.file.getName.takeRight(4).toLowerCase)) {
                    try {
                      val fis = new FileInputStream(fw.file)
                      try {
                        val zis = new ZipInputStream(fis)
                        ziply(zis, Nil)
                        zis.close
                      }
                      catch { case t if NonFatal(t) => log("Could not recurse into file "+fw.file.getPath+" because "+exceptionAsString(t)) }
                      finally { fis.close }
                    }
                    catch { case t if NonFatal(t) => log("Could not recurse into file "+fw.file.getPath+" because "+exceptionAsString(t)) }
                  }
                  else log("Do not know how to recurse into " + fw.file.getPath)
                }
              case _ =>
            }
            inner()
          case Nil =>
        }
      }
      
      inner()
    }
    
    def tree(
      pick: FileWalk => Stance = Pick.files,
      canonize: Boolean = false
    ): Ok[Vector[String], Array[File]] = {
      val picked = Array.newBuilder[File]
      val log = FileLogger.vector
      walk(FileWalk.listed(pick, canonize)(picked += _.file), log)
      val v = log.result()
      if (v.nonEmpty) No(v) else Yes(picked.result())
    }

    def createParents() {
      var p = canon.getParentFile
      var ps = List.empty[File]
      while (p != null && !p.exists) { ps = p :: ps; p = p.getParentFile }
      ps.foreach(pi =>
        if (!pi.mkdir) throw new IOException("Unable to create parent directory " + pi.getPath)
      )
    }
    
    def copyTo(target: File) {
      val fis = new java.io.FileInputStream(underlying)
      try {
        val fos = new java.io.FileOutputStream(target)
        try { fos.getChannel.transferFrom(fis.getChannel, 0, Long.MaxValue ) }
        finally { fos.close }
      }
      finally { fis.close }
    }
    
    def slurpAll(p: String => Boolean, inZip: Boolean = false): Ok[Ok[Throwable,(Map[String,Vector[String]], Map[String,Vector[String]])], Map[String, Vector[String]]] = {
      val slurped = Vector.newBuilder[(String, Vector[String])]
      val errors = FileLogger.map
      safe{
        val pickAll = if (inZip) Pick.leaves else Pick.files
        val select: (FileWalk => Stance) = (fw) => pickAll(fw) match {
          case sel: Selected =>
            val keep = if (fw.zes.isEmpty) p(fw.file.getPath) else p(fw.file.getPath + fw.zes.mkString("//","//",""))
            if (keep) sel else Reject // No select + recurse allowed in this API
          case x => x
        }
        walk(FileWalk(select, true)(_ => ()){ fb =>
            slurped += ((fb.name, scala.io.Source.fromInputStream(new ByteArrayInputStream(fb.buffer)).getLines.toVector))
          }{ fis =>
            slurped += ((fis.name, scala.io.Source.fromInputStream(fis.stream).getLines.toVector))
          },
          errors,
          1048576 // 1M buffer should be plenty for efficiently grabbing small files
        )
        val em = errors.result
        if (em.isEmpty) Yes(slurped.result.toMap) else No((em, slurped.result.toMap))
      } match {
        case No(t) => No(No(t))
        case Yes(No(x)) => No(Yes(x))
        case Yes(Yes(y)) => Yes(y)
      }
    }
  }
  
  implicit class InputStreamsShouldDoThis(private val underlying: InputStream) extends AnyVal {
    def stepper(size: Int = 8192): Stepper[Array[Byte]] = new InputStreamStepper(underlying, size)
  }
  
  implicit class ConvenientFileOutput(private val underlying: TraversableOnce[String]) extends AnyVal {
    def toFile(f: File, lineEnding: String = null) {
      val p = new java.io.PrintWriter(f)
      try { if (lineEnding == null) underlying.foreach(p.println) else underlying.foreach(x => p.print(x + lineEnding)) } finally { p.close() }
    }
  }
  
  private[eio] def exceptionAsString(t: Throwable) = t.getClass.getName + ": " + Option(t.getMessage).getOrElse("") + "; " + t.getStackTrace.take(2).mkString("; ")
}

package eio {
  import java.io._
  import java.util.zip._
  
  /** Note: the minimum chunk size is 256 */
  class InputStreamStepper(is: InputStream, chunkSize: Int) extends Stepper[Array[Byte]] {
    private var buf = new Array[Byte](math.max(256, chunkSize))
    private var isEmpty = false
    def step(f: Array[Byte] => Unit) = {
      if (isEmpty) false
      else {
        var i, k = 0
        var zeros = 0
        while (i < buf.length && zeros < 4 && k >= 0) {
          k = is.read(buf, i, buf.length - i)
          if (k > 0) {
            zeros = 0
            i += k
          }
          else zeros += 1
        }
        if (k < 0) isEmpty = true
        if (i <= 0) false
        else {
          f(java.util.Arrays.copyOf(buf,i))
          true
        }
      }
    }
  }
  
  class SteppedByteArrayInputStream(stepper: Stepper[Array[Byte]]) extends InputStream {
    private[this] var working: Array[Byte] = null
    private[this] var taken: Int = 0
    private[this] var exhausted = false
    private[this] def prepared(): Boolean = {
      if (!exhausted) {
        if (working == null || taken >= working.length) {
          taken = 0
          exhausted = !stepper.step(working = _)
          prepared()
        }
        else true
      }
      else { working = null; false }
    }
    override def available() = if (exhausted || working == null) 0 else (working.length - taken)
    override def close() {}
    override def mark(readlimit: Int) {}
    override def markSupported = false
    override def read(): Int = { if (prepared()) { var ans = working(taken) & 0xFF; taken += 1; ans } else -1 }
    override def read(b: Array[Byte]): Int = read(b, 0, b.length)
    override def read(b: Array[Byte], off: Int, len: Int): Int = if (exhausted) -1 else if (len <= 0) 0 else {
      var m = len
      var o = off
      while (prepared()) {
        val k = math.min(working.length - taken, m)
        System.arraycopy(working, taken, b, o, k)
        o += k
        taken += k
        m -= k
        if (m <= 0) return len
      }
      if (len == m) -1 else len - m
    }
    override def reset() {}
    override def skip(n: Long): Long = if (exhausted) -1 else if (n <= 0) 0 else {
      var m = n
      while (prepared()) {
        if (m < working.length - taken) {
          taken = m.toInt
          return n
        }
        else m -= (working.length - taken)
      }
      if (m >= n) -1 else n - m
    }
  }
  
  
  sealed trait Stance { def selected: Boolean = false; def recursed: Boolean = false }
  sealed trait Recursed extends Stance { override def recursed = true }
  sealed trait Selected extends Stance { override def selected = true }
  case object Reject extends Stance
  case object Recurse extends Recursed { def &(s: Select.type) = RecurseSelect }
  case object Select extends Selected { def &(r: Recurse.type) = RecurseSelect }
  case object RecurseSelect extends Recursed with Selected {}
  
  object Pick {
    val files: FileWalk => Stance = fw => {
      if (fw.zes.nonEmpty) Reject
      else {
        // (Some versions of?) Windows has an infuriating habit of making root directories hidden.  Ugly hack to fix this.
        if (fw.file.isHidden && (File.separatorChar != '\\' || { val af = fw.file.getAbsoluteFile; af.getParentFile != null })) Reject
        else if (fw.file.isDirectory) Recurse
        else Select
      }
    }
    
    val leaves: FileWalk => Stance = fw => fw.zes match {
      case Nil =>
        files(fw) match {
          case Select if fw.file.getName.toLowerCase.endsWith("zip") => Recurse
          case x => x
        }
        case ze :: more =>
          if (ze.isDirectory) Reject
          else if (ze.getName.toLowerCase.endsWith("zip")) Recurse
          else Select
    }
    
    def toDepth(depth: Int): FileWalk => Stance = fw => leaves(fw) match {
      case Recurse if fw.zes.lengthCompare(depth) <= 0 && fw.zes.headOption.exists(_.getName.toLowerCase.endsWith("zip")) => Select
      case x => x
    }
    
    def filesNamed(p: String => Boolean): FileWalk => Stance = fw => files(fw) match {
      case Select if (!p(fw.file.getName)) => Reject
      case x => x
    }
    def leavesNamed(p: String => Boolean): FileWalk => Stance = fw => leaves(fw) match {
      case Select if (!p(fw.zes.headOption.map(_.getName).getOrElse(fw.file.getName))) => Reject
      case x => x
    }
    
    def filesFiltered(p: File => Boolean): FileWalk => Stance = fw => files(fw) match {
      case Select if (!p(fw.file)) => Reject
      case x => x
    }
  }
  
  trait FileLogger extends (String => Unit) {
    protected def explain(where: String, what: String, reason: String) = s"$what in $where because $reason"
    def apply(where: String, what: String, reason: String) { apply(explain(where, what, reason)) }
    def apply(s: String): Unit
  }
  trait FileLogsTo[A] extends FileLogger {
    def result(): A
  }
  object FileLogger {
    def vector: FileLogsTo[Vector[String]] = new FileLogsTo[Vector[String]] {
      private[this] val myLog = Vector.newBuilder[String]
      def apply(s: String) { myLog += s }
      def result() = myLog.result()
    }
    def map: FileLogsTo[Map[String, Vector[String]]] = new FileLogsTo[Map[String, Vector[String]]] {
      private type Vb = collection.mutable.Builder[String, Vector[String]]
      private[this] val myLog = new collection.mutable.AnyRefMap[String, Vb]
      def apply(s: String) { myLog.getOrElseUpdate("", Vector.newBuilder[String]) += s }
      override def apply(where: String, what: String, reason: String) { myLog.getOrElseUpdate(where, Vector.newBuilder[String]) += explain(where, what, reason) }
      def result() = {
        val mb = Map.newBuilder[String, Vector[String]]
        myLog.foreach{ case (k,v) => mb += ((k, v.result())) }
        myLog.clear()
        mb.result()
      }
    }
  }
  
  sealed trait FileWalker { def name: String }
  sealed abstract class FileWalk extends FileWalker {
    def file: File
    def zes: List[ZipEntry]
    def name: String = if (zes.isEmpty) file.getPath else file.getPath + zes.mkString("//","//","")
  }
  sealed trait FileBufferWalk extends FileWalk {
    def buffer: Array[Byte]
  }
  sealed trait FileStreamWalk extends FileWalk {
    def stream: InputStream
  }
  private[eio] sealed abstract class FileWalkImpl extends FileWalk {
    var file: File = null
    var zes: List[ZipEntry] = Nil
    def picker: Stance
    def canonized: Boolean
    def listOp: Unit
  }
  private[eio] sealed trait FileWalkOnBuffers extends FileWalkImpl with FileBufferWalk {
    var buffer: Array[Byte] = null
    def bufOp: Unit
  }
  private [eio] sealed trait FileWalkOnStreams extends FileWalkImpl with FileStreamWalk {
    var stream: InputStream = null
    def streamOp: Unit
  }
  object FileWalk {
    def listed(pick: FileWalk => Stance, canonize: Boolean = false)(f: FileWalk => Unit): FileWalker = new FileWalkImpl {
      def picker = pick(this)
      def canonized = canonize
      def listOp { f(this) }
    }
      
    def blocked(pick: FileWalk => Stance, canonize: Boolean = false)(fdir: FileWalk => Unit)(fbuf: FileBufferWalk => Unit): FileWalker = new FileWalkOnBuffers {
      def picker = pick(this)
      def canonized = canonize
      def listOp { fdir(this) }
      def bufOp { fbuf(this) }
    }
    
    def streamed(pick: FileWalk => Stance, canonize: Boolean = false)(fdir: FileWalk => Unit)(fis: FileStreamWalk => Unit): FileWalker = new FileWalkOnStreams {
      def picker = pick(this)
      def canonized = canonize
      def listOp { fdir(this) }
      def streamOp{ fis(this) }
    }
    
    def apply(pick: FileWalk => Stance, canonize: Boolean = false)(fdir: FileWalk => Unit)(fbuf: FileBufferWalk => Unit)(fis: FileStreamWalk => Unit): FileWalker = new FileWalkOnBuffers with FileWalkOnStreams {
      def picker = pick(this)
      def canonized = canonize
      def listOp { fdir(this) }
      def bufOp { fbuf(this) }
      def streamOp { fis(this) }
    }    
  }

  object Args {
    class OptionSource(val options: Array[String]) {
      val ops = collection.mutable.AnyRefMap[String, List[String]]()
      options.reverse.foreach(o => (if (o.startsWith("--")) o.drop(2) else o.drop(1)) fn { s =>
        val i = s.indexOf('=')
        if (i < 0) ops += s -> ("" :: ops.getOrElse(s, Nil))
        else {
          val k = s.take(i)
          ops += k -> (s.drop(i+1) :: ops.getOrElse(k, Nil))
        }
      })
      def has(s: String): Boolean = {
        ops.get(s) match {
          case Some(x :: more) => if (more.isEmpty) ops -= s else ops += s -> more; true
          case _ => false
        }
      }
      def get(s: String): Option[String] = {
        ops.get(s).flatMap{ _ match {
          case x :: more => if (more.isEmpty) ops -= s else ops += s -> more; Some(x)
          case _ => None
        }}
      }
      def peek = ops.toArray.flatMap{ case (k,v) => v.map(x => if (x.nonEmpty) "-" + k + "=" + x else "-" + k) }.sorted
      def isEmpty = ops.forall{ case (k,v) => v.isEmpty }
    }
    
    def apply(args: Array[String]): (Array[String], OptionSource) = {
      val i = args.indexWhere(_ == "--")
      if (i < 0) args.partition(a => !a.startsWith("-")) _2Fn ( x => new OptionSource(x) )
      else args.take(i).partition(a => !a.startsWith("-")) eachFn (_ ++ args.drop(i+1), x => new OptionSource(x))
    }
  }

  /*
  object Text {
    def margin(label: String, content: Seq[String], lmargin: Int, rmargin: Int = 79, wrapIndicator: String = "", mergeShort: String = null): Seq[String] = {
      if (content.length == 0) content :+ label
      else {
        val iL = lmargin min 0
        val iR = iL max rmargin
        val N = 1 + iR - iL
        val cN = content.map(_.length).sum
        if (content.length == 1 || (mergeShort != null && cN + (content.length-1)*mergeShort.length <= N)) {
          if (label.length + 1 < iL) {
            if (content.length == 1)
          }
        }        
      }
    }    
  }
  */
}
