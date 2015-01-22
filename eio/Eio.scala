package kse

import scala.annotation.tailrec
import scala.util._
import scala.util.control.NonFatal
import scala.util.control.Breaks._

import kse.typecheck._
import kse.flow._

package object eio {
  import java.io._
  import java.nio._
  import java.util.zip._
  
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
          val k = p0.lastIndexOf('/',fid)
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
      val p0 = f0.getParentFile
      val name = f0.getName + "." + ext
      if (p0 == null) new File(name) else new File(p0, name)
    }
    
    def \:(parent: File) = {
      if (underlying.isAbsolute) new File(parent, underlying.getName)
      else new File(parent, underlying.getPath)
    }
    
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
    
    def gulp: Ok[String, Array[Byte]] = okay[String]{ fail =>
      if (underlying.isDirectory) fail(s"${underlying.getPath} is a directory")
      val sz = try { underlying.length } catch { case t if NonFatal(t) => fail(s"Could not read length of ${underlying.getPath}") }
      if (sz >= Int.MaxValue) fail(s"${underlying.getPath} is too big")
      try {
        val buf = try { new Array[Byte](sz.toInt) } catch { case oome: OutOfMemoryError => fail(s"Not enough memory to read ${underlying.getPath}") }
        val fis = new FileInputStream(underlying)
        try {
          val fc = fis.getChannel
          val bb = java.nio.ByteBuffer.wrap(buf)
          var ret = 0
          var zeros = 0
          while (bb.hasRemaining && ret != -1 && zeros < 4) {
            ret = fc.read(bb)
            if (ret > 0) zeros = 0 else zeros += 1
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
    
    def walk(
      act: Ok[(File, List[ZipEntry]) => Unit, (File, List[ZipEntry], Option[Array[Byte]]) => Unit],
      log: String => Unit,
      pick: File => Stance,
      canonize: Boolean,
      unzip: Option[(File, List[ZipEntry]) => Stance]
    ) {
      val seen = new collection.mutable.AnyRefMap[File, Unit]()
      val unit: Unit = ()
      var pending = underlying :: Nil
      
      def isZip(s: String) = s == ".zip" || s == ".jar"
          
      def ziply(zis: ZipInputStream, pze: (File, List[ZipEntry]) => Stance, inf: File, inze: List[ZipEntry] = Nil) {
        var ze = zis.getNextEntry
        while (ze != null) {
          val where = ze :: inze
          val stance = pze(inf, where)
          val myName = inf.getPath + "//" + where.map(_.getName).mkString("//")
          var buf: Array[Byte] = null
          
          def zebuf(): Boolean = {
            if (buf != null) true
            else {
              try {
                val sz = ze.getSize
                if (sz >= Int.MaxValue) {
                  log("Zip entry too big: " + myName)
                  false
                }
                else {
                  if (sz < 0) {
                    val bufs = Vector.newBuilder[Array[Byte]]
                    var n,k = 0
                    while (n >= 0 && k >= 0) {
                      val bufi = new Array[Byte](8192)
                      var i = 0
                      var zeros = 0
                      while (i < 8192 && zeros < 4 && k >= 0) {
                        k = zis.read(bufi, i, buf.length - i)
                        if (k > 0) {
                          zeros = 0
                          i += k
                        }
                        else zeros += 1
                      }
                      n += i
                      if (k < 0 && i < 8192) bufs += java.util.Arrays.copyOf(bufi, i)
                      else if (zeros >= 4) n = -1
                      else bufs += bufi
                    }
                    if (n < 0) buf = null
                    else {
                      buf = new Array[Byte](n)
                      var i = 0
                      bufs.result().foreach{ a => 
                        java.lang.System.arraycopy(a, 0, buf, i, a.length)
                        i += a.length
                      }
                    }
                  }
                  else { 
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
                  if (buf == null) {
                    log("Could not read zip entry: " + myName)
                    false
                  }
                  else {
                    zis.closeEntry
                    true
                  }
                }
              }
              catch {
                case soe: StackOverflowError => log("Stack overflow while recursing into " + myName); false
                case oome: OutOfMemoryError => log("Not enough memory to recurse into " + myName); false
                case t if NonFatal(t) => log("Unable to extract zip entry " + myName); false
              }
            }
          }
          
          stance match {
            case _: Selected =>
              act match {
                case No(op) => op(inf, where)
                case Yes(op) => if (zebuf()) op(inf, where, Some(buf))
              }
            case _ =>
          }
          stance match {
            case _: Recursed if isZip(ze.getName.takeRight(4).toLowerCase) && zebuf() =>
              try {
                val bais = new ByteArrayInputStream(buf)
                val zis2 = new ZipInputStream(bais)
                ziply(zis2, pze, inf, where)
              }
              catch { 
                case soe: StackOverflowError => log("Stack overflow while recursing into " + myName)
                case oome: OutOfMemoryError => log("Not enough memory to recurse into " + myName)
                case t if NonFatal(t) => log("Could not recurse into " + myName)
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
                val g = if (canonize) cf else f
                val p = try { pick(g) } catch { case t if NonFatal(t) => log("Error when considering "+g.getPath); Reject }
                val dir = try { g.isDirectory } catch { case t if NonFatal(t) => false }
                if (p.selected) act match {
                  case No(op) => op(g, Nil)
                  case Yes(op) => 
                    if (dir) op(g, Nil, None)
                    else (new FileShouldDoThis(g)).gulp match {
                      case Yes(a) => op(g, Nil, Some(a))
                      case _ => log("Failed to read file " + g.getPath)
                  }
                }
                if (dir && p.recursed) {
                  val children = try { g.listFiles.sortBy(_.getName) } catch { case t if NonFatal(t) => log("Could not read files in "+g.getName); Array[File]() }
                  var i = children.length
                  while (i > 0) {
                    i -= 1
                    pending = children(i) :: pending
                  }
                }
                else if (p.recursed) unzip match {
                  case Some(q) if isZip(g.getName.takeRight(4).toLowerCase) =>
                    try {
                      val fis = new FileInputStream(g)
                      try {
                        val zis = new ZipInputStream(fis)
                        ziply(zis, q, g)
                      }
                      catch { case t if NonFatal(t) => log("Could not recurse into file "+g.getPath+" because "+exceptionAsString(t)) }
                      finally { fis.close }
                    }
                    catch { case t if NonFatal(t) => log("Could not recurse into file "+g.getPath+" because "+exceptionAsString(t)) }
                  case _ =>
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
      pick: File => Stance = Pick.files,
      canonize: Boolean = false
    ): Ok[Vector[String], Array[File]] = {
      val picked = Array.newBuilder[File]
      val log = Vector.newBuilder[String]
      walk(No((f,_) => { picked += f; () }), s => { log += s; () }, pick, canonize, None)
      val v = log.result()
      if (v.nonEmpty) No(v) else Yes(picked.result())
    }
  }
  
  private[eio] def exceptionAsString(t: Throwable) = t.getClass.getName + ": " + Option(t.getMessage).getOrElse("") + "; " + t.getStackTrace.take(2).mkString("; ")
}

package eio {
  sealed trait Stance { def selected: Boolean = false; def recursed: Boolean = false }
  sealed trait Recursed extends Stance { override def recursed = true }
  sealed trait Selected extends Stance { override def selected = true }
  case object Reject extends Stance
  case object Recurse extends Recursed { def &(s: Select.type) = RecurseSelect }
  case object Select extends Selected { def &(r: Recurse.type) = RecurseSelect }
  case object RecurseSelect extends Recursed with Selected {}
  
  object Pick {
    import java.io._
    
    val files: File => Stance = f => {
      val n = f.getName
      if (f.isDirectory) { if (n.startsWith(".") && !{ val p = f.getPath; p == "." || p == ".." }) Reject else Recurse }
      else { if (n.startsWith(".")) Reject else Select }
    }
    
    def apply(p: String => Boolean): File => Stance = f => files(f) match {
      case Select if (!p(f.getName)) => Reject
      case x => x
    }
  }
}
