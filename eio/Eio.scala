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
        catch {
          case t if NonFatal(t) => fail(s"Error while reading ${underlying.getPath}")
        }
        finally { try { fis.close } catch { case t if NonFatal(t) => } }
      }
      catch {
        case t if NonFatal(t) => fail(s"Could not read ${underlying.getPath}")
      }
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
      act: Ok[(File, Option[ZipEntry]) => Unit, (File, Option[ZipEntry], Option[Array[Byte]]) => Unit],
      pick: File => Boolean,
      deeper: (File, Boolean) => Boolean,
      canonize: Boolean,
      unzip: Option[(File, ZipEntry) => Boolean]
    ) {
      val seen = new collection.mutable.AnyRefMap[File, Unit]()
      val unit: Unit = ()
      var pending = underlying :: Nil
      @tailrec def inner() {
        pending match {
          case f :: rest =>
            pending = rest
            val exists = try { f.exists } catch { case t if NonFatal(t) => false }
            if (exists) safeOption(f.getCanonicalFile) match {
              case Some(cf) if (!(seen contains cf)) =>
                seen += cf -> unit
                val g = if (canonize) cf else f
                val p = try { pick(g) } catch { case t if NonFatal(t) => false }
                val dir = try { g.isDirectory } catch { case t if NonFatal(t) => false }
                if (p) act match {
                  case No(op) => op(g, None)
                  case Yes(op) => 
                    if (!dir) op(g, None, None)
                    else (new FileShouldDoThis(g)).gulp match {
                    case Yes(a) => op(g, None, Some(a))
                    case _ =>
                  }
                }
                if (dir && { try { deeper(g,p) } catch { case t if NonFatal(t) => false } }) {
                  val children = g.listFiles.sortBy(_.getName)
                  var i = children.length
                  while (i > 0) {
                    i -= 1
                    pending = children(i) :: pending
                  }
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
      pick: File => Boolean = f => !f.isDirectory && !f.getName.startsWith("."),
      deeper: (File, Boolean) => Boolean = (f,b) => !b && { val n = f.getName; !n.startsWith(".") || n == "." || n == ".." },
      canonize: Boolean = false
    ): Array[File] = {
      val picked = Array.newBuilder[File]
      walk(No((f,_) => { picked += f; () }), pick, deeper, canonize, None)
      picked.result()
    }
  }
}

package eio {
}
