package kse

import scala.util._
import scala.util.control.NonFatal
import kse.flow._

package object eio {
  import java.io._
  import java.nio._
  
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
  }
}
