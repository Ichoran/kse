package kse.proc

import java.io._
import java.nio.file._

import kse.flow._


final class FindFiles(nm: String, roots: Seq[Path], pick: Path => Boolean, val model: Act.InTime.Model = FindFiles.mkDefaultModel())
extends Acts[Array[Ok[Path, Path]], FindFiles.PickPath, Array[Path]](roots.iterator.map(p => new FindFiles.PickPath(p, model, pick)), nm) {
  import FindFiles.PickPath
  type E = String
  type S = collection.mutable.HashSet[Path]

  def cost = 1.0
  protected def excuse(message: String) = message
  protected def handler(t: Throwable) = t.explain()

  protected def process(act: PickPath)(result: Acts.Result[Array[Ok[Path, Path]], PickPath])(s: S) = {
    s += act.input
    s
  }

  protected def complete(results: Acts.Results[Array[Ok[Path, Path]], PickPath], state: S): Ok[String, Array[Path]] = {
    val pb = Array.newBuilder[Path]
    results.successes.foreach(_.out.foreach{ case Yes(p) => pb += p; case _ => })
    Yes(pb.result)
  }

  protected def expand(act: PickPath)(result: Array[Ok[Path, Path]])(state: S) =
    result.collect{ case No(p) if Files.isDirectory(p) && !state(p) => new PickPath(p, model, pick) }

  protected def fatality(act: PickPath)(error: act.E) = error match {
    case Act.InTime.Mistake(e) => Some(s"Problem while scanning directories:\n$e")
    case Act.InTime.Slow(d) => Some(s"Needed more time, $d, to scan directories")
  }

  protected def initialState() = new collection.mutable.HashSet[Path]()
  protected def finalState(s: collection.mutable.HashSet[Path], killed: Boolean) = { s.clear(); s }
  protected def timedOut(act: PickPath)(error: act.E) = false
}
object FindFiles {
  final class PickPath(p: Path, m: Act.InTime.Model, pick: Path => Boolean)
  extends Act.InTime[Path, String, Array[Ok[Path, Path]]](p, x => PickPath.list(x.toRealPath(), pick), p.getFileName.toString, _ => 1.0, m) {
    protected def excuse(message: String) = Act.InTime.Mistake(message)
    protected def handler(t: Throwable) = Act.InTime.Mistake(t.explain())
  }
  object PickPath {
    val noPaths = new Array[Ok[Path, Path]](0)
    def list(p: Path, pick: Path => Boolean): Ok[String, Array[Ok[Path, Path]]] = safe {
      if (Files isDirectory p) {
        val pb = Array.newBuilder[Ok[Path, Path]]
        val ls = Files list p
        ls.forEach{ pi =>
          val rp = 
            if (Files isSymbolicLink pi) Files readSymbolicLink pi
            else pi
          pb += (if (pick(rp)) Yes(rp) else No(rp))
        }
        ls.close
        pb.result
      }
      else noPaths
    }.mapNo(e => s"Could not read directory $p\n${e.explain()}")
  }

  def mkDefaultModel() = new Act.InTime.Model.Affine()

  def apply(s: String): FindFiles = apply(Paths get s)
  def apply(f: File): FindFiles = apply(f.toPath)
  def apply(p: Path): FindFiles = apply(p, q => !Files.isDirectory(q))
  def apply(s: String, pick: Path => Boolean): FindFiles = apply(Paths get s, pick)
  def apply(f: File, pick: Path => Boolean): FindFiles = apply(f.toPath, pick)
  def apply(p: Path, pick: Path => Boolean): FindFiles = new FindFiles("", p :: Nil, pick)
}
