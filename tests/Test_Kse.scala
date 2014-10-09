package kse.tests

import kse.flow._

trait Test_Kse { self =>
  protected def explainThrowable(t: Throwable): Vector[String] = {
    val title = t.getClass.getName + Option(t.getMessage).map(": " + _).getOrElse("")
    title +: t.getStackTrace.map("  " + _).toVector
  }

  private val registration = Test_Kse.register(this)

  def run: Ok[Vector[Vector[String]],Int] = {
    val ms = this.getClass.getMethods.
      filter(_.getName.startsWith("test_")).
      filter{ m =>
        import java.lang.reflect.Modifier._
        (m.getModifiers & (PUBLIC | ABSTRACT)) == PUBLIC
      }.
      filter{ m =>
        val r = m.getReturnType
        (classOf[Boolean] isAssignableFrom r) || (classOf[Test_Kse.TestResult] isAssignableFrom r)
      }.
      groupBy(_.getName).
      mapValues(_.reduce{ (m,n) =>
        if (m.getReturnType isAssignableFrom n.getReturnType) m
        else if (n.getReturnType isAssignableFrom m.getReturnType) n
        else return No(Vector(Vector("Incompatible return types in methods named " + m.getName)))
      }).
      map(_._2).toArray.
      sortBy(_.getName)
    val oks = ms.map{ m =>
      safeOk(explainThrowable){ fail =>
        val o: Any = m.invoke(self)
        o match {
          case b: Boolean if b == false => fail(Vector(m.getName + " returned false."))
          case Test_Kse.TestResult(Some(v)) => fail(v)
          case _ => ()
        }
      }
    }
    No( oks.collect{ case No(n) => n }.toVector ) accept {
      case v if v.length == 0 => oks.length
    }
  }
}

object Test_Kse {
  private val myRegistered = new collection.mutable.ArrayBuffer[Test_Kse]()

  def registered: Seq[Test_Kse] = myRegistered
  def register(tk: Test_Kse) = { myRegistered += tk; registered.length }

  case class TestResult(result: Option[Vector[String]] = None) {}
}
