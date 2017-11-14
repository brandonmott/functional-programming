package brandonmott.functional.chapters.testing

import Prop._
import brandonmott.functional.chapters.laziness.Stream
import brandonmott.functional.chapters.state.{RNG, State}

case class Prop(run: (MaxSize, TestCases, RNG) => Result){
  def &&(p: Prop): Prop = Prop{
    (max,n,rng) => run(max,n,rng) match {
      // In case of pass, run the other prop
      case Passed =>  p.run(max,n,rng)
      // In case of failure, run the other prop
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop{
    (max,n,rng) => run(max,n,rng) match {
      // In case of failure, run the other prop
      case Failed(failure, count) => p.tag(failure).run(max,n,rng)
      // In case of pass, return pass
      case x => x 
    }
  }

  /* In the event of failure, prepend the given message on a newline in front of the existing message. */
  def tag(msg: String): Prop = Prop{
    (max,n,rng) => run(max,n,rng) match {
      case Failed(failure, count) => Failed(msg + "\n" + failure, count)
      case x => x 
    }
  }
}

object Prop {
  type SuccessCount = Int 
  type FailCase = String
  type TestCases = Int
  type MaxSize = Int
  
  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified: Boolean = false
  }

  case class Failed(failure: FailCase, successes: SuccessCount) extends Result {
    def isFalsified: Boolean = true
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Failed(a.toString, i)
        } catch {
          case e: Exception => Failed(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }


}
