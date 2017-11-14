package brandonmott.functional.core
package monads

import java.io.{File, PrintWriter}
import scala.io.Source


/**
  * Returns the next state when moving between different operations.
  * By calling it when we pass a state, we make sure that different operations cause a change in state.
  * 
  * The fact that the trait is sealed helps us to make sure nobody can extend our state outside the file,
  * but being sealed is not enough. We need to make sure all the implementations of the state are hidden. 
  */
sealed trait State {
  def next: State
}

/**
  * `FileIO` hides the `State` by creating a private class `FileIOState` that nobody can instantiate 
  */
abstract class FileIO {

  // Hides the state by being `private` 
  private class FileIOState(id: Int) extends State {
    override def next: State = new FileIOState(id + 1)
  }

  def run(args: Array[String]): Unit = {
    val action = runIO(args(0), args(1))
    action(new FileIOState(0))
  }

  def runIO(readPath: String, writePath: String): IOAction[_]
}

/**
  * The IOAction signature `((State) => (State, T))`
  * It extends a function from an old state to a tuple of the new state and the result of the operation.
  * Defined the IOAction to be sealed.
  */
sealed abstract class IOAction[T] extends (State => (State, T)) {

  def unit[Y](value: Y): IOAction[Y] = IOAction.unit(value)

  def flatMap[Y](f: (T) => IOAction[Y]): IOAction[Y] = {
    val self = this
    new IOAction[Y] {
      override def apply(state: State): (State, Y) = {
        val (state2, res) = self(state)
        val action2 = f(res)
        action2(state2)
      }
    }
  }

  def map[Y](f: T => Y): IOAction[Y] =
    flatMap(i => unit(f(i)))


}

/** Enables us to define actions and only execute them whenever we have a state available. */
object IOAction {
  /**
    * An apply method for the IOAction object, which allows the users to instantiate actions.
    * Takes a by name parameter.
    */
  def apply[T](result: => T): IOAction[T] = new SimpleAction[T](result)

  /**
    * A private implementation of IOAction.
    * It only takes a by name parameter, which means that it will only be evaluated when the apply method is called
    */
  private class SimpleAction[T](result: => T) extends IOAction[T] {
    override def apply(state: State): (State, T) =
      (state.next, result)
  }

  def unit[T](value: T): IOAction[T] = new EmptyAction[T](value)

  private class EmptyAction[T](value: T) extends IOAction[T] {
    override def apply(state: State): (State, T) =
      (state, value)
  }

}

object fileOps {
  def readFile(path: String) =
    IOAction(Source.fromFile(path).getLines())

  def writeFile(path: String, lines: Iterator[String]) =
    IOAction({
      val file = new File(path)
      printToFile(file) { p => lines.foreach(p.println) }
    })

  private def printToFile(file: File)(writeOp: PrintWriter => Unit): Unit = {
    val writer = new PrintWriter(file)
    try {
      writeOp(writer)
    } finally {
      writer.close()
    }
  }
}



object FileIOExample extends FileIO {

  def main(args: Array[String]): Unit = {
    run(args)
  }

  // readFile & writeFile is defined in package object
  override def runIO(readPath: String, writePath: String): IOAction[_] =
    for {
      lines <- fileOps.readFile(readPath)
      _ <- fileOps.writeFile(writePath, lines.map(_.toUpperCase))
    } yield ()
}
