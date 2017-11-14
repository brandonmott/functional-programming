package brandonmott.functional.chapters.testing

import brandonmott.functional.chapters.state._

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
  
  def map[B](f: A => B): SGen[B] = 
    SGen(g andThen (_ map f)) //> SGen(n => g(n).map(f))
  
  def flatMap[B](f: A => Gen[B]): SGen[B] = 
    SGen(g andThen (_ flatMap f)) //> SGen(n => g(n).flatMap(f))
  
  def listOfN(g2: Gen[A]): SGen[List[A]] = 
    SGen(n => Gen.listOfN(n,g2))
   
}