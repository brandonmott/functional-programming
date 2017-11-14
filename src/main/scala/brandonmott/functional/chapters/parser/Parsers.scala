package brandonmott.functional.chapters.parser


trait Parsers[ParserError, Parser[+_]] { self => /** `self` refer's to this Parsers instance; so inner classes may call methods of trait (it’s used later in ParserOps.) */
    
  def run[A](p: Parser[A])(input: String): Either[ParserError,A]
  
  def char(c: Char): Parser[Char]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  /** 
    * Scala will automatically promote a String to a Parser, and we get infix operators 
    * for any type that can be converted to a Parser[String] 
    */
  /** implicit conversions */
  implicit def string(s: String): Parser[String]
  
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  
  /**
    * This will work for all implementations of Parsers. Other binary operators or methods can be added to 
    * the body of ParserOps. We’ll follow the discipline of keeping the primary definition directly in Parsers and 
    * delegating in ParserOps to this primary definition.
    */
  case class ParserOps[A](p: Parser[A]) {
    /** Use `self` to explicitly disambiguate reference to the or method on the trait. */
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  
}

object Parsers {
  type ParserError = String
  
}

