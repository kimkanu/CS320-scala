import scala.util.parsing.combinator._
import Util._

trait TFAEImpl {
  // TFAE ty
  trait TFAE
  case class Num(num: Int) extends TFAE                                 // e ::= n
  case class Add(left: TFAE, right: TFAE) extends TFAE              //     | {+ e e}
  case class Sub(left: TFAE, right: TFAE) extends TFAE              //     | {- e e}
  case class Id(name: String) extends TFAE                              //     | x
  case class Fun(id: String, ty: Type, body: TFAE) extends TFAE       //     | {fun {x} e}
  case class App(closure: TFAE, param: TFAE) extends TFAE           //     | {e e}

  // Type ty
  trait Type
  case object NumT extends Type
  case class ArrowT(param: Type, result: Type) extends Type

  // Parser for TFAE
  object TFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    def wrapArrow[T](rule: Parser[T]): Parser[T] = "(" ~> rule <~ ")"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z0-9]+""".r
    lazy val ty: Parser[Type] =
      "num"                                          ^^ { case _ => NumT }                          |
      wrapArrow((ty <~ "->") ~ ty)                   ^^ { case l ~ r => ArrowT(l, r) }
    lazy val expr: Parser[TFAE] =
      int                                            ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                       ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                       ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("fun" ~> wrap((str <~ ":") ~ ty) ~ expr)  ^^ { case s ~ t ~ b => Fun(s, t, b) }          |
      wrap(expr ~ expr)                              ^^ { case f ~ a => App(f, a) }                 |
      str                                            ^^ { case x => Id(x) }
    def apply(str: String): TFAE = parse(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a TFAE program contained in a string
  def run(str: String): String

  // Write your own tests
  def ownTests: Unit
}
