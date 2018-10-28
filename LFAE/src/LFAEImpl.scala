import scala.util.parsing.combinator._
import Util._

trait LFAEImpl {
  // LFAE type
  trait LFAE
  case class Num(num: Int) extends LFAE                                 // e ::= n
  case class Add(left: LFAE, right: LFAE) extends LFAE                  //     | {+ e e}
  case class Sub(left: LFAE, right: LFAE) extends LFAE                  //     | {- e e}
  case class Id(name: String) extends LFAE                              //     | x
  case class Fun(id: String, body: LFAE) extends LFAE                   //     | {fun {x} e}
  case class App(closure: LFAE, param: LFAE) extends LFAE               //     | {e e}

  // Parser for LFAE
  object LFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z0-9]+""".r
    lazy val expr: Parser[LFAE] =
      int                                       ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("fun" ~> wrap(str) ~ expr)           ^^ { case p ~ b => Fun(p, b) }                 |
      wrap(expr ~ expr)                         ^^ { case f ~ a => App(f, a) }                 |
      str                                       ^^ { case x => Id(x) }
    def apply(str: String): LFAE = parse(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a LFAE program contained in a string
  def run(str: String): String

  // Write your own tests
  def ownTests: Unit
}
