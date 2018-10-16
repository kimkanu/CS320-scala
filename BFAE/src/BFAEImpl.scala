import scala.util.parsing.combinator._
import Util._

trait BFAEImpl {
  // BFAE type
  trait BFAE
  case class Num(num: Int) extends BFAE                                 // e ::= n
  case class Add(left: BFAE, right: BFAE) extends BFAE                  //     | {+ e e}
  case class Sub(left: BFAE, right: BFAE) extends BFAE                  //     | {- e e}
  case class Id(name: String) extends BFAE                              //     | x
  case class Fun(id: String, body: BFAE) extends BFAE                   //     | {fun {x} e}
  case class App(closure: BFAE, param: BFAE) extends BFAE               //     | {e e}
  case class NewBox(expr: BFAE) extends BFAE                            //     | {newbox e}
  case class SetBox(box: BFAE, value: BFAE) extends BFAE                //     | {setbox e e}
  case class OpenBox(box: BFAE) extends BFAE                            //     | {openbox e}
  case class Seqn(first: BFAE, second: BFAE) extends BFAE               //     | {seqn e e}

  // Parser for BFAE
  object BFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z0-9]+""".r
    lazy val expr: Parser[BFAE] =
      int                                       ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("fun" ~> wrap(str) ~ expr)           ^^ { case p ~ b => Fun(p, b) }                 |
      wrap("newbox" ~> expr)                    ^^ { case e => NewBox(e) }                     |
      wrap("setbox" ~> expr ~ expr)             ^^ { case b ~ v => SetBox(b, v) }              |
      wrap("openbox" ~> expr)                   ^^ { case b => OpenBox(b) }                    |
      wrap("seqn" ~> expr ~ expr)               ^^ { case f ~ s => Seqn(f, s) }                |
      wrap(expr ~ expr)                         ^^ { case f ~ a => App(f, a) }                 |
      str                                       ^^ { case x => Id(x) }
    def apply(str: String): BFAE = parse(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a BFAE program contained in a string
  def run(str: String): String

  // Write your own tests
  def ownTests: Unit
}
