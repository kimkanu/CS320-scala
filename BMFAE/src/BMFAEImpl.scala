import scala.util.parsing.combinator._
import Util._

trait BMFAEImpl {
  // BMFAE type
  trait BMFAE
  case class Num(num: Int) extends BMFAE                                // e ::= n
  case class Add(left: BMFAE, right: BMFAE) extends BMFAE               //     | {+ e e}
  case class Sub(left: BMFAE, right: BMFAE) extends BMFAE               //     | {- e e}
  case class Id(name: String) extends BMFAE                             //     | x
  case class Fun(id: String, body: BMFAE) extends BMFAE                 //     | {fun {x} e}
  case class App(closure: BMFAE, param: BMFAE) extends BMFAE            //     | {e e}
  case class NewBox(expr: BMFAE) extends BMFAE                          //     | {newbox e}
  case class SetBox(box: BMFAE, value: BMFAE) extends BMFAE             //     | {setbox e e}
  case class OpenBox(box: BMFAE) extends BMFAE                          //     | {openbox e}
  case class Seqn(first: BMFAE, second: BMFAE) extends BMFAE            //     | {seqn e e}
  case class Set(name: String, value: BMFAE) extends BMFAE              //     | {set x e}

  // Parser for BMFAE
  object BMFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z0-9]+""".r
    lazy val expr: Parser[BMFAE] =
      int                                       ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                  ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                  ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("fun" ~> wrap(str) ~ expr)           ^^ { case p ~ b => Fun(p, b) }                 |
      wrap("newbox" ~> expr)                    ^^ { case e => NewBox(e) }                     |
      wrap("setbox" ~> expr ~ expr)             ^^ { case b ~ v => SetBox(b, v) }              |
      wrap("openbox" ~> expr)                   ^^ { case b => OpenBox(b) }                    |
      wrap("seqn" ~> expr ~ expr)               ^^ { case f ~ s => Seqn(f, s) }                |
      wrap("set" ~> str ~ expr)                 ^^ { case x ~ e => Set(x, e) }                 |
      wrap(expr ~ expr)                         ^^ { case f ~ a => App(f, a) }                 |
      str                                       ^^ { case x => Id(x) }
    def apply(str: String): BMFAE = parse(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a BMFAE program contained in a string
  def run(str: String): String

  // Write your own tests
  def ownTests: Unit
}
