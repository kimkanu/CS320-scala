import scala.util.parsing.combinator._
import Util._

trait TRCFAEImpl {
  // TRCFAE ty
  trait TRCFAE
  case class Num(num: Int) extends TRCFAE                                 // e ::= n
  case class Add(left: TRCFAE, right: TRCFAE) extends TRCFAE              //     | {+ e e}
  case class Sub(left: TRCFAE, right: TRCFAE) extends TRCFAE              //     | {- e e}
  case class Id(name: String) extends TRCFAE                              //     | x
  case class Fun(id: String, ty: Type, body: TRCFAE) extends TRCFAE       //     | {fun {x:t} e}
  case class App(closure: TRCFAE, param: TRCFAE) extends TRCFAE           //     | {e e}
  case class If0(c: TRCFAE, t: TRCFAE, f: TRCFAE) extends TRCFAE          //     | {if0 e e e}
  case class Rec(f: String, fty: Type,
                 x: String, xty: Type,
                 body: TRCFAE) extends TRCFAE                             //     | {recfun {x:t x:t} e}

  // Type ty
  trait Type
  case object NumT extends Type
  case class ArrowT(param: Type, result: Type) extends Type

  // Parser for TRCFAE
  object TRCFAE extends RegexParsers {
    def wrap[T](rule: Parser[T]): Parser[T] = "{" ~> rule <~ "}"
    def wrapArrow[T](rule: Parser[T]): Parser[T] = "(" ~> rule <~ ")"
    lazy val int: Parser[Int] = """-?\d+""".r ^^ (_.toInt)
    lazy val str: Parser[String] = """[a-zA-Z0-9]+""".r
    lazy val ty: Parser[Type] =
      "num"                                          ^^ { case _ => NumT }                          |
      wrapArrow((ty <~ "->") ~ ty)                   ^^ { case l ~ r => ArrowT(l, r) }
    lazy val expr: Parser[TRCFAE] =
      int                                            ^^ { case n => Num(n) }                        |
      wrap("+" ~> expr ~ expr)                       ^^ { case l ~ r => Add(l, r) }                 |
      wrap("-" ~> expr ~ expr)                       ^^ { case l ~ r => Sub(l, r) }                 |
      wrap("fun" ~> wrap((str <~ ":") ~ ty) ~ expr)  ^^ { case s ~ t ~ b => Fun(s, t, b) }          |
      wrap(expr ~ expr)                              ^^ { case f ~ a => App(f, a) }                 |
      wrap("if0" ~> expr ~ expr ~ expr)              ^^ { case c ~ t ~ f => If0(c, t, f) }          |
      (wrap("recfun" ~> wrap((str <~ ":") ~ ty ~ (str <~ ":") ~ ty) ~ expr)
                                       ^^ { case f ~ fty ~ x ~ xty ~ b => Rec(f, fty, x, xty, b) }) |
      str                                            ^^ { case x => Id(x) }
    def apply(str: String): TRCFAE = parse(expr, str).getOrElse(error(s"bad syntax: $str"))
  }

  // Evaluate a TRCFAE program contained in a string
  def run(str: String): String

  // Write your own tests
  def ownTests: Unit
}
