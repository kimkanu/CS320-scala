import Util._

object Main extends LFAEImpl {
  type Addr = Int

  trait LFAEValue
  case class NumV(num: Int) extends LFAEValue
  case class CloV(id: String, body: LFAE, env: Env) extends LFAEValue
  case class ExprV(expr: LFAE, env: Env, var v: Option[LFAEValue]) extends LFAEValue

  type Env = Map[String, LFAEValue]

  // Evaluate a LFAE program contained in a string
  def run(str: String): String = {
    def interp(body: LFAE, env: Env): LFAEValue = body match {
      case Num(num) => NumV(num)

      case Add(l, r) =>
        val numVAdd = numVop(_ + _)
        NumV(numVAdd(interp(l, env), interp(r, env)))

      case Sub(l, r) =>
        val numVSub = numVop(_ - _)
        NumV(numVSub(interp(l, env), interp(r, env)))

      case Id(name) => env.getOrElse(name, error(s"free identifier: $name"))

      case Fun(id, body) => CloV(id, body, env)

      case App(f, a) =>
        val fv = strict(interp(f, env))
        val av = ExprV(a, env, None)
        fv match {
          case CloV(x, b, fenv) => interp(b, fenv + (x -> av))
          case v => error(s"not a closure: $v")
        }
    }
    
    def numVop[V](op: (Int, Int) => V): (LFAEValue, LFAEValue) => V = (l, r) => (strict(l), strict(r)) match {
      case (NumV(x), NumV(y)) => op(x, y)
      case (x, y) => error(s"not both numbers: $x, $y")
    }

    def strict(v: LFAEValue): LFAEValue = v match {
      case ev@ExprV(e, env, r) => r match {
        case Some(cache) => cache
        case _ =>
          val cache = strict(interp(e, env))
          ev.v = Some(cache)
          cache
      }
      case _ => v
    }

    interp(LFAE(str), Map()) match {
      case NumV(n) => s"$n"
      case CloV(_, _, _) => "function"
      case ExprV(_, _, _) => "expression"
      case _ => "unknown"
    }
  }

  // Write your own tests
  def ownTests: Unit = {
    println(run("{+ 3 2}"))
    println(run("{{fun {x} 0} {+ 1 {fun {y} 2}}}"))
  }
}
