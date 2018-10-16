import Util._

object Main extends BFAEImpl {
  type Addr = Int

  trait BFAEValue
  case class NumV(num: Int) extends BFAEValue
  case class CloV(id: String, body: BFAE, env: Env) extends BFAEValue
  case class BoxV(addr: Addr) extends BFAEValue

  type Env = Map[String, BFAEValue]
  type Sto = Map[Addr, BFAEValue]

  // Evaluate a BFAE program contained in a string
  def run(str: String): String = {
    def interp(body: BFAE, env: Env, sto: Sto): (BFAEValue, Sto) = body match {
      case Num(num) => (NumV(num), sto)
      case Add(left, right) => {
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (lv, rv) match {
          case (NumV(l), NumV(r)) => (NumV(l + r), rs)
          case _ => error(s"contains an NaN: $lv, $rv")
        }
      }
      case Sub(left, right) => {
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (lv, rv) match {
          case (NumV(l), NumV(r)) => (NumV(l - r), rs)
          case _ => error(s"contains an NaN: $lv, $rv")
        }
      }
      case Id(name) => (env.getOrElse(name, error(s"free identifier: $name")), sto)
      case Fun(id, body) => (CloV(id, body, env), sto)
      case App(f, a) => {
        val (fv, fs) = interp(f, env, sto)
        val (av, as) = interp(a, env, fs)
        fv match {
          case CloV(x, b, fenv) => interp(b, fenv + (x -> av), as)
          case _ => error(s"not a closure: $fv")
        }
      }
      case NewBox(e) => {
        val (v, s) = interp(e, env, sto)
        val addr = malloc(s)
        (BoxV(addr), s + (addr -> v))
      }
      case SetBox(b, e) => {
        val (bv, bs) = interp(b, env, sto)
        bv match {
          case BoxV(addr) => {
            val (v, s) = interp(e, env, bs)
            (v, s + (addr -> v))
          }
          case _ => error(s"not a box: $bv")
        }
      }
      case OpenBox(b) => {
        val (bv, bs) = interp(b, env, sto)
        bv match {
          case BoxV(addr) =>
            (bs.getOrElse(addr, error(s"invalid box: no value at address $addr")), bs)
          case _ => error(s"not a box: $bv")
        }
      }
      case Seqn(f, s) => {
        val (fv, fs) = interp(f, env, sto)
        interp(s, env, fs)
      }
    }

    def malloc(sto: Sto): Addr = 1 + sto.keySet.+(0).max

    interp(BFAE(str), Map(), Map()) match {
      case (NumV(n), sto) => s"$n"
      case (CloV(x, body, env), sto) => "function"
      case (BoxV(addr), sto) => s"box at $addr"
      case (_, _) => "unknown"
    }
  }

  // Write your own tests
  def ownTests: Unit = {
    println(run("{+ 3 2}"))
    println(run("{{fun {b} {seqn {setbox b 10} {openbox b}}} {newbox 0}}"))
    println(run("{{fun {q} {setbox {seqn {setbox q 12} q} {openbox q}}} {newbox 10}}"))
  }
}
