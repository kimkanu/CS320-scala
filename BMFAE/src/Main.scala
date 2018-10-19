import Util._

object Main extends BMFAEImpl {
  type Addr = Int
  type Env = Map[String, Addr]
  type Sto = Map[Addr, BMFAEValue]

  trait BMFAEValue
  case class NumV(num: Int) extends BMFAEValue
  case class CloV(id: String, body: BMFAE, env: Env) extends BMFAEValue
  case class BoxV(addr: Addr) extends BMFAEValue

  // Evaluate a BMFAE program contained in a string
  def run(str: String): String = {
    def interp(body: BMFAE, env: Env, sto: Sto): (BMFAEValue, Sto) = body match {
      case Num(num) => (NumV(num), sto)

      case Add(left, right) =>
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (lv, rv) match {
          case (NumV(l), NumV(r)) => (NumV(l + r), rs)
          case _ => error(s"contains an NaN: $lv, $rv")
        }

      case Sub(left, right) =>
        val (lv, ls) = interp(left, env, sto)
        val (rv, rs) = interp(right, env, ls)
        (lv, rv) match {
          case (NumV(l), NumV(r)) => (NumV(l - r), rs)
          case _ => error(s"contains an NaN: $lv, $rv")
        }

      case Id(name) =>
        val addr = env.getOrElse(name, error(s"free identifier: $name"))
        val value = sto.getOrElse(addr, error(s"address not assigned: $addr"))
        (value, sto)

      case Fun(id, body) => (CloV(id, body, env), sto)

      // Call-by-reference
      case App(f, a) => a match {
        case Id(name) =>
          val (fv, fs) = interp(f, env, sto)
          fv match {
            case CloV(x, b, fenv) =>
              val addr = env.getOrElse(name, error(s"free identifier: $name"))
              interp(b, fenv + (x -> addr), fs)
            case _ => error(s"not a closure: $fv")
          }
        case _ =>
          val (fv, fs) = interp(f, env, sto)
          val (av, as) = interp(a, env, fs)
          fv match {
            case CloV(x, b, fenv) =>
              val addr = malloc(as)
              interp(b, fenv + (x -> addr), as + (addr -> av))
            case _ => error(s"not a closure: $fv")
          }
      }

      case NewBox(e) => 
        val (v, s) = interp(e, env, sto)
        val addr = malloc(s)
        (BoxV(addr), s + (addr -> v))

      case SetBox(b, e) => 
        val (bv, bs) = interp(b, env, sto)
        bv match {
          case BoxV(addr) =>
            val (v, s) = interp(e, env, bs)
            (v, s + (addr -> v))
          case _ => error(s"not a box: $bv")
        }

      case OpenBox(b) =>
        val (bv, bs) = interp(b, env, sto)
        bv match {
          case BoxV(addr) =>
            (bs.getOrElse(addr, error(s"invalid box: no value at address $addr")), bs)
          case _ => error(s"not a box: $bv")
        }

      case Seqn(f, s) =>
        val (fv, fs) = interp(f, env, sto)
        interp(s, env, fs)

      case Set(x, e) =>
        val (v, s) = interp(e, env, sto)
        (v, s + (env.getOrElse(x, error(s"free identifier: $x")) -> v))
    }

    def malloc(sto: Sto): Addr = 1 + sto.keySet.+(0).max

    interp(BMFAE(str), Map(), Map()) match {
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
