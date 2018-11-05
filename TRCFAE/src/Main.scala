import Util._

object Main extends TRCFAEImpl {
  type Addr = Int

  trait TRCFAEValue
  case class NumV(num: Int) extends TRCFAEValue
  case class CloV(id: String, body: TRCFAE, var env: Env) extends TRCFAEValue

  type Env = Map[String, TRCFAEValue]
  type TypeEnv = Map[String, Type]

  // Evaluate a TRCFAE program contained in a string
  def run(str: String): String = {
    def typeCheck(body: TRCFAE, tyEnv: TypeEnv): Type = body match {
      case Num(_) => NumT

      case Add(l, r) =>
        mustSame(typeCheck(l, tyEnv), NumT)
        mustSame(typeCheck(r, tyEnv), NumT)
        NumT

      case Sub(l, r) =>
        mustSame(typeCheck(l, tyEnv), NumT)
        mustSame(typeCheck(r, tyEnv), NumT)
        NumT

      case Id(name) => tyEnv.getOrElse(name, notype(s"$name is a free identifier"))

      case Fun(x, t, b) => ArrowT(t, typeCheck(b, tyEnv + (x -> t)))

      case App(f, a) =>
        val funT = typeCheck(f, tyEnv)
        val argT = typeCheck(a, tyEnv)
        funT match {
          case ArrowT(param, result) if same(argT, param) => result
          case _ => notype(s"apply ${typeToString(argT)} to ${typeToString(funT)}")
        }

      case If0(c, t, f) =>
        mustSame(typeCheck(c, tyEnv), NumT)
        mustSame(typeCheck(t, tyEnv), typeCheck(f, tyEnv))

      case Rec(f, ft, x, xt, b) =>
        mustSame(ft, ArrowT(xt, typeCheck(b, tyEnv + (f -> ft, x -> xt))))
    }

    def interp(body: TRCFAE, env: Env): TRCFAEValue = body match {
      case Num(num) => NumV(num)

      case Add(l, r) =>
        val numVAdd = numVop(_ + _)
        NumV(numVAdd(interp(l, env), interp(r, env)))

      case Sub(l, r) =>
        val numVSub = numVop(_ - _)
        NumV(numVSub(interp(l, env), interp(r, env)))

      case Id(name) => env.getOrElse(name, error(s"free identifier: $name"))

      case Fun(id, _, body) => CloV(id, body, env)

      case App(f, a) =>
        val fv = interp(f, env)
        val av = interp(a, env)
        fv match {
          case CloV(x, b, fenv) => interp(b, fenv + (x -> av))
          case v => error(s"not a closure: $v")
        }
      
      case If0(c, t, f) => interp(c, env) match {
        case NumV(0) => interp(t, env)
        case _ => interp(f, env)
      }

      case Rec(f, _, x, _, b) => 
        val cloV = CloV(x, b, env)
        cloV.env = env + (f -> cloV)
        cloV
    }

    def numVop[V](op: (Int, Int) => V): (TRCFAEValue, TRCFAEValue) => V = (l, r) => (l, r) match {
      case (NumV(x), NumV(y)) => op(x, y)
      case (x, y) => error(s"not both numbers: $x, $y")
    }

    def mustSame(left: Type, right: Type): Type = 
      if (same(left, right)) left
      else notype(s"$left is not equal to $right")
    
    def same(left: Type, right: Type): Boolean = (left, right) match {
      case (NumT, NumT) => true
      case (ArrowT(p1, r1), ArrowT(p2, r2)) => same(p1, p2) && same(r1, r2)
      case _ => false
    }

    def notype(msg: String): Nothing = error(s"no type: $msg")

    def typeToString(ty: Type): String = ty match {
      case NumT => "num"
      case ArrowT(l, r) => s"(${typeToString(l)} -> ${typeToString(r)})"
      case _ => "unknown"
    }

    val expr = TRCFAE(str)
    val ty = typeCheck(expr, Map())
    interp(expr, Map()) match {
      case NumV(n) => s"$n: ${typeToString(ty)}"
      case CloV(_, _, _) => s"function: ${typeToString(ty)}"
      case _ => "unknown"
    }
  }

  // Write your own tests
  def ownTests: Unit = {
    println(run("{+ 3 2}"))
    println(run("{fun {x: num} {+ x 3}}"))
    println(run("{{fun {count:(num -> num)} {count 8}} {recfun {count:(num -> num) n:num} {if0 n 0 {+ 1 {count {- n 1}}}}}}"))
  }
}
