package auto

import specs.UnitSpec
import VirtualClasses._

class ExprModelTests1 extends UnitSpec {
  "ExprModelChain" should "be evaluatable" in {
    val em = ExprModelChain1()
    val two = em.Constant(2)
    val three = em.Constant(3)
    val chain = em.AddChain()
    chain.chain = List(three, two, two)
    val plus = em.Add(two, chain)
    plus.eval should equal(9)
  }

  "ExprFormatExtended" should "be formatable and evaluatable" in {
    val ef = ExprFormatExtended1()
    val three2 = ef.Constant(3)
    val five = ef.Constant(5)
    val seven = ef.Constant(7)
    val twelf = ef.Constant(12)
    val eight = ef.Constant(8)
    val mul = ef.Mult(five, seven)
    val sub = ef.Sub(twelf, eight)
    val add2 = ef.Add(mul, sub)
    val div = ef.Div(add2, three2)
    div.rest should equal(0)
    div.format should equal("(((5 * 7) + (12 - 8)) / 3)")
    div.eval should equal (13)
  }

  "ExprFormatExtended" should "be formatable" in {
    val epf = ExprFormatWithPrePost1()
    val three3 = epf.Constant(3)
    val five2 = epf.Constant(5)
    val seven2 = epf.Constant(7)
    val twelf2 = epf.Constant(12)
    val mul2 = epf.Mult(five2, seven2)
    val add3 = epf.Add(mul2, twelf2)

    add3.format should equal("((5 * 7) + 12)")
    add3.formatPre should equal("(+ (* 5 7) 12)")
    add3.formatPost should equal("5 7 * 12 +")
  }
}

@virtualContext class ExprModel1 {
  @virtual abstract class Expr {
    def eval: Int
  }

  @virtual class Constant(val value: Int) extends Expr {
    def eval: Int = value
  }

  @virtual abstract class BinExpr(val left: Expr, val right: Expr) extends Expr {
  }

  @virtual class Add(val left: Expr, val right: Expr) extends BinExpr {
    def eval: Int = left.eval + right.eval
  }

  @virtual class Mult(val left: Expr, val right: Expr) extends BinExpr {
    def eval: Int = left.eval * right.eval
  }
}

@virtualContext class ExprModelChain1 extends ExprModel1 {
  @virtual abstract class Chain extends Expr {
    var chain: List[Expr] = null
  }

  @virtual class AddChain extends Chain {
    def eval: Int = chain.foldRight(0)(_.eval + _)
  }

  @virtual class MultChain extends Chain {
    def eval: Int = chain.foldRight(1)(_.eval * _)
  }
}

@virtualContext class ExprFormat1 extends ExprModel1 {
  @virtual override abstract class Expr {
    def format: String
  }

  @virtual override class Constant {
    def format: String = value.toString
  }

  @virtual override abstract class BinExpr {
    def op: String
    def format: String = "(" + left.format + " " + op + " " + right.format + ")"
  }

  @virtual override class Add {
    def op: String = "+"
  }

  @virtual override class Mult {
    def op: String = "*"
  }
}

@virtualContext class ExprFormatChain extends ExprModelChain1 with ExprFormat1 {
  @virtual override abstract class Chain {
    def op: String
    def format: String = "(" + chain.mkString(" " + op + " ") + ")"
  }

  @virtual override class AddChain {
    def op = "+"
  }

  @virtual override class MultChain {
    def op = "*"
  }
}

@virtualContext class ExprFormatExtended1 extends ExprFormat1 {
  @virtual class Sub(val left: Expr, val right: Expr) extends BinExpr {
    def op: String = "-"
    def eval: Int = left.eval - right.eval
  }

  @virtual class Div(val left: Expr, val right: Expr) extends BinExpr {
    def op: String = "/"
    def eval: Int = left.eval / right.eval
    val c: Constant = Constant(4)

    def rest: Int = left.eval % right.eval
  }
}

@virtualContext class ExprFormatPrePost1 extends ExprPrefixFormat1 {
  @virtual override abstract class Expr {
    def formatPre: String
    def formatPost: String
  }

  @virtual override class Constant {
    override def formatPre = value.toString
    def formatPost = value.toString
  }

  @virtual override abstract class BinExpr {
    def op2: String
    override def formatPre: String = "(" + op2 + " " + left.formatPre + " " + right.formatPre + ")"
    def formatPost: String = left.formatPost + " " + right.formatPost + " " + op2
  }

  @virtual override class Add {
    def op2 = "+"
  }
  @virtual override class Mult {
    def op2 = "*"
  }
}

@virtualContext class ExprPrefixFormat1 extends ExprFormat1 {
  @virtual override abstract class Expr {
    def formatPre: String
  }

  @virtual override class Constant {
    def formatPre = value.toString
  }

  @virtual override abstract class BinExpr {
    override def formatPre: String = s"($op ${left.formatPre} ${right.formatPre})"
  }
}

@virtualContext class ExprPostFixFormat1 extends ExprFormat1 {
  @virtual override abstract class Expr {
    def formatPost: String
  }

  @virtual override class Constant {
    def formatPost = value.toString
  }

  @virtual override abstract class BinExpr {
    override def formatPost: String = s"${left.formatPost} ${right.formatPost} $op"
  }
}

@virtualContext class ExprFormatWithPrePost1 extends ExprFormat1 with ExprPrefixFormat1 with ExprPostFixFormat1 {

}