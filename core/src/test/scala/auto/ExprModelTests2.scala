package auto

import specs.UnitSpec
import VirtualClasses._

class ExprModelTests2 extends UnitSpec {
  "ExprEvalWithFormat" should "eval and format right" in {
    val epf = ExprEvalWithFormat()
    val three3 = epf.Constant()
    three3.value = 3
    val five2 = epf.Constant()
    five2.value = 5
    val seven2 = epf.Constant()
    seven2.value = 7
    val twelf2 = epf.Constant()
    twelf2.value = 12
    val mul2 = epf.Mult()
    mul2.left = five2
    mul2.right = seven2
    val add3 = epf.Add()
    add3.left = mul2
    add3.right = twelf2
    val div = epf.Div()
    div.left = add3
    div.right = three3

    div.format should equal ("(((5 * 7) + 12) / 3)")
    div.eval should equal (15)
    div.formatPre should equal ("(/ (+ (* 5 7) 12) 3)")
    div.formatPost should equal ("5 7 * 12 + 3 /")
  }
}

@virtualContext class ExprModel {
  @virtual abstract class Expr {
  }

  @virtual class Constant extends Expr {
    var value: Int = 0
  }

  @virtual abstract class BinExpr extends Expr {
    var left: Expr = null
    var right: Expr = null
  }

  @virtual class Add extends BinExpr {
  }

  @virtual class Mult extends BinExpr {
  }
}

@virtualContext class ExprEval extends ExprModel {
  @virtual override abstract class Expr {
    def eval: Int
  }

  @virtual override class Constant {
    def eval: Int = value
  }

  @virtual override abstract class BinExpr {
  }

  @virtual override class Add {
    def eval: Int = left.eval + right.eval
  }

  @virtual override class Mult {
    def eval: Int = left.eval * right.eval
  }
}

@virtualContext class ExprModelOp extends ExprModel {
  @virtual override abstract class BinExpr {
    def op: String
  }
  @virtual override class Add {
    def op = "+"
  }
  @virtual override class Mult {
    def op = "*"
  }
}

@virtualContext class ExprFormat extends ExprModelOp {
  @virtual override abstract class Expr {
    def format: String
  }

  @virtual override class Constant {
    def format: String = value.toString
  }

  @virtual override abstract class BinExpr {
    def format: String = "(" + left.format + " " + op + " " + right.format + ")"
    //def op: String
  }

  @virtual override class Add {
    //def op = "+"
  }

  @virtual override class Mult {
    //def op = "*"
  }
}

@virtualContext class ExprFormatPrePost extends ExprFormat {
  @virtual override abstract class Expr {
    def formatPre: String
    def formatPost: String
  }

  @virtual override class Constant {
    def formatPre = value.toString
    def formatPost = value.toString
  }

  @virtual override abstract class BinExpr {
    def formatPre: String = "(" + op + " " + left.formatPre + " " + right.formatPre + ")"
    def formatPost: String = left.formatPost + " " + right.formatPost + " " + op
  }
}

@virtualContext class ExprEvalWithFormat extends ExprEval with ExprFormatPrePost {
  @virtual class Div extends BinExpr {
    def op = "/"
    def eval = left.eval / right.eval
  }

  @virtual class Sub extends BinExpr {
    def op = "-"
    def eval = left.eval - right.eval
  }
}