package exprmodel

import VirtualClasses._

object ExprModelTest2 extends App {
  val epf = ExprEvalWithFormat()
  import epf._
  val div = Div(Add(Mult(Constant(2), Constant(7)), Constant(13)), Constant(3))

  println(div.format + " = " + div.eval) // (((2 * 7) + 13) / 3) = 9
  println(div.formatPre)
  println(div.formatPost)
  println(div.asInstanceOf[EvalTrait].eval) // 9
}

@family class ExprModel {
  @virtual abstract class Expr

  @virtual class Constant(val value: Int) extends Expr {
  }

  @virtual abstract class BinExpr(val left: Expr, val right: Expr) extends Expr

  @virtual class Add(val left: Expr, val right: Expr) extends BinExpr 

  @virtual class Mult(val left: Expr, val right: Expr) extends BinExpr
}

trait EvalTrait {
  def eval: Int
}

@family class ExprEval extends ExprModel {
  @virtual override abstract class Expr extends EvalTrait

  @virtual override class Constant {
    def eval: Int = value
  }

  @virtual override class Add {
    def eval: Int = left.eval + right.eval
  }

  @virtual override class Mult {
    def eval: Int = left.eval * right.eval
  }
}

@family class ExprModelOp extends ExprModel {
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

@family class ExprFormat extends ExprModelOp {
  @virtual override abstract class Expr {
    def format: String
  }

  @virtual override class Constant {
    def format: String = value.toString
  }

  @virtual override abstract class BinExpr {
    def format: String = s"(${left.format} $op ${right.format})"
  }
}

@family class ExprFormatPrePost extends ExprFormat {
  @virtual override abstract class Expr {
    def formatPre: String
    def formatPost: String
  }

  @virtual override class Constant {
    def formatPre = value.toString
    def formatPost = value.toString
  }

  @virtual override abstract class BinExpr {
    def formatPre: String = s"($op ${left.formatPre} ${right.formatPre})"
    def formatPost: String = s"${left.formatPost} ${right.formatPost} $op"
  }
}

@family class ExprEvalWithFormat extends ExprEval with ExprFormatPrePost {
  @virtual class Div(val left: Expr, val right: Expr) extends BinExpr {
    def op = "/"
    def eval = left.eval / right.eval
  }

  @virtual class Sub(val left: Expr, val right: Expr) extends BinExpr {
    def op = "-"
    def eval = left.eval - right.eval
  }
}