import VirtualClasses._

object ExprModelTest2 extends App {
  val epf = ExprEvalWithFormat()
  import epf._
  val div = Div(Add(Mult(Constant(2), Constant(7)), Constant(13)), Constant(3))

  println(div.format + " = " + div.eval)
  println(div.formatPre)
  println(div.formatPost)
  println(div.asInstanceOf[EvalTrait].eval)
}

@virtualContext class ExprModel {
  @virtual abstract class Expr

  @virtual class Constant(_value: Int) extends Expr {
    var value: Int = _value
  }

  @virtual abstract class BinExpr(val left: Expr, val right: Expr) extends Expr

  @virtual class Add(val left: Expr, val right: Expr) extends BinExpr 

  @virtual class Mult(val left: Expr, val right: Expr) extends BinExpr
}

trait EvalTrait {
  def eval: Int
}

@virtualContext class ExprEval extends ExprModel {
  @virtualOverride abstract class Expr extends EvalTrait

  @virtualOverride class Constant {
    def eval: Int = value
  }

  @virtualOverride class Add {
    def eval: Int = left.eval + right.eval
  }

  @virtualOverride class Mult {
    def eval: Int = left.eval * right.eval
  }
}

@virtualContext class ExprModelOp extends ExprModel {
  @virtualOverride abstract class BinExpr {
    def op: String
  }
  @virtualOverride class Add {
    def op = "+"
  }
  @virtualOverride class Mult {
    def op = "*"
  }
}

@virtualContext class ExprFormat extends ExprModelOp {
  @virtualOverride abstract class Expr {
    def format: String
  }

  @virtualOverride class Constant {
    def format: String = value.toString
  }

  @virtualOverride abstract class BinExpr {
    def format: String = "(" + left.format + " " + op + " " + right.format + ")"
  }
}

@virtualContext class ExprFormatPrePost extends ExprFormat {
  @virtualOverride abstract class Expr {
    def formatPre: String
    def formatPost: String
  }

  @virtualOverride class Constant {
    def formatPre = value.toString
    def formatPost = value.toString
  }

  @virtualOverride abstract class BinExpr {
    def formatPre: String = "(" + op + " " + left.formatPre + " " + right.formatPre + ")"
    def formatPost: String = left.formatPost + " " + right.formatPost + " " + op
  }
}

@virtualContext class ExprEvalWithFormat extends ExprEval with ExprFormatPrePost {
  @virtual class Div(val left: Expr, val right: Expr) extends BinExpr {
    def op = "/"
    def eval = left.eval / right.eval
  }

  @virtual class Sub(val left: Expr, val right: Expr) extends BinExpr {
    def op = "-"
    def eval = left.eval - right.eval
  }
}