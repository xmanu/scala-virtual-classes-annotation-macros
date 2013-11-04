object ExprTest extends App {
  val em: ExprModel = ExprModel()
  val two = em.Constant()
  two.value = 2
  val three = em.Constant()
  three.value = 3
  val plus = em.Add()
  plus.left = two
  plus.right = three
  println(plus.eval)
  
  val ef = ExprFormatExtended()
  val three2 = ef.Constant()
  three2.value = 3
  val five = ef.Constant()
  five.value = 5
  val seven = ef.Constant()
  seven.value = 7
  val twelf = ef.Constant()
  twelf.value = 12
  val eight = ef.Constant()
  eight.value = 8
  val mul = ef.Mult()
  mul.left = five
  mul.right = seven
  val sub = ef.Sub()
  sub.left = twelf
  sub.right = eight
  val add2 = ef.Add()
  add2.left = mul
  add2.right = sub
  val div = ef.Div()
  div.left = add2
  div.right = three2
  
  println(div.format + " = " + div.eval)
}

@virtualContext class ExprModel {
	@virtual abstract class Expr {
	  def eval: Int
	}
	
	@virtual class Constant extends Expr {
	  var value: Int = 0
	  def eval: Int = value
	}
	
	@virtual abstract class BinExpr extends Expr {
	  var left: Expr = null
	  var right: Expr = null
	}
	
	@virtual class Add extends BinExpr {
	  def eval: Int = left.eval + right.eval
	}
	
	@virtual class Mult extends BinExpr {
	  def eval: Int = left.eval * right.eval
	}
}

@virtualContext class ExprFormat extends ExprModel {
	@virtual abstract class Expr {
	  def format: String
	}
	
	@virtual class Constant {
	  def format: String = value.toString
	}
	
	@virtual abstract class BinExpr {
	  def op: String
	  def format: String = "(" + left.format + " " + op + " " + right.format + ")"
	}
	
	@virtual class Add {
	  def op: String = "+"
	}
	
	@virtual class Mult {
	  def op: String = "*"
	}
}

@virtualContext class ExprFormatExtended extends ExprFormat {
  @virtual class Sub extends BinExpr {
    def op: String = "-"
    def eval: Int = left.eval - right.eval
  }
  
  @virtual class Div extends BinExpr {
    def op: String = "/"
    def eval: Int = left.eval / right.eval
  }
}

/*@virtualContext class ExprFormatExtended extends ExprFormat {
  @virtual abstract class Expr {
    def formatPre: String
    def formatPost: String
  }
  
  @virtual abstract class BinExpr {
    def formatPre: String = "(" + op + " " + left.formatPre + " " + right.formatPre + ")"
    def formatPost: String = "(" + left.formatPost + " " + right.formatPost + " " + op + ")"
  }
}*/

/*@virtualContext class ExprPrefixFormat extends ExprFormat {
  @virtual abstract class BinExpr {
    override def format: String = "(" + op + " " + left.format + " " + right.format + ")"
  }
}*/

/*@virtualContext class ExprFormatExtended extends ExprFormat {
  @virtual abstract class Expr {
    def formatPre: String
    def formatPost: String
  }
  
  @virtual abstract class BinExpr {
    def formatPre: String = "(" + op + " " + left.formatPre + " " + right.formatPre + ")"
    def formatPost: String = "(" + left.formatPost + " " + right.formatPost + " " + op + ")"
  }
}*/