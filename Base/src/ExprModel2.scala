object ExprTest2 extends App { 
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
  
  println(add3.format + " = " + add3.eval)
  println(add3.formatPre)
  println(add3.formatPost)
}

@virtualContext class ExprModel2 {
	@virtual abstract class Expr {
	  //def something$1: Int
	}
	
	@virtual class Constant extends Expr {
	  var value: Int = 0
	  //def something$1: Int = 0
	}
	
	@virtual abstract class BinExpr extends Expr {
	  var left: Expr = null
	  var right: Expr = null
	  //def something$1: Int = 0
	  //def something$2: Int
	}
	
	@virtual class Add extends BinExpr {
	  //def something$2: Int = 0
	}
	
	@virtual class Mult extends BinExpr {
	  //def something$2: Int = 0
	}
}

@virtualContext class ExprModelWithSub extends ExprModel2 {
  @virtual class Sub extends BinExpr {
    //def something$2: Int = 0
  }
}

@virtualContext class ExprEval extends ExprModel2 {
	@virtual abstract class Expr {
	  def eval: Int
	}
	
	@virtual class Constant {
	  def eval: Int = value
	}
	
	@virtual abstract class BinExpr {
	}
	
	@virtual class Add {
	  def eval: Int = left.eval + right.eval
	}
	
	@virtual class Mult {
	  def eval: Int = left.eval * right.eval
	}
}

/*@virtualContext class ExprEvalWithSub extends ExprEval with ExprModelWithSub {
  @virtual class Sub {
    def eval: Int = left.eval - right.eval
  }
}*/

@virtualContext class ExprFormat2 extends ExprModel2 {
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

@virtualContext class ExprFormatPrePost2 extends ExprModel2 {
  @virtual abstract class Expr {
    def formatPre: String
    def formatPost: String
  }
  
  @virtual class Constant {
    def formatPre = value.toString
    def formatPost = value.toString
  }
  
  @virtual abstract class BinExpr {
    def op2: String
    def formatPre: String = "(" + op2 + " " + left.formatPre + " " + right.formatPre + ")"
    def formatPost: String = left.formatPost + " " + right.formatPost + " " + op2
  }
  
  @virtual class Add {
    def op2 = "+"
  }
  @virtual class Mult {
    def op2 = "*"
  }
}

@virtualContext class ExprFormatWithPrePost2 extends ExprFormat2 with ExprFormatPrePost2 {
  
}

@virtualContext class ExprEvalWithFormat extends ExprEval with ExprFormat2 with ExprFormatPrePost2 {
  
}