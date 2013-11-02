object ExprTest extends App {
  val em = ExprModel()
  val two = em.Constant()
  two.value = 2
  val three = em.Constant()
  three.value = 3
  val plus = em.Add()
  plus.left = two
  plus.right = three
  println(plus.eval)
  
  val ef = ExprFormat()
  val five = ef.Constant()
  five.value = 5
  val seven = ef.Constant()
  seven.value = 7
  val eight = ef.Constant()
  eight.value = 8
  val mul = ef.Mult()
  mul.left = five
  mul.right = seven
  val add2 = ef.Add()
  add2.left = mul
  add2.right = eight
  
  println(add2.format + " = " + add2.eval)
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

@virtualContext class E2 extends ExprFormat {
  
}

@virtualContext class ExprPrefixFormat extends ExprFormat {
  @virtual abstract class BinExpr {
    def format: String = "(" + op + " " + left.format + " " + right.format + ")"
  }
}