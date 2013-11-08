object ExprTest extends App {
  val em = ExprModelChain()
  val two = em.Constant()
  two.value = 2
  val three = em.Constant()
  three.value = 3
  val chain = em.AddChain()
  chain.chain = List(three, two, two)
  val plus = em.Add()
  plus.left = two
  plus.right = chain
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
  println(div.c.eval)
  println(div.rest)
  
  println(div.format + " = " + div.eval)
  
  val epf = ExprFormatWithPrePost()
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

@virtualContext class ExprModelChain extends ExprModel {
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

/*@virtualContext class ExprFormatChain extends ExprFormat with ExprModelChain {
  @virtual abstract class Chain {
    def op: String
    def format: String = "(" + chain.mkString(" " + op + " ") + ")"
  }
  
  @virtual class AddChain {
    def op = "+"
  }
  
  @virtual class MultChain {
    def op = "*"
  }
}*/

@virtualContext class ExprFormatExtended extends ExprFormat {
  @virtual class Sub extends BinExpr {
    def op: String = "-"
    def eval: Int = left.eval - right.eval
  }
  
  @virtual class Div extends BinExpr {
    def op: String = "/"
    def eval: Int = left.eval / right.eval
    val c: Constant = Constant()
    c.value = 4
    
    def rest: Int = left.eval % right.eval
  }
}

@virtualContext class ExprFormatPrePost extends ExprModel {
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

@virtualContext class ExprPrefixFormat extends ExprFormat {
  @virtual abstract class BinExpr {
    override def format: String = "(" + op + " " + left.format + " " + right.format + ")"
  }
}

@virtualContext class ExprFormatWithPrePost extends ExprFormat with ExprFormatPrePost {
  
}