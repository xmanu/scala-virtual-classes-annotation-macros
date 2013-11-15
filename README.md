scala-virtual-classes-annotaion-macros
======================================

Virtual Classes for Scala implemented as annotation macros

Usage:
-------
1. Check out the repository
2. Load project "Macro" in Eclipse
3. Create an own project and add the "Macro" Project as required dependency
4. Be sure to include the macro paradise plugin to compile correctly.

Syntax:
--------
- use `@virtualContext` to define a class family.
- inside use `@virtual` to declare a class as virtual

Example:
--------

```
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

@virtualContext class ExprFormatWithPrePost extends ExprFormat with ExprFormatPrePost {
  
}

@virtualContext class ExprEvalWithFormat extends ExprEval with ExprFormat with ExprFormatPrePost {
  @virtual class Div extends BinExpr {
    def op = "/"
    def op2 = "/"
    def eval = left.eval / right.eval
  }
  
  @virtual class Sub extends BinExpr {
    def op = "-"
    def op2 = "-"
    def eval = left.eval - right.eval
  }
}

```

Current limitations:
-----
1. Constructor arguments are not supported
2. type parameters are not supported
3. only classes can be declared as `@virtual`
4. mixin-composition currently only works on linear cases (mixed class families cannot be mixed in again)
5. classes cannot override implementations derived of other class families
