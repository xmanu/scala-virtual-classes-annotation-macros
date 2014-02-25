package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test4 extends UnitSpec {
  "AST" should "print fine" in {
    val ast = EvalPrettyPrintNegAST();
		val l1 = ast.Literal(5);
		val l2 = ast.Literal(4);
		val add = ast.AddExpression(l1, l2);
		val neg = ast.NegExpression(add);

		neg.print()+" = "+neg.eval() should equal ("-( ( 5 + 4 ) ) = -9")
  }
}

@virtualContext class AST {

  @virtual class Expression {
  }

  @virtual class AddExpression(val l: Expression, val r: Expression) extends Expression {
  }

  @virtual class Literal(val value: Int) extends Expression {
  }
}

//=========================================================
@virtualContext class EvalAST extends AST {
  @virtual override class Expression {
    def eval(): Int = 0
  }

  @virtual override class AddExpression {
    override def eval() = {
      l.eval() + r.eval();
    }
  }

  @virtual override class Literal {
    override def eval() = value
  }
}

//=========================================================
@virtualContext class PrettyPrintAST extends AST {
  @virtual override class Expression {
    def print() = ""
  }

  @virtual override class AddExpression {
    override def print() = s"( ${l.print()} + ${r.print()} )"
  }

  @virtual override class Literal {
    override def print() = value.toString();
  }
}

//=========================================================
@virtualContext class NegAST extends AST {
  @virtual class NegExpression(val expr: Expression) extends Expression {
  }
}

//=========================================================
@virtualContext class EvalPrettyPrintNegAST extends EvalAST with PrettyPrintAST with NegAST {
  @virtual override class NegExpression {
    override def eval() = -expr.eval() 
    override def print() = s"-( ${expr.print()} )";
  }
}