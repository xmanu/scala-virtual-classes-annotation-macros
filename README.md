scala-virtual-classes-annotaion-macros
======================================

Virtual Classes for Scala implemented as annotation macros

master (2.11.2; Java 8): [![Build Status](https://travis-ci.org/xmanu/scala-virtual-classes-annotation-macros.svg?branch=master)](https://travis-ci.org/xmanu/scala-virtual-classes-annotation-macros)

2.10: [![Build Status](https://travis-ci.org/xmanu/scala-virtual-classes-annotation-macros.svg?branch=2.10)](https://travis-ci.org/xmanu/scala-virtual-classes-annotation-macros)

Usage:
-------
1. Check out the repository
2. Add your own project to the `Build.scala` like this:
```scala
lazy val myproject: Project = Project(
  "myproject",
  file("myproject")
) dependsOn(macros)
````
3. Run `sbt eclipse`
4. Import the projects in Eclipse.

Syntax:
--------
- use `@family` to define a class family.
- inside use `@virtual` to declare a class as virtual

Example:
--------

```scala
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

@family class ExprModel {
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
    def format: String = "(" + left.format + " " + op + " " + right.format + ")"
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
    def formatPre: String = "(" + op + " " + left.formatPre + " " + right.formatPre + ")"
    def formatPost: String = left.formatPost + " " + right.formatPost + " " + op
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
```

Current limitations:
-----
1. Constructor arguments support is very limited and type parameters are not supported. Constructor parameters can only be added in sub-families and also it is not possible to pass paremeters to base classes yet.
2. type parameters are not supported
3. Virtual classes in virtual classes are not handled correctly
4. Virtual case classes are not supported
