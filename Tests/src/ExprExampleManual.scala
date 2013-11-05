/*object ExprExampleManual extends App {
  val t = ExprPrefixFormat()
  val two = t.Constant()
  two.value = 2
  val three = t.Constant()
  three.value = 3
  val add = t.Add()
  add.left = two
  add.right = three
  println(add.format + " = " + add.eval)
}

 abstract class ExprModel extends scala.AnyRef {
    type Expr >: _root_.scala.Null <: VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprModel$Expr extends scala.AnyRef { self: Expr => 
      def eval: Int
    };
    object Constant extends scala.AnyRef {
      def apply() = VC_NEW$Constant
    };
    def VC_NEW$Constant: Constant;
    type Constant >: _root_.scala.Null <: Expr with VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprModel$Constant extends scala.AnyRef { self: Constant => 
      var value: Int = 0;
      def eval: Int = value
    };
    type BinExpr >: _root_.scala.Null <: Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprModel$BinExpr extends scala.AnyRef { self: BinExpr => 
      var left: Expr = null;
      var right: Expr = null
    };
    object Add extends scala.AnyRef {
      def apply() = VC_NEW$Add
    };
    def VC_NEW$Add: Add;
    type Add >: _root_.scala.Null <: BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprModel$Add extends scala.AnyRef { self: Add => 
      def eval: Int = left.eval.$plus(right.eval)
    };
    object Mult extends scala.AnyRef {
      def apply() = VC_NEW$Mult
    };
    def VC_NEW$Mult: Mult;
    type Mult >: _root_.scala.Null <: BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprModel$Mult extends scala.AnyRef { self: Mult => 
      def eval: Int = left.eval.$times(right.eval)
    }
  };
  object ExprModel extends scala.AnyRef {
    class VC_FINAL$ExprModel extends ExprModel {
      type Expr = VC_TRAIT$ExprModel$Expr;
      abstract class VC_FIX$ExprModel$Expr extends VC_TRAIT$ExprModel$Expr {
      };
      def VC_NEW$Constant = new VC_FIX$ExprModel$Constant();
      type Constant = Expr with VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprModel$Expr;
      class VC_FIX$ExprModel$Constant extends VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprModel$Expr {
      };
      type BinExpr = Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr;
      abstract class VC_FIX$ExprModel$BinExpr extends VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr {
      };
      def VC_NEW$Add = new VC_FIX$ExprModel$Add();
      type Add = BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr;
      class VC_FIX$ExprModel$Add extends VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr {
      };
      def VC_NEW$Mult = new VC_FIX$ExprModel$Mult();
      type Mult = BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr;
      class VC_FIX$ExprModel$Mult extends VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr {
      }
    };
    def apply() = new VC_FINAL$ExprModel()
  };


abstract class ExprFormat extends ExprModel {
    type Expr >: _root_.scala.Null <: VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Expr;
    abstract trait VC_TRAIT$ExprFormat$Expr extends scala.AnyRef { self: Expr => 
      def format: String
    };
    type Constant >: _root_.scala.Null <: Expr with VC_TRAIT$ExprModel$Constant;
    abstract trait VC_TRAIT$ExprFormat$Constant extends scala.AnyRef { self: Constant => 
      def format: String = value.toString
    };
    type BinExpr >: _root_.scala.Null <: Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprModel$BinExpr;
    abstract trait VC_TRAIT$ExprFormat$BinExpr extends scala.AnyRef { self: BinExpr => 
      def op: String;
      def format: String = "(".$plus(left.format).$plus(" ").$plus(op).$plus(" ").$plus(right.format).$plus(")")
    };
    type Add >: _root_.scala.Null <: BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprFormat$Add extends scala.AnyRef { self: Add => 
      def op: String = "+"
    };
    type Mult >: _root_.scala.Null <: BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprFormat$Mult extends scala.AnyRef { self: Mult => 
      def op: String = "*"
    }
  };
  object ExprFormat extends scala.AnyRef {
    class VC_FINAL$ExprFormat extends ExprFormat {
      type Expr = VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Expr;
      abstract class VC_FIX$ExprFormat$Expr extends VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Expr {
      };
      def VC_NEW$Constant = new VC_FIX$ExprFormat$Constant();
      type Constant = Expr with VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprFormat$Constant;
      class VC_FIX$ExprFormat$Constant extends VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Constant with VC_TRAIT$ExprFormat$Expr {
      };
      type BinExpr = Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprFormat$BinExpr;
      abstract class VC_FIX$ExprFormat$BinExpr extends VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr {
      };
      def VC_NEW$Add = new VC_FIX$ExprFormat$Add();
      type Add = BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Add;
      class VC_FIX$ExprFormat$Add extends VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Add with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr {
      };
      def VC_NEW$Mult = new VC_FIX$ExprFormat$Mult();
      type Mult = BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Mult;
      class VC_FIX$ExprFormat$Mult extends VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Mult with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr {
      }
    };
    def apply() = new VC_FINAL$ExprFormat()
  };
  
  /*abstract class ExprPrefixFormat extends ExprFormat {
    type BinExpr >: _root_.scala.Null <: Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr;
    abstract trait VC_TRAIT$ExprPrefixFormat$BinExpr extends scala.AnyRef { self: BinExpr => 
      override def format: String = "(".$plus(op).$plus(" ").$plus(left.format).$plus(" ").$plus(right.format).$plus(")")
    }
  };
  object ExprPrefixFormat extends scala.AnyRef {
    class VC_FINAL$ExprPrefixFormat extends ExprPrefixFormat {
      type BinExpr = Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprPrefixFormat$BinExpr;
      abstract class VC_FIX$ExprPrefixFormat$BinExpr extends VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprPrefixFormat$BinExpr {
      };
      def VC_NEW$Mult = new VC_FIX$ExprPrefixFormat$Mult();
      type Mult = VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprPrefixFormat$BinExpr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Mult with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Mult;
      class VC_FIX$ExprPrefixFormat$Mult extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprPrefixFormat$BinExpr with VC_TRAIT$ExprFormat$Mult with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Mult {
      };
      def VC_NEW$Add = new VC_FIX$ExprPrefixFormat$Add();
      type Add = VC_TRAIT$ExprFormat$Expr  with VC_TRAIT$ExprFormat$BinExpr  with VC_TRAIT$ExprFormat$Add with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprPrefixFormat$BinExpr;
      class VC_FIX$ExprPrefixFormat$Add extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprPrefixFormat$BinExpr with VC_TRAIT$ExprFormat$Add with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Add {
      };
      def VC_NEW$Constant = new VC_FIX$ExprPrefixFormat$Constant();
      type Constant = VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$Constant with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$Constant;
      class VC_FIX$ExprPrefixFormat$Constant extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$Constant with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$Constant {
      };
      type Expr = VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprModel$Expr;
      abstract class VC_FIX$ExprPrefixFormat$Expr extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprModel$Expr {
      }
    };
    def apply() = new VC_FINAL$ExprPrefixFormat()
  };*/
  
  abstract class ExprPrefixFormat extends ExprFormat {
    type BinExpr >: _root_.scala.Null <: Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprPrefixFormat$BinExpr;
    abstract trait VC_TRAIT$ExprPrefixFormat$BinExpr extends scala.AnyRef { self: BinExpr => 
      override def format: String = "(".$plus(op).$plus(" ").$plus(left.format).$plus(" ").$plus(right.format).$plus(")")
    }
  };
  object ExprPrefixFormat extends scala.AnyRef {
    class VC_FINAL$ExprPrefixFormat extends ExprPrefixFormat {
      type BinExpr = Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprPrefixFormat$BinExpr;
      abstract class VC_FIX$ExprPrefixFormat$BinExpr extends VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprPrefixFormat$BinExpr {
      };
      def VC_NEW$Mult = new VC_FIX$ExprPrefixFormat$Mult();
      type Mult = VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Mult with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprPrefixFormat$BinExpr;
      class VC_FIX$ExprPrefixFormat$Mult extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Mult with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprPrefixFormat$BinExpr {
      };
      def VC_NEW$Add = new VC_FIX$ExprPrefixFormat$Add();
      type Add = VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Add with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprPrefixFormat$BinExpr;
      class VC_FIX$ExprPrefixFormat$Add extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprFormat$Add with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprPrefixFormat$BinExpr {
      };
      def VC_NEW$Constant = new VC_FIX$ExprPrefixFormat$Constant();
      type Constant = VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$Constant with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$Constant;
      class VC_FIX$ExprPrefixFormat$Constant extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$Constant with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$Constant {
      };
      type Expr = VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprModel$Expr;
      abstract class VC_FIX$ExprPrefixFormat$Expr extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprModel$Expr {
      }
    };
    def apply() = new VC_FINAL$ExprPrefixFormat()
  };*/
