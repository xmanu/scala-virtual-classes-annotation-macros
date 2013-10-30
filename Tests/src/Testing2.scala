abstract class ExprModel extends scala.AnyRef {
    type Expr >: _root_.scala.Null <: VC_TRAIT$ExprModel$Expr;
    abstract trait VC_TRAIT$ExprModel$Expr extends scala.AnyRef { self: Expr => 
      def eval: Int
    };
    def VC_NEW$Constant: VC_TRAIT$ExprModel$Constant;
    type Constant >: _root_.scala.Null <: Expr with VC_TRAIT$ExprModel$Constant;
    abstract trait VC_TRAIT$ExprModel$Constant extends scala.AnyRef { self: Constant => 
      var value: Int = 0;
      def eval: Int = value
    };
    type BinExpr >: _root_.scala.Null <: Expr with VC_TRAIT$ExprModel$BinExpr;
    abstract trait VC_TRAIT$ExprModel$BinExpr extends scala.AnyRef { self: BinExpr => 
      var left: Expr = null;
      var right: Expr = null
    };
    def VC_NEW$Add: VC_TRAIT$ExprModel$Add;
    type Add >: _root_.scala.Null <: BinExpr with VC_TRAIT$ExprModel$Add;
    abstract trait VC_TRAIT$ExprModel$Add extends scala.AnyRef { self: Add => 
      def eval: Int = left.eval.$plus(right.eval)
    };
    def VC_NEW$Mult: VC_TRAIT$ExprModel$Mult;
    type Mult >: _root_.scala.Null <: BinExpr with VC_TRAIT$ExprModel$Mult;
    abstract trait VC_TRAIT$ExprModel$Mult extends scala.AnyRef { self: Mult => 
      def eval: Int = left.eval.$times(right.eval)
    }
  };
  object ExprModel extends scala.AnyRef {
    class VC_FINAL$ExprModel extends ExprModel {
      type Expr = VC_TRAIT$ExprModel$Expr;
      abstract class VC_FIX$ExprModel$Expr extends VC_TRAIT$ExprModel$Expr {
      };
      object Constant extends scala.AnyRef {
        def apply() = VC_NEW$Constant
      };
      def VC_NEW$Constant = new VC_FIX$ExprModel$Constant();
      type Constant = Expr with VC_TRAIT$ExprModel$Constant;
      class VC_FIX$ExprModel$Constant extends VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprModel$Expr {
      };
      type BinExpr = Expr with VC_TRAIT$ExprModel$BinExpr;
      abstract class VC_FIX$ExprModel$BinExpr extends VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr {
      };
      object Add extends scala.AnyRef {
        def apply() = VC_NEW$Add
      };
      def VC_NEW$Add = new VC_FIX$ExprModel$Add();
      type Add = BinExpr with VC_TRAIT$ExprModel$Add;
      class VC_FIX$ExprModel$Add extends VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr {
      };
      object Mult extends scala.AnyRef {
        def apply() = VC_NEW$Mult
      };
      def VC_NEW$Mult = new VC_FIX$ExprModel$Mult();
      type Mult = BinExpr with VC_TRAIT$ExprModel$Mult;
      class VC_FIX$ExprModel$Mult extends VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Expr {
      }
    };
    def apply() = new VC_FINAL$ExprModel()
  };

abstract class ExprFormat extends ExprModel {
    type Expr >: _root_.scala.Null <: VC_TRAIT$ExprFormat$Expr;
    abstract trait VC_TRAIT$ExprFormat$Expr extends scala.AnyRef { self: Expr => 
      def format: String
    };
    type Constant >: _root_.scala.Null <: VC_TRAIT$ExprFormat$Constant;
    abstract trait VC_TRAIT$ExprFormat$Constant extends scala.AnyRef { self: Constant => 
      def format: String = value.toString
    };
    type BinExpr >: _root_.scala.Null <: VC_TRAIT$ExprFormat$BinExpr;
    abstract trait VC_TRAIT$ExprFormat$BinExpr extends scala.AnyRef { self: BinExpr => 
      def op: String;
      def format: String = left.format.$plus(" ").$plus(op).$plus(" ").$plus(right.format)
    };
    type Add >: _root_.scala.Null <: VC_TRAIT$ExprFormat$Add;
    abstract trait VC_TRAIT$ExprFormat$Add extends scala.AnyRef { self: Add => 
      def op: String = "+"
    };
    type Mult >: _root_.scala.Null <: VC_TRAIT$ExprFormat$Mult;
    abstract trait VC_TRAIT$ExprFormat$Mult extends scala.AnyRef { self: Mult => 
      def op: String = "*"
    }
  };
  object ExprFormat extends scala.AnyRef {
    class VC_FINAL$ExprFormat extends ExprFormat {
      type Expr = VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Expr;
      abstract class VC_FIX$ExprFormat$Expr extends VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprModel$Expr {
      };
      object Constant extends scala.AnyRef {
        def apply() = VC_NEW$Constant
      };
      def VC_NEW$Constant = new VC_FIX$ExprFormat$Constant();
      type Constant = Expr with VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprFormat$Constant;
      class VC_FIX$ExprFormat$Constant extends VC_TRAIT$ExprFormat$Constant with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$Constant with VC_TRAIT$ExprFormat$Expr {
      };
      type BinExpr = Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprFormat$BinExpr;
      abstract class VC_FIX$ExprFormat$BinExpr extends VC_TRAIT$ExprFormat$BinExpr with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprFormat$Expr {
      };
      object Add extends scala.AnyRef {
        def apply() = VC_NEW$Add
      };
      def VC_NEW$Add = new VC_FIX$ExprFormat$Add();
      type Add = BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Add;
      class VC_FIX$ExprFormat$Add extends VC_TRAIT$ExprFormat$Add with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Add with VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr {
      };
      object Mult extends scala.AnyRef {
        def apply() = VC_NEW$Mult
      };
      def VC_NEW$Mult = new VC_FIX$ExprFormat$Mult();
      type Mult = BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprFormat$Mult;
      class VC_FIX$ExprFormat$Mult extends VC_TRAIT$ExprFormat$Mult with VC_TRAIT$ExprModel$Expr with VC_TRAIT$ExprModel$BinExpr with VC_TRAIT$ExprModel$Mult with VC_TRAIT$ExprFormat$Expr with VC_TRAIT$ExprFormat$BinExpr {
      }
    };
    def apply() = new VC_FINAL$ExprFormat()
  };