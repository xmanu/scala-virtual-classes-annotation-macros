abstract class TestClass extends scala.AnyRef {
    type Test >: _root_.scala.Null <: VC_TRAIT$TestClass$Test;
    abstract trait VC_TRAIT$TestClass$Test extends scala.AnyRef { self: Test => 
      def testMethod = println("Hello");
      def value = 2
    }
  };
  object TestClass extends scala.AnyRef {
    class VC_FINAL$TestClass extends TestClass {
      type Test = VC_TRAIT$TestClass$Test;
      abstract trait VC_FIX$TestClass$Test extends VC_TRAIT$TestClass$Test {
      }
    };
    def apply() = new VC_FINAL$TestClass()
  };
