/*abstract class TestClass extends scala.AnyRef {
    object Test extends scala.AnyRef {
      def apply() = VC_NEW$Test
    };
    def VC_NEW$Test: Test;
    type Test >: _root_.scala.Null <: VC_TRAIT$TestClass$Test;
    abstract trait VC_TRAIT$TestClass$Test extends scala.AnyRef { self: Test => 
      def testMethod = println("Hello");
      def value = 2
    };
    object Test3 extends scala.AnyRef {
      def apply() = VC_NEW$Test3
    };
    def VC_NEW$Test3[T]: Test3[T];
    type Test3[+T >: _root_.scala.Nothing <: Test] >: _root_.scala.Null <: VC_TRAIT$TestClass$Test3[T];
    abstract trait VC_TRAIT$TestClass$Test3[+T >: _root_.scala.Nothing <: Test] extends scala.AnyRef { self: Test3[T] => 
    }
  };
  object TestClass extends scala.AnyRef {
    class VC_FINAL$TestClass extends TestClass {
      def VC_NEW$Test = new VC_FIX$TestClass$Test();
      type Test = VC_TRAIT$TestClass$Test;
      class VC_FIX$TestClass$Test extends VC_TRAIT$TestClass$Test {
      };
      def VC_NEW$Test3 = new VC_FIX$TestClass$Test3();
      type Test3 = VC_TRAIT$TestClass$Test3;
      class VC_FIX$TestClass$Test3 extends VC_TRAIT$TestClass$Test3 {
      }
    };
    def apply() = new VC_FINAL$TestClass()
  };*/