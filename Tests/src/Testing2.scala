abstract class TestClass extends scala.AnyRef {
    def VC_NEW$Test: VC_TRAIT$TestClass$Test;
    type Test >: _root_.scala.Null <: VC_TRAIT$TestClass$Test;
    abstract trait VC_TRAIT$TestClass$Test extends scala.AnyRef { self: Test => 
      def testMethod = println("Hello")
      def value = 2
    }
  };
  object TestClass extends scala.AnyRef {
    class VC_FINAL$TestClass extends TestClass {
      object Test extends scala.AnyRef {
        def apply() = VC_NEW$Test
      };
      def VC_NEW$Test = new VC_FIX$TestClass$Test();
      type Test = VC_TRAIT$TestClass$Test;
      class VC_FIX$TestClass$Test extends VC_TRAIT$TestClass$Test {
      }
    };
    def apply() = new VC_FINAL$TestClass()
  };

  abstract class TestClass2 extends TestClass {
    //type Test >: _root_.scala.Null <: VC_TRAIT$TestClass$Test with VC_TRAIT$TestClass2$Test;
    //abstract trait VC_TRAIT$TestClass2$Test extends scala.AnyRef { self: Test => 
    //  def testMethod2 = println("World!")
    //};
    def VC_NEW$Test2: VC_TRAIT$TestClass2$Test2;
    type Test2 >: _root_.scala.Null <: Test with VC_TRAIT$TestClass2$Test2;
    abstract trait VC_TRAIT$TestClass2$Test2 extends scala.AnyRef { self: Test2 => 
      def test = println("Test" + value)
    }
  };
  object TestClass2 extends scala.AnyRef {
    class VC_FINAL$TestClass2 extends TestClass2 {
      object Test extends scala.AnyRef {
    	def apply() = VC_NEW$Test
      }
      def VC_NEW$Test = new VC_FIX$TestClass2$Test()
      type Test = VC_TRAIT$TestClass$Test
      class VC_FIX$TestClass2$Test extends VC_TRAIT$TestClass$Test {
      }
      object Test2 extends scala.AnyRef {
        def apply() = VC_NEW$Test2
      };
      def VC_NEW$Test2 = new VC_FIX$TestClass2$Test2();
      type Test2 = Test with VC_TRAIT$TestClass2$Test2;
      class VC_FIX$TestClass2$Test2 extends VC_TRAIT$TestClass$Test with VC_TRAIT$TestClass2$Test2 {
      }
    };
    def apply() = new VC_FINAL$TestClass2()
  };
