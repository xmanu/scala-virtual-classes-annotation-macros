object Test extends App {
  val tc = TestClass2()
  val t: tc.Test = tc.Test()
  t.testMethod
  t.testMethod2
  val t2 = tc.Test2()
  t2.test
}

@virtualContext
class TestClass {

  @virtual
  class Test {
    def testMethod = println("Hello")
    def value = 2
  }
}

@virtualContext
class TestClass2 extends TestClass {
  @virtual
  class Test {
    def testMethod2 = println("World!")
  }
  
  
  @virtual
  class Test2 extends Test {
    def test = println("Test " + value)
  }
}