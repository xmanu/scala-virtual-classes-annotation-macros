package auto

import VirtualClasses._

object Test extends App {
  val tc = TestClass2()
  val t: tc.Test = tc.Test()
  t.testMethod
  //t.testMethod2
  val t2 = tc.Test2()
  t2.test
}

@family
class TestClass {

  @virtual
  class Test {
    def testMethod = println("Hello")
    def value = 2
  }

  /*@virtual class Test3[T <: Test] {

  }*/
}

@family
class TestClass2 extends TestClass {
  /*@virtual
  class Test {
    def testMethod2 = println("World!")
  }*/


  @virtual
  class Test2 extends Test {
    def test = println("Test " + value)
  }
}