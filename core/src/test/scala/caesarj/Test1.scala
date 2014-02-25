package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test1 extends UnitSpec {
  "A derived virtual class" should "be able to access super" in {
    val a = A()
    val b = B()
    a.X().test should equal ("a.X")
    b.X().test should equal ("a.X->b.X")
  }
}

@virtualContext class A {
  @virtual class X {
    def test: String = "a.X"
  }
}

@virtualContext class B extends A {
  @virtual override class X {
    override def test: String = super.test + "->b.X"
  }
}