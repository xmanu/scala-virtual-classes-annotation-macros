package auto

import specs.UnitSpec
import VirtualClasses._

class SimpleTest extends UnitSpec {
  "Basic example" should "work" in {
    val foo = Foo()
    val fooBar = foo.Bar()
    fooBar.foo should equal (1)

    val subFoo = SubFoo()
    val subFooBar = subFoo.Bar()
    subFooBar.foo should equal (42)

    val superFoo: Foo = subFoo
    val superFooBar = superFoo.Bar()
    superFooBar.foo should equal (42)
  }
}

@virtualContext class Foo {
  @virtual class Bar {
    def foo = 1
  }
}

@virtualContext class SubFoo extends Foo {
  @virtual override class Bar {
    override def foo = 42
  }
}
