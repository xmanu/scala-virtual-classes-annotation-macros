package basicexample

import VirtualClasses._

object BasicTest extends App {
  val foo = Foo()
  val fooBar = foo.Bar()
  println(fooBar.foo)

  val subFoo = SubFoo()
  val subFooBar = subFoo.Bar()
  println(subFooBar.foo)

  val superFoo: Foo = subFoo
  val superFooBar = superFoo.Bar()
  println(superFooBar.foo)
  assert(superFooBar.foo == 42) // Without virtual classes, this would return 1
}

@virtualContext
class Foo {
  @virtual
  class Bar {
    def foo = 1
  }
}

@virtualContext
class SubFoo extends Foo {
  @virtualOverride
  class Bar {
    override def foo = 42
  }
}
