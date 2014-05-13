package basicexample

import VirtualClasses._

object BasicTest extends App {
  val foo = Foo()
  val fooBar = foo.Bar()
  assert(fooBar.foo == 1)
  println(fooBar.foo)

  val subFoo = SubFoo()
  val subFooBar = subFoo.Bar()
  assert(subFooBar.foo == 42)
  println(subFooBar.foo)

  val superFoo: Foo = subFoo
  val superFooBar = superFoo.Bar()
  println(superFooBar.foo)
  assert(superFooBar.foo == 42) // Without virtual classes, this would return 1
}

@family class Foo {
  @virtual class Bar {
    def foo = 1
  }
}

@family class SubFoo extends Foo {
  @virtual override class Bar {
    override def foo = 42
  }
}
