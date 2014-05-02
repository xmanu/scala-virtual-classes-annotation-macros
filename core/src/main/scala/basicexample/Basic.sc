package basicexample

object Basic {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  class Foo {
    class Bar {
      def test = 1
    }
  }

  class SubFoo extends Foo {
    class Bar {
      def test = 42
    }
  }

  val foo = new Foo()                             //> foo  : basicexample.Basic.Foo = basicexample.Basic$$anonfun$main$1$Foo$1@4b0
                                                  //| ab323
  println((new foo.Bar).test)                     //> 1

  val subfoo = new SubFoo()                       //> subfoo  : basicexample.Basic.SubFoo = basicexample.Basic$$anonfun$main$1$Sub
                                                  //| Foo$1@ac980c9

  println((new subfoo.Bar).test)                  //> 42

  val superFoo: Foo = subfoo                      //> superFoo  : basicexample.Basic.Foo = basicexample.Basic$$anonfun$main$1$SubF
                                                  //| oo$1@ac980c9

  println((new superFoo.Bar).test)                //> 1
}