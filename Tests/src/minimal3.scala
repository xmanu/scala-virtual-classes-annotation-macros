object ManualTest extends App {
  val x = X()
  val y: x.Y = x.Y()
  y.foo
  
  val a = A()
  val y2: a.Y = a.Y()
  y2.foo
  y2.bar
}

abstract class X {
  type Y <: VC_TRAIT$Y

  trait VC_TRAIT$Y { self: Y =>
	  def foo = println("bar")
  }

  def VC_NEW$Y: Y
}

object X {
  class VC_FINAL$X extends X {
    type Y = VC_TRAIT$Y

    class VC_FIX$Y extends VC_TRAIT$Y {
    	
    }

    def VC_NEW$Y = new VC_FIX$Y

    object Y {
      def apply() = VC_NEW$Y
    }
  }

  def apply() = new VC_FINAL$X
}

abstract class A extends X {
  type Y <: VC_TRAIT$Y with VC_TRAIT$A$Y
  
  trait VC_TRAIT$A$Y { self: Y =>
    def bar = println("foo")
  }
}

object A {
  @print class VC_FINAL$A extends A {
    type Y = VC_TRAIT$Y with VC_TRAIT$A$Y

    class VC_FIX$A$Y extends VC_TRAIT$Y with VC_TRAIT$A$Y {
    	
    }

    def VC_NEW$Y = new VC_FIX$A$Y

    object Y {
      def apply() = VC_NEW$Y
    }
  }

  def apply() = new VC_FINAL$A
}