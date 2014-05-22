package basicexample

import VirtualClasses._

object ConstructorParameters extends App {
  val oa = OuterA()
  val ia1 = oa.InnerA(42)
  println(s"${ia1.i} == 42")
  println(s"${oa.inner.i} == 7")
  
  val ob = OuterB()
  val ia2 = ob.InnerA(42, "truth")
  println(s"${ia2.i} == 42")
  println(s"${ia2.s} == truth")
  println(s"${ob.inner.i} == 7")
  println(s"${ob.inner.s} == Test")
}


@family class OuterA {
  @virtual class InnerA(val i: Int)
  
  val inner = InnerA(7)
}

@family class OuterB extends OuterA {
  @virtual override class InnerA(val i: Int, val s: String = "Test") 
}