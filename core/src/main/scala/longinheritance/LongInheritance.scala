package longinheritance

import VirtualClasses._

object LongInheritance extends App {
  val loc = LongOuterC()
  val lih = loc.LongInnerH()
  println(lih)
}

@family class LongOuterA {
  @virtual class LongInnerA
  @virtual class LongInnerB extends LongInnerA
  @virtual class LongInnerC extends LongInnerB
  @virtual class LongInnerD extends LongInnerC
}


@family class LongOuterB extends LongOuterA {
  @virtual class LongInnerE extends LongInnerD
  @virtual class LongInnerF extends LongInnerE
}

@family class LongOuterC extends LongOuterB {
  @virtual class LongInnerG extends LongInnerF
  @virtual class LongInnerH extends LongInnerG
}