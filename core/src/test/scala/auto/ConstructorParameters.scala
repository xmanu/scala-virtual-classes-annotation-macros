package auto

import VirtualClasses._
import specs.UnitSpec

class ConstructorParameters extends UnitSpec {
  "new constructors" should "be addable" in {
    val oa = OuterA()
    val ia1 = oa.InnerA(42)
    assert(ia1.i == 42)
    assert(oa.inner.i == 7)

    val ob = OuterB()
    val ia2 = ob.InnerA(42, "truth")
    assert(ia2.i == 42)
    assert(ia2.s == "truth")
    assert(ob.inner.i == 7)
    assert(ob.inner.s == "Test")
    
    //val od = OuterD()
  }
}

@family class OuterA {
  @virtual class InnerA(val i: Int)

  val inner = InnerA(7)
}

@family class OuterB extends OuterA {
  @virtual override class InnerA(val i: Int, val s: String = "Test")
}

@family class OuterC extends OuterA {
  @virtual override class InnerA(val i: Int, val j: Int = 0)
}

/*@family class OuterD extends OuterB with OuterC {
  @virtual override class InnerA(val i: Int, val s: String = "Test2", val j: Int = 9)
}*/