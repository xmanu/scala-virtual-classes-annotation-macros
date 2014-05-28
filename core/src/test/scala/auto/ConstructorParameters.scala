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
    
    val od = OuterD()
    val ia3 = od.InnerA(42, "truth", 49, "truth2")
    assert(ia3.i == 42)
    assert(ia3.s == "truth")
    assert(ia3.j == 49)
    assert(ia3.k == "truth2")
    assert(od.inner.i == 7)
    assert(od.inner.s == "Test")
    assert(od.inner.j == 0)
    assert(od.inner.k == "k")
    
    val oe = OuterE()
    val ia4 = oe.InnerA(42, "truth", 49, "truth2")
    assert(ia4.i == 42)
    assert(ia4.s == "truth")
    assert(ia4.j == 49)
    assert(ia4.k == "truth2")
    assert(oe.inner.i == 7)
    assert(oe.inner.s == "Test")
    assert(oe.inner.j == 1)
    assert(oe.inner.k == "k2")
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

@family class OuterD extends OuterB with OuterC {
  @virtual override class InnerA(val i: Int, val s: String, val j: Int, val k: String = "k")
}

@family class OuterE extends OuterD {
  @virtual override class InnerA {
    override val k: String = "k2"
    override val j: Int = 1
  }
}