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
    assert(od.inner.s == "Test2")
    assert(od.inner.j == 1)
    assert(od.inner.k == "k")
    
    val oe = OuterE()
    val ia4 = oe.InnerA(42, "truth", 49, "truth2")
    assert(ia4.i == 42)
    assert(ia4.s == "truth")
    assert(ia4.j == 49)
    assert(ia4.k == "truth2")
    assert(oe.inner.i == 7)
    assert(oe.inner.s == "Test2")
    assert(oe.inner.j == 2)
    assert(oe.inner.k == "k2")
    
    val of = OuterF()
    val ia5 = of.InnerB(42, "truth", 49, "truth2", "m2")
    assert(ia5.i == 42)
    assert(ia5.s == "truth")
    assert(ia5.j == 49)
    assert(ia5.k == "truth2")
    assert(ia5.m == "m2")
    assert(of.inner.i == 7)
    assert(of.innerb.i == 8)
    assert(of.inner.s == "Test2")
    assert(of.inner.j == 2)
    assert(of.inner.k == "k2")
    assert(of.innerb.m == "m")
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
  @virtual override class InnerA(val i: Int, val s: String = "Test2", val j: Int = 1, val k: String = "k")
}

@family class OuterE extends OuterD {
  @virtual override class InnerA {
    override val k: String = "k2"
    override val j: Int = 2
  }
}

@family class OuterF extends OuterE {
  @virtual class InnerB(val i: Int, val s: String = "Test3", val j: Int = 3, val k: String = "k3", val m: String = "m") extends InnerA {
    
  }
  
  val innerb = InnerB(8)
}

/*@family class OuterG extends OuterF {
  // constructor parameters with type parameters are not allowed
  @virtual class InnerC(val lst: List[String])
}*/