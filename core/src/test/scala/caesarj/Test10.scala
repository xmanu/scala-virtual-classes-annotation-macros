package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test10 extends UnitSpec {
  "factory methods of outer classes" should "work" in {
    val expectedResult = "A.A.A, A.B.A, B.A.A, A.B.A, B.C.A, B.A.A, A.B.A"

    var oa: OuterA2 = OuterA2();
    val ob = OuterB2();

    val resAA = oa.InnerA2().queryA
    val resAB = oa.InnerB2().queryA
    val resBA = ob.InnerA2().queryA
    val resBB = ob.InnerB2().queryA
    val resBC = ob.InnerC2().queryA

    oa = ob;
    val resBA1 = oa.InnerA2().queryA
    val resBB1 = oa.InnerB2().queryA

    val result = resAA + ", " + resAB + ", " + resBA + ", " + resBB + ", " + resBC + ", " + resBA1 + ", " + resBB1;

    println(result);
    result should equal(expectedResult);
  }
}

@family class OuterA2 {
  @virtual class InnerA2 {
    def queryA = "A.A.A"
  }

  @virtual class InnerB2 {
    def queryA = "A.B.A"
  }
}

@family class OuterB2 extends OuterA2 {
  @virtual override class InnerA2 {
    override def queryA = "B.A.A"
  }

  @virtual class InnerC2 {
    def queryA = "B.C.A"
  }
}