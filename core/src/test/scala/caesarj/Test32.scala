package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test32 extends UnitSpec {
  "introducing new inheritance" should "work" in {
    val expectedResult = "A.A.A, A.B.A, A.C.A";

    val ob = OuterB32();
    val bc = ob.InnerC();

    val result = bc.queryA;

    System.out.println(result);
    
    result should equal (expectedResult)
  }
}

@family class OuterA32 {
  @virtual class InnerA {
    def queryA = "A.A.A";
  }

  @virtual class InnerB extends InnerA {
    override def queryA = super.queryA + ", A.B.A";
  }

  @virtual class InnerC extends InnerA {
    override def queryA = super.queryA + ", A.C.A";
  }
}

@family class OuterB32 extends OuterA32 {
  @virtual override class InnerC extends InnerB {}
}