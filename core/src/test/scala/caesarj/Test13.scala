package caesarj

import VirtualClasses._

class Test13 extends UnitSpec {
  "inherited methods" should "work" in {
    val expectedResult = "A.A.A, A.B.B, A.B.C, B.A.D, A.B.E, B.B.F, B.B.G";

    
     val ob = OuterB3();
		val bb = ob.InnerB();

		val resA = bb.queryA;
		val resB = bb.queryB;
		val resC = bb.queryC;
		val resD = bb.queryD;
		val resE = bb.queryE;
		val resF = bb.queryF;
		val resG = bb.queryG;
		val result = resA + ", " + resB + ", " + resC + ", " + resD + ", " + resE + ", " + resF + ", " + resG;

		System.out.println(result);
		result should equal (expectedResult);
  }
}

@virtualContext class OuterA3 {
  @virtual class InnerA {
    def queryA = "A.A.A";

    def queryB = "A.A.B";

    def queryC = "A.A.C";

    def queryD = "A.A.D";

    def queryG = "A.A.G";
  }

  @virtual class InnerB extends InnerA {
    override def queryB = "A.B.B";

    override def queryC = "A.B.C";

    def queryE = "A.B.E";

    def queryF = "A.B.F";

    override def queryG = "A.B.G";
  }
}

@virtualContext class OuterB3 extends OuterA3 {
  @virtualOverride class InnerA {
    override def queryB = "B.A.B";

    override def queryD = "B.A.D";

    def queryE = "B.A.E";

    def queryF = "B.A.F";

    override def queryG = "B.A.G";
  }

  @virtualOverride class InnerB {
    // BUG: this should not be neccessary!
    override def queryE = super[VC_TRAIT$OuterA3$InnerB].queryE

    override def queryF = "B.B.F";

    override def queryG = "B.B.G";
  }
}