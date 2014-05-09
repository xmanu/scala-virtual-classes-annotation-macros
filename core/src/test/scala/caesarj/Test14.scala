package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test14 extends UnitSpec {
  "Super calls" should "work" in {
    def expectedResultBB = "A.A.A; A.A.B, B.A.B, A.B.B; A.A.C, A.B.C; A.A.D, B.A.D; A.B.E; A.B.F, B.B.F; A.A.G, B.A.G, A.B.G, B.B.G";
	def expectedResultBA = "A.A.A; A.A.B, B.A.B; A.A.C; A.A.D, B.A.D; B.A.E; B.A.F; A.A.G, B.A.G"; 
	
	val ob: OuterB14 = OuterB14();
		val bb: ob.InnerB = ob.InnerB();

		var resA = bb.queryA;
		var resB = bb.queryB;
		var resC = bb.queryC;
		var resD = bb.queryD;
		var resE = bb.queryE;
		var resF = bb.queryF;
		var resG = bb.queryG;
		var result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG;

		result should equal (expectedResultBB);

		def ba: ob.InnerA = ob.InnerA();

		resA = ba.queryA;
		resB = ba.queryB;
		resC = ba.queryC;
		resD = ba.queryD;
		resE = ba.queryE;
		resF = ba.queryF;
		resG = ba.queryG;
		result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG;
		result should equal (expectedResultBA);
  }
}

@virtualContext class OuterA14 {
  @virtual class InnerA {
    def queryA = "A.A.A";

    def queryB = "A.A.B";

    def queryC = "A.A.C";

    def queryD = "A.A.D";

    def queryG = "A.A.G";
  }

  @virtual class InnerB extends InnerA {
    override def queryB = super.queryB + ", A.B.B";

    override def queryC = super.queryC + ", A.B.C";

    def queryE = "A.B.E";

    def queryF = "A.B.F";

    override def queryG = super.queryG + ", A.B.G";
  }
}

@virtualContext class OuterB14 extends OuterA14 {
  @virtual override class InnerA {
    override def queryB = super.queryB + ", B.A.B";

    override def queryD = super.queryD + ", B.A.D";

    def queryE = "B.A.E";

    def queryF = "B.A.F";

    override def queryG = super.queryG + ", B.A.G";
  }

  @virtual override class InnerB {
    // this should not be necessary!
    override def queryE = super[VC_TRAIT$OuterA14$InnerB].queryE

    override def queryF = super.queryF + ", B.B.F";

    override def queryG = super.queryG + ", B.B.G";
  }
}