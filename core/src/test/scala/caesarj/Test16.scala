package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test16 extends UnitSpec {
  "state inheritance" should "work" in {
    def expectedResultBB = "A.A.a, A.A.a, A.A.a, A.A.a; " +
      "A.A.b, B.A.b, A.B.b, A.B.b; " +
      "A.A.c, A.A.c, A.B.c, A.B.c; " +
      "A.A.d, B.A.d, B.A.d, B.A.d; " +
      "A.B.e, A.B.e; " +
      "A.B.f, B.B.f; " +
      "A.A.g, B.A.g, A.B.g, B.B.g";
    def expectedResultBA = "A.A.a, A.A.a; " +
      "A.A.b, B.A.b; " +
      "A.A.c, A.A.c; " +
      "A.A.d, B.A.d; " +
      "B.A.e; " +
      "B.A.f; " +
      "A.A.g, B.A.g";

    val ob: OuterB16 = OuterB16();
		val bb: ob.InnerB = ob.InnerB();

		/*var resA = bb.queryA;
		var resB = bb.queryB;
		var resC = bb.queryC;
		var resD = bb.queryD;
		var resE = bb.queryE;
		var resF = bb.queryF;
		var resG = bb.queryG;
		var result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG;

		result should equal (expectedResultBB);*/
		bb.queryA should equal ("A.A.a, A.A.a, A.A.a, A.A.a")
		bb.queryB should equal ("A.A.b, B.A.b, A.B.b, A.B.b")

		val ba: ob.InnerA = ob.InnerA();

		/*resA = ba.queryA;
		resB = ba.queryB;
		resC = ba.queryC;
		resD = ba.queryD;
		resE = ba.queryE;
		resF = ba.queryF;
		resG = ba.queryG;
		result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG;

		result should equal (expectedResultBA);*/
  }
}

@virtualContext class OuterA16 {
  @virtual class InnerA {
    def _a = "A.A.a";

    def _b = "A.A.b";

    def _c = "A.A.c";

    def _d = "A.A.d";

    def _g = "A.A.g";

    def queryA = _a;

    def queryB = _b;

    def queryC = _c;

    def queryD = _d;

    def queryG = _g;
  }

  @virtual class InnerB extends InnerA {
    override def _b = "A.B.b";

    override def _c = "A.B.c";

    def _e = "A.B.e";

    def _f = "A.B.f";

    override def _g = "A.B.g";

    override def queryA = super.queryA + ", " + _a;

    override def queryB = super.queryB + ", " + _b;

    override def queryC = super.queryC + ", " + _c;

    override def queryD = super.queryD + ", " + _d;

    def queryE = _e;

    def queryF = _f;

    override def queryG = super.queryG + ", " + _g;
  }
}

@virtualContext class OuterB16 extends OuterA16 {
  @virtual override class InnerA {
    override def _b = "B.A.b";

    override def _d = "B.A.d";

    def _e = "B.A.e";

    def _f = "B.A.f";

    override def _g = "B.A.g";

    override def queryA = super.queryA + ", " + _a;

    override def queryB = super.queryB + ", " + _b;

    override def queryC = super.queryC + ", " + _c;

    override def queryD = super.queryD + ", " + _d;

    def queryE = _e;

    def queryF = _f;

    override def queryG = super.queryG + ", " + _g;
  }

  @virtual override class InnerB {
    override def _e = super[VC_TRAIT$OuterA16$InnerB]._e

    override def _f = "B.B.f";

    override def _g = "B.B.g";

    override def queryA = super.queryA + ", " + _a;

    override def queryB = super.queryB + ", " + _b;

    override def queryC = super.queryC + ", " + _c;

    override def queryD = super.queryD + ", " + _d;

    override def queryE = super.queryE + ", " + _e;

    override def queryF = super.queryF + ", " + _f;

    override def queryG = super.queryG + ", " + _g;
  }
}