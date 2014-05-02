package caesarj

import specs.UnitSpec
import VirtualClasses._

class Test19 extends UnitSpec {
  "multiple inheritance of state" should "work" in {
    def expectedResult = "A.a, A.a, A.a, A.a; " +
      "A.b, A.b, B.b, B.b; " +
      "A.c, C.c, C.c, C.c; " +
      "A.d, C.d, B.d, B.d; " +
      "B.e, B.e; " +
      "B.f, D.f; " +
      "A.g, C.g, B.g, D.g";

    val d = OuterD19();

    val resA = d.queryA;
    val resB = d.queryB;
    val resC = d.queryC;
    val resD = d.queryD;
    val resE = d.queryE;
    val resF = d.queryF;
    val resG = d.queryG;
    val result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG;

    result should equal(expectedResult);
  }
}

@virtualContext class OuterA19 {
  def _a = "A.a";

  def _b = "A.b";

  def _c = "A.c";

  def _d = "A.d";

  def _g = "A.g";

  def queryA = _a;
  def queryB = _b;
  def queryC = _c;
  def queryD = _d;
  def queryG = _g;
}

@virtualContext class OuterB19 extends OuterA19 {
  override def _b = "B.b";

  override def _d = "B.d";

  def _e = "B.e";

  def _f = "B.f";

  override def _g = "B.g";

  override def queryA = super.queryA + ", " + _a;
  override def queryB = super.queryB + ", " + _b;
  override def queryC = super.queryC + ", " + _c;
  override def queryD = super.queryD + ", " + _d;
  def queryE = _e;
  def queryF = _f;
  override def queryG = super.queryG + ", " + _g;

}

@virtualContext class OuterC19 extends OuterA19 {
  override def _c = "C.c";

  override def _d = "C.d";

  def _e = "C.e";

  def _f = "C.f";

  override def _g = "C.g";

  override def queryA = super.queryA + ", " + _a;
  override def queryB = super.queryB + ", " + _b;
  override def queryC = super.queryC + ", " + _c;
  override def queryD = super.queryD + ", " + _d;
  def queryE = _e;
  def queryF = _f;
  override def queryG = super.queryG + ", " + _g;
}

@virtualContext class OuterD19 extends OuterB19 with OuterC19 {
  override def _e = super[OuterC19]._e
  override def _f = "D.f";

  override def _g = "D.g";

  override def queryA = super.queryA + ", " + _a;
  override def queryB = super.queryB + ", " + _b;
  override def queryC = super.queryC + ", " + _c;
  override def queryD = super.queryD + ", " + _d;
  override def queryE = super.queryE + ", " + _e;
  override def queryF = super.queryF + ", " + _f;
  override def queryG = super.queryG + ", " + _g;
}