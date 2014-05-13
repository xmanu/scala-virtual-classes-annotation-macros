package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test18 extends UnitSpec {
  "multiple inheritance of methods" should "work" in {
    def expectedResult = "A.a; A.b, B.b; A.c, C.c; A.d, C.d, B.d; B.e; B.f, D.f; A.g, C.g, B.g, D.g"
    val d: OuterD18 = OuterD18();

    def resA = d.queryA;
    def resB = d.queryB;
    def resC = d.queryC;
    def resD = d.queryD;
    def resE = d.queryE;
    def resF = d.queryF;
    def resG = d.queryG;
    def result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG;

    result should equal (expectedResult);
  }
}

@family class OuterA18 {
  private[this] val _a = "A.a";

  private[this] val _b = "A.b";

  private[this] val _c = "A.c";

  private[this] val _d = "A.d";

  private[this] val _g = "A.g";

  def queryA = _a;

  def queryB = _b;

  def queryC = _c;

  def queryD = _d;

  def queryG = _g;
}

@family class OuterB18 extends OuterA18 {
  private[this] val _b = "B.b";

  private[this] val _d = "B.d";

  private[this] val _e = "B.e";

  private[this] val _f = "B.f";

  private[this] val _g = "B.g";

  override def queryB = super.queryB + ", " + _b;

  override def queryD = super.queryD + ", " + _d;

  def queryE = _e;

  def queryF = _f;

  override def queryG = super.queryG + ", " + _g;
}

@family class OuterC18 extends OuterA18 {
  private[this] val _c = "C.c";

  private[this] val _d = "C.d";

  private[this] val _e = "C.e";

  private[this] val _f = "C.f";

  private[this] val _g = "C.g";

  override def queryC = super.queryC + ", " + _c;

  override def queryD = super.queryD + ", " + _d;

  def queryE = _e;

  def queryF = _f;

  override def queryG = super.queryG + ", " + _g;
}

@family class OuterD18 extends OuterC18 with OuterB18 {
  private[this] val _f = "D.f";

  private[this] val _g = "D.g";

  //not in the original tests, but by design wanted here...
  override def queryE = super[OuterB18].queryE

  override def queryF = super.queryF + ", " + _f;

  override def queryG = super.queryG + ", " + _g;
}