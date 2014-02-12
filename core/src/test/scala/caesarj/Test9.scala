package caesarj

import VirtualClasses._

class Test9 extends UnitSpec {
  "other traits" should "be mixable" in {
    def expectedResult = "B.A.A, A.B.B, A.A.C, B.A.D, B.A.E, B.A.F";
    val ob: OuterB9 = OuterB9();
    val oa: OuterA9 = ob;
    val ba: oa.InnerA = ob.InnerA().asInstanceOf[oa.InnerA];

    val resA = ba.asInstanceOf[InterfaceA].queryA;
    val resB = ba.asInstanceOf[InterfaceB].queryB;
    val resC = ba.queryC;
    val resD = ba.queryD;
    val resE = ba.asInstanceOf[InterfaceE].queryE;
    val resF = ba.asInstanceOf[oa.InnerB].queryF;
    val result = resA + ", " + resB + ", " + resC + ", " + resD + ", " + resE + ", " + resF;

    println(result);
    result should equal(expectedResult)
  }
}

trait InterfaceA {
  def queryA: String;
}

trait InterfaceB {
  def queryB: String;
}

trait InterfaceE {
  def queryE: String;
}

@virtualContext class OuterA9 {
  @virtual abstract class InnerB extends InterfaceE {
    def queryB = "A.B.B";

    def queryE = "A.B.E";

    def queryF = "A.B.F";
  }

  @virtual class InnerA extends InnerB with InterfaceA {
    def queryA = "A.A.A";

    def queryC = "A.A.C";

    def queryD = "A.A.D";
  }
}

@virtualContext class OuterB9 extends OuterA9 {
  @virtualOverride class InnerA extends InterfaceB {
    override def queryA = "B.A.A";

    override def queryD = "B.A.D";

    override def queryE = "B.A.E";

    override def queryF = "B.A.F";
  }
}