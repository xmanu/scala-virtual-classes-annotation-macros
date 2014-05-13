package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test8 extends UnitSpec {
  "type inheritance of inner classes" should "work" in {
    def expectedResult = "A.A.A, B.A.B, A.A.C, B.A.D, A.B.E, A.B.F, A.B.F"
    val ob: OuterB8 = OuterB8();
    val oa: OuterA8 = ob;
    val ba: ob.InnerA = ob.InnerA();

    val resA = ba.asInstanceOf[InterfaceA8].queryA;
    val resB = ba.asInstanceOf[InterfaceB8].queryB;
    val resC = ba.asInstanceOf[oa.InnerA].queryC;
    val resD = ba.queryD;
    val resE = ba.asInstanceOf[InterfaceE8].queryE;
    val resF = ba.asInstanceOf[oa.InnerB].queryF;
    val resF1 = ba.asInstanceOf[ob.InnerB].queryF;
    val result = resA + ", " + resB + ", " + resC + ", " + resD + ", " +
      resE + ", " + resF + ", " + resF1;

    result should equal(expectedResult);
  }
}

trait InterfaceA8 {
  def queryA: String;
}

trait InterfaceB8 {
  def queryB: String;
}

trait InterfaceE8 {
  def queryE: String;
}

@family class OuterA8 {
  @virtual class InnerB extends InterfaceE8 {
    def queryE = "A.B.E"

    def queryF = "A.B.F"
  }

  @virtual class InnerA extends InnerB with InterfaceA8 {
    def queryA = "A.A.A";

    def queryC = "A.A.C";
  }
}

@family class OuterB8 extends OuterA8 {
  @virtual override class InnerA extends InterfaceB8 {
    def queryB = "B.A.B";

    def queryD = "B.A.D";
  }
}