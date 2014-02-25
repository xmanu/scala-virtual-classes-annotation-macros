package caesarj;

import VirtualClasses._
import specs.UnitSpec

class Test7 extends UnitSpec {

  "polymorphism of outer classes" should "work" in {
    def expectedResult = "B.A, A.B, B.C, A.D";

    val ob: OuterA7 = OuterB7();

    def resA: String = ob.asInstanceOf[InterfaceA7].queryA;
    def resB: String = ob.asInstanceOf[InterfaceB7].queryB;
    def resC: String = ob.queryC;
    def resD: String = ob.queryD;
    def result = resA + ", " + resB + ", " + resC + ", " + resD;

    result should equal (expectedResult);
  }
}

trait InterfaceA7 {
  def queryA: String;
}

trait InterfaceB7 {
  def queryB: String;
}

@virtualContext class OuterA7 extends InterfaceA7 {
  def queryA: String = "A.A";

  def queryB: String = "A.B";

  def queryC: String = "A.C";

  def queryD: String = "A.D";

  // not used
  @virtual class InnerA {
    def queryA: String = "A.A.A";
  }
}

@virtualContext class OuterB7 extends OuterA7 with InterfaceB7 {
  override def queryA: String = "B.A";

  override def queryC: String = "B.C";

  // not used
  @virtual override class InnerA {
    override def queryA: String = "B.A.A";
  }
}