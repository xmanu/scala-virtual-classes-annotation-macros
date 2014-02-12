package caesarj

import VirtualClasses._

class Test5 extends UnitSpec {
  "outer access" should "work" in {
    def expectedResult1 = "A.A";

    def expectedResult2 = "B.A, B.A, B.B";

    val oa = OuterA();
    val ob = OuterB();
    val a1 = oa.InnerA();
    val a2 = ob.InnerA();

    System.out.println(a1.accessOuterA());
    a1.accessOuterA() should equal(expectedResult1);

    System.out.println(a2.accessOuterB());
    a2.accessOuterB() should equal(expectedResult2);
  }
}

@virtualContext class OuterA {
  def queryA() = "A.A"

  @virtual class InnerA {
    def accessOuterA() = outer.queryA()
  }
}

@virtualContext class OuterB extends OuterA {
  override def queryA() = "B.A"

  def queryB() = "B.B"

  @virtualOverride class InnerA {
    def accessOuterB() = accessOuterA() + ", " + queryA() + ", " + queryB()
  }
}