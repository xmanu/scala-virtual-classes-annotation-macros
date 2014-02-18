package caesarj

import VirtualClasses._

class Test15 extends UnitSpec {
  "long inheritance sequence" should "work" in {
    val expectedResultCD = "A.A.A, B.A.A, A.B.A, B.B.A, C.B.A, A.C.A, C.C.A, B.D.A";
    val expectedResultCF = "A.A.A, B.A.A, C.E.A, C.F.A";

    val oc = OuterC4();
    val cd = oc.InnerD();

    var result = cd.queryA;

    println(s"Test15: $result");
    result should equal(expectedResultCD);

    val cf = oc.InnerF();

    result = cf.queryA;

    println(result);
    result should equal(expectedResultCF);
  }
}

@virtualContext class OuterA4 {
  @virtual class InnerA {
    def queryA = "A.A.A";
  }

  @virtual class InnerB extends InnerA {
    override def queryA = super.queryA + ", A.B.A";
  }

  @virtual class InnerC extends InnerB {
    override def queryA = super.queryA + ", A.C.A";
  }

}

@virtualContext class OuterB4 extends OuterA4 {
  @virtual override class InnerA {
    override def queryA = super.queryA + ", B.A.A";
  }

  @virtual override class InnerB {
    override def queryA = super.queryA + ", B.B.A";
  }

  @virtual class InnerD extends InnerC {
    override def queryA = super.queryA + ", B.D.A";
  }
}

@virtualContext class OuterC4 extends OuterB4 {
  @virtual override class InnerB {
    override def queryA = super.queryA + ", C.B.A";
  }

  @virtual override class InnerC {
    override def queryA = super.queryA + ", C.C.A";
  }

  @virtual class InnerE extends InnerA {
    override def queryA = super.queryA + ", C.E.A";
  }

  @virtual class InnerF extends InnerE {
    override def queryA = super.queryA + ", C.F.A";
  }
}