package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test23 extends UnitSpec {
  "inner classes" should "join correctly" in {
    def expectedResult =
      ("A.A, C.A, B.A; " + // A
        "A.A, C.A, B.A, A.B, C.B; " + // B < A
        "A.A, C.A, B.A, A.C, C.C, B.C; " + // C < A
        "A.A, C.A, B.A, A.C, C.C, B.C, A.B, C.B, A.D, C.D, B.D; " + // D < B & C
        "A.A, C.A, B.A, A.E; " + // E < A
        "A.A, C.A, B.A, A.E, A.C, C.C, B.C, A.B, C.B, A.D, C.D, B.D, A.F, B.F; " + // F < D & A
        "A.A, C.A, B.A, A.E, A.B, C.B, A.C, C.C, B.C, A.D, C.D, B.D, A.F, B.F, B.G; " + // G < C & F
        "A.A, C.A, B.A, A.C, C.C, B.C, A.B, C.B, C.H; " + // H < B & C
        "A.A, C.A, B.A, A.E, A.C, C.C, B.C, A.B, C.B, A.D, C.D, B.D, A.F, B.F, D.I"); // I < F & D 

    val od = OuterD23();

    val resA = od.InnerA().queryA;
    val resB = od.InnerB().queryA;
    val resC = od.InnerC().queryA;
    val resD = od.InnerD().queryA;
    val resE = od.InnerE().queryA;
    val resF = od.InnerF().queryA;
    val resG = od.InnerG().queryA;
    val resH = od.InnerH().queryA;
    val resI = od.InnerI().queryA;

    val result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG +
      "; " + resH + "; " + resI;

    System.out.println(result);
    
    resA should equal ("A.A, C.A, B.A")
    resB should equal ("A.A, C.A, B.A, A.B, C.B")
    resC should equal ("A.A, C.A, B.A, A.C, C.C, B.C")
    resD should equal ("A.A, C.A, B.A, A.C, C.C, B.C, A.B, C.B, A.D, C.D, B.D")
    resE should equal ("A.A, C.A, B.A, A.E")
    resF should equal ("A.A, C.A, B.A, A.E, A.C, C.C, B.C, A.B, C.B, A.D, C.D, B.D, A.F, B.F")
    resG should equal ("A.A, C.A, B.A, A.E, A.C, C.C, B.C, A.B, C.B, A.D, C.D, B.D, A.F, B.F, B.G")
    resH should equal ("A.A, C.A, B.A, A.C, C.C, B.C, A.B, C.B, C.H")
    resI should equal ("A.A, C.A, B.A, A.E, A.C, C.C, B.C, A.B, C.B, A.D, C.D, B.D, A.F, B.F, D.I")
    
    //result should equal (expectedResult)
  }
}

@virtualContext class OuterA23 {
  @virtual class InnerA {
    def queryA = "A.A"
  }

  @virtual class InnerB extends InnerA {
    override def queryA = super.queryA + ", A.B";
  }

  @virtual class InnerC extends InnerA {
    override def queryA = super.queryA + ", A.C";
  }

  @virtual class InnerD extends InnerB with InnerC {
    override def queryA = super.queryA + ", A.D";
  }

  @virtual class InnerE extends InnerA {
    override def queryA = super.queryA + ", A.E";
  }

  @virtual class InnerF extends InnerD with InnerE {
    override def queryA = super.queryA + ", A.F";
  }
}

@virtualContext class OuterB23 extends OuterA23 {
  @virtual override class InnerA {
    override def queryA = super.queryA + ", B.A";
  }

  @virtual override class InnerC {
    override def queryA = super.queryA + ", B.C";
  }

  @virtual override class InnerD {
    override def queryA = super.queryA + ", B.D";
  }

  @virtual override class InnerF {
    override def queryA = super.queryA + ", B.F";
  }

  @virtual class InnerG extends InnerC with InnerF {
    override def queryA = super.queryA + ", B.G";
  }
}

@virtualContext class OuterC23 extends OuterA23 {
  @virtual override class InnerA {
    override def queryA = super.queryA + ", C.A";
  }

  @virtual override class InnerB {
    override def queryA = super.queryA + ", C.B";
  }

  @virtual override class InnerC {
    override def queryA = super.queryA + ", C.C";
  }

  @virtual override class InnerD {
    override def queryA = super.queryA + ", C.D";
  }

  @virtual class InnerH extends InnerB with InnerC {
    override def queryA = super.queryA + ", C.H";
  }
}

@virtualContext class OuterD23 extends OuterB23 with OuterC23 {
  @virtual class InnerI extends InnerF with InnerD {
    override def queryA = super.queryA + ", D.I";
  }
}