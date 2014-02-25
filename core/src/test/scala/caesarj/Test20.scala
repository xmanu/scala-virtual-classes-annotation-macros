package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test20 extends UnitSpec {
  "linearization of state" should "work" in {
    val expectedResult = ("A.a; " + // A
      "A.a, B.a; " + // B < A
      "A.a, C.a; " + // C < A
      "A.a, B.a, D.a; " + // D < B
      "A.a, C.a, B.a; " + // E < B & C
      "A.a, C.a, F.a; " + // F < A & C
      "A.a, B.a, D.a, C.a; " + // G < E & D
      "A.a, C.a, F.a, B.a, H.a; " + // H < E & F
      "A.a, C.a, F.a, B.a, H.a, D.a"); // I < G & H

    val resA = OuterA20().InnerA().queryA;
    val resB = OuterB20().InnerA().queryA;
    val resC = OuterC20().InnerA().queryA;
    val resD = OuterD20().InnerA().queryA;
    val resE = OuterE20().InnerA().queryA;
    val resF = OuterF20().InnerA().queryA;
    val resG = OuterG20().InnerA().queryA;
    val resH = OuterH20().InnerA().queryA;
    val resI = OuterI20().InnerA().queryA;
    val result = resA + "; " + resB + "; " + resC + "; " + resD + "; " + resE + "; " + resF + "; " + resG +"; " + resH + "; " + resI;

    System.out.println(result);
    result should equal(expectedResult);
  }
}

@virtualContext class OuterA20 {
  @virtual class InnerA {
    private val _a = "A.a";

    def queryA = _a;
  }
}

@virtualContext class OuterB20 extends OuterA20 {
  @virtual override class InnerA {
    private val _a = "B.a";

    override def queryA = super.queryA + ", " + _a
  }
}

@virtualContext class OuterC20 extends OuterA20 {
  @virtual override class InnerA {
    private val _a = "C.a";

    override def queryA = super.queryA + ", " + _a
  }
}

@virtualContext class OuterD20 extends OuterB20 {
  @virtual override class InnerA {
    private val _a = "D.a";

    override def queryA = super.queryA + ", " + _a
  }
}

@virtualContext class OuterE20 extends OuterB20 with OuterC20 {

}

@virtualContext class OuterF20 extends OuterA20 with OuterC20 {
  @virtual override class InnerA {
    private val _a = "F.a";

    override def queryA = super.queryA + ", " + _a
  }
}

@virtualContext class OuterG20 extends OuterE20 with OuterD20 {

}

@virtualContext class OuterH20 extends OuterE20 with OuterF20 {
  @virtual override class InnerA {
    private val _a = "H.a";

    override def queryA = super.queryA + ", " + _a
  }
}

@virtualContext class OuterI20 extends OuterG20 with OuterH20 {

}