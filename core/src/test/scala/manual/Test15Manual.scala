package manual

import caesarj.UnitSpec

object Test15Manual extends UnitSpec {
  "long inheritance sequence" should "work in manual example" in {
    val expectedResultCD = "A.A.A, B.A.A, A.B.A, B.B.A, C.B.A, A.C.A, C.C.A, B.D.A";
    val expectedResultCF = "A.A.A, B.A.A, C.E.A, C.F.A";

    val oc = OuterC15();
    val cd = oc.InnerD();

    var result = cd.queryA;

    println(s"$result\n$expectedResultCD")

    result should equal (expectedResultCD)

    val cf = oc.InnerF();

    result = cf.queryA;

    println(s"$result\n$expectedResultCF")

    result should equal (expectedResultCF)
  }
}

abstract trait OuterA15 extends scala.AnyRef { outer =>

  object InnerA extends scala.AnyRef {

    def apply() = VC_NEW$InnerA()
  };
  def VC_NEW$InnerA(): InnerA;
  type InnerA >: _root_.scala.Null <: VC_TRAIT$OuterA15$InnerA;
  abstract trait VC_TRAIT$OuterA15$InnerA { self: InnerA =>

    def queryA = "A.A.A"
  };
  object InnerB extends scala.AnyRef {

    def apply() = VC_NEW$InnerB()
  };
  def VC_NEW$InnerB(): InnerB;
  type InnerB >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterA15$InnerB;
  abstract trait VC_TRAIT$OuterA15$InnerB { self: InnerB =>
    def VC_SUPER$OuterA15$InnerB$queryA: String

    override def queryA = VC_SUPER$OuterA15$InnerB$queryA.$plus(", A.B.A")
  };
  object InnerC extends scala.AnyRef {

    def apply() = VC_NEW$InnerC()
  };
  def VC_NEW$InnerC(): InnerC;
  type InnerC >: _root_.scala.Null <: InnerB with VC_TRAIT$OuterA15$InnerC;
  abstract trait VC_TRAIT$OuterA15$InnerC { self: InnerC =>
    def VC_SUPER$OuterA15$InnerC$queryA: String

    override def queryA = VC_SUPER$OuterA15$InnerC$queryA.$plus(", A.C.A")
  }
};
object OuterA15 extends scala.AnyRef {
  class VC_FINAL$OuterA15 extends OuterA15 {

    def VC_NEW$InnerA() = new VC_FIX$OuterA15$InnerA();
    type InnerA = VC_TRAIT$OuterA15$InnerA;
    class VC_FIX$OuterA15$InnerA extends VC_TRAIT$OuterA15$InnerA {
    };
    def VC_NEW$InnerB() = new VC_FIX$OuterA15$InnerB();
    type InnerB = InnerA with VC_TRAIT$OuterA15$InnerB;
    class VC_FIX$OuterA15$InnerB extends VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterA15$InnerA {
      override def queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
      def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerC() = new VC_FIX$OuterA15$InnerC();
    type InnerC = InnerB with VC_TRAIT$OuterA15$InnerC;
    class VC_FIX$OuterA15$InnerC extends VC_TRAIT$OuterA15$InnerC with VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterA15$InnerB {
      def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
      def VC_SUPER$OuterA15$InnerC$queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
    }
  };
  def apply() = new VC_FINAL$OuterA15()
};

abstract trait OuterB15 extends OuterA15 { outer =>

  type InnerA >: _root_.scala.Null <: VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerA;
  abstract trait VC_TRAIT$OuterB15$InnerA { self: InnerA =>
    def VC_SUPER$OuterB15$InnerA$queryA: String
    override def queryA = VC_SUPER$OuterB15$InnerA$queryA.$plus(", B.A.A")
  };
  type InnerB >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterB15$InnerB;
  abstract trait VC_TRAIT$OuterB15$InnerB { self: InnerB =>
    def VC_SUPER$OuterB15$InnerB$queryA: String
    override def queryA = VC_SUPER$OuterB15$InnerB$queryA.$plus(", B.B.A")
  };
  object InnerD extends scala.AnyRef {
    ;
    def apply() = VC_NEW$InnerD()
  };
  def VC_NEW$InnerD(): InnerD;
  type InnerD >: _root_.scala.Null <: AnyRef with VC_TRAIT$OuterB15$InnerD with InnerC;
  abstract trait VC_TRAIT$OuterB15$InnerD { self: InnerD =>
    def VC_SUPER$OuterB15$InnerD$queryA: String
    override def queryA = VC_SUPER$OuterB15$InnerD$queryA.$plus(", B.D.A")
  };
  type InnerC >: _root_.scala.Null <: InnerB with VC_TRAIT$OuterA15$InnerC
};
object OuterB15 extends scala.AnyRef {
  ;
  class VC_FINAL$OuterB15 extends OuterB15 {
    ;
    def VC_NEW$InnerA() = new VC_FIX$OuterB15$InnerA();
    type InnerA = VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerA;
    class VC_FIX$OuterB15$InnerA extends VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA {
      override def queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerB() = new VC_FIX$OuterB15$InnerB();
    type InnerB = InnerA with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterB15$InnerB;
    class VC_FIX$OuterB15$InnerB extends VC_TRAIT$OuterB15$InnerB with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA {
      override def queryA = super[VC_TRAIT$OuterB15$InnerB].queryA
      def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
      def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
      def VC_SUPER$OuterB15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
    };
    def VC_NEW$InnerD() = new VC_FIX$OuterB15$InnerD();
    type InnerD = InnerC with VC_TRAIT$OuterB15$InnerD;
    class VC_FIX$OuterB15$InnerD extends VC_TRAIT$OuterB15$InnerD with VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerB with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterA15$InnerC {
      override def VC_SUPER$OuterB15$InnerD$queryA = super[VC_TRAIT$OuterA15$InnerC].queryA
      override def VC_SUPER$OuterA15$InnerC$queryA = super[VC_TRAIT$OuterB15$InnerB].queryA
      override def VC_SUPER$OuterB15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
      override def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      override def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerC() = new VC_FIX$OuterB15$InnerC();
    type InnerC = InnerB with VC_TRAIT$OuterA15$InnerC;
    class VC_FIX$OuterB15$InnerC extends VC_TRAIT$OuterA15$InnerC with VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerB with VC_TRAIT$OuterA15$InnerB {
      def VC_SUPER$OuterA15$InnerC$queryA = super[VC_TRAIT$OuterB15$InnerB].queryA
      def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
      def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      def VC_SUPER$OuterB15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
    }
  };
  def apply() = new VC_FINAL$OuterB15()
};

abstract trait OuterC15 extends OuterB15 { outer =>

  type InnerB >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterB15$InnerB with VC_TRAIT$OuterC15$InnerB;
  abstract trait VC_TRAIT$OuterC15$InnerB extends { self: InnerB =>
    def VC_SUPER$OuterC15$InnerB$queryA: String
    override def queryA = VC_SUPER$OuterC15$InnerB$queryA.$plus(", C.B.A")
  };
  type InnerC >: _root_.scala.Null <: InnerB with VC_TRAIT$OuterA15$InnerC with VC_TRAIT$OuterC15$InnerC;
  abstract trait VC_TRAIT$OuterC15$InnerC { self: InnerC =>
    def VC_SUPER$OuterC15$InnerC$queryA: String
    override def queryA = VC_SUPER$OuterC15$InnerC$queryA.$plus(", C.C.A")
  };
  object InnerE extends scala.AnyRef {
    def apply() = VC_NEW$InnerE()
  };
  def VC_NEW$InnerE(): InnerE;
  type InnerE >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterC15$InnerE;
  abstract trait VC_TRAIT$OuterC15$InnerE { self: InnerE =>
    def VC_SUPER$OuterC15$InnerE$queryA: String
    override def queryA = VC_SUPER$OuterC15$InnerE$queryA.$plus(", C.E.A")
  };
  object InnerF extends scala.AnyRef {
    ;
    def apply() = VC_NEW$InnerF()
  };
  def VC_NEW$InnerF(): InnerF;
  type InnerF >: _root_.scala.Null <: InnerE with VC_TRAIT$OuterC15$InnerF;
  abstract trait VC_TRAIT$OuterC15$InnerF { self: InnerF =>
    def VC_SUPER$OuterC15$InnerF$queryA: String
    override def queryA = VC_SUPER$OuterC15$InnerF$queryA.$plus(", C.F.A")
  };
  type InnerD >: _root_.scala.Null <: AnyRef with InnerC with VC_TRAIT$OuterB15$InnerD;
  type InnerA >: _root_.scala.Null <: VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerA
};
object OuterC15 extends scala.AnyRef {
  ;
  class VC_FINAL$OuterC15 extends OuterC15 {
    ;
    def VC_NEW$InnerB() = new VC_FIX$OuterC15$InnerB();
    type InnerB = InnerA with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterB15$InnerB with VC_TRAIT$OuterC15$InnerB;
    class VC_FIX$OuterC15$InnerB extends VC_TRAIT$OuterC15$InnerB with VC_TRAIT$OuterB15$InnerB with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerA {
      override def VC_SUPER$OuterC15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerB].queryA
      override def VC_SUPER$OuterB15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
      override def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      override def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerC() = new VC_FIX$OuterC15$InnerC();
    type InnerC = InnerB with VC_TRAIT$OuterA15$InnerC with VC_TRAIT$OuterC15$InnerC;
    class VC_FIX$OuterC15$InnerC extends VC_TRAIT$OuterC15$InnerC with VC_TRAIT$OuterA15$InnerC with VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterC15$InnerB with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterB15$InnerB {
      override def VC_SUPER$OuterC15$InnerC$queryA = super[VC_TRAIT$OuterA15$InnerC].queryA
      override def VC_SUPER$OuterA15$InnerC$queryA = super[VC_TRAIT$OuterC15$InnerB].queryA
      override def VC_SUPER$OuterC15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerB].queryA
      override def VC_SUPER$OuterB15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
      override def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      override def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerE() = new VC_FIX$OuterC15$InnerE();
    type InnerE = InnerA with VC_TRAIT$OuterC15$InnerE;
    class VC_FIX$OuterC15$InnerE extends VC_TRAIT$OuterC15$InnerE with VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerA {
      override def VC_SUPER$OuterC15$InnerE$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      override def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerF() = new VC_FIX$OuterC15$InnerF();
    type InnerF = InnerE with VC_TRAIT$OuterC15$InnerF;
    class VC_FIX$OuterC15$InnerF extends VC_TRAIT$OuterC15$InnerF with VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterC15$InnerE {
      override def queryA = super[VC_TRAIT$OuterC15$InnerF].queryA
      override def VC_SUPER$OuterC15$InnerF$queryA = super[VC_TRAIT$OuterC15$InnerE].queryA
      override def VC_SUPER$OuterC15$InnerE$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      override def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerD() = new VC_FIX$OuterC15$InnerD();
    type InnerD = InnerC with VC_TRAIT$OuterB15$InnerD;
    class VC_FIX$OuterC15$InnerD extends AnyRef with VC_TRAIT$OuterB15$InnerD with VC_TRAIT$OuterC15$InnerC with VC_TRAIT$OuterA15$InnerC with VC_TRAIT$OuterC15$InnerB with VC_TRAIT$OuterB15$InnerB with VC_TRAIT$OuterA15$InnerB with VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA {
      override def queryA = super[VC_TRAIT$OuterB15$InnerD].queryA
      override def VC_SUPER$OuterB15$InnerD$queryA = super[VC_TRAIT$OuterC15$InnerC].queryA
      override def VC_SUPER$OuterC15$InnerC$queryA = super[VC_TRAIT$OuterA15$InnerC].queryA
      override def VC_SUPER$OuterA15$InnerC$queryA = super[VC_TRAIT$OuterC15$InnerB].queryA
      override def VC_SUPER$OuterC15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerB].queryA
      override def VC_SUPER$OuterB15$InnerB$queryA = super[VC_TRAIT$OuterA15$InnerB].queryA
      override def VC_SUPER$OuterA15$InnerB$queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      override def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    };
    def VC_NEW$InnerA() = new VC_FIX$OuterC15$InnerA();
    type InnerA = VC_TRAIT$OuterA15$InnerA with VC_TRAIT$OuterB15$InnerA;
    class VC_FIX$OuterC15$InnerA extends VC_TRAIT$OuterB15$InnerA with VC_TRAIT$OuterA15$InnerA {
      override def queryA = super[VC_TRAIT$OuterB15$InnerA].queryA
      override def VC_SUPER$OuterB15$InnerA$queryA = super[VC_TRAIT$OuterA15$InnerA].queryA
    }
  };
  def apply() = new VC_FINAL$OuterC15()
};
