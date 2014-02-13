package manual

import caesarj.UnitSpec

object Test15Manual extends UnitSpec {
  "long inheritance sequence" should "work in manual example" in {
    val expectedResultCD = "A.A.A, B.A.A, A.B.A, B.B.A, C.B.A, A.C.A, C.C.A, B.D.A";
    val expectedResultCF = "A.A.A, B.A.A, C.E.A, C.F.A";

    val oc = OuterC4();
    val cd = oc.InnerD();

    var result = cd.queryA;

    println(s"$result\n$expectedResultCD")

    assert(result == expectedResultCD)

    val cf = oc.InnerF();

    result = cf.queryA;

    println(s"$result\n$expectedResultCF")

    assert(result == expectedResultCF)
  }
}

abstract trait OuterA4 extends scala.AnyRef { outer =>

  object InnerA extends scala.AnyRef {

    def apply() = VC_NEW$InnerA()
  };
  def VC_NEW$InnerA(): InnerA;
  type InnerA >: _root_.scala.Null <: VC_TRAIT$OuterA4$InnerA;
  abstract trait VC_TRAIT$OuterA4$InnerA { self: InnerA =>

    def queryA = "A.A.A"
  };
  object InnerB extends scala.AnyRef {

    def apply() = VC_NEW$InnerB()
  };
  def VC_NEW$InnerB(): InnerB;
  type InnerB >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterA4$InnerB;
  abstract trait VC_TRAIT$OuterA4$InnerB { self: InnerB =>
    def VC_SUPER$OuterA4$InnerB$queryA: String

    override def queryA = VC_SUPER$OuterA4$InnerB$queryA.$plus(", A.B.A")
  };
  object InnerC extends scala.AnyRef {

    def apply() = VC_NEW$InnerC()
  };
  def VC_NEW$InnerC(): InnerC;
  type InnerC >: _root_.scala.Null <: InnerB with VC_TRAIT$OuterA4$InnerC;
  abstract trait VC_TRAIT$OuterA4$InnerC { self: InnerC =>
    def VC_SUPER$OuterA4$InnerC$queryA: String

    override def queryA = VC_SUPER$OuterA4$InnerC$queryA.$plus(", A.C.A")
  }
};
object OuterA4 extends scala.AnyRef {
  class VC_FINAL$OuterA4 extends OuterA4 {

    def VC_NEW$InnerA() = new VC_FIX$OuterA4$InnerA();
    type InnerA = VC_TRAIT$OuterA4$InnerA;
    class VC_FIX$OuterA4$InnerA extends VC_TRAIT$OuterA4$InnerA {
    };
    def VC_NEW$InnerB() = new VC_FIX$OuterA4$InnerB();
    type InnerB = InnerA with VC_TRAIT$OuterA4$InnerB;
    class VC_FIX$OuterA4$InnerB extends VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterA4$InnerA {
      override def queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
      def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerC() = new VC_FIX$OuterA4$InnerC();
    type InnerC = InnerB with VC_TRAIT$OuterA4$InnerC;
    class VC_FIX$OuterA4$InnerC extends VC_TRAIT$OuterA4$InnerC with VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterA4$InnerB {
      def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
      def VC_SUPER$OuterA4$InnerC$queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
    }
  };
  def apply() = new VC_FINAL$OuterA4()
};

abstract trait OuterB4 extends OuterA4 { outer =>

  type InnerA >: _root_.scala.Null <: VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerA;
  abstract trait VC_TRAIT$OuterB4$InnerA { self: InnerA =>
    def VC_SUPER$OuterB4$InnerA$queryA: String
    override def queryA = VC_SUPER$OuterB4$InnerA$queryA.$plus(", B.A.A")
  };
  type InnerB >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterB4$InnerB;
  abstract trait VC_TRAIT$OuterB4$InnerB { self: InnerB =>
    def VC_SUPER$OuterB4$InnerB$queryA: String
    override def queryA = VC_SUPER$OuterB4$InnerB$queryA.$plus(", B.B.A")
  };
  object InnerD extends scala.AnyRef {
    ;
    def apply() = VC_NEW$InnerD()
  };
  def VC_NEW$InnerD(): InnerD;
  type InnerD >: _root_.scala.Null <: AnyRef with VC_TRAIT$OuterB4$InnerD with InnerC;
  abstract trait VC_TRAIT$OuterB4$InnerD { self: InnerD =>
    def VC_SUPER$OuterB4$InnerD$queryA: String
    override def queryA = VC_SUPER$OuterB4$InnerD$queryA.$plus(", B.D.A")
  };
  type InnerC >: _root_.scala.Null <: InnerB with VC_TRAIT$OuterA4$InnerC
};
object OuterB4 extends scala.AnyRef {
  ;
  class VC_FINAL$OuterB4 extends OuterB4 {
    ;
    def VC_NEW$InnerA() = new VC_FIX$OuterB4$InnerA();
    type InnerA = VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerA;
    class VC_FIX$OuterB4$InnerA extends VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA {
      override def queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerB() = new VC_FIX$OuterB4$InnerB();
    type InnerB = InnerA with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterB4$InnerB;
    class VC_FIX$OuterB4$InnerB extends VC_TRAIT$OuterB4$InnerB with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA {
      override def queryA = super[VC_TRAIT$OuterB4$InnerB].queryA
      def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
      def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
      def VC_SUPER$OuterB4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
    };
    def VC_NEW$InnerD() = new VC_FIX$OuterB4$InnerD();
    type InnerD = InnerC with VC_TRAIT$OuterB4$InnerD;
    class VC_FIX$OuterB4$InnerD extends VC_TRAIT$OuterB4$InnerD with VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerB with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterA4$InnerC {
      override def VC_SUPER$OuterB4$InnerD$queryA = super[VC_TRAIT$OuterA4$InnerC].queryA
      override def VC_SUPER$OuterA4$InnerC$queryA = super[VC_TRAIT$OuterB4$InnerB].queryA
      override def VC_SUPER$OuterB4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
      override def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      override def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerC() = new VC_FIX$OuterB4$InnerC();
    type InnerC = InnerB with VC_TRAIT$OuterA4$InnerC;
    class VC_FIX$OuterB4$InnerC extends VC_TRAIT$OuterA4$InnerC with VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerB with VC_TRAIT$OuterA4$InnerB {
      def VC_SUPER$OuterA4$InnerC$queryA = super[VC_TRAIT$OuterB4$InnerB].queryA
      def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
      def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      def VC_SUPER$OuterB4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
    }
  };
  def apply() = new VC_FINAL$OuterB4()
};

abstract trait OuterC4 extends OuterB4 { outer =>

  type InnerB >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterB4$InnerB with VC_TRAIT$OuterC4$InnerB;
  abstract trait VC_TRAIT$OuterC4$InnerB extends { self: InnerB =>
    def VC_SUPER$OuterC4$InnerB$queryA: String
    override def queryA = VC_SUPER$OuterC4$InnerB$queryA.$plus(", C.B.A")
  };
  type InnerC >: _root_.scala.Null <: InnerB with VC_TRAIT$OuterA4$InnerC with VC_TRAIT$OuterC4$InnerC;
  abstract trait VC_TRAIT$OuterC4$InnerC { self: InnerC =>
    def VC_SUPER$OuterC4$InnerC$queryA: String
    override def queryA = VC_SUPER$OuterC4$InnerC$queryA.$plus(", C.C.A")
  };
  object InnerE extends scala.AnyRef {
    def apply() = VC_NEW$InnerE()
  };
  def VC_NEW$InnerE(): InnerE;
  type InnerE >: _root_.scala.Null <: InnerA with VC_TRAIT$OuterC4$InnerE;
  abstract trait VC_TRAIT$OuterC4$InnerE { self: InnerE =>
    def VC_SUPER$OuterC4$InnerE$queryA: String
    override def queryA = VC_SUPER$OuterC4$InnerE$queryA.$plus(", C.E.A")
  };
  object InnerF extends scala.AnyRef {
    ;
    def apply() = VC_NEW$InnerF()
  };
  def VC_NEW$InnerF(): InnerF;
  type InnerF >: _root_.scala.Null <: InnerE with VC_TRAIT$OuterC4$InnerF;
  abstract trait VC_TRAIT$OuterC4$InnerF { self: InnerF =>
    def VC_SUPER$OuterC4$InnerF$queryA: String
    override def queryA = VC_SUPER$OuterC4$InnerF$queryA.$plus(", C.F.A")
  };
  type InnerD >: _root_.scala.Null <: AnyRef with InnerC with VC_TRAIT$OuterB4$InnerD;
  type InnerA >: _root_.scala.Null <: VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerA
};
object OuterC4 extends scala.AnyRef {
  ;
  class VC_FINAL$OuterC4 extends OuterC4 {
    ;
    def VC_NEW$InnerB() = new VC_FIX$OuterC4$InnerB();
    type InnerB = InnerA with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterB4$InnerB with VC_TRAIT$OuterC4$InnerB;
    class VC_FIX$OuterC4$InnerB extends VC_TRAIT$OuterC4$InnerB with VC_TRAIT$OuterB4$InnerB with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerA {
      override def VC_SUPER$OuterC4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerB].queryA
      override def VC_SUPER$OuterB4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
      override def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      override def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerC() = new VC_FIX$OuterC4$InnerC();
    type InnerC = InnerB with VC_TRAIT$OuterA4$InnerC with VC_TRAIT$OuterC4$InnerC;
    class VC_FIX$OuterC4$InnerC extends VC_TRAIT$OuterC4$InnerC with VC_TRAIT$OuterA4$InnerC with VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterC4$InnerB with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterB4$InnerB {
      override def VC_SUPER$OuterC4$InnerC$queryA = super[VC_TRAIT$OuterA4$InnerC].queryA
      override def VC_SUPER$OuterA4$InnerC$queryA = super[VC_TRAIT$OuterC4$InnerB].queryA
      override def VC_SUPER$OuterC4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerB].queryA
      override def VC_SUPER$OuterB4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
      override def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      override def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerE() = new VC_FIX$OuterC4$InnerE();
    type InnerE = InnerA with VC_TRAIT$OuterC4$InnerE;
    class VC_FIX$OuterC4$InnerE extends VC_TRAIT$OuterC4$InnerE with VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerA {
      override def VC_SUPER$OuterC4$InnerE$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      override def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerF() = new VC_FIX$OuterC4$InnerF();
    type InnerF = InnerE with VC_TRAIT$OuterC4$InnerF;
    class VC_FIX$OuterC4$InnerF extends VC_TRAIT$OuterC4$InnerF with VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterC4$InnerE {
      override def queryA = super[VC_TRAIT$OuterC4$InnerF].queryA
      override def VC_SUPER$OuterC4$InnerF$queryA = super[VC_TRAIT$OuterC4$InnerE].queryA
      override def VC_SUPER$OuterC4$InnerE$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      override def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerD() = new VC_FIX$OuterC4$InnerD();
    type InnerD = InnerC with VC_TRAIT$OuterB4$InnerD;
    class VC_FIX$OuterC4$InnerD extends AnyRef with VC_TRAIT$OuterB4$InnerD with VC_TRAIT$OuterC4$InnerC with VC_TRAIT$OuterA4$InnerC with VC_TRAIT$OuterC4$InnerB with VC_TRAIT$OuterB4$InnerB with VC_TRAIT$OuterA4$InnerB with VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA {
      override def queryA = super[VC_TRAIT$OuterB4$InnerD].queryA
      override def VC_SUPER$OuterB4$InnerD$queryA = super[VC_TRAIT$OuterC4$InnerC].queryA
      override def VC_SUPER$OuterC4$InnerC$queryA = super[VC_TRAIT$OuterA4$InnerC].queryA
      override def VC_SUPER$OuterA4$InnerC$queryA = super[VC_TRAIT$OuterC4$InnerB].queryA
      override def VC_SUPER$OuterC4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerB].queryA
      override def VC_SUPER$OuterB4$InnerB$queryA = super[VC_TRAIT$OuterA4$InnerB].queryA
      override def VC_SUPER$OuterA4$InnerB$queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      override def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    };
    def VC_NEW$InnerA() = new VC_FIX$OuterC4$InnerA();
    type InnerA = VC_TRAIT$OuterA4$InnerA with VC_TRAIT$OuterB4$InnerA;
    class VC_FIX$OuterC4$InnerA extends VC_TRAIT$OuterB4$InnerA with VC_TRAIT$OuterA4$InnerA {
      override def queryA = super[VC_TRAIT$OuterB4$InnerA].queryA
      override def VC_SUPER$OuterB4$InnerA$queryA = super[VC_TRAIT$OuterA4$InnerA].queryA
    }
  };
  def apply() = new VC_FINAL$OuterC4()
};
