package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test14b extends UnitSpec {
  /*"super calls using extends for furtherbindings" should "work" in {
    fail("fails because of volatile problem")
  }*/
}
 
/*@virtualContext class _synth {
	@virtual class Cat1 {}
	@virtual class Cat2 {}
	@virtual class Leaf1 {}
}

// [A, 0]
@virtualContext class OuterA14b extends _synth
{
	// [A.C1, 0.C1]
	@virtual override class Cat1
	{
		def queryA = "A.C1";
	}

//	 [A.L1, O.L1, A.C1, 0.C1]
	@virtual override class Leaf1 extends Cat1
	{
		override def queryA = super.queryA + ", A.L1";
	}
}

// [B, A, 0]
@virtualContext class OuterB14b extends _synth
{
	// [B.C2, O.C2]
	@virtual override class Cat2
	{
		def queryA = "B.C2";
	}

	// [B.L1, B.C2, O.C2]
	@virtual override class Leaf1 extends Cat2
	{
		override def queryA = super.queryA + ", B.L1";
	}
}

// [C, A, B, 0]
@virtualContext class OuterC14b extends OuterA14b with OuterB14b
{
	// [C.C2, O.C2, C.C1, A.C1, B.C1, O.C1]
	@virtual override class Cat2 extends Cat1 {
		override def queryA = super.queryA + ", C.C2";
	}

	// Leaf1 : [A.L1, B.L1, C.C2, O.C2, C.C1, A.C1, B.C1, O.C1]
}*/