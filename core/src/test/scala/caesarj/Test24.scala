package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test24 extends UnitSpec {
  "lists" should "work" in {
    val expectedResult = "B.a0, A.a0, B.a1, A.a1, B.a2, A.a2, B.a3, A.a3";

    val ob = OuterB24();

    val result = ob.createDefaultA.queryA;

    System.out.println(result);
    result should equal (expectedResult)
  }
}

@virtualContext class OuterA24 {
  @virtual class InnerA(val _name: String) {
    var arr: List[InnerA] = null;
    var name: String = _name;

    def queryA: String =
      {
        var res = "A." + name;
        if (arr != null) {
          res += arr.foldLeft("")((a, b) => a + ", " + b.queryA)
        }
        res
      }
  }

  def createDefaultA: InnerA =
    {
      val a: InnerA = this.InnerA("a0");
      a.arr = List(this.InnerA("a1"), this.InnerA("a2"))
      a
    }
}

@virtualContext class OuterB24 extends OuterA24 {
  @virtual override class InnerA {
    override def queryA = "B." + name + ", " + super.queryA;
  }

  override def createDefaultA =
    {
      val a: InnerA = super.createDefaultA;
      val arr = a.arr

      a.arr = arr ++ List(InnerA("a3"));

      a
    }
}