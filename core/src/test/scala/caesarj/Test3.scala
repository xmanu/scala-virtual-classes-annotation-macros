package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test3 extends UnitSpec {
  val g = CWG();
  val n1 = g.N("n1");
  val n2 = g.N("n2");
  val e = g.UE();
  e.setEdges(n1, n2);

  g.doSomethingWithEdge(e);

  System.out.println("e: " + e);

  "Graph example with mixins" should "print right" in {
    e.toString() should equal("col:#a1babe, n1->n2, n2->n1")
  }
  it should "connect right" in {
    System.out.println("connecting: " + e.isConnecting(n1, n2))
    e.isConnecting(n1, n2) should equal(true)
  }
}

@virtualContext class G {
  def doSomethingWithEdge(e: E) {
    println("G.doSomethingWithEdge");
  }

  @virtual class E {
    var n1: N = null
    var n2: N = null

    def setEdges(n1: N, n2: N) {
      this.n1 = n1;
      this.n2 = n2;
    }

    def isConnecting(n1: N, n2: N) = {
      this.n1 == n1 && this.n2 == n2;
    }

    override def toString() = n1 + "->" + n2;
  }

  @virtual class UE extends E {
    override def isConnecting(n1: N, n2: N) = {
      super.isConnecting(n1, n2) || super.isConnecting(n2, n1);
    }

    override def toString() = super.toString() + ", " + n2 + "->" + n1
  }

  @virtual class N(val name: String) {
    override def toString() = name
  }
}

//=========================================================
@virtualContext class CG extends G {
  // test: signature should be same as in G
  override def doSomethingWithEdge(e: E) {
    super.doSomethingWithEdge(e);
    e.col = "#a1babe";
    println("CG.doSomethingWithEdge");
  }

  @virtual override class E {
    var col: String = "";

    def someSpecialAlg() {
      val n: N = n1;
    }

    override def toString() = {
      "col:" + col + ", " + super.toString();
    }
  }
}

//=========================================================
@virtualContext class WG extends G {
  @virtual override class E {
    var w: Float = 0;
  }
}

//=========================================================
@virtualContext class CWG extends CG with WG {
  @virtual override class E {
    def nowWeHaveItAll() {
      val w = this.w;
      val col = this.col;

      // note that the type of n1,n2 has been bound to the most specific node,
      // namely CWG.N
      val n1: N = this.n1;
      val n2: N = this.n2;
    }
  }
}