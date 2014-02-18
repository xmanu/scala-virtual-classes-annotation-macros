package caesarj

import VirtualClasses._
import java.awt.Color

class Test2 extends UnitSpec {
  "UE e.isConnecting(n1, n2)" should "be false" in {
    val g = G1();
    val e = g.UE("n1", "n2");

    assert(e.isConnecting("n1", "n2") == false);
  }
}

@virtualContext class G1 {
  @virtual class E(val n1: String, val n2: String) {
    def isConnecting(n1: String, n2: String): Boolean = {
      this.n1.equals(n1) && this.n2.equals(n1);
    }
  }

  @virtual class UE(val n1: String, val n2: String) extends E {
    override def isConnecting(n1: String, n2: String): Boolean = {
      super.isConnecting(n1, n2) || super.isConnecting(n2, n1);
    }
  }
}

//=========================================================
@virtualContext class CG1 extends G1 {
  @virtual override class E {
    var col: Color = null;
  }
}