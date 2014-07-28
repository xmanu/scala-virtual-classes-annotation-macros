package auto

import specs.UnitSpec
import typeCheckTests._
import VirtualClasses._

class FailTests extends UnitSpec {
  "@virtual" should "reside in @family" in {
    ShouldNotTypecheck("@virtual class AAAAAA", "@virtual annotations have to reside inside @family annotations. Only classes can be annotated with @virtual.")
  }
  
  "@virtual" should "only annotate classes" in {
    ShouldNotTypecheck("@family class fam { @virtual def test: Int = 0 }", "@virtual annotations have to reside inside @family annotations. Only classes can be annotated with @virtual.")
  }
  
  "typed virtual classes" should "not be allowed" in {
    ShouldNotTypecheck("@family class fam { @virtual class A[T <: String] }", "Type parameters are currently not supported.")
  }
  
  "recusion" should "be stopped" in {
    ShouldNotTypecheck("@family class A extends B; @family class B extends A", "illegal cyclic reference involving trait A")
  }
  
  "unknown parents in families" should "be handled correctly" in {
    ShouldNotTypecheck("@family class A extends SomeNotExistingClass", "not found: type SomeNotExistingClass")
  }
  
  "constructor parameters" should "not have tparams" in {
    ShouldNotTypecheck("@family class A { @virtual class B(val c: List[String]) }", "Constructor parameter c cannot have type parameters.")
  }

  /*"exisiting virtual classes in base families" should "be overriden by using the override modifier" in {
    ShouldNotTypecheck("@family class A { @virtual class InnerA }; @family class B extends A { @virtual class InnerA }")
  }*/
}