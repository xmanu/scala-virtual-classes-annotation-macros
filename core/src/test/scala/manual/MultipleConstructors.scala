package manual

import specs.UnitSpec
import scala.language.experimental.macros
import scala.reflect.macros.Context
import VirtualClasses._

object MultipleConstructors extends App {
  val oa = OuterA()
  val ia: oa.InnerA = oa.InnerA(42)
  println(s"${ia.a} == 42")
  println(s"${oa.innerInstance.a} == 7")

  val ob = OuterB()
  val ia2 = ob.InnerA(42, "truth")
  println(s"${ia2.a} == 42")
  println(s"${ia2.b} == truth")
  println(s"${ob.innerInstance.a} == 7")
  println(s"${ob.innerInstance.b} == null")
}

trait OuterA {
  type InnerA >: Null <: AnyRef with VC_TRAIT$OuterA$InnerA
  trait VC_TRAIT$OuterA$InnerA {
    val a: Int = 0 // = _
  }
  def InnerA(a: Int): InnerA
  val innerInstance = InnerA(7)
}

object OuterA {
  class VC_FINAL$OuterA extends OuterA {
    type InnerA = AnyRef with VC_TRAIT$OuterA$InnerA
    def InnerA(a: Int) = new VC_FIX$InnerA(a)
    class VC_FIX$InnerA(override val a: Int) extends VC_TRAIT$OuterA$InnerA
  }
  def apply() = new VC_FINAL$OuterA()
}

trait OuterB extends OuterA {
  type InnerA >: Null <: AnyRef with VC_TRAIT$OuterA$InnerA with VC_TRAIT$OuterB$InnerA
  trait VC_TRAIT$OuterB$InnerA extends VC_TRAIT$OuterA$InnerA {
    val b: String = null // = _
  }
  def InnerA(a: Int): InnerA
  def InnerA(a: Int, b: String): InnerA
}

object OuterB {
  class VC_FINAL$OuterB extends OuterB {
    type InnerA = AnyRef with VC_TRAIT$OuterA$InnerA with VC_TRAIT$OuterB$InnerA
    def InnerA(a: Int) = new VC_FIX$InnerA(a)
    class VC_FIX$InnerA(override val a: Int) extends AnyRef with VC_TRAIT$OuterA$InnerA with VC_TRAIT$OuterB$InnerA
    def InnerA(a: Int, b: String) = new VC_FIX$InnerA$1(a, b)
    class VC_FIX$InnerA$1(override val a: Int, override val b: String) extends AnyRef with VC_TRAIT$OuterA$InnerA with VC_TRAIT$OuterB$InnerA
  }
  def apply() = new VC_FINAL$OuterB()
}