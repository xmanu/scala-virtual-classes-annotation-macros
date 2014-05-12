package caesarj

import VirtualClasses._
import specs.UnitSpec

class Test99 extends UnitSpec {

  "A maple instance of WoodmanEagleSubject" should "have the foodValue 0 after chopping down" in {
    /*val wes: WoodmanEagleSubject = WoodmanEagleSubject()
    val maple: wes.Maple = wes.Maple()
    println("setting maple food value to 100")
    maple.foodValue = 100
    println("chopping the maple")
    assert(maple.foodValue == 100)
    maple.chopDown
    assert(maple.foodValue == 0)
    println(s"new maple food value is ${maple.foodValue}")*/
    //fail("We have the volatile problem!")
  }
}

/*@virtualContext class Forest {
  @virtual class Maple
  @virtual class Cherry
  @virtual class Locust
  @virtual class Pine
  @virtual class Dandellon
  @virtual class Bird
  @virtual class Woodman
}

@virtualContext class EagleSubject extends Forest {
  @virtual class Plant {
    var foodValue: Int = 0;
  }

  @virtual class Nestable {
    var nestSafetyRating: Int = 0
  }

  @virtual class Predator {
    var rating: Int = 0
  }

  @virtual class NectarPlant extends Plant {
  }

  @virtual class InsectPlant extends Plant {
  }

  @virtual override class Maple extends Nestable {
  }

  @virtual override class Cherry extends NectarPlant with InsectPlant with Nestable {
  }

  @virtual override class Locust extends InsectPlant with Nestable {
  }

  @virtual override class Pine extends InsectPlant with Nestable {
  }

  @virtual override class Dandellon extends NectarPlant {
  }

  @virtual override class Woodman extends Predator {
    var attackingRating: Int = 0
  }
  
}

@virtualContext class WoodmanSubject extends Forest {
  @virtual class Tree {
    var timeToChopDown: Int = 0

    def chopDown { println("tree chopped down.") }
  }

  @virtual class NonTree {
  }

  @virtual class Softwood extends Tree {
  }

  @virtual class Hardwood extends Tree {
  }

  @virtual override class Maple extends Hardwood {
  }

  @virtual override class Cherry extends Hardwood {
  }

  @virtual override class Pine extends Softwood {
  }

  @virtual override class Dandellon extends NonTree {
  }

  @virtual override class Bird extends NonTree {
  }

  @virtual override class Woodman extends NonTree {
    var salary: Int = 0
  }
}

@virtualContext class WoodmanEagleSubject extends WoodmanSubject with EagleSubject {

  @virtual override class Tree extends Plant {
    override def chopDown {
      super.chopDown
      foodValue = 0
      println(s"maple chopped down. Food value $foodValue")
    }
  }
}*/