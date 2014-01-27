@virtualContext class Building {
  @virtual abstract class Location {
    
  }
  
  @virtual abstract class CompositeLocation extends Location {
    type T <: Location
    
    var locations: List[T] = List()
  }
  
  @virtual class Room extends Location {
    
  }
  
  @virtual class Floor extends CompositeLocation {
    type T = Room
  }
  
  @virtual class Building extends CompositeLocation {
    type T = Floor
  }
}

class Light { def turnOn() {  println("light on")  }; def turnOff() {  println("light off")  }; }

@virtualContext class Lights extends Building {
  @virtualOverride abstract class Location {
    var _lights: List[Light] = List()
    
    def lights: List[Light] = _lights

    def turnLightsOn = lights.foreach(_.turnOn())

    def turnLightsOff = lights.foreach(_.turnOff())
  }
  
  @virtualOverride abstract class CompositeLocation {
    override def lights: List[Light] = _lights ++ locations.flatMap(location => location.lights)
  }
}

class Shutter { def raise() { println("Shutter raised") }; def lower() { println("Shutter lowered") }; }

@virtualContext class Shutters extends Building {
  @virtualOverride abstract class Location {
    var _shutters: List[Shutter] = List()
    def shutters: List[Shutter] = _shutters

    def lower = shutters.foreach(_.lower)

    def raise = shutters.foreach(_.raise)
  }
  
  @virtualOverride abstract class CompositeLocation {
    override def shutters: List[Shutter] = _shutters ++ locations.flatMap(location => location.shutters)
  }
}

@virtualContext class LightsAndShutters extends Lights with Shutters {
  
}

@virtualContext class Estate extends Building with LightsAndShutters {
  @virtual class Garage extends Location {
    def Car = "Audi R8"
  }
}

object SmartHomeVC extends App {
  val e = Estate()
  val house = e.Building()
  val floor = e.Floor()
  floor.locations = List(e.Room(), e.Room())
  house.locations = List(floor)
  floor.locations(0)._lights = List(new Light())
  floor.locations(1)._lights = List(new Light())
  floor.locations(0)._shutters = List(new Shutter())
  house.lower
  house.turnLightsOff
  house.turnLightsOn
  val g = e.Garage()
  g._shutters = List(new Shutter())
  g._lights = List(new Light())
  g.turnLightsOn
  g.raise
  println(g.Car)
}