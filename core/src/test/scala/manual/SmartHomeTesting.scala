package manual

import scala.language.higherKinds

import VirtualClasses.print

object SmartHomeTesting extends App {
  @print
abstract class Building extends scala.AnyRef {
    object Location extends scala.AnyRef {
      def apply() = VC_NEW$Location
    };
    def VC_NEW$Location: Location;
    type Location >: _root_.scala.Null <: VC_TRAIT$Building$Location;
    abstract trait VC_TRAIT$Building$Location extends scala.AnyRef { self: Location =>
    };
    object Room extends scala.AnyRef {
      def apply() = VC_NEW$Room
    };
    def VC_NEW$Room: Room;
    type Room >: _root_.scala.Null <: VC_TRAIT$Building$Room with Location;
    abstract trait VC_TRAIT$Building$Room extends scala.AnyRef { self: Room =>
    };
    type CompositeLocation[L >: _root_.scala.Nothing <: Location] >: _root_.scala.Null <: Location with VC_TRAIT$Building$CompositeLocation[L] with VC_TRAIT$Building$Location;
    abstract trait VC_TRAIT$Building$CompositeLocation[L >: _root_.scala.Nothing <: Location] extends scala.AnyRef { self: CompositeLocation[L] =>
      var locations: List[L] = null
    };
    object Floor extends scala.AnyRef {
      def apply() = VC_NEW$Floor
    };
    def VC_NEW$Floor: Floor;
    type Floor >: _root_.scala.Null <: VC_TRAIT$Building$Floor with CompositeLocation[Room];
    abstract trait VC_TRAIT$Building$Floor extends scala.AnyRef { self: Floor =>
    };
    object House extends scala.AnyRef {
      def apply() = VC_NEW$House
    };
    def VC_NEW$House: House;
    type House >: _root_.scala.Null <: VC_TRAIT$Building$House with CompositeLocation[Floor];
    abstract trait VC_TRAIT$Building$House extends scala.AnyRef { self: House =>
    }
  };
  object Building extends scala.AnyRef {
    class VC_FINAL$Building extends Building {
      def VC_NEW$Location = new VC_FIX$Building$Location();
      type Location = VC_TRAIT$Building$Location;
      class VC_FIX$Building$Location extends VC_TRAIT$Building$Location {
      };
      def VC_NEW$Room = new VC_FIX$Building$Room();
      type Room = VC_TRAIT$Building$Room with VC_TRAIT$Building$Location;
      class VC_FIX$Building$Room extends VC_TRAIT$Building$Room with VC_TRAIT$Building$Location {
      };
      type CompositeLocation[L >: _root_.scala.Nothing <: Location] = Location with VC_TRAIT$Building$CompositeLocation[L] with VC_TRAIT$Building$Location;
      abstract class VC_FIX$Building$CompositeLocation[L >: _root_.scala.Nothing <: Location] extends VC_TRAIT$Building$CompositeLocation[L] with VC_TRAIT$Building$Location {
      };
      def VC_NEW$Floor = new VC_FIX$Building$Floor();
      type Floor = VC_TRAIT$Building$Floor with VC_TRAIT$Building$Location with VC_TRAIT$Building$CompositeLocation[Room];
      class VC_FIX$Building$Floor extends VC_TRAIT$Building$Floor with VC_TRAIT$Building$Location with VC_TRAIT$Building$CompositeLocation[Room] {
      };
      def VC_NEW$House = new VC_FIX$Building$House();
      type House = VC_TRAIT$Building$House with VC_TRAIT$Building$Location with VC_TRAIT$Building$CompositeLocation[Floor];
      class VC_FIX$Building$House extends VC_TRAIT$Building$House with VC_TRAIT$Building$Location with VC_TRAIT$Building$CompositeLocation[Floor] {
      }
    };
    def apply() = new VC_FINAL$Building()
  };
}

