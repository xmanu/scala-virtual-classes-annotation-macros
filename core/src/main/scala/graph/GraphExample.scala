package graph

import VirtualClasses._
import scala.util.control.Breaks._

object GraphExample extends App {
  val ugf = WeightedUndirectedGraphWithPrettyPrintAndFeatures()
  import ugf._

  val a = Node("A")
  val b = Node("B")
  val c = Node("C")
  val d = Node("D")

  ugf.nodes = List(a, b, c, d)
  ugf.edges = List(a.connect(b), b.connect(a), b.connect(c), d.connect(c))
  for { i <- (0 until edges.size) } ugf.edges(i).w = i

  println(ugf)
  println(a.connectedNodes)
  println(a.allConnectedNodes)

  val gf = WeightedGraphWithPrettyPrintAndFeatures()

  val a2 = gf.Node("A")
  val b2 = gf.Node("B")
  val c2 = gf.Node("C")
  val d2 = gf.Node("D")

  gf.nodes = List(a2, b2, c2, d2)
  gf.edges = List(a2.connect(b2), b2.connect(a2), b2.connect(c2), d2.connect(c2))
  for { i <- (0 until edges.size) } gf.edges(i).w = i

  println(gf)
  println(a2.connectedNodes)
  println(a2.allConnectedNodes)

  
  // incredibly inefficient Dijkstra...
  val dijkstra = Dijkstra()

  val max = 10

  for { i <- (1 to max); j <- (1 to max) } {
    val node = dijkstra.Node(s"$i,$j")
    dijkstra.nodes ++= List(node)
  }
  for { i <- (1 to max); j <- (1 to max) } {
    val node = dijkstra.nodes.find(_.name == s"$i,$j").get
    if (i > 1 && j > 1)
      dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${(i - 1)},${j - 1}").get))
    if (i > 1 && j < max)
      dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${(i - 1)},${j + 1}").get))
    if (i < max && j > 1)
      dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${(i + 1)},${j - 1}").get))
    if (i < max && j < max)
      dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${(i + 1)},${j + 1}").get))
  }
  val r = new scala.util.Random()
  for { e <- dijkstra.edges } {
    e.w = 1 //r.nextInt(1000)
  }

  println(dijkstra)
  println(dijkstra.findShortestPath(dijkstra.nodes(0), dijkstra.nodes(max * max - 1)))
}

@family class Graph {
  var nodes: List[Node] = List()
  var edges: List[Edge] = List()

  @virtual class Node(val name: String) {
    def connect(other: Node): Edge = Edge(this, other)
  }
  @virtual class Edge(val from: Node, val to: Node)
}

@family class GraphFeatures extends Graph {
  @virtual override class Node {
    var discovered = false

    def connectedNodes: List[Node] =
      edges.filter(e => e.from == this).map(_.to)
    def allConnectedNodes: List[Node] = {
      discovered = true
      this :: connectedNodes.flatMap(n => if (!n.discovered) n.allConnectedNodes else List())
    }
  }
}

@family class UndirectedGraphFeatures extends GraphFeatures {
  @virtual override class Node {
    override def connectedNodes: List[Node] =
      (edges.filter(e => e.from == this).map(_.to) ++
        edges.filter(e => e.to == this).map(_.from)).distinct
  }
}

@family class PrettyPrintGraph extends Graph {
  @virtual override class Node {
    override def toString: String = s"[$name]"
  }

  @virtual override class Edge {
    override def toString: String = s"$from -> $to"
  }

  override def toString: String = s"Nodes: ${nodes.mkString(", ")}\nEdges: ${edges.mkString(", ")}"
}

@family class UndirectedPrettyPrintedGraph extends PrettyPrintGraph with UndirectedGraphFeatures {
  @virtual override class Edge {
    override def toString: String = s"$from - $to"
  }
}

@family class GraphWithPrettyPrintAndFeatures extends GraphFeatures with PrettyPrintGraph

@family class PositionedGraph extends Graph {
  @virtual override class Node {
    var x: Int = 0
    var y: Int = 0
  }
}

@family class DrawableGraph extends PositionedGraph {

}

@family class WeightedGraph extends Graph {
  @virtual override class Edge {
    var w: Int = 0
  }
}

@family class WeightedPrettyPrintGraph extends WeightedGraph with PrettyPrintGraph {
  @virtual override class Edge {
    override def toString: String = s"$from -$w-> $to"
  }
}

@family class UndirectedWeightedPrettyPrintGraph extends WeightedPrettyPrintGraph with UndirectedPrettyPrintedGraph {
  @virtual override class Edge {
    override def toString: String = s"$from -$w- $to"
  }
}

@family class WeightedGraphWithPrettyPrintAndFeatures extends WeightedPrettyPrintGraph with GraphWithPrettyPrintAndFeatures

@family class WeightedUndirectedGraphWithPrettyPrintAndFeatures extends UndirectedWeightedPrettyPrintGraph with UndirectedGraphFeatures

@family class Dijkstra extends WeightedPrettyPrintGraph with GraphFeatures {
  @virtual override class Node {
    var dist: Int = 0
    var prev: Node = null
  }

  def findShortestPath(start: Node, end: Node): List[Node] = {
    for { n <- nodes } {
      n.dist = Int.MaxValue
      n.prev = null
    }
    start.dist = 0

    var q = nodes

    while (!q.isEmpty) {
      var u = q.sortWith(_.dist < _.dist).head
      q = q.filter(_ != u)
      if (u == end) {
        var res = List[Node]()
        while (u.prev != null) {
          res = u :: res
          u = u.prev
        }
        return u :: res
      }
      if (u.dist == Int.MaxValue)
        break
      for { v <- u.connectedNodes } {
        val alt = v.dist + edges.filter(e => e.from == u && e.to == v).head.w
        if (alt < v.dist) {
          v.dist = alt
          v.prev = u
        }
      }
    }
    return List()
  }
}

class Library(val g: Graph) {
  def copyEdge(e: g.Edge): g.Edge = {
    val from: g.Node = e.from
    val to: g.Node = e.to
    g.Edge(from, to)
  }
}