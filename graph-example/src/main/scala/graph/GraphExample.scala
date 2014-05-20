package graph

import VirtualClasses._
import scala.util.control.Breaks._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.beans.property.ReadOnlyBooleanProperty.sfxReadOnlyBooleanProperty2jfx
import scalafx.scene.paint.Color.sfxColor2jfx
import scalafx.scene.control.Label
import scalafx.scene.Group
import scalafx.scene.layout.Pane
import scalafx.scene.layout.StackPane
import scalafx.scene.shape.Circle
import scalafx.geometry.Pos.CENTER
import scalafx.scene.layout.AnchorPane
import scalafx.scene.text.Font
import scalafx.scene.shape.Line
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.MouseButton

object GraphExample extends JFXApp {
  var dijkstra: DrawableDijkstra = DrawableDijkstra()
  fillDijkstra(dijkstra)

  stage = new JFXApp.PrimaryStage {
    title = "Dijkstra"
    width = 500
    height = 500
    scene = new Scene {
      fill = Color.LIGHTGREEN
      root = new AnchorPane {
        content = Set(dijkstra.uiRepresentation)
        onMouseClicked = (ae: MouseEvent) => {
          println("Mouse clicked")
          if (ae.button == MouseButton.PRIMARY)
            dijkstra = DrawableDijkstra()
          else
            dijkstra = RectangleDrawableDijkstra()
          fillDijkstra(dijkstra)
          content = Set(dijkstra.uiRepresentation)
        }
      }
    }
  }

  def fillDijkstra(dijkstra: HighlightableDijkstra) {
    // incredibly inefficient Dijkstra...

    val max = 15

    for { i <- (1 to max); j <- (1 to max) } {
      val node = dijkstra.Node(s"$i,$j")
      node.x = 30 * (i - 1) + 25
      node.y = 30 * (j - 1) + 25
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
      if (i > 1)
        dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${(i - 1)},${j}").get))
      if (i < max)
        dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${(i + 1)},${j}").get))
      if (j > 1)
        dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${i},${j - 1}").get))
      if (j < max)
        dijkstra.edges ++= List(node.connect(dijkstra.nodes.find(_.name == s"${i},${j + 1}").get))
    }
    val r = new scala.util.Random()
    for { e <- dijkstra.edges } {
      e.w = r.nextInt(255)
    }

    val path = dijkstra.findShortestPath(dijkstra.nodes(0), dijkstra.nodes(max * max - 1))

    println(dijkstra)
    println(path)
    for { n <- path } n.highlighted = true
  }
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

@family class DrawableGraph extends PositionedGraph with WeightedGraph {
  @virtual override class Node {
    lazy val uiRepresentation = new StackPane {
      content = Set(new Circle {
        radius = 10
        fill = Color.RED
      }, new Label {
        text = name
        font = new Font(10)
      })
    }
  }

  @virtual override class Edge {
    lazy val uiRepresentation = new Line {
      val offsetY = if (to.x > from.x) 1 else -1
      val offsetX = (if (to.y > from.y) 1 else -1) * offsetY
      startX = from.x + offsetX
      startY = from.y + offsetY
      endX = to.x + offsetX
      endY = to.y + offsetY
      stroke = Color.rgb(255 - w, 255 - w, 255 - w)
    }
  }

  def uiRepresentation = {
    val anchorPane = new AnchorPane {
      content = (for { e <- edges } yield e.uiRepresentation) ++ (for { n <- nodes } yield n.uiRepresentation)
    }
    for { n <- nodes } {
      AnchorPane.setTopAnchor(n.uiRepresentation, n.y - 10)
      AnchorPane.setLeftAnchor(n.uiRepresentation, n.x - 10)
    }
    anchorPane
  }
}

@family class RectangleGraph extends DrawableGraph {
  @virtual override class Node {
    override lazy val uiRepresentation = new StackPane {
      content = Set(new Rectangle {
        width = 20
        height = 15
        fill = Color.RED
      }, new Label {
        text = name
        font = new Font(10)
      })
    }
  }

  @virtual override class Edge {
    override lazy val uiRepresentation = new Line {
      val offsetY = if (to.x > from.x) 1 else -1
      val offsetX = (if (to.y > from.y) 1 else -1) * offsetY
      startX = from.x + offsetX
      startY = from.y + offsetY
      endX = to.x + offsetX
      endY = to.y + offsetY
      stroke = Color.rgb(255 - w, 255 - w, 255)
    }
  }
}

@family class HighlightableGraph extends Graph {
  @virtual override class Node {
    var highlighted = false
  }
}

@family class DrawableHighlightableGraph extends DrawableGraph with HighlightableGraph {
  @virtual override class Node {
    override lazy val uiRepresentation = new StackPane {
      content = Set(new Circle {
        radius = 10
        fill = if (highlighted) Color.GREEN else Color.RED
      }, new Label {
        text = name
        font = new Font(10)
      })
    }
  }
}

@family class RectangleHighlightableGraph extends RectangleGraph with HighlightableGraph {
  @virtual override class Node {
    override lazy val uiRepresentation = new StackPane {
      content = Set(new Rectangle {
        width = 20
        height = 15
        fill = if (highlighted) Color.GREEN else Color.RED
      }, new Label {
        text = name
        font = new Font(10)
      })
    }
  }
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

@family class HighlightableDijkstra extends Dijkstra with HighlightableGraph with PositionedGraph

@family class DrawableDijkstra extends HighlightableDijkstra with DrawableHighlightableGraph

@family class RectangleDrawableDijkstra extends HighlightableDijkstra with RectangleHighlightableGraph with DrawableDijkstra

class Library(val g: Graph) {
  def copyEdge(e: g.Edge): g.Edge = {
    val from: g.Node = e.from
    val to: g.Node = e.to
    g.Edge(from, to)
  }
}