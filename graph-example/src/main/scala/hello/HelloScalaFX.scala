package hello

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
import scalafx.scene.shape.Line

object HelloScalaFX extends JFXApp {

  def singleNode = new StackPane {
    content = Set(new Circle {
      radius = 10
      fill <== when(hover) choose Color.GREEN otherwise Color(0,0,0,0)
    }, new Label {
      text = "T"
    })
    alignment = CENTER
  }

  lazy val line = new Line {
    startX = 10
    startY = 10
    endX = 100
    endY = 100
  }
  
  val contentSet = (for {i <- (0 to 10)} yield singleNode) ++ List(line)
  
  stage = new JFXApp.PrimaryStage {
    title = "Hello World"
    width = 600
    height = 450
    scene = new Scene {
      fill = Color.LIGHTGREEN
      root = new AnchorPane {
        prefWidth = 600
        prefHeight = 450
        content = contentSet
      }
    }
  }
  for {i <- (0 to 10)} AnchorPane.setTopAnchor(contentSet(i), 30*i)
  for {i <- (0 to 10)} AnchorPane.setLeftAnchor(contentSet(i), 30*i)
}