package tipsy.frontend

import tipsy.lexer._
import tipsy.parser._


import java.awt.Dimension
import javax.swing.{ JFrame, JLabel, ImageIcon  }
import java.io.File
import javax.imageio.ImageIO

import forcelayout.renderer.ImageRenderer
import forcelayout.{ Node, Edge, SpringGraph }

object DistanceDraw {
  def apply(network: List[(Int, Int, Double)], length: Int) {
    println(network)
    val nodes = (1 to length).map(x => Node(""+x, "Node "+x))
    val edges = network.map{
      case (a, b, c) => Edge(nodes(a), nodes(b), c)
    }

    val graph = new SpringGraph(nodes, edges)
    val frame = new JFrame()
    frame.setPreferredSize(new Dimension(500,500))
    val imgIcon = new ImageIcon()
    val imgLabel = new JLabel(imgIcon)
    frame.add(imgLabel)
    frame.pack()
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    graph.doLayout(
      onComplete = (it => {
        println("Completed in " + it + " iterations")
        // Renders a 500x500 pixel image of the final graph layout
        val image = ImageRenderer.drawGraph(graph, 500, 500)

        // Writes the image to a PNG file
        ImageIO.write(image, "png", new File("forcegraph.png"))
      }),
      onIteration = (it => imgLabel.setIcon(new ImageIcon(ImageRenderer.drawGraph(graph, 500, 500))))
    )
  }
}
