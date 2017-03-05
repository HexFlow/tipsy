package tipsy.frontend

import tipsy.lexer._
import tipsy.parser._


import java.awt.Dimension
import javax.swing.{ JFrame, JLabel, ImageIcon  }

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
	frame.pack();
	frame.setVisible(true);

	graph.doLayout(
	  onComplete = (it => println("completed in " + it + " iterations")),
	  onIteration = (it => imgLabel.setIcon(new ImageIcon(ImageRenderer.drawGraph(graph, 500, 500)))))
  }
}
