package tipsy.frontend

import tipsy.lexer._
import tipsy.parser._


import java.awt.Dimension
import javax.swing.{ JFrame, JLabel, ImageIcon  }
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JFrame;


import forcelayout.renderer.ImageRenderer
import forcelayout.renderer._
import forcelayout.{ Node, Edge, SpringGraph }

object DistanceDraw {
  def apply(network: List[(Int, Int, Double)], length: Int, names: List[String]) {
    println(network)
    val avg = network.map(_._3).sum/network.size
    println(avg)
    val nodes = (0 to length-1).map(x => Node(""+names(x), "Node "+names(x)))
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
        //val image = ImageRenderer.drawGraph(graph, 500, 500)

        // Writes the image to a PNG file
        //ImageIO.write(image, "png", new File("forcegraph.png"))
        val vis = new BufferedInteractiveGraphRenderer(graph)
        val frame = new JFrame("Les Miserables")
        frame.setSize(920, 720)
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        frame.getContentPane().add(vis)
        frame.pack()

        // Pops up the JFrame on the screen, and starts the layout process
        frame.setVisible(true)
        vis.start
      }),
      onIteration = (it => imgLabel.setIcon(new ImageIcon(ImageRenderer.drawGraph(graph, 500, 500))))
    )
  }
}
