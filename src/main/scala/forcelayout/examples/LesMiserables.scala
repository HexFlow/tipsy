/**
  * Copyright (C) 2013 AIT Austrian Institute of Technology GmbH
  *
  * Permission is hereby granted, free of charge, to any person
  * obtaining a copy of this software and associated documentation
  * files (the "Software"), to deal in the Software without
  * restriction, including without limitation the rights to use,
  * copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the
  * Software is furnished to do so, subject to the following
  * conditions:
  *
  * The above copyright notice and this permission notice shall be
  * included in all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  * OTHER DEALINGS IN THE SOFTWARE.
  * Copyright (C) 2013 AIT Austrian Institute of Technology GmbH
  */


package forcelayout.examples

import rapture.io._
import scala.io.Source
import javax.swing.JFrame
import java.awt.{ BasicStroke, Color, Dimension, Graphics2D }
import java.awt.geom.Ellipse2D
import forcelayout.{ Edge, Node, SpringGraph }
import forcelayout.renderer.{ BufferedInteractiveGraphRenderer, Node2D }
import forcelayout.renderer.ColorPalette

object LesMiserables {

  val apply = {
    val json = Json.parse(Source.fromFile("src/test/resources/examples/miserables.json").mkString)

    val nodes: Seq[Node] = json.nodes.get[List[Json]].map(json => {
      val name = json.name.get[String].toString
      val group = json.group.get[Int]
      Node(name, name, 1.0, group)
    })

    val edges = json.links.get[List[Json]].map(json => {
      val value = json.value.get[Int]
      Edge(nodes(json.source.get[Int]), nodes(json.target.get[Int]), value.toDouble)
    })

    val graph = new SpringGraph(nodes, edges)

    val vis = new BufferedInteractiveGraphRenderer(graph)

    val nodePainter = (nodes: Seq[Node2D], g2d: Graphics2D) => {
      nodes.foreach(n2d => {
        val (x, y, n) = (n2d.x, n2d.y, n2d.node)
        val size = 6 + (n.links.size / 2)
        g2d.setColor(ColorPalette.getColor(n.group))
        g2d.fill(new Ellipse2D.Double(x - size / 2, y - size / 2, size, size))
        g2d.setStroke(new BasicStroke(2));
        g2d.setColor(Color.WHITE)
        g2d.draw(new Ellipse2D.Double(x - size / 2, y - size / 2, size, size))
      })
    }
    vis.setNodePainter(nodePainter)

    val frame = new JFrame("Les Miserables")
    frame.setPreferredSize(new Dimension(920,720))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.getContentPane().add(vis)
    frame.pack()
    frame.setVisible(true)

    vis.start
  }
}
