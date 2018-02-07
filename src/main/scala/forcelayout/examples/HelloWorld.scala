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

import java.awt.Dimension
import javax.swing.{ JFrame, JLabel, ImageIcon }

import forcelayout.renderer.ImageRenderer
import forcelayout.{ Node, Edge, SpringGraph }

object HelloWorld {

  val apply = {
    val nodes = Seq(
      Node("A", "Node A"),
      Node("B", "Node B"),
      Node("C", "Node C"),
      Node("D", "Node D"))

    val edges = Seq(
      Edge(nodes(0), nodes(1)),
      Edge(nodes(1), nodes(2)),
      Edge(nodes(2), nodes(3)),
      Edge(nodes(0), nodes(3)))

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
      onIteration = (it =>
        imgLabel.setIcon(new ImageIcon(ImageRenderer.drawGraph(graph, 500, 500)))))
  }
}
