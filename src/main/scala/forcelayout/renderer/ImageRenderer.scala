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


package forcelayout.renderer

import java.awt.{ Graphics2D, RenderingHints }
import java.awt.image.BufferedImage

import forcelayout.SpringGraph

/**
 * A graph drawing utility. 
 */
object ImageRenderer extends GraphRenderer {
  
  def drawGraph(graph: SpringGraph, width: Int, height: Int, showLabels: Boolean = false) =
    draw(graph, width, height, showLabels, None, None)
  
  def drawGraph(graph: SpringGraph, width: Int, height: Int, showLabels: Boolean, nodePainter: (Seq[Node2D], Graphics2D) => Unit) = 
    draw(graph, width, height, showLabels, Some(nodePainter), None)
  
  def drawGraph(graph: SpringGraph, width: Int, height: Int, showLabels: Boolean, nodePainter: (Seq[Node2D], Graphics2D) => Unit, edgePainter: (Seq[Edge2D], Graphics2D) => Unit) =
    draw(graph, width, height, showLabels, Some(nodePainter), Some(edgePainter))
      
  private def draw(graph: SpringGraph, width: Int, height: Int, showLabels: Boolean, 
      nodePainter: Option[(Seq[Node2D], Graphics2D) => Unit] = None,
      edgePainter: Option[(Seq[Edge2D], Graphics2D) => Unit]): BufferedImage = {
    
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val g = image.getGraphics.asInstanceOf[Graphics2D]
    g.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(
        RenderingHints.KEY_FRACTIONALMETRICS,
        RenderingHints.VALUE_FRACTIONALMETRICS_ON)

    if (nodePainter.isDefined)
      setNodePainter(nodePainter.get)
      
    if (edgePainter.isDefined)
      setEdgePainter(edgePainter.get)
      
    render(g, graph, width, height)
    
    image
  }

}
