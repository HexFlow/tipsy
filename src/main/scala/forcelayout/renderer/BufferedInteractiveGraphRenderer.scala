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

import java.awt.{ Canvas, Dimension, Image, Graphics, Graphics2D, GraphicsEnvironment, Point, RenderingHints }
import java.awt.event.{ KeyAdapter, KeyEvent, MouseAdapter, MouseEvent, MouseWheelListener, MouseWheelEvent }
import forcelayout.{ Node, SpringGraph }

class BufferedInteractiveGraphRenderer(graph: SpringGraph) extends Canvas with GraphRenderer {

  private var offscreenImage: Image = null
  private var offscreenGraphics: Graphics2D = null
  private var offscreenDimension: Dimension = null
  
  private var currentZoom = 1.0
  private var currentXOffset = 0.0
  private var currentYOffset = 0.0
  private var lastMousePos = new Point(0, 0)
  
  private var selectedNode: Option[Node] = None
  
  private var showLabels = false
  
  addKeyListener(new KeyAdapter() {
    override def keyPressed(e: KeyEvent) {
      if (e.getKeyCode == 76) {
        showLabels = !showLabels
        repaint()
      }
    }  
  })
  
  addMouseMotionListener(new MouseAdapter() {
    override def mouseDragged(e: MouseEvent) {
      currentXOffset += e.getX - lastMousePos.getX
      currentYOffset += e.getY - lastMousePos.getY
      lastMousePos = e.getPoint
      repaint()
    }
  })
  
  addMouseListener(new MouseAdapter() {
    override def mouseClicked(e: MouseEvent) {
      val size = getSize()
      val coords = toGraphCoords(graph, e.getPoint, size.getWidth.toInt, size.getHeight.toInt, currentXOffset, currentYOffset, currentZoom)
      selectedNode = Some(graph.getNearestNode(coords))
      repaint() 
    }
    
    override def mousePressed(e: MouseEvent) = lastMousePos = e.getPoint 
  })
  
  addMouseWheelListener(new MouseWheelListener() {
    override def mouseWheelMoved(e: MouseWheelEvent) {
      // TODO make zooming sensitive to mouse position
      if (e.getWheelRotation() > 0)
        currentZoom /= 1.1
      else
        currentZoom *= 1.1
        
      repaint()
    }
  })
  
  override def paint(g : Graphics) {
    val currentSize = getSize
    val (width, height) = (currentSize.getWidth.toInt, currentSize.getWidth.toInt)    
    val gfxConfig = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration
    
    if(offscreenImage == null || !currentSize.equals(offscreenDimension)) {
      if (offscreenImage != null)
        offscreenGraphics.dispose
        
      offscreenImage = gfxConfig.createCompatibleImage(currentSize.width, currentSize.height) 
      offscreenGraphics = offscreenImage.getGraphics.asInstanceOf[Graphics2D]
      offscreenDimension = currentSize
      offscreenGraphics.setRenderingHint(
                      RenderingHints.KEY_ANTIALIASING,
                      RenderingHints.VALUE_ANTIALIAS_ON)
      offscreenGraphics.setRenderingHint(
                      RenderingHints.KEY_FRACTIONALMETRICS,
                      RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    }

    render(offscreenGraphics, graph, currentSize.getWidth.toInt, currentSize.getHeight.toInt, selectedNode, currentXOffset, currentYOffset, currentZoom, showLabels)
    g.drawImage(offscreenImage, 0, 0, this)
  }
 
  override def update(g: Graphics) = paint(g)

  def start = graph.doLayout(onComplete = (it => { println("completed in " + it + " iterations"); repaint() }),
                             onIteration = (it => repaint()))  
}
