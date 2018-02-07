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


package forcelayout.quadtree

import forcelayout.{ Bounds, Vector2D }

/**
 * An immutable quadtree implementation.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
class QuadTree[T](bounds: Bounds, bodies: Seq[Body[T]]) {

  import QuadTree._
  
  val root = build(bounds, bodies)
  
  def build(bounds: Bounds, bodies: Seq[Body[T]]): Quad[T] = {
    if (bodies.isEmpty) {
      Quad(bounds, bounds.center, 0)
    } else if (bodies.size == 1) {
      val body = bodies.head
      Quad(bounds, body.pos, 1, Some(body))
    } else {
      val children = subdivideBounds(bounds)
        .map(subbounds => build(subbounds, 	clipBodies(bodies, subbounds)))
      Quad(bounds, computeCenter(bodies), bodies.size, None, Some(children))
    }
  }  

}

object QuadTree {
  
  def subdivideBounds(bounds: Bounds) = Seq(
    Bounds(bounds.minX, bounds.minY + bounds.height / 2, bounds.minX + bounds.width / 2, bounds.maxY),
    Bounds(bounds.minX + bounds.width / 2, bounds.minY + bounds.height / 2, bounds.maxX, bounds.maxY),
    Bounds(bounds.minX + bounds.width / 2, bounds.minY, bounds.maxX, bounds.minY + bounds.height / 2),
    Bounds(bounds.minX, bounds.minY, bounds.minX + bounds.width / 2, bounds.minY + bounds.height / 2))
  
  def clipBodies[T](bodies: Seq[Body[T]], bounds: Bounds) = bodies.filter(b => bounds.contains(b.pos))
  
  def computeCenter[T](bodies: Seq[Body[T]]) = {
    val x = bodies.map(_.pos.x).fold(0.0)(_ + _) / bodies.size
    val y = bodies.map(_.pos.y).fold(0.0)(_ + _) / bodies.size
    Vector2D(x,y)
  }
  
}
