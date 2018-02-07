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


package forcelayout

/**
 * 2D bounds, plus some convenience methods.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Bounds(minX: Double, minY: Double, maxX: Double, maxY: Double) {
  
  lazy val width = maxX - minX
  
  lazy val height = maxY - minY
  
  lazy val center = Vector2D((minX + maxX) / 2, (minY + maxY) / 2) 
  
  lazy val area = width * height
  
  def contains(pt: Vector2D) = {
    if (pt.x < minX)
      false
    else if (pt.x > maxX)
      false
    else if (pt.y < minY)
      false
    else if (pt.y > maxY)
      false
    else
      true
  }
  
}
