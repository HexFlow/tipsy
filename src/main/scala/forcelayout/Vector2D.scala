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

import scala.util.Random

/**
 * A basic 2D vector, plus some convenience methods.
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Vector2D(val x: Double, val y: Double ) {
  
  def add(v: Vector2D) = Vector2D(x + v.x, y + v.y)
  
  def +(v: Vector2D) = Vector2D.this.add(v)
  
  def substract(v: Vector2D) = Vector2D(x - v.x, y - v.y)
  
  def -(v: Vector2D) = Vector2D.this.substract(v)

  def multiply(n: Double) = Vector2D(x * n, y * n)
  
  def *(n: Double) = Vector2D.this.multiply(n)
  
  def divide(n: Double) = Vector2D(x / n, y /n)
  
  def /(n: Double) = Vector2D.this.divide(n)

  lazy val magnitude = Math.sqrt(x * x + y * y)  
  
  lazy val normalize = Vector2D.this.divide(Vector2D.this.magnitude)
  
}

object Vector2D {
  
  def random(r: Double = 1.0, center: Vector2D = Vector2D(0, 0)) = Vector2D(center.x + Random.nextDouble * r - r / 2, center.y + Random.nextDouble * r - r / 2)

}
