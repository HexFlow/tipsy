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
 * A node in the force layout simulation. The node has an immutable component, representing the actual 
 * graph node, and a mutable 'state' field, containing the force simulation state. 
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class Node private[forcelayout] (id: String, label: String, mass: Double, group: Int, inlinks: Seq[Edge], outlinks: Seq[Edge], state: NodeState) {
  
  def this(id: String, label: String, mass: Double = 1.0, group: Int = 0) =
    this(id, label, mass, group, Seq.empty[Edge], Seq.empty[Edge], NodeState())
    
  lazy val links = inlinks ++ outlinks
  
}

object Node {
  // Shortcut, so the auxiliary constructor works in the normal case-class way
  def apply(id: String, label: String, mass: Double = 1.0, group: Int = 0) = new Node(id, label, mass, group)
}

/**
 * A container for the (mutable) force simulation state of a graph node. 
 * @author Rainer Simon <rainer.simon@ait.ac.at>
 */
case class NodeState(var pos: Vector2D = Vector2D.random(1.0), var velocity: Vector2D = Vector2D(0, 0), var force: Vector2D = Vector2D(0, 0))  
