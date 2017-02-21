package tipsy

import tipsy.frontend._

object Tipsy {
  def main(args: Array[String]): Unit = {
    // Using the CLI frontend by default
    println(FlowGraph(args))
    ParseTreeImg(args)
  }
}
