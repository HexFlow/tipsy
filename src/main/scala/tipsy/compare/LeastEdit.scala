package tipsy.compare

import tipsy.parser._

import scala.math._

sealed trait DiffChange {
  def string(): String = {
    this match {
      case ADD_d => "Add"
      case DEL_d => "Delete"
      case REPLACE_d => "Replace"
    }
  }
}
case object ADD_d     extends DiffChange
case object DEL_d     extends DiffChange
case object REPLACE_d extends DiffChange

case class Diff (
  change: DiffChange,
  addEntry: Option[CFEnum],
  delEntry: Option[CFEnum],
  fxn: String = "") {
  lazy val position = delEntry.map(x => (x.line, x.column))
}

case class EditRet (diffs: List[Diff], dist: Double) {
  def correct(d: Diff, dis: Double): EditRet = {
    this.copy(diffs = diffs ++ List(d), dist = dist + dis)
  }

  def +(v: Double): EditRet = {
    this match {
      case EditRet(x, y) => EditRet(x, y + v)
    }
  }

  def +(v: EditRet): EditRet = {
    this match {
      case EditRet(x, y) => EditRet(x ++ v.diffs, y + v.dist)
    }
  }


  def /(v: Double) = {
    this match {
      case EditRet(x, y) => {
        EditRet(x, y/v)
      }
    }
  }
  def >(x: EditRet): Boolean = {
    dist > x.dist
  }
  def <=(x: EditRet): Boolean = {
    dist <= x.dist
  }
  def min(x: EditRet): EditRet = {
    if (this <= x) this else x
  }
  def apply(a: EditRet) {
    this.copy(diffs = a.diffs, dist = a.dist)
  }
}

object LeastEdit {

  def apply(trees: List[ParseTree], cluster: Boolean): List[(Int, Int, Double)] = {
    if (trees.length < 2) {
      println("[Warning] Least Edit mode requires at least 2 trees")
    }
    trees.zipWithIndex.combinations(2).map {
      case Seq(t1, t2) => {
        println("Starting 2 new trees " + t1._2 + " " + t2._2)
        val k = compareTwoTrees(t1._1, t2._1)
        /*val mb = 1024 * 1024
         *println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
         *println("** Free Memory:  " + (runtime.freeMemory / mb))
         *println("** Total Memory: " + (runtime.totalMemory) / mb)
         *println("** Max Memory:   " + (runtime.maxMemory) / mb)
         */
        cluster match {
          case true  => (t1._2, t2._2, k.dist)
          case false => (t1._2, t2._2, 10 / (k.dist + 0.1))
        }
      }
      case _ => ???
    }.toList
  }

  def compareWithTrees(prog: ParseTree, trees: List[ParseTree]) : List[(ParseTree, Double)] = {
    trees.map { case tree =>
      val k = compareTwoTrees(prog, tree)
      (tree, 1 / (k.dist + 0.1))
    }.toList
  }

  def compareTwoTrees(tree1: ParseTree, tree2: ParseTree): EditRet = {
    val cfEnum1 = FlowGraphTweaks(tree1.compress).toVector
    val cfEnum2 = FlowGraphTweaks(tree2.compress).toVector
    val l1 = cfEnum1.length
    val l2 = cfEnum2.length

    lazy val editDistTable: LazyVector[LazyVector[(EditRet, (Int, Int))]] =
      LazyVector.tabulate(l1 + 1, l2 + 1) { (x, y) => distance(x, y) }

    def distance(x: Int, y: Int): (EditRet, (Int, Int)) = {
      (x, y) match {
        case (0, 0) => (EditRet(List(), 1), (0, 0)) // otherwise 1/0.1 = 10 and all other < 1
        case (i, 0) => go(i - 1, 0, Some(DEL_d))
        case (0, j) => go(0, j - 1, Some(ADD_d))
        case (i, j) => {
          if (cfEnum1(i - 1) == cfEnum2(j - 1)) go(i - 1, j - 1, None)
          else Seq( go(i - 1, j, Some(DEL_d))
                  , go(i, j - 1, Some(ADD_d))
                  , go(i - 1, j - 1, Some(REPLACE_d))
               ).minBy(_._1.dist)
        }
      }
    }

    def cost(i: Int, j: Int, action: DiffChange, param: Int): Double = {
      action match {
        case DEL_d => 2.0 + param * 5.0
        case ADD_d => 2.0 + param * 5.0
        case _     => {
          (cfEnum1(i), cfEnum2(j)) match {
            case (POSTEXPR(expr1), POSTEXPR(expr2)) => {
              1.0 + 5.0 * param + compareTwoExpr(expr1.toVector, expr2.toVector, param)
            }
            case _ => 1.0 + 5.0 * param
          }
        }
      }
    }

    def go(i: Int, j: Int, action: Option[DiffChange]): (EditRet, (Int, Int)) = {
      val (editRet, (b, c)) = editDistTable(i)(j)
      if (!action.isDefined) (editRet, (b, c))
      else action.get match {
             case DEL_d => (editRet.correct(Diff(DEL_d, None, Some(cfEnum1(i))), cost(i, j, DEL_d, 3 * b)), (b + 1, c))
             case ADD_d => (editRet.correct(Diff(ADD_d, Some(cfEnum2(j)), None), cost(i, j, ADD_d, 3 * b)), (b + 1, c))
             case _     => (editRet.correct(Diff(REPLACE_d, Some(cfEnum2(j)), Some(cfEnum1(i))), cost(i, j, REPLACE_d, c)), (b, c + 1))
           }
    }

    editDistTable(l1)(l2)._1
  }

  def compareTwoExpr(expr1: Vector[String], expr2: Vector[String], param: Int): Double = {
    val l1 = expr1.length
    val l2 = expr2.length

    lazy val editDistTable: LazyVector[LazyVector[(Double, (Int, Int))]] =
      LazyVector.tabulate(l1 + 1, l2 + 1) { (x, y) => distance(x, y) }

      def distance(x: Int, y: Int): (Double, (Int, Int)) = {
        (x, y) match {
          case (0, 0) => (0.0, (param, param))
          case (i, 0) => go(i - 1, 0, 2, 1, 0)
          case (0, j) => go(0, j - 1, 2, 1, 0)
          case (i, j) => {
            if (expr1(i - 1) == expr2(j - 1)) go(i - 1, j - 1, 0.0, 0, 0)
            else Seq( go(i - 1, j, 2, 1, 0)
                    , go(i, j - 1, 2, 1, 0)
                    , go(i - 1, j - 1, 1, 0, 1)
                    ).minBy(_._1)
          }
        }
      }

      def go(i: Int, j: Int, cost: Double, _b: Int, _c: Int): (Double, (Int, Int)) = {
        val (dist, (b, c)) = editDistTable(i)(j)
        if (_b == 0 && _c == 0) (dist + cost, (b, c))
        else if (_b == 1) (dist + 0.5 * b, (b + 1, c))
        else (dist + 0.5 * c, (b, c + 1))
      }

      editDistTable(l1)(l2)._1
  }

  def levenshteinDist[A](a: Iterable[A], b: Iterable[A]) = {
    ((0 to b.size).toList /: a) { (prev, x) =>
      (prev zip prev.tail zip b).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) =>
          min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }
    } last
  }
}
