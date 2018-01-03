package tipsy.compare

class LazyVector[A](thunks: Vector[() => A]) {

  private val values: Array[Option[A]] = Array.fill(thunks.length)(None)

  def apply(i: Int): A = {
    if (!values(i).isDefined) {
      values(i) = Some(thunks(i)())
    }
    values(i).get
  }

}

object LazyVector {

  def tabulate[A](n: Int)(f: Int => A): LazyVector[A] = {
    new LazyVector(Vector.tabulate(n)(i => () => f(i)))
  }

  def tabulate[A](m: Int, n: Int)(f: (Int, Int) => A): LazyVector[LazyVector[A]] = {
    tabulate(m)(i => tabulate(n)(f(i, _)))
  }

}
