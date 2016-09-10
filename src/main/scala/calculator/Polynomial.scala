package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bVal = b()
      val aVal = a()
      val cVal = c()
      (bVal*bVal)-(4*aVal*cVal)
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val deltaVal = delta()
      val bVal = b()
      val aVal = a()
      val cVal = c()
      if (deltaVal > 0) {
        val sqrt = Math.sqrt(deltaVal)
        ((-bVal) + sqrt) / (2 * aVal)
        Set(((-bVal) + sqrt) / (2 * aVal), ((-bVal) - sqrt) / (2 * aVal))
      } else if (deltaVal == 0) {
        Set((-bVal) / (2 * aVal))
      } else {
        Set()
      }
    }
  }
}
