package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal {
    val bVal = b()
    bVal * bVal - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    delta() match {
      case value if value.compareTo(0d) < 0 => Set()
      case value if value.compareTo(0d) == 0 => Set(-b() / (2 * a()))
      case value => Set((-b() - Math.sqrt(value)) / (2 * a()), (-b() + Math.sqrt(value)) / (2 * a()))
    }

  }
}
