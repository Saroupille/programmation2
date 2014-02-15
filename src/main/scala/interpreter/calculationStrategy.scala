trait CalculationStrategy {
  def interprete();
}


class SynchroneousStrategy extends CalculationStrategy {
  def interprete() {
  }
}

class AsynchroneousStrategy extends CalculationStrategy {
  def interprete() {
  }
}
