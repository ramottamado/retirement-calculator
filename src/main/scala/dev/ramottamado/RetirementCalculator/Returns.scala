package dev.ramottamado.RetirementCalculator

sealed trait Returns

object Returns {

  def monthlyRate(returns: Returns, month: Int): Double =
    returns match {
      case FixedReturns(annualRate) => annualRate / 12
      case VariableReturns(returns) => returns(month % returns.length).monthlyRate
      case OffsetReturns(orig, offset) => monthlyRate(orig, month + offset)
      case VariableReturn(monthId, monthlyRate) => monthlyRate
    }

  def fromEquityandInflationData(equities: Vector[EquityData], inflations: Vector[InflationData]): VariableReturns = {
    VariableReturns(
      (((equities zip inflations) sliding 2) collect) {
        case (prevEquity, prevInflation) +: (equity, inflation) +: Vector() =>
          val inflationRate = inflation.value / prevInflation.value
          val totalReturn = (equity.value + equity.monthlyDividend) / prevEquity.value
          val realTotalReturn = totalReturn - inflationRate

          VariableReturn(equity.monthId, realTotalReturn)
      }.toVector
    )
  }

}

case class FixedReturns(annualRate: Double) extends Returns
case class OffsetReturns(orig: Returns, offset: Int) extends Returns
case class VariableReturn(monthId: String, monthlyRate: Double) extends Returns

case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {

  def fromUntil(monthIDFrom: String, monthIdUntil: String): VariableReturns =
    VariableReturns(returns dropWhile (_.monthId != monthIDFrom) takeWhile (_.monthId != monthIdUntil))

}
