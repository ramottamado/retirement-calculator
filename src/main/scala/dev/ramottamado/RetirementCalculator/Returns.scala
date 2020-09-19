package dev.ramottamado.RetirementCalculator

sealed trait Returns

object Returns {

  def monthlyRate(returns: Returns, month: Int): Either[RetCalcError, Double] =
    returns match {
      case FixedReturns(annualRate) => Right(annualRate / 12)
      case VariableReturns(returns) =>
        if (returns.isDefinedAt(month))
          Right(returns(month).monthlyRate)
        else
          Left(RetCalcError.ReturnMonthOutOfBounds(month, returns.size - 1))
      case OffsetReturns(orig, offset) => monthlyRate(orig, month + offset)
      case VariableReturn(monthId, monthlyRate) => Right(monthlyRate)
    }

  def fromEquityandInflationData(equities: Vector[EquityData], inflations: Vector[InflationData]): VariableReturns = {
    VariableReturns(
      ((equities zip inflations) sliding 2).collect {
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
