package dev.ramottamado.RetirementCalculator

import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int, netIncome: Int, currentExpenses: Int, initialCapital: Double)

object RetCalc {

  def futureCapital(
    returns: Returns,
    nbOfMonths: Int,
    netIncome: Int,
    currentExpenses: Int,
    initialCapital: Double
  ): Double = {
    val monthlySavings = netIncome - currentExpenses

    (0 until nbOfMonths foldLeft initialCapital) { (accumulated, month) =>
      accumulated * (1 + Returns.monthlyRate(returns, month)) + monthlySavings
    }
  }

  def simulatePlan(
    returns: Returns,
    nbOfMonthsSaving: Int,
    params: RetCalcParams
  ): (Double, Double) = {
    import params._
    val capitalAtRetirement = futureCapital(
      returns,
      nbOfMonthsSaving,
      netIncome,
      currentExpenses,
      initialCapital
    )

    val capitalAfterDeath = futureCapital(
      OffsetReturns(returns, nbOfMonthsSaving),
      nbOfMonthsInRetirement,
      0,
      currentExpenses,
      capitalAtRetirement
    )

    (capitalAtRetirement, capitalAfterDeath)
  }

  def nbOfMonthsSaving(
    returns: Returns,
    params: RetCalcParams
  ): Int = {
    import params._
    @tailrec
    def loop(months: Int): Int = {
      val (capitalAtRetirement, capitalAfterDeath) = simulatePlan(
        returns,
        months,
        params
      )

      if (capitalAfterDeath > 0.0)
        months
      else
        loop(months + 1)
    }
    if (netIncome > currentExpenses)
      loop(0)
    else
      Int.MaxValue
  }

}
