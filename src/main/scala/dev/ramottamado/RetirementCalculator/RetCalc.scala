package dev.ramottamado.RetirementCalculator

import dev.ramottamado.RetirementCalculator.RetCalcError.MoreExpensesThanIncome
import scala.annotation.tailrec

case class RetCalcParams(nbOfMonthsInRetirement: Int, netIncome: Int, currentExpenses: Int, initialCapital: Double)

object RetCalc {

  def futureCapital(
    returns: Returns,
    nbOfMonths: Int,
    netIncome: Int,
    currentExpenses: Int,
    initialCapital: Double
  ): Either[RetCalcError, Double] = {
    val monthlySavings = netIncome - currentExpenses

    (0 until nbOfMonths).foldLeft[Either[RetCalcError, Double]](Right(initialCapital)) {
      case (accumulated, month) =>
        for {
          acc <- accumulated
          monthlyRate <- Returns.monthlyRate(returns, month)
        } yield acc * (1 + monthlyRate) + monthlySavings
    }
  }

  def simulatePlan(
    returns: Returns,
    nbOfMonthsSaving: Int,
    params: RetCalcParams
  ): Either[RetCalcError, (Double, Double)] = {
    import params._
    for {
      capitalAtRetirement <- futureCapital(returns, nbOfMonthsSaving, netIncome, currentExpenses, initialCapital)
      capitalAfterDeath <- futureCapital(
        OffsetReturns(returns, nbOfMonthsSaving),
        nbOfMonthsInRetirement,
        0,
        currentExpenses,
        capitalAtRetirement
      )
    } yield (capitalAtRetirement, capitalAfterDeath)
  }

  def nbOfMonthsSaving(
    returns: Returns,
    params: RetCalcParams
  ): Either[RetCalcError, Int] = {
    import params._
    @tailrec
    def loop(months: Int): Either[RetCalcError, Int] = {
      simulatePlan(returns, months, params) match {
        case Right((capitalAtRetirement, capitalAfterDeath)) =>
          if (capitalAfterDeath > 0.0)
            Right(months)
          else
            loop(months + 1)
        case Left(err) => Left(err)
      }
    }
    if (netIncome > currentExpenses)
      loop(0)
    else
      Left(MoreExpensesThanIncome(netIncome, currentExpenses))
  }

}
