package dev.ramottamado.RetirementCalculator

import org.scalactic.{ Equality, TolerantNumerics, TypeCheckedTripleEquals }
import org.scalatest.{ Matchers }
import org.scalatest.wordspec.AnyWordSpec

class RetCalcSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics tolerantDoubleEquality 0.001

  "RetCalc.futureCapital" should {
    "calculate the amount of savings I will have in n months" in {
      val actual = RetCalc futureCapital (FixedReturns(0.04), 25 * 12, 3000, 2000, 10000)
      val expected = 541267.1990
      actual should ===(expected)
    }
  }

  "RetCalc.futureCapital" should {
    "calculate how much savings will be left after having taken a pension for n months" in {
      val actual = RetCalc futureCapital (FixedReturns(0.04), 40 * 12, 0, 2000, 541267.1990)
      val expected = 309867.53176
      actual should ===(expected)
    }
  }

  val params =
    RetCalcParams(nbOfMonthsInRetirement = 40 * 12, netIncome = 3000, currentExpenses = 2000, initialCapital = 10000)

  "RetCalc.simulatePlan" should {
    "calculate the capital at retirement and the capital after death" in {
      val (capitalAtRetirement, capitalAfterDeath) =
        RetCalc simulatePlan (FixedReturns(0.04), 25 * 12, params)
      capitalAtRetirement should ===(541267.1990)
      capitalAfterDeath should ===(309867.53176)
    }

    "use different returns for capitalisation and drawdown" in {
      val nbOfMonthsSaving = 25 * 12
      val returns = VariableReturns(
        (Vector tabulate (nbOfMonthsSaving + params.nbOfMonthsInRetirement)) { x =>
          if (x < nbOfMonthsSaving)
            VariableReturn(x.toString, 0.04 / 12)
          else
            VariableReturn(x.toString, 0.03 / 12)
        }
      )
      val (capitalAtRetirement, capitalAfterDeath) = RetCalc simulatePlan (returns, nbOfMonthsSaving, params)
      capitalAtRetirement should ===(541267.1990)
      capitalAfterDeath should ===(-57737.7227)
    }
  }

  "RetCalc.nbOfMonthsSaving" should {
    "calculate how long I need to save before I can retire" in {
      val actual = RetCalc nbOfMonthsSaving (FixedReturns(0.04), params)
      val expected = 23 * 12 + 1
      actual should ===(expected)
    }

    "not crash if the resulting nbOfMonths is very high" in {
      val actual = RetCalc nbOfMonthsSaving (FixedReturns(0.01), RetCalcParams(40 * 12, 3000, 2999, 0))
      val expected = 8280
      actual should ===(expected)
    }

    "not loop forever if I enter bad parameters" in {
      val actual = RetCalc nbOfMonthsSaving (FixedReturns(0.04), RetCalcParams(40 * 12, 1000, 2000, 10000))
      actual should ===(Int.MaxValue)
    }
  }

  "VariableReturns.fromUntil" should {
    "keep only a window of the returns" in {
      val variableReturns = VariableReturns((Vector tabulate 12) { x =>
        val d = (x + 1).toDouble
        VariableReturn(f"2017.$d%02.0f", d)
      })

      (variableReturns fromUntil ("2017.07", "2017.09")).returns should ===(
        Vector(
          VariableReturn("2017.07", 7.0),
          VariableReturn("2017.08", 8.0)
        )
      )

      (variableReturns fromUntil ("2017.10", "2018.01")).returns should ===(
        Vector(
          VariableReturn("2017.10", 10.0),
          VariableReturn("2017.11", 11.0),
          VariableReturn("2017.12", 12.0)
        )
      )
    }
  }

}
