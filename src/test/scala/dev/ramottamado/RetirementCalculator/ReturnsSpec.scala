package dev.ramottamado.RetirementCalculator

import org.scalactic.{ Equality, TolerantNumerics, TypeCheckedTripleEquals }
import org.scalatest.{ Matchers }
import org.scalatest.wordspec.AnyWordSpec

class ReturnsSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics tolerantDoubleEquality 0.001

  "Returns.monthlyRate" should {
    "return a fixed rate for a FixedReturn" in {
      (Returns monthlyRate (FixedReturns(0.04), 0)) should ===(0.04 / 12)
      (Returns monthlyRate (FixedReturns(0.04), 10)) should ===(0.04 / 12)
    }

    val variableReturns = VariableReturns(
      Vector(
        VariableReturn("2000.01", 0.1),
        VariableReturn("2000.02", 0.2)
      )
    )

    "return the nth rate for VariableReturn" in {
      (Returns monthlyRate (variableReturns, 0)) should ===(0.1)
      (Returns monthlyRate (variableReturns, 1)) should ===(0.2)
    }

    "roll over from the first rate if n > length" in {
      (Returns monthlyRate (variableReturns, 2)) should ===(0.1)
      (Returns monthlyRate (variableReturns, 3)) should ===(0.2)
      (Returns monthlyRate (variableReturns, 4)) should ===(0.1)
    }

    "return the n+offset-th rate for OffsetReturn" in {
      val returns = OffsetReturns(variableReturns, 1)
      (Returns monthlyRate (returns, 0)) should ===(0.2)
    }
  }

  "Returns.fromEquityAndInflationData" should {
    "compute real total returns from equity and inflation data" in {
      val equities = Vector(
        EquityData("2117.01", 100.0, 10.0),
        EquityData("2117.02", 101.0, 12.0),
        EquityData("2117.03", 102.0, 12.0)
      )

      val inflations = Vector(
        InflationData("2117.01", 100.0),
        InflationData("2117.02", 102.0),
        InflationData("2117.03", 102.0)
      )

      val returns = Returns.fromEquityandInflationData(equities, inflations)

      returns should ===(
        VariableReturns(
          Vector(
            VariableReturn("2117.02", (101.0 + 12.0 / 12) / 100.0 - 102.0 / 100.0),
            VariableReturn("2117.03", (102.0 + 12.0 / 12) / 101.0 - 102.0 / 102.0)
          )
        )
      )
    }
  }
}
