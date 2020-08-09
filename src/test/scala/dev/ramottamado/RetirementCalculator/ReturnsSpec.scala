package dev.ramottamado.RetirementCalculator

import org.scalactic.{ Equality, TolerantNumerics, TypeCheckedTripleEquals }
import org.scalatest.{ Matchers }
import org.scalatest.wordspec.AnyWordSpec

class ReturnsSpec extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics tolerantDoubleEquality 0.001

  "Returns.monthlyRate" should {
    "return a fixed rate for a FixedReturn" in {
      Returns.monthlyRate(FixedReturns(0.04), 0) should ===(0.04 / 12)
      Returns.monthlyRate(FixedReturns(0.04), 10) should ===(0.04 / 12)
    }

    val variableReturns = VariableReturns(
      Vector(
        VariableReturn("2000.01", 0.1),
        VariableReturn("2000.02", 0.2)
      )
    )

    "return the nth rate for VariableReturn" in {
      Returns.monthlyRate(variableReturns, 0) should ===(0.1)
      Returns.monthlyRate(variableReturns, 1) should ===(0.2)
    }

    "roll over from the first rate if n > length" in {
      Returns.monthlyRate(variableReturns, 2) should ===(0.1)
      Returns.monthlyRate(variableReturns, 3) should ===(0.2)
      Returns.monthlyRate(variableReturns, 4) should ===(0.1)
    }
  }
}
