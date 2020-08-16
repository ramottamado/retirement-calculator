package dev.ramottamado.RetirementCalculator

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SimulatePlanAppIT extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
  "SimulatePlanApp.strMain" should {
    "simulate a retirement plan" in {
      val actualResult = SimulatePlanApp.strMain(
        Array("1997.09,2017.09", "25", "40", "3000", "2000", "10000")
      )

      val expectedResult =
        s"""Capital after 25 years of savings: 499923
           |Capital after 40 years in retirement: 586435
           |""".stripMargin

      actualResult should ===(expectedResult)
    }
  }
}
