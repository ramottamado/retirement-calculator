package dev.ramottamado.RetirementCalculator

object SimulatePlanApp extends App {
  println(strMain(args))

  def strMain(args: Array[String]): String = {
    val (from +: until +: Nil) = (args(0) split ",").toList
    val nbOfYearsSaving = args(1).toInt
    val nbOfYearsInRetirement = args(2).toInt

    val allReturns = Returns.fromEquityandInflationData(
      EquityData fromResource "sp500.tsv",
      InflationData fromResource "cpi.tsv"
    )

    val (capitalAtRetirement, capitalAfterDeath) = RetCalc.simulatePlan(
      allReturns fromUntil (from, until),
      nbOfYearsSaving * 12,
      RetCalcParams(nbOfYearsInRetirement * 12, args(3).toInt, args(4).toInt, args(5).toInt)
    )

    s"""Capital after $nbOfYearsSaving years of savings: ${capitalAtRetirement.round}
       |Capital after $nbOfYearsInRetirement years in retirement: ${capitalAfterDeath.round}
       |""".stripMargin
  }

}
