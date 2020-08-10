package dev.ramottamado.RetirementCalculator

import scala.io.Source
import scala.annotation.meta.field

case class EquityData(monthId: String, value: Double, annualDividend: Double) {
  val monthlyDividend = annualDividend / 12
}

object EquityData {

  def fromResource(resource: String): Vector[EquityData] =
    ((Source fromResource resource).getLines drop 1).map { line =>
      val fields = line split "\t"
      EquityData(fields(0), fields(1).toDouble, fields(2).toDouble)
    }.toVector

}
