package dev.ramottamado.RetirementCalculator

import scala.io.Source

case class InflationData(monthId: String, value: Double)

object InflationData {

  def fromResource(resource: String): Vector[InflationData] = {
    ((Source fromResource resource).getLines drop 1).map { line =>
      val fields = line split "\t"
      InflationData(fields(0), fields(1).toDouble)
    }.toVector
  }

}
