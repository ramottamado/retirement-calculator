import Dependencies._

ThisBuild / scalaVersion     := "2.12.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "dev.ramottamado"
ThisBuild / organizationName := "Tamado Sitohang"

lazy val root = (project in file("."))
  .settings(
    name := "RetirementCalculator",
    libraryDependencies ++= Seq(
          scalaTest % Test,
          "org.typelevel" %% "cats-core" % "1.0.1"
        )
  )

scalacOptions += "-Ypartial-unification"

mainClass in Compile := Some("dev.ramottamado.RetirementCalculator.SimulatePlanApp")

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
