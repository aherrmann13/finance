name := "finance"

version := "1.0.0"

scalaVersion := "2.12.8"

val CatsVersion            = "1.6.1"
val CatsEffectVersion      = "1.3.1"
val NScalaTimeVersion      = "2.22.0"
val ScalaMockVersion       = "4.1.0"
val ScalaTestVersion       = "3.0.7"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % NScalaTimeVersion,
  "org.typelevel" %% "cats-core" % CatsVersion,
  "org.typelevel" %% "cats-effect" % CatsEffectVersion,
  "org.scalamock" %% "scalamock" % ScalaMockVersion % Test,
  "org.scalatest" %% "scalatest" % ScalaTestVersion % Test

)

coverageEnabled := true

coverageMinimum := 100

coverageFailOnMinimum := true

scalacOptions ++= Seq(
  "-language:higherKinds"
)