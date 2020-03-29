name := "finance"

version := "0.0.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-language:postfixOps"
)

val CatsVersion = "2.1.0"
val CatsEffectVersion = "2.0.0"
val CirceVersion = "0.12.3"
val Http4sVersion = "0.21.1"
val NScalaTimeVersion = "2.22.0"
val ScalaMockVersion = "4.4.0"
val ScalaTestVersion = "3.1.1"

libraryDependencies ++= Seq(
    "com.github.nscala-time"  %% "nscala-time"          % NScalaTimeVersion,
    "io.circe"                %% "circe-generic"        % CirceVersion,
    "org.http4s"              %% "http4s-blaze-client"  % Http4sVersion,
    "org.http4s"              %% "http4s-blaze-server"  % Http4sVersion,
    "org.http4s"              %% "http4s-circe"         % Http4sVersion,
    "org.http4s"              %% "http4s-dsl"           % Http4sVersion,
    "org.scalamock"           %% "scalamock"            % ScalaMockVersion % Test,
    "org.scalatest"           %% "scalatest"            % ScalaTestVersion % Test,
    "org.typelevel"           %% "cats-core"            % CatsVersion,
    "org.typelevel"           %% "cats-effect"          % CatsEffectVersion
)


coverageEnabled := true
coverageMinimum := 100
coverageFailOnMinimum := true
coverageExcludedPackages := "com.finance.service.endpoints.*"

guardrailTasks in Compile := List(
  ScalaServer(
    specPath = file("../model/dist/model.yaml"),
    pkg = "com.finance.service.endpoints",
    framework = "http4s"
 )
)

