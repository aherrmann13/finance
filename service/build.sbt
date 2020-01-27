name := "finance"

version := "0.0.1"

scalaVersion := "2.13.1"

val CatsVersion = "2.1.0"
val CatsEffectVersion = "2.0.0"
val CirceVersion = "0.12.3"
val Http4sVersion = "0.21.0-M6"
val NScalaTimeVersion = "2.22.0"


libraryDependencies ++= Seq(
    "com.github.nscala-time"  %% "nscala-time"          % NScalaTimeVersion,
    "io.circe"                %% "circe-generic"        % CirceVersion,
    "org.http4s"              %% "http4s-blaze-client"  % Http4sVersion,
    "org.http4s"              %% "http4s-blaze-server"  % Http4sVersion,
    "org.http4s"              %% "http4s-circe"         % Http4sVersion,
    "org.http4s"              %% "http4s-dsl"           % Http4sVersion,
    "org.typelevel"           %% "cats-core"            % CatsVersion,
    "org.typelevel"           %% "cats-effect"          % CatsEffectVersion
)

guardrailTasks in Compile := List(
  ScalaServer(
    specPath = file("../model/dist/model.yaml"),
    pkg = "com.finance.service.endpoints",
    framework = "http4s"
 )
)
 
