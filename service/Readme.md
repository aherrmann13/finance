## Service

#### Targets

| Commands  | Description  |
|-----------|--------------|
| `clean`   | deletes all generated files in target dir |  
| `compile` | compiles all main sources |  
| `test`    | compiles and runs all tests  |  
| `coverageReport` | generate coverage reports |

#### libraries used

[cats](https://github.com/typelevel/cats)  
[cats-effect](https://github.com/typelevel/cats-effect)  
[circe](https://github.com/circe/circe)  
[http4s](https://github.com/http4s/http4s)  
[nscala-time](https://github.com/nscala-time/nscala-time)  
[sbt-guardrail](https://github.com/twilio/sbt-guardrail)  
[sbt-scalafmt](https://github.com/scalameta/sbt-scalafmt)  
[sbt-scoverage](https://github.com/scoverage/sbt-scoverage)  
[scalamock](https://github.com/paulbutcher/ScalaMock)    
[scalatest](https://github.com/scalatest/scalatest)  

#### typeclasses

maybe move to [this lib](https://github.com/typelevel/simulacrum) in the future but for now to reduce complexity
and to get used to how the scala implementation works im going to code the typeclasses by hand