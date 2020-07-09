## Service

#### Targets

| Commands  | Description  |
|-----------|--------------|
| `clean`   | deletes all generated files in target dir |  
| `compile` | compiles all main sources |  
| `test`    | compiles and runs all tests  |  
| `scalafmtCheck` | checks to see if all files in main/ are formatted correctly |
| `test:scalafmtCheck` | checks to see if all files in test/ are formatted correctly |
| `scalafmt` | formats all files in main/ |
| `test:scalafmt` | formats all files in test/ |
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
[scalafmt](https://github.com/scalameta/scalafmt)  (in `.scalafmt.conf`)  
[scalamock](https://github.com/paulbutcher/ScalaMock)    
[scalatest](https://github.com/scalatest/scalatest)  
#### notes

<details>
  <summary>typeclasses</summary>
  <p>
    maybe move to <a href="https://github.com/typelevel/simulacrum">this lib</a> in the future but for now to reduce
    complexity and to get used to how the scala implementation works im going to code the typeclasses by hand
  </p> 
</details>  

<details>
  <summary>scalamock complaining about mocking classes</summary>
  <p>
    according to the <a href="https://github.com/paulbutcher/ScalaMock/issues/56">github issue here</a> this should be
    possible but I was getting errors
<pre><code>[error] /home/adam/code/finance/service/src/test/scala/com/finance/service/converters/AccountHandlerImplSpec.scala:25:40: type mismatch;
[error]  found   : com.finance.business.validation.AccountValidationAlgebra[F]
[error]  required: com.finance.business.validation.AccountValidationAlgebra[cats.Id]
[error]   private val mockAccountService = stub[AccountService[IdMonad]]
</code></pre>
    when I tried to compile without extending the Service class to a test class with ctor parameters
    passed in
  </p>
</details>  