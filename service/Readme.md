## Service

#### Libraries used

[cats](https://github.com/typelevel/cats)  
[cats-effect](https://github.com/typelevel/cats-effect)  
[circe](https://github.com/circe/circe)  
[http4s](https://github.com/http4s/http4s)  
[nscala-time](https://github.com/nscala-time/nscala-time)  
[sbt-guardrail](https://github.com/twilio/sbt-guardrail)  
[sbt-scalafmt](https://github.com/scalameta/sbt-scalafmt/releases)  


#### Notes

<details>
  <summary>not using the latest version of http4s</summary>
  <p>
    as of <a href="https://http4s.org/changelog/">v0.21.0-RC1 (2020-01-21)</a> the <code>EntityDecoder</code> signature
    for decode looks like this <code>def decode(m: Media[F], strict: Boolean): DecodeResult[F, T]</code> and the
    guardrail generator expects it to use <code>Message</code> instead of <code>Media</code>.  Issue being tracked
    <a href="https://github.com/twilio/guardrail/issues/529">here</a>
  </p>
</details>
