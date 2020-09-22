package com.finance.service

import cats.effect._
import cats.implicits._
import com.finance.service.config.{HttpAppBuilder, ServiceConfig}
import io.circe.config.parser
import io.circe.generic.auto._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{Server => H4Server}

object Server extends IOApp {
  private def createServer[F[_]: ConcurrentEffect: Timer]: Resource[F, H4Server[F]] =
    for {
      conf <- Resource.liftF(parser.decodePathF[F, ServiceConfig]("service"))
      baseHttpApp = HttpAppBuilder.build
      server <-
        BlazeServerBuilder[F]
          .bindHttp(conf.port, conf.host)
          .withHttpApp(baseHttpApp)
          .resource
    } yield server

  override def run(args: List[String]): IO[ExitCode] = createServer[IO].use(_ => IO.never).as(ExitCode.Success)
}
