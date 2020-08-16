package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.services.ReportingService
import com.finance.business.services.query.{AccountValueQuery => AccountValueQueryModel}
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.converters.ReportingMapping._
import com.finance.service.endpoints.definitions.{AccountValue, AccountValueQuery, Error}
import com.finance.service.endpoints.reporting.{GetAccountValueResponse, GetNetWorthResponse, ReportingHandler}
import com.finance.service.time.TimeProvider

class ReportingHandlerImpl[F[_]: Monad: TimeProvider](reportingService: ReportingService[F])
    extends ReportingHandler[F] {
  override def getAccountValue(respond: GetAccountValueResponse.type)(
    filter: Option[AccountValueQuery]
  ): F[GetAccountValueResponse] =
    filter.map { f =>
      TimeProvider[F].now.flatMap { date =>
        reportingService
          .getAccountValue(f.mapTo[AccountValueQueryModel], date)
          .map[GetAccountValueResponse](x => respond.Ok(x.map(_.mapTo[AccountValue]).toVector))
      }
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[GetAccountValueResponse])

  override def getNetWorth(respond: GetNetWorthResponse.type)(): F[GetNetWorthResponse] =
    TimeProvider[F].now.flatMap { date =>
      reportingService.getNetWorth(date).map(x => respond.Ok(x.value))
    }
}
