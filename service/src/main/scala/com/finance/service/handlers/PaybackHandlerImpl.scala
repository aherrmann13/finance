package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.model.payback.{Payback => PaybackModel}
import com.finance.business.model.types.{DateRange, Id}
import com.finance.business.services.PaybackService
import com.finance.service.converters.CommonMapping._
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.converters.PaybackMapping._
import com.finance.service.endpoints.definitions.{Error, Payback, PaybackBalance, PaybackBalanceQuery}
import com.finance.service.endpoints.payback._

class PaybackHandlerImpl[F[_]: Monad](
  paybackService: PaybackService[F]
) extends PaybackHandler[F] {
  override def createPayback(respond: CreatePaybackResponse.type)(body: Option[Payback]): F[CreatePaybackResponse] =
    body.map { b =>
      paybackService
        .create(b.mapTo[PaybackModel].copy(id = None))
        .fold[CreatePaybackResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Payback])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[CreatePaybackResponse])

  override def updatePayback(
    respond: UpdatePaybackResponse.type
  )(id: Int, body: Option[Payback]): F[UpdatePaybackResponse] =
    body.map { b =>
      paybackService
        .update(b.mapTo[PaybackModel].copy(id = Some(Id(id))))
        .fold[UpdatePaybackResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Payback])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[UpdatePaybackResponse])

  override def deletePayback(respond: DeletePaybackResponse.type)(id: Int): F[DeletePaybackResponse] =
    paybackService
      .delete(Id(id))
      .fold[DeletePaybackResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        _ => respond.Ok
      )

  override def getPaybackBalance(respond: GetPaybackBalanceResponse.type)(
    query: PaybackBalanceQuery
  ): F[GetPaybackBalanceResponse] =
    paybackService.getPaybackBalance(query.range.mapTo[DateRange]) map { balance =>
      respond.Ok(balance.map(_.mapTo[PaybackBalance]).toVector)
    }

  override def getPaybackBalanceSummary(
    respond: GetPaybackBalanceSummaryResponse.type
  )(): F[GetPaybackBalanceSummaryResponse] =
    paybackService.getPaybackBalanceSummary map { balance => respond.Ok(balance.value) }
}
