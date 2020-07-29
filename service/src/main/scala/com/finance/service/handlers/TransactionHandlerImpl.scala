package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.model.transaction.{Transaction => TransactionModel}
import com.finance.business.model.types.Id
import com.finance.business.services.TransactionService
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.converters.TransactionMapping._
import com.finance.service.endpoints.definitions.{Error, Transaction}
import com.finance.service.endpoints.transaction.{
  CreateTransactionResponse,
  DeleteTransactionResponse,
  TransactionHandler,
  UpdateTransactionResponse
}

class TransactionHandlerImpl[F[_]: Monad](transactionService: TransactionService[F]) extends TransactionHandler[F] {
  override def createTransaction(respond: CreateTransactionResponse.type)(
    body: Option[Transaction]
  ): F[CreateTransactionResponse] =
    body.map { b =>
      transactionService
        .create(b.mapTo[TransactionModel].copy(id = None))
        .fold[CreateTransactionResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Transaction])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[CreateTransactionResponse])

  override def updateTransaction(
    respond: UpdateTransactionResponse.type
  )(id: Int, body: Option[Transaction]): F[UpdateTransactionResponse] =
    body.map { b =>
      transactionService
        .update(b.mapTo[TransactionModel].copy(id = Some(Id(id))))
        .fold[UpdateTransactionResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Transaction])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[UpdateTransactionResponse])

  override def deleteTransaction(respond: DeleteTransactionResponse.type)(id: Int): F[DeleteTransactionResponse] =
    transactionService
      .delete(Id(id))
      .fold[DeleteTransactionResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        _ => respond.Ok
      )
}
