package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.model.transfer.{Transfer => TransferModel}
import com.finance.business.model.types.Id
import com.finance.business.services.TransferService
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.converters.TransferMapping._
import com.finance.service.endpoints.definitions.{Error, Transfer}
import com.finance.service.endpoints.transfer.{
  CreateTransferResponse,
  DeleteTransferResponse,
  TransferHandler,
  UpdateTransferResponse
}

class TransferHandlerImpl[F[_]: Monad](transferService: TransferService[F]) extends TransferHandler[F] {
  override def createTransfer(respond: CreateTransferResponse.type)(body: Option[Transfer]): F[CreateTransferResponse] =
    body.map { b =>
      transferService
        .create(b.mapTo[TransferModel].copy(id = None))
        .fold[CreateTransferResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Transfer])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[CreateTransferResponse])

  override def updateTransfer(
    respond: UpdateTransferResponse.type
  )(id: Int, body: Option[Transfer]): F[UpdateTransferResponse] =
    body.map { b =>
      transferService
        .update(b.mapTo[TransferModel].copy(id = Some(Id(id))))
        .fold[UpdateTransferResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Transfer])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[UpdateTransferResponse])

  override def deleteTransfer(respond: DeleteTransferResponse.type)(id: Int): F[DeleteTransferResponse] =
    transferService
      .delete(Id(id))
      .fold[DeleteTransferResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        _ => respond.Ok
      )
}
