package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.model.source.{Source => SourceModel}
import com.finance.business.model.types.Id
import com.finance.business.services.SourceService
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.converters.SourceMapping._
import com.finance.service.endpoints.definitions.{Error, Source, SourceQuery}
import com.finance.service.endpoints.source._

class SourceHandlerImpl[F[_]: Monad](sourceService: SourceService[F]) extends SourceHandler[F] {
  override def createSource(respond: CreateSourceResponse.type)(body: Option[Source]): F[CreateSourceResponse] =
    body.map { b =>
      sourceService
        .create(b.mapTo[SourceModel].copy(id = None))
        .fold[CreateSourceResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Source])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[CreateSourceResponse])

  override def updateSource(
    respond: UpdateSourceResponse.type
  )(id: Int, body: Option[Source]): F[UpdateSourceResponse] =
    body.map { b =>
      sourceService
        .update(b.mapTo[SourceModel].copy(id = Some(Id(id))))
        .fold[UpdateSourceResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Source])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[UpdateSourceResponse])

  override def deleteSource(respond: DeleteSourceResponse.type)(id: Int): F[DeleteSourceResponse] =
    sourceService
      .delete(Id(id))
      .fold[DeleteSourceResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        _ => respond.Ok
      )

  override def getSources(respond: GetSourcesResponse.type)(filter: Option[SourceQuery]): F[GetSourcesResponse] =
    sourceService.get(filter.flatMap(_.search).getOrElse("")).map(s => respond.Ok(s.map(_.mapTo[Source]).toVector))
}
