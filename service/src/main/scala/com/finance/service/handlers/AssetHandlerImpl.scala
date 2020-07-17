package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.services.AssetService
import com.finance.business.model.asset.{Stock => StockModel}
import com.finance.business.model.types.Id
import com.finance.service.endpoints.asset.{
  AssetHandler,
  CreateAssetResponse,
  DeleteAssetResponse,
  GetStockValueResponse,
  UpdateAssetResponse
}
import com.finance.service.endpoints.definitions.{Asset, Error, StockValue}
import com.finance.service.converters.AssetMapping._
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._

class AssetHandlerImpl[F[_]: Monad](
  assetService: AssetService[F]
) extends AssetHandler[F] {
  override def createAsset(respond: CreateAssetResponse.type)(body: Option[Asset]): F[CreateAssetResponse] =
    body
      .flatMap { b =>
        b.stock.map { s =>
          assetService
            .create(s.mapTo[StockModel].copy(id = None))
            .fold[CreateAssetResponse](
              e => respond.BadRequest(e.mapTo[Error]),
              r => respond.Ok(r.mapTo[Asset])
            )
        }
      }
      .getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[CreateAssetResponse])

  override def updateAsset(respond: UpdateAssetResponse.type)(id: Int, body: Option[Asset]): F[UpdateAssetResponse] =
    body
      .flatMap { b =>
        b.stock.map { s =>
          assetService
            .update(s.mapTo[StockModel].copy(id = Some(Id(id))))
            .fold[UpdateAssetResponse](
              e => respond.BadRequest(e.mapTo[Error]),
              r => respond.Ok(r.mapTo[Asset])
            )
        }
      }
      .getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[UpdateAssetResponse])

  override def deleteAsset(respond: DeleteAssetResponse.type)(id: Int): F[DeleteAssetResponse] =
    assetService
      .delete(Id(id))
      .fold[DeleteAssetResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        _ => respond.Ok
      )

  override def getStockValue(respond: GetStockValueResponse.type)(): F[GetStockValueResponse] =
    assetService.getStockValue.map { stocks =>
      GetStockValueResponse.Ok(stocks.map(_.mapTo[StockValue]).toVector)
    }
}
