package com.finance.business.services

import cats.Monad
import cats.data.{EitherT, OptionT}
import cats.implicits._
import com.finance.business.model.asset.{Asset, Stock, StockValue}
import com.finance.business.model.types.Id
import com.finance.business.operations.StockOps._
import com.finance.business.remotecalls.StockPriceRetriever
import com.finance.business.repository.AssetRepository
import com.finance.business.repository.query.StockQuery
import com.finance.business.validation.AssetValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class AssetService[F[_]: Monad](
  validator: AssetValidationAlgebra[F],
  repository: AssetRepository[F],
  stockPriceRetriever: StockPriceRetriever[F]
) extends CommandService[F, Asset]
    with QueryService[F, Asset] {
  override def create(model: Asset): EitherT[F, ValidationError, Asset] =
    for {
      _ <- validator idIsNone model
      _ <- validator accountIdExists model
      _ <- model match {
        case stock: Stock => validator stockActionsAreValid stock
      }
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Asset): EitherT[F, ValidationError, Asset] =
    for {
      _ <- validator exists model
      _ <- validator accountIdExists model
      _ <- model match {
        case stock: Stock => validator stockActionsAreValid stock
      }
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    EitherT.liftF(repository.delete(id))

  override def get(id: Id): OptionT[F, Asset] = repository.get(id)

  override def getMany(ids: Seq[Id]): F[Seq[Asset]] = repository.getMany(ids)

  override def getAll: F[Seq[Asset]] = repository.getAll

  def getStockValue: F[Seq[StockValue]] =
    repository.getAllStocks flatMap {
      _.toList.traverse(stock => stockPriceRetriever.call(stock.ticker) map (stock withPrice _))
    } map {
      _.toSeq
    }

  def getStocks(query: StockQuery): F[Seq[Stock]] = repository.getStocks(query)
}
