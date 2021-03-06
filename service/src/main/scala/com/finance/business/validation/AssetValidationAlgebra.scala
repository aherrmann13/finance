package com.finance.business.validation

import cats.data.EitherT
import com.finance.business.model.asset.{Asset, Stock}
import com.finance.business.validation.errors._

trait AssetValidationAlgebra[F[_]] {
  def idIsNone(asset: Asset): EitherT[F, IdMustBeNone, Unit]

  def exists(asset: Asset): EitherT[F, DoesNotExist, Unit]

  def accountIdExists(asset: Asset): EitherT[F, DoesNotExist, Unit]

  def stockActionsAreValid(stock: Stock): EitherT[F, StockActionsInvalid, Unit]

  def stockActionsAreInOrder(stock: Stock): EitherT[F, StockActionsOutOfOrder, Unit]

}
