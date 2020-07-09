package com.finance.business.repository

import com.finance.business.model.asset.{Asset, Stock}
import com.finance.business.model.types.Id
import com.finance.business.repository.query.StockQuery

trait AssetRepository[F[_]] extends Repository[F, Asset] {
  def anyWithAccountId(accountId: Id): F[Boolean]

  def getAllStocks: F[Seq[Stock]]

  def getStocks(query: StockQuery): F[Seq[Stock]]
}
