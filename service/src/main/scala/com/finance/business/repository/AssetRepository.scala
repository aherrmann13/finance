package com.finance.business.repository

import com.finance.business.model.asset.{Asset, Stock}
import com.finance.business.model.types.Id

trait AssetRepository[F[_]] extends Repository[F, Asset] {
  def anyWithAccountId(accountId: Id): F[Boolean]

  def getAllStocks: F[Seq[Stock]]
}