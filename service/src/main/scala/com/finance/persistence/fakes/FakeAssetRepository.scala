package com.finance.persistence.fakes

import cats.Monad
import cats.implicits.catsSyntaxApplicativeId
import com.finance.business.model.asset.{Asset, Stock}
import com.finance.business.model.types.Id
import com.finance.business.repository.AssetRepository
import com.finance.business.repository.query.StockQuery

class FakeAssetRepository[F[_]: Monad] extends FakeRepository[F, Asset] with AssetRepository[F] {
  override def anyWithAccountId(accountId: Id): F[Boolean] = true.pure[F]

  override def getAllStocks: F[Seq[Stock]] = Seq.empty[Stock].pure[F]

  override def getStocks(query: StockQuery): F[Seq[Stock]] = Seq.empty[Stock].pure[F]
}
