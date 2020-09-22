package com.finance.persistence.fakes

import java.time.OffsetDateTime

import cats.Monad
import cats.implicits._
import com.finance.business.model.asset.StockPriceAsOf
import com.finance.business.model.types.Usd
import com.finance.business.remotecalls.StockPriceRetriever

class FakeStockPriceRetriever[F[_]: Monad] extends StockPriceRetriever[F] {
  override def call(ticker: String, asOf: OffsetDateTime): F[StockPriceAsOf] =
    StockPriceAsOf(Usd(50), Usd(60), asOf).pure[F]
}
