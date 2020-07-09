package com.finance.business.remotecalls

import java.time.OffsetDateTime

import com.finance.business.model.asset.StockPriceAsOf

trait StockPriceRetriever[F[_]] {
  def call(ticker: String): F[StockPriceAsOf]

  def call(ticker: String, asOf: OffsetDateTime): F[StockPriceAsOf]
}
