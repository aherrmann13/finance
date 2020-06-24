package com.finance.business.remotecalls

import com.finance.business.model.asset.StockPriceAsOf
import com.github.nscala_time.time.Imports._


trait StockPriceRetriever[F[_]] {
  def call(ticker: String): F[StockPriceAsOf]

  def call(ticker: String, asOf: DateTime): F[StockPriceAsOf]
}
