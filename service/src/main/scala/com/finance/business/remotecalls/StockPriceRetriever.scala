package com.finance.business.remotecalls

import com.finance.business.model.asset.StockPriceAsOf

trait StockPriceRetriever[F[_]] {
  def call(ticker: String): F[StockPriceAsOf]
}
