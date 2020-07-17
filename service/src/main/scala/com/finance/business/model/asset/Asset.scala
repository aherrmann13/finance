package com.finance.business.model.asset

import com.finance.business.model.base.Model
import com.finance.business.model.types.Id

sealed trait Asset extends Model {
  val accountId: Id
}

case class Stock(id: Option[Id], accountId: Id, ticker: String, actions: Seq[StockAction]) extends Asset
