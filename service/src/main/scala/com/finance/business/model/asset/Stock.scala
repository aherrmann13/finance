package com.finance.business.model.asset

import com.finance.business.model.base.Model
import com.finance.business.model.types.Id

case class Stock(id: Option[Id], ticker: String, actions: Seq[StockAction]) extends Asset with Model
