package com.finance.business.model.asset

import com.finance.business.model.base.Model
import com.finance.business.model.types._
import com.github.nscala_time.time.Imports._

case class Asset(
    id: Option[Id],
    action: Action,
    units: Float,
    amount: Usd,
    date: DateTime,
    metadata: StockMetadata // when another type of asset is introduced this should be a union type
) extends Model
