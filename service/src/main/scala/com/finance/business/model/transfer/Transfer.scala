package com.finance.business.model.transfer

import com.finance.business.model.base.Model
import com.finance.business.model.types._
import com.github.nscala_time.time.Imports._

case class Transfer(
    id: Option[Id],
    from: Id,
    fromDate: DateTime,
    to: Id,
    toDate: DateTime,
    amount: Usd
) extends Model
