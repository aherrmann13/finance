package com.finance.business.model.category

import com.finance.business.model.types._

case class Budget(effectiveTime: Seq[DateRange], amount: Usd)
