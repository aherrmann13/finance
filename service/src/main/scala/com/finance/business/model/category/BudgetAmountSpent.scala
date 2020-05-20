package com.finance.business.model.category

import com.finance.business.model.types.{DateRange, Usd}

case class BudgetAmountSpent(effectiveTime: Seq[DateRange], in: Usd, out: Usd, amount: Usd)
