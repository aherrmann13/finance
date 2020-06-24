package com.finance.business.model.reporting

import com.finance.business.model.types.{DateRange, Id, Usd}

case class AccountValue(
  dateRange: DateRange,
  accountId: Id,
  value: Usd
)
