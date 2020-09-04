package com.finance.business.model.reporting

import com.finance.business.model.types.{Id, Usd}

case class AccountBalance(
  accountId: Id,
  balance: Usd
)
