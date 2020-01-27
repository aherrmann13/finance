package com.finance.business.model.account

import com.finance.business.model.types._

case class Account(
    id: Id,
    name: Name,
    description: Description,
    accountType: AccountType
)
