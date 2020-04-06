package com.finance.business.model.account

import com.finance.business.model.base.Model
import com.finance.business.model.types._

case class Account(
    id: Option[Id],
    name: Name,
    description: Description,
    accountType: AccountType
) extends Model
