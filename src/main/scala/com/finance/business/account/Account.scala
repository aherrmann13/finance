package com.finance.business.account

import com.finance.business.common.HasId

case class Account(id: Int, userId: Int, name: String, description: String, accountType: AccountType) extends HasId;
