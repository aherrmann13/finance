package com.finance.business.model.account

import com.finance.business.common.{HasId, HasUserId}

object Account {
  def apply(userId: Int, name: String, description: String, accountType: AccountType) =
    new Account(Option.empty, userId, name, description, accountType)
}

case class Account(id: Option[Int], userId: Int, name: String, description: String, accountType: AccountType)
  extends HasId with HasUserId;
