package com.finance.business.account

import com.finance.business.common.HasId

object Account {
  // kindof arbitrary lengths
  def maxNameLength = 128
  def maxDescriptionLength = 512

  def apply(userId: Int, name: String, description: String, accountType: AccountType) =
    new Account(Option.empty, userId, name, description, accountType)
}

case class Account(id: Option[Int], userId: Int, name: String, description: String, accountType: AccountType)
    extends HasId;
