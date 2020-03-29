package com.finance.business.model.account

import com.finance.business.model.base.Model
import com.finance.business.model.types._

object Account {
  def apply(name: Name, description: Description, accountType: AccountType): Account =
    new Account(None, name, description, accountType)

  def apply(id: Id, name: Name, description: Description, accountType: AccountType): Account =
    new Account(Some(id), name, description, accountType)
}

case class Account(
    id: Option[Id],
    name: Name,
    description: Description,
    accountType: AccountType
) extends Model
