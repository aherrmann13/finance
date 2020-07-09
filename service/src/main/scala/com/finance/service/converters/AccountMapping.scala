package com.finance.service.converters

import com.finance.business.model.account.{Bank, Brokerage, Account => AccountModel, AccountType => AccountTypeModel}
import com.finance.business.model.types.{Description, Id, Name, Usd}
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.Account
import com.finance.service.endpoints.definitions.Account.AccountType

object AccountMapping {
  private implicit val accountTypeRequestMapping: Mapping[Account.AccountType, AccountTypeModel] = {
    case AccountType.members.Bank => Bank
    case AccountType.members.Brokerage => Brokerage
  }
  private implicit val accountTypeResponseMapping: Mapping[AccountTypeModel, Account.AccountType] = {
    case Bank => AccountType.members.Bank
    case Brokerage => AccountType.members.Brokerage
  }
  implicit val accountRequestMapping: Mapping[Account, AccountModel] = (a: Account) =>
    AccountModel(
      id = Some(Id(a.id)),
      name = Name(a.name),
      description = Description(a.description),
      accountType = a.accountType.mapTo[AccountTypeModel],
      initialAmount = Usd(a.initialAmount)
    )

  implicit val accountResponseMapping: Mapping[AccountModel, Account] = (a: AccountModel) =>
    Account(
      id = a.id.map(_.value).getOrElse(-1), // TODO: map to -1?  feels 'optional'
      name = a.name.value,
      description = a.description.value,
      accountType = a.accountType.mapTo[Account.AccountType],
      initialAmount = a.initialAmount.value
    )
}
