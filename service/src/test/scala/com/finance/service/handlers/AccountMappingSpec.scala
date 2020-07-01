package com.finance.service.handlers

import com.finance.business.model.account.{Bank, Brokerage, Account => AccountModel}
import com.finance.business.model.types.{Description, Id, Name, Usd}
import com.finance.service.converters.AccountMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.Account
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AccountMappingSpec extends AnyFreeSpec with Matchers {
  "AccountMapping" - {
    "should contain implicits that" - {
      "map account request to account model with account type bank" in {
        Account(5, "name", "desc", Account.AccountType.Bank, 150).mapTo[AccountModel] shouldEqual
          AccountModel(Some(Id(5)), Name("name"), Description("desc"), Bank, Usd(150))
      }
      "map account request to account model with account type brokerage" in {
        Account(5, "name", "desc", Account.AccountType.Brokerage, 150).mapTo[AccountModel] shouldEqual
          AccountModel(Some(Id(5)), Name("name"), Description("desc"), Brokerage, Usd(150))
      }
      "map account model to response with account type bank" in {
        AccountModel(Some(Id(5)), Name("name"), Description("desc"), Bank, Usd(150)).mapTo[Account] shouldEqual
          Account(5, "name", "desc", Account.AccountType.Bank, 150)
      }
      "map account model to response with account type brokerage" in {
        AccountModel(Some(Id(5)), Name("name"), Description("desc"), Brokerage, Usd(150)).mapTo[Account] shouldEqual
          Account(5, "name", "desc", Account.AccountType.Brokerage, 150)
      }
    }
  }
}
