package com.finance.service.handlers

import cats.data.EitherT
import cats.{Id => IdMonad}
import com.finance.business.model.account.{Brokerage, Account => AccountModel}
import com.finance.business.model.types.{Description, Id, Name, Usd}
import com.finance.business.repository.AccountRepository
import com.finance.business.services.AccountService
import com.finance.business.validation.AccountValidationAlgebra
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.account.{
  CreateAccountResponse,
  DeleteAccountResponse,
  GetAllAccountsResponse,
  UpdateAccountResponse
}
import com.finance.service.endpoints.definitions.{Account, Error}
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AccountHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {
  private case object TestError extends ValidationError

  // https://github.com/paulbutcher/ScalaMock/issues/56
  // theoretically this is solved but still throws errors
  private class AccountServiceTest
      extends AccountService[IdMonad](stub[AccountValidationAlgebra[IdMonad]], stub[AccountRepository[IdMonad]])
  private val mockAccountService = stub[AccountServiceTest]

  private val handler = new AccountHandlerImpl(mockAccountService)

  private val account = Account(5, "name", "desc", Account.AccountType.Brokerage, 150)
  private val accountModel = AccountModel(Some(Id(5)), Name("name"), Description("desc"), Brokerage, Usd(150))

  "AccountHandlerImpl" - {
    "createAccount" - {
      "should return CreateAccountResponse.BadRequest on error in service" in {
        (mockAccountService.create _).when(accountModel.copy(id = None)).returns(EitherT.leftT(TestError))

        handler.createAccount(CreateAccountResponse)(Some(account)) shouldEqual
          CreateAccountResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateAccountResponse.BadRequest on null body" in {
        handler.createAccount(CreateAccountResponse)(None) shouldEqual
          CreateAccountResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateAccountResponse.Ok with account on successful create in service" in {
        (mockAccountService.create _).when(accountModel.copy(id = None)).returns(EitherT.rightT(accountModel))

        handler.createAccount(CreateAccountResponse)(Some(account)) shouldEqual
          CreateAccountResponse.Ok(account)
      }
    }
    "updateAccount" - {
      val id = 8
      "should return CreateAccountResponse.BadRequest on error in service" in {
        (mockAccountService.update _).when(accountModel.copy(id = Some(Id(id)))).returns(EitherT.leftT(TestError))

        handler.updateAccount(UpdateAccountResponse)(id, Some(account)) shouldEqual
          UpdateAccountResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateAccountResponse.BadRequest on null body" in {
        handler.updateAccount(UpdateAccountResponse)(id, None) shouldEqual
          UpdateAccountResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateAccountResponse.Ok with account on successful update in service" in {
        (mockAccountService.update _).when(accountModel.copy(id = Some(Id(id)))).returns(EitherT.rightT(accountModel))

        handler.updateAccount(UpdateAccountResponse)(id, Some(account)) shouldEqual
          UpdateAccountResponse.Ok(account)
      }
    }
    "deleteAccount" - {
      val id = 1
      "should return CreateAccountResponse.BadRequest on error in service" in {
        (mockAccountService.delete _).when(Id(id)).returns(EitherT.leftT(TestError))

        handler.deleteAccount(DeleteAccountResponse)(id) shouldEqual
          DeleteAccountResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateAccountResponse.Ok on successful delete" in {
        (mockAccountService.delete _).when(Id(id)).returns(EitherT.rightT(()))

        handler.deleteAccount(DeleteAccountResponse)(id) shouldEqual
          DeleteAccountResponse.Ok
      }
    }
    "getAllAccounts" - {
      "should return CreateAccountResponse.Ok with accounts from service" in {
        (mockAccountService.getAll _).when().returns(Seq(accountModel, accountModel, accountModel))

        handler.getAllAccounts(GetAllAccountsResponse)() shouldEqual
          GetAllAccountsResponse.Ok(Vector(account, account, account))
      }
    }
  }
}
