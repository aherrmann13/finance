package com.finance.business.validation

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.account.{Account, Bank, Brokerage}
import com.finance.business.model.types.{Description, Id, ModelName, Name}
import com.finance.business.repository.{AccountRepository, AssetRepository, PaybackRepository, TransactionRepository}
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AccountValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {

  private val mockAccountRepository = stub[AccountRepository[IdMonad]]
  private val mockTransactionRepository = stub[TransactionRepository[IdMonad]]
  private val mockAssetRepository = stub[AssetRepository[IdMonad]]
  private val mockPaybackRepository = stub[PaybackRepository[IdMonad]]

  private val accountValidationInterpreter = new AccountValidationInterpreter[IdMonad](
    mockAccountRepository,
    mockTransactionRepository,
    mockAssetRepository,
    mockPaybackRepository
  )

  private val fakeAccountWithId = Account(Some(Id(4)), Name("Name"), Description("Description"), Bank)
  private val fakeAccountWithNoId = fakeAccountWithId.copy(id = None)
  private val accountName = ModelName("Account")

  "AccountValidationInterpreter" - {
    "idIsNone" - {
      "should return Left(IdMustBeNone) when id is Some" in {
        accountValidationInterpreter.idIsNone(fakeAccountWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](IdMustBeNone(accountName)).value
      }
      "should return Right(()) when id is None" in {
        accountValidationInterpreter.idIsNone(fakeAccountWithNoId).value shouldEqual
          EitherT.rightT[IdMonad, IdMustBeNone](()).value
      }
    }
    "exists" - {
      "should return Left(DoesNotExist) when id is None" in {
        accountValidationInterpreter.exists(fakeAccountWithNoId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(accountName)).value
      }
      "should return Left(DoesNotExist) when repository does not contain Account" in {
        (mockAccountRepository get _).when(fakeAccountWithId.id.get).returns(OptionT.none)
        accountValidationInterpreter.exists(fakeAccountWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(accountName, fakeAccountWithId.id)).value
      }
      "should return Right(()) when repository contains Account" in {
        (mockAccountRepository get _).when(fakeAccountWithId.id.get).returns(OptionT.pure(fakeAccountWithId))
        accountValidationInterpreter.exists(fakeAccountWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "nameIsValid" - {
      "should return Left(NameIsTooLong) when name is too long" in {
        val name = Name((0 to 128).map(_ => "a").fold("")(_ + _))
        accountValidationInterpreter.nameIsValid(fakeAccountWithId.copy(name = name)).value shouldEqual
          EitherT.leftT[IdMonad, Unit](NameTooLong(accountName, name)).value
      }
      "should return Right(()) when name is correct length" in {
        val name = Name((0 to 127).map(_ => "a").fold("")(_ + _))
        accountValidationInterpreter.nameIsValid(fakeAccountWithId.copy(name = name)).value shouldEqual
          EitherT.rightT[IdMonad, NameTooLong](()).value
      }
    }
    "descriptionIsValid" - {
      "should return Left(DescriptionIsTooLong) when description is too long" in {
        val desc = Description((0 to 512).map(_ => "a").fold("")(_ + _))
        accountValidationInterpreter.descriptionIsValid(fakeAccountWithId.copy(description = desc)).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DescriptionTooLong(accountName, desc)).value
      }
      "should return Right(()) when description is correct length" in {
        val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))
        accountValidationInterpreter.descriptionIsValid(fakeAccountWithId.copy(description = desc)).value shouldEqual
          EitherT.rightT[IdMonad, DescriptionTooLong](()).value
      }
    }
    "accountTypeIsValid" - {
      "if account type is Bank" - {
        "should return Left(BankCantHaveAssets) if there are assets with account id" in {
          (mockAssetRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(true.pure[IdMonad])
          accountValidationInterpreter.accountTypeIsValid(fakeAccountWithId.copy(accountType = Bank)).value shouldEqual
            EitherT.leftT[IdMonad, Unit](BankCantHaveAssets).value
        }
        "should return Right(()) if there are no assets with account id" in {
          (mockAssetRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(false.pure[IdMonad])
          accountValidationInterpreter.accountTypeIsValid(fakeAccountWithId.copy(accountType = Bank)).value shouldEqual
            EitherT.rightT[IdMonad, AccountTypeInvalid](()).value
        }
      }
      "if account type is Brokerage" - {
        "should return Left(BrokerageCantHaveTransactions) if there are transactions with account id" in {
          (mockTransactionRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(true.pure[IdMonad])
          accountValidationInterpreter.accountTypeIsValid(fakeAccountWithId.copy(accountType = Brokerage)).value shouldEqual
            EitherT.leftT[IdMonad, Unit](BrokerageCantHaveTransactions).value
        }
        "should return Left(BrokerageCantHavePaybacks) if there are paybacks with account id" in {
          (mockTransactionRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(false.pure[IdMonad])
          (mockPaybackRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(true.pure[IdMonad])
          accountValidationInterpreter.accountTypeIsValid(fakeAccountWithId.copy(accountType = Brokerage)).value shouldEqual
            EitherT.leftT[IdMonad, Unit](BrokerageCantHavePaybacks).value
        }
        "should return Right(()) if there are are no transactions or paybacks with account id" in {
          (mockTransactionRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(false.pure[IdMonad])
          (mockPaybackRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(false.pure[IdMonad])
          accountValidationInterpreter.accountTypeIsValid(fakeAccountWithId.copy(accountType = Brokerage)).value shouldEqual
            EitherT.rightT[IdMonad, AccountTypeInvalid](()).value
        }
        "should return Right(()) when account id is None" in {
          accountValidationInterpreter.accountTypeIsValid(fakeAccountWithNoId).value shouldEqual
            EitherT.rightT[IdMonad, AccountTypeInvalid](()).value
        }
      }
    }
    "hasNoTransactions" - {
      "should return Left(HasTransactions) if there are transactions with account id" in {
        (mockTransactionRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(true.pure[IdMonad])
        accountValidationInterpreter.hasNoTransactions(fakeAccountWithId.id.get).value shouldEqual
          EitherT.leftT[IdMonad, Unit](HasTransactions(accountName)).value
      }
      "should return Right(()) if there are no transactions with no account id" in {
        (mockTransactionRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(false.pure[IdMonad])
        accountValidationInterpreter.hasNoTransactions(fakeAccountWithId.id.get).value shouldEqual
          EitherT.rightT[IdMonad, HasTransactions](()).value
      }
    }
    "hasNoAssets" - {
      "should return Left(HasAssets) if there are assets with account id" in {
        (mockAssetRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(true.pure[IdMonad])
        accountValidationInterpreter.hasNoAssets(fakeAccountWithId.id.get).value shouldEqual
          EitherT.leftT[IdMonad, Unit](HasAssets(accountName)).value
      }
      "should return Right(()) if there are no assets with account id" in {
        (mockAssetRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(false.pure[IdMonad])
        accountValidationInterpreter.hasNoAssets(fakeAccountWithId.id.get).value shouldEqual
          EitherT.rightT[IdMonad, HasAssets](()).value
      }
    }
    "hasNoPaybacks" - {
      "should return Left(HasPaybacks) if there are paybacks with account id" in {
        (mockPaybackRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(true.pure[IdMonad])
        accountValidationInterpreter.hasNoPaybacks(fakeAccountWithId.id.get).value shouldEqual
          EitherT.leftT[IdMonad, Unit](HasPaybacks(accountName)).value
      }
      "should return Right(()) if there are no paybacks with account id" in {
        (mockPaybackRepository anyWithAccountId _).when(fakeAccountWithId.id.get).returns(false.pure[IdMonad])
        accountValidationInterpreter.hasNoPaybacks(fakeAccountWithId.id.get).value shouldEqual
          EitherT.rightT[IdMonad, HasPaybacks](()).value
      }
    }
  }
}