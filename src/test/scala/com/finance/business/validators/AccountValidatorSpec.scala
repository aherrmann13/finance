package com.finance.business.validators

import cats.effect.IO
import com.finance.business.common.Constants._
import com.finance.business.errors._
import com.finance.business.model.account._
import com.finance.business.model.transaction.TransactionRepository
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class AccountValidatorSpec extends FreeSpec with Matchers with MockFactory {

  private val accountRepository = stub[AccountRepository[IO]]
  private val transactionRepository = stub[TransactionRepository[IO]]
  private val validator = AccountValidator(accountRepository, transactionRepository)

  "AccountValidator method" - {
    "exists" - {
      "should return Left(AccountDoesNotExistError) when repository does not have account" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (accountRepository.get _).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option.empty))

        val result = validator.exists(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }
      "should return Left(AccountDoesNotExistError) when id is empty" in {
        val fakeAccount = Account(Option.empty, 2, "name", "description", Bank)

        val result = validator.exists(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }
      "should return Right(()) when account exists" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (accountRepository.get _).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option(fakeAccount)))

        val result = validator.exists(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "propertiesAreValid" - {
      "should return Left(NameMustBeDefinedError) if name is null" in {
        val fakeAccount = Account(2, null, "desc", Bank)

        val result = validator.propertiesAreValid(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }

      "should return Left(DescriptionMustBeDefinedError) if description is null" in {
        val fakeAccount = Account(2, "name", null, Bank)

        val result = validator.propertiesAreValid(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }

      "should return Left(NameExceedsMaxLengthError) if name is too long" in {
        val name = (0 until (MaxNameLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        val fakeAccount = Account(2, name, "description", Bank)

        val result = validator.propertiesAreValid(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(NameExceedsMaxLengthError)
      }

      "should return Left(DescriptionExceedsMaxLengthError) if name and description are too long" in {
        val desc = (0 until (MaxDescriptionLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        val fakeAccount = Account(2, "name", desc, Bank)

        val result = validator.propertiesAreValid(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(DescriptionExceedsMaxLengthError)
      }

      "should return Right(()) if name and description are correct length" in {
        val fakeAccount = Account(2, "name", "desc", Bank)

        val result = validator.propertiesAreValid(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "doesNotExist" - {
      "should return Right(()) when repository does not have account" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (accountRepository.get _).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option.empty))

        val result = validator.doesNotExist(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right(()) when id is empty" in {
        val fakeAccount = Account(Option.empty, 2, "name", "description", Bank)

        val result = validator.doesNotExist(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(AccountAlreadyExistsError) when account is empty" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (accountRepository.get _).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option(fakeAccount)))

        val result = validator.doesNotExist(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountAlreadyExistsError)
      }
    }

    "hasTransactions" - {
      "should return Right(()) when account has None as id" in {
        val fakeAccount = Account(Option.empty, 2, "name", "description", Bank)

        val result = validator.hasTransactions(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right(()) when account has no transactions" in {
        val fakeAccount = Account(Some(1), 2, "name", "description", Bank)

        (transactionRepository.anyWithAccountId _)
          .when(fakeAccount.userId, fakeAccount.id.get)
          .returns(IO(false))

        val result = validator.hasTransactions(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(ReferencedByTransactionError) when account has transactions" in {
        val fakeAccount = Account(Some(1), 2, "name", "description", Bank)

        (transactionRepository.anyWithAccountId _)
          .when(fakeAccount.userId, fakeAccount.id.get)
          .returns(IO(true))

        val result = validator.hasTransactions(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(ReferencedByTransactionError)
      }
    }
  }
}
