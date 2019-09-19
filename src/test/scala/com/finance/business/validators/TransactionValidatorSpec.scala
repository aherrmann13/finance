package com.finance.business.validators

import cats.effect.IO
import com.finance.business.common.Constants.MaxDescriptionLength
import com.finance.business.errors._
import com.finance.business.model.account.{Account, AccountRepository, Brokerage}
import com.finance.business.model.source.{Source, SourceRepository}
import com.finance.business.model.transaction._
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class TransactionValidatorSpec extends FreeSpec with Matchers with MockFactory {

  private val transactionRepository = stub[TransactionRepository[IO]]
  private val sourceRepository = stub[SourceRepository[IO]]
  private val accountRepository = stub[AccountRepository[IO]]
  private val validator = TransactionValidator(transactionRepository, sourceRepository, accountRepository)

  private val fakeTransaction =
    Transaction(Some(1), 2, Seq(Amount(3, 10.5, "amountDesc")), "transDesc", DateTime.now, DateTime.now, 5, 6, 7)

  "TransactionValidator method" - {
    "exists" - {
      "should return Left(TransactionDoesNotExistError) when repository does not have transaction" in {
        (transactionRepository.get _).when(fakeTransaction.userId, fakeTransaction.id.get).returns(IO(Option.empty))

        val result = validator.exists(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(TransactionDoesNotExistError)
      }
      "should return Left(TransactionDoesNotExistError) when id is empty" in {
        val result = validator.exists(fakeTransaction.copy(id = None))

        result.value.unsafeRunSync shouldBe Left(TransactionDoesNotExistError)
      }
      "should return Right(()) when transaction exists" in {
        (transactionRepository.get _)
          .when(fakeTransaction.userId, fakeTransaction.id.get)
          .returns(IO(Option(fakeTransaction)))

        val result = validator.exists(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "doesNotExist" - {
      "should return Right when repository does not have transaction" in {
        (transactionRepository.get _).when(fakeTransaction.userId, fakeTransaction.id.get).returns(IO(Option.empty))

        val result = validator.doesNotExist(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right when id is empty" in {
        val result = validator.doesNotExist(fakeTransaction.copy(id = None))

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(TransactionAlreadyExistsError) when source is empty" in {
        (transactionRepository.get _)
          .when(fakeTransaction.userId, fakeTransaction.id.get)
          .returns(IO(Option(fakeTransaction)))

        val result = validator.doesNotExist(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(TransactionAlreadyExistsError)
      }
    }

    "propertiesAreValid" - {
      "should return Left(DescriptionMustBeDefinedError) if null description" in {
        val result = validator.propertiesAreValid(fakeTransaction.copy(description = null))

        result.value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }
      "should return Left(DescriptionExceedsMaxLengthError) if description is too long" in {
        val result = validator.propertiesAreValid(
          fakeTransaction.copy(description = (0 until (MaxDescriptionLength + 1)).foldLeft("a")((acc, _) => acc + "a")))

        result.value.unsafeRunSync shouldBe Left(DescriptionExceedsMaxLengthError)
      }
      "should return Left(DescriptionMustBeDefinedError) if any amount description is null" in {
        val result =
          validator.propertiesAreValid(fakeTransaction.copy(amount = fakeTransaction.amount :+ Amount(2, 6, null)))

        result.value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }
      "should return Left(DescriptionExceedsMaxLengthError) if any amount description is too long" in {
        val result = validator.propertiesAreValid(
          fakeTransaction.copy(
            amount = fakeTransaction.amount :+ Amount(
              2,
              6,
              (0 until (MaxDescriptionLength + 1)).foldLeft("a")((acc, _) => acc + "a"))))

        result.value.unsafeRunSync shouldBe Left(DescriptionExceedsMaxLengthError)
      }
      "should return Right(()) if all properties are valid" in {
        val result = validator.propertiesAreValid(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "sourceExists" - {
      "should return Right(()) if source exists" in {
        (sourceRepository.get _)
          .when(fakeTransaction.userId, fakeTransaction.sourceId)
          .returns(IO(Option(Source(Some(5), fakeTransaction.userId, "name", "description"))))

        val result = validator.sourceExists(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(SourceDoesNotExistError) if source does not exist" in {
        (sourceRepository.get _)
          .when(fakeTransaction.userId, fakeTransaction.sourceId)
          .returns(IO(None))

        val result = validator.sourceExists(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(SourceDoesNotExistError)
      }
    }

    "accountExists" - {
      "should return Right(()) if account exists" in {
        (accountRepository.get _)
          .when(fakeTransaction.userId, fakeTransaction.accountId)
          .returns(IO(Option(Account(Some(5), fakeTransaction.userId, "name", "description", Brokerage))))

        val result = validator.accountExists(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(AccountDoesNotExistError) if account does not exist" in {
        (accountRepository.get _)
          .when(fakeTransaction.userId, fakeTransaction.accountId)
          .returns(IO(None))

        val result = validator.accountExists(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }
    }
  }
}
