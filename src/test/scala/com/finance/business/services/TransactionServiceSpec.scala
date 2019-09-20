package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import com.finance.business.common.{IdRepository, RelationValidator}
import com.finance.business.errors._
import com.finance.business.model.account.AccountRepository
import com.finance.business.model.source.SourceRepository
import com.finance.business.model.transaction.{Amount, Transaction, TransactionRepository}
import com.finance.business.validators.TransactionValidator
import com.github.nscala_time.time.Imports.DateTime
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class TransactionServiceSpec extends FreeSpec with Matchers with MockFactory {
  private val repository = stub[TransactionRepository[IO]]

  //https://github.com/paulbutcher/ScalaMock/issues/170
  class TransactionValidatorWithIO(
      transactionRepository: TransactionRepository[IO],
      sourceRepository: SourceRepository[IO],
      accountRepository: AccountRepository[IO])
      extends TransactionValidator[IO](transactionRepository, sourceRepository, accountRepository)

  class RelationValidatorWithIO(repository: IdRepository[IO]) extends RelationValidator[IO](repository)

  private val validator = mock[TransactionValidatorWithIO]
  private val relationValidator = mock[RelationValidatorWithIO]

  private val service = TransactionService(repository, validator, relationValidator)

  private val fakeTransaction =
    Transaction(Some(1), 2, Seq(Amount(3, 10.5, "amountDesc")), "transDesc", DateTime.now, DateTime.now, 5, 6, 7)

  "Source service" - {
    "create" - {
      "should return Left(BusinessError) when properties are invalid" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.leftT[IO, Unit](DescriptionMustBeDefinedError))

        val result = service.create(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }

      "should return Left(BusinessError) when transaction exists" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](TransactionAlreadyExistsError))

        val result = service.create(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(TransactionAlreadyExistsError)
      }

      "should return Left(BusinessError) when user does not exist" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.create(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }

      "should return Left(BusinessError) when account does not exist" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .accountExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](AccountDoesNotExistError))

        val result = service.create(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }

      "should return Left(BusinessError) when source does not exist" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .accountExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .sourceExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](SourceDoesNotExistError))

        val result = service.create(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(SourceDoesNotExistError)
      }

      "should return Right(()) on success" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .accountExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .sourceExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.create _ when fakeTransaction).returns(IO(fakeTransaction))

        val result = service.create(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(fakeTransaction)
      }
    }

    "update" - {
      "should return Left(BusinessError) when properties are invalid" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.leftT[IO, Unit](DescriptionMustBeDefinedError))

        val result = service.update(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }

      "should return Left(BusinessError) when transaction does not" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](TransactionAlreadyExistsError))

        val result = service.update(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(TransactionAlreadyExistsError)
      }

      "should return Left(BusinessError) when user does not exist" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.update(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }

      "should return Left(BusinessError) when account does not exist" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .accountExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](AccountDoesNotExistError))

        val result = service.update(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }

      "should return Left(BusinessError) when source does not exist" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .accountExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .sourceExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.leftT[IO, Unit](SourceDoesNotExistError))

        val result = service.update(fakeTransaction)

        result.value.unsafeRunSync shouldBe Left(SourceDoesNotExistError)
      }

      "should return Right(()) on success" in {
        (validator
          .propertiesAreValid(_: Transaction)(_: Monad[IO]))
          .expects(fakeTransaction, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .accountExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .sourceExists(_: Transaction))
          .expects(fakeTransaction)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.update _ when fakeTransaction).returns(IO(fakeTransaction))

        val result = service.update(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(fakeTransaction)
      }
    }
    "delete" - {
      "should return Right(()) and delete on success" in {
        (repository.delete _).when(fakeTransaction.userId, fakeTransaction.id.get).returns(IO(()))

        val result = service.delete(fakeTransaction)

        result.value.unsafeRunSync shouldBe Right(())
      }

      "should return Right(()) and skip delete when id is None" in {
        val result = service.delete(fakeTransaction.copy(id = None))

        result.value.unsafeRunSync shouldBe Right(())

        (repository.delete _).verify(*, *) never
      }
    }
    "get" - {
      "should call get on repository" in {
        val userId = 5
        val id = 3
        service.get(userId, id)

        (repository.get _).verify(userId, id)
      }

      "should return results from repository" in {
        (repository.get _).when(fakeTransaction.userId, fakeTransaction.id.get).returns(IO(Some(fakeTransaction)))

        val result = service.get(fakeTransaction.userId, fakeTransaction.id.get)
        result.unsafeRunSync shouldBe Option.apply(fakeTransaction)
      }
    }
    "getMany" - {
      "should call getMany on repository" in {
        val userId = 4
        val ids = Seq(1, 2, 3, 4)
        service.getMany(userId, ids)

        (repository.getMany _).verify(userId, ids)
      }

      "should return results from repository" in {
        val sources = Seq(fakeTransaction, fakeTransaction.copy(id = Option.apply(5)))
        val ids = sources.flatMap(_.id)
        (repository.getMany _).when(fakeTransaction.userId, ids).returns(IO(sources))

        val result = service.getMany(fakeTransaction.userId, ids)
        result.unsafeRunSync shouldBe sources
      }
    }
    "getAll" - {
      "should call getAll on repository" in {
        val userId = 4
        service.getAll(userId)

        (repository.getAll _).verify(userId)
      }

      "should return results from repository" in {
        val sources = Seq(fakeTransaction, fakeTransaction.copy(id = Option.apply(5)))
        (repository.getAll _).when(fakeTransaction.userId).returns(IO(sources))

        val result = service.getAll(fakeTransaction.userId)
        result.unsafeRunSync shouldBe sources
      }
    }

  }
}
