package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import com.finance.business.common.{IdRepository, RelationValidator}
import com.finance.business.errors._
import com.finance.business.model.account._
import com.finance.business.validators.AccountValidator
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class AccountServiceSpec extends FreeSpec with Matchers with MockFactory {
  private val repository = stub[AccountRepository[IO]]

  //https://github.com/paulbutcher/ScalaMock/issues/170
  class AccountValidatorWithIO(repository: AccountRepository[IO]) extends AccountValidator[IO](repository)
  class RelationValidatorWithIO(repository: IdRepository[IO]) extends RelationValidator[IO](repository)
  private val validator = mock[AccountValidatorWithIO]
  private val relationValidator = mock[RelationValidatorWithIO]

  private val service = AccountService(repository, validator, relationValidator)

  val fakeAccount = Account(Option(1), 2, "name", "description", Bank)

  "Account service" - {
    "create" - {
      "should return Left(AccountValidationError) error on invalid properties" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.leftT[IO, Unit](NameMustBeDefinedError))

        val result = service.create(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(AccountValidationError) error when account exists" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Account))
          .expects(fakeAccount)
          .returning(EitherT.leftT[IO, Unit](AccountAlreadyExistsError))

        val result = service.create(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountAlreadyExistsError)
      }
      "should return Left(UserDoesNotExistError) when user does not exist" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Account))
          .expects(fakeAccount)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeAccount)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.create(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }
      "should return Right(Account) on success" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Account))
          .expects(fakeAccount)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeAccount)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.create _ when fakeAccount).returns(IO(fakeAccount))

        val result = service.create(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(fakeAccount)
      }
    }
    "update" - {
      "should return Left(AccountValidationError) error on invalid properties" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.leftT[IO, Unit](NameMustBeDefinedError))

        val result = service.update(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(AccountValidationError) error when account does not exist" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Account))
          .expects(fakeAccount)
          .returning(EitherT.leftT[IO, Unit](AccountDoesNotExistError))

        val result = service.update(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }
      "should return Left(UserDoesNotExistError) when user does not exist" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Account))
          .expects(fakeAccount)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeAccount)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.update(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }
      "should return Right(Account) on success" in {
        (validator
          .propertiesAreValid(_: Account)(_: Monad[IO]))
          .expects(fakeAccount, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Account))
          .expects(fakeAccount)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeAccount)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.update _ when fakeAccount).returns(IO(fakeAccount))

        val result = service.update(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(fakeAccount)
      }
    }
    "delete" - {
      "should call delete on repository" in {
        service.delete(fakeAccount.userId, fakeAccount.id.get)

        (repository.delete _).verify(fakeAccount.userId, fakeAccount.id.get)
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
        (repository.get _).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Some(fakeAccount)))

        val result = service.get(fakeAccount.userId, fakeAccount.id.get)
        result.unsafeRunSync shouldBe Option.apply(fakeAccount)
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
        val accounts = Seq(fakeAccount, fakeAccount.copy(id = Option.apply(5)))
        val ids = accounts.flatMap(_.id)
        (repository.getMany _).when(fakeAccount.userId, ids).returns(IO(accounts))

        val result = service.getMany(fakeAccount.userId, ids)
        result.unsafeRunSync shouldBe accounts
      }
    }
    "getAll" - {
      "should call getAll on repository" in {
        val userId = 4
        service.getAll(userId)

        (repository.getAll _).verify(userId)
      }

      "should return results from repository" in {
        val accounts = Seq(fakeAccount, fakeAccount.copy(id = Option.apply(5)))
        (repository.getAll _).when(fakeAccount.userId).returns(IO(accounts))

        val result = service.getAll(fakeAccount.userId)
        result.unsafeRunSync shouldBe accounts
      }
    }
  }
}
