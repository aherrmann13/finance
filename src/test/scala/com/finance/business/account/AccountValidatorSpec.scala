package com.finance.business.account

import cats.effect.IO
import org.scalatest.{FreeSpec, Matchers}
import org.scalamock.scalatest.MockFactory



class AccountValidatorSpec extends FreeSpec with Matchers with MockFactory {

  private val repository = stub[AccountRepository[IO]]
  private val validator = AccountValidator(repository)

  "AccountValidator method" - {
    "exists" - {
      "should return Left(AccountDoesNotExistError) when repository does not have account" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (repository.get(_,_)).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option.empty))

        val result = validator.exists(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }
      "should return Left(AccountDoesNotExistError) when id is empty" in {
        val fakeAccount = Account(Option.empty, 2, "name", "description", Bank)

        val result = validator.exists(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountDoesNotExistError)
      }
      "should return Right when account exists" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (repository.get(_,_)).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option(fakeAccount)))

        val result = validator.exists(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "doesNotExist" - {
      "should return Right when repository does not have account" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (repository.get(_,_)).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option.empty))

        val result = validator.doesNotExist(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right when id is empty" in {
        val fakeAccount = Account(Option.empty, 2, "name", "description", Bank)

        val result = validator.doesNotExist(fakeAccount)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(AccountAlreadyExistsError) when account is empty" in {
        val fakeAccount = Account(Option(1), 2, "name", "description", Bank)
        (repository.get(_,_)).when(fakeAccount.userId, fakeAccount.id.get).returns(IO(Option(fakeAccount)))

        val result = validator.doesNotExist(fakeAccount)

        result.value.unsafeRunSync shouldBe Left(AccountAlreadyExistsError)
      }
    }
  }
}