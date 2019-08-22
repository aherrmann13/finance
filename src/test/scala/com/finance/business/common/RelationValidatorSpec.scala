package com.finance.business.common

import cats.effect.IO
import com.finance.business.common.errors.UserDoesNotExistError
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class RelationValidatorSpec extends FreeSpec with Matchers with MockFactory {

  private val repository = stub[IdRepository[IO]]

  private val validator = RelationValidator(repository)
  private val mockUser = mock[HasUserId]

  "RelationValidator" - {
    "userExists" - {
      "should return Left(UserDoesNotExistError) when user does not exist" in {
        val userId = 5
        (mockUser.userId _).expects().returns(userId)

        (repository.userExists _ when userId).returns(IO[Boolean](false))

        val result = validator.userExists(mockUser)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }
      "should return Right() when user exists" in {
        val userId = 5
        (mockUser.userId _).expects().returns(userId)

        (repository.userExists _ when userId).returns(IO[Boolean](true))

        val result = validator.userExists(mockUser)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }
  }
}
