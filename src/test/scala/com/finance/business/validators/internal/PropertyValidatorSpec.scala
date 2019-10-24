package com.finance.business.validators.internal

import cats.effect.IO
import com.finance.business.common.Constants.{MaxDescriptionLength, MaxNameLength}
import com.finance.business.errors._
import com.finance.business.validators.internal.PropertyValidators._
import org.scalatest.{FreeSpec, Matchers}


class PropertyValidatorSpec extends FreeSpec with Matchers {

  "PropertyValidator" - {
    "validateName" - {
      "should return Right(()) when name is valid" in {
        validateName[IO]("validName").value.unsafeRunSync shouldBe Right(())
      }

      "should return Left(NameMustBeDefinedError) when name null" in {
        validateName[IO](null).value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(NameExceedsMaxLengthError) when name too long" in {
        val name = (0 until (MaxNameLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        validateName[IO](name).value.unsafeRunSync shouldBe Left(NameExceedsMaxLengthError)
      }
    }

    "validateDesc" - {
      "should return Right(()) when desc is valid" in {
        validateDesc[IO]("validDesc").value.unsafeRunSync shouldBe Right(())
      }

      "should return Left(NameMustBeDefinedError) when desc null" in {
        validateDesc[IO](null).value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }
      "should return Left(NameExceedsMaxLengthError) when desc too long" in {
        val desc = (0 until (MaxDescriptionLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        validateDesc[IO](desc).value.unsafeRunSync shouldBe Left(DescriptionExceedsMaxLengthError)
      }
    }
  }
}
