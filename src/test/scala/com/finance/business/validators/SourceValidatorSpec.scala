package com.finance.business.validators

import cats.effect.IO
import com.finance.business.errors.{DescriptionExceedsMaxLengthError, DescriptionMustBeDefinedError, NameExceedsMaxLengthError, NameMustBeDefinedError, SourceAlreadyExistsError, SourceDoesNotExistError}
import com.finance.business.model.source._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class SourceValidatorSpec extends FreeSpec with Matchers with MockFactory {

  private val repository = stub[SourceRepository[IO]]
  private val validator = SourceValidator(repository)

  "SourceValidator method" - {
    "exists" - {
      "should return Left(SourceDoesNotExistError) when repository does not have source" in {
        val fakeSource = Source(Option(1), 2, "name", "description")
        (repository.get(_,_)).when(fakeSource.userId, fakeSource.id.get).returns(IO(Option.empty))

        val result = validator.exists(fakeSource)

        result.value.unsafeRunSync shouldBe Left(SourceDoesNotExistError)
      }
      "should return Left(SourceDoesNotExistError) when id is empty" in {
        val fakeSource = Source(Option.empty, 2, "name", "description")

        val result = validator.exists(fakeSource)

        result.value.unsafeRunSync shouldBe Left(SourceDoesNotExistError)
      }
      "should return Right when source exists" in {
        val fakeSource = Source(Option(1), 2, "name", "description")
        (repository.get(_,_)).when(fakeSource.userId, fakeSource.id.get).returns(IO(Option(fakeSource)))

        val result = validator.exists(fakeSource)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "propertiesAreValid" - {
      "should return Left(NameMustBeDefinedError) if name is null" in {
        val fakeSource = Source(2, null, "desc")

        val result = validator.propertiesAreValid(fakeSource)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }

      "should return Left(DescriptionMustBeDefinedError) if description is null" in {
        val fakeSource = Source(2, "name", null)

        val result = validator.propertiesAreValid(fakeSource)

        result.value.unsafeRunSync shouldBe Left(DescriptionMustBeDefinedError)
      }

      "should return Left(NameExceedsMaxLengthError) if name is too long" in {
        val name = (0 until (Source.maxNameLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        val fakeSource = Source(2, name, "description")

        val result = validator.propertiesAreValid(fakeSource)

        result.value.unsafeRunSync shouldBe Left(NameExceedsMaxLengthError)
      }

      "should return Left(DescriptionExceedsMaxLengthError) if name and description are too long" in {
        val desc = (0 until (Source.maxDescriptionLength + 1)).foldLeft("a")((acc, _) => acc + "a")
        val fakeSource = Source(2, "name", desc)

        val result = validator.propertiesAreValid(fakeSource)

        result.value.unsafeRunSync shouldBe Left(DescriptionExceedsMaxLengthError)
      }

      "should return Right(()) if name and description are correct length" in {
        val fakeSource = Source(2, "name", "desc")

        val result = validator.propertiesAreValid(fakeSource)

        result.value.unsafeRunSync shouldBe Right(())
      }
    }

    "doesNotExist" - {
      "should return Right when repository does not have source" in {
        val fakeSource = Source(Option(1), 2, "name", "description")
        (repository.get(_,_)).when(fakeSource.userId, fakeSource.id.get).returns(IO(Option.empty))

        val result = validator.doesNotExist(fakeSource)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Right when id is empty" in {
        val fakeSource = Source(Option.empty, 2, "name", "description")

        val result = validator.doesNotExist(fakeSource)

        result.value.unsafeRunSync shouldBe Right(())
      }
      "should return Left(SourceAlreadyExistsError) when source is empty" in {
        val fakeSource = Source(Option(1), 2, "name", "description")
        (repository.get(_,_)).when(fakeSource.userId, fakeSource.id.get).returns(IO(Option(fakeSource)))

        val result = validator.doesNotExist(fakeSource)

        result.value.unsafeRunSync shouldBe Left(SourceAlreadyExistsError)
      }
    }
  }
}