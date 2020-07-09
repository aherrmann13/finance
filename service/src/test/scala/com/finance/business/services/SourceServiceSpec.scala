package com.finance.business.services

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.source.Source
import com.finance.business.model.types.{Description, Id, ModelName, Name}
import com.finance.business.repository.SourceRepository
import com.finance.business.validation.SourceValidationAlgebra
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SourceServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[SourceValidationAlgebra[IdMonad]]
  private val mockRepository = mock[SourceRepository[IdMonad]]

  private val service = new SourceService[IdMonad](mockValidationAlgebra, mockRepository)

  private val sourceId = Id(5)
  private val source = Source(Some(sourceId), Name("Name"), Description("Description"))

  "SourceService" - {
    "create" - {
      "should return Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Source")))
        (mockValidationAlgebra idIsNone _) when source returns returnVal
        (mockRepository create _) expects source never

        service.create(source) shouldEqual returnVal
      }
      "should return Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Source"), Name("Name")))
        (mockValidationAlgebra idIsNone _) when source returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra nameIsValid _) when source returns returnVal
        (mockRepository create _) expects source never

        service.create(source) shouldEqual returnVal
      }
      "should return Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Source"), Description("Desc")))
        (mockValidationAlgebra idIsNone _) when source returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra nameIsValid _) when source returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when source returns returnVal
        (mockRepository create _) expects source never

        service.create(source) shouldEqual returnVal
      }
      "should return Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when source returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra nameIsValid _) when source returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when source returns EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockRepository create _) expects source returns source.pure[IdMonad]

        service.create(source) shouldEqual EitherT.rightT[IdMonad, ValidationError](source)
      }
    }
    "update" - {
      "should return Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Source")))
        (mockValidationAlgebra exists _) when source returns returnVal
        (mockRepository update _) expects source never

        service.update(source) shouldEqual returnVal
      }
      "should return Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Source"), Name("Name")))
        (mockValidationAlgebra exists _) when source returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra nameIsValid _) when source returns returnVal
        (mockRepository update _) expects source never

        service.update(source) shouldEqual returnVal
      }
      "should return Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Source"), Description("Desc")))
        (mockValidationAlgebra exists _) when source returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra nameIsValid _) when source returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when source returns returnVal
        (mockRepository update _) expects source never

        service.update(source) shouldEqual returnVal
      }
      "should return Right(()) and updates model when validation passes" in {
        (mockValidationAlgebra exists _) when source returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra nameIsValid _) when source returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when source returns EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockRepository update _) expects source returns source.pure[IdMonad]

        service.update(source) shouldEqual EitherT.rightT[IdMonad, ValidationError](source)
      }
    }
    "delete" - {
      "should return Left(HasTransactions) from validation algebra hasNoTransactions" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](HasTransactions(ModelName("Source")))
        (mockValidationAlgebra hasNoTransactions _) when sourceId returns returnVal
        (mockRepository delete _) expects sourceId never

        service.delete(sourceId) shouldEqual returnVal
      }
      "should return Right(()) and deletes when validation passes" in {
        (mockValidationAlgebra hasNoTransactions _) when sourceId returns EitherT.rightT[IdMonad, HasTransactions](())
        (mockRepository delete _) expects sourceId returns ().pure[IdMonad]

        service.delete(sourceId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
    "get" - {
      "should return repository get" in {
        (mockRepository get _) expects sourceId returns OptionT.pure(source)

        service.get(sourceId).value shouldEqual Some(source)
      }
    }
    "getMany" - {
      "should return repository getMany" in {
        (mockRepository getMany _) expects Seq(sourceId, Id(sourceId.value + 1)) returns
          Seq(source, source).pure[IdMonad]

        service.getMany(Seq(sourceId, Id(sourceId.value + 1))) shouldEqual Seq(source, source)
      }
    }
    "getAll" - {
      "should return repository getAll" - {
        (mockRepository.getAll _).expects().returns(Seq(source, source).pure[IdMonad])

        service.getAll shouldEqual Seq(source, source)
      }
    }
    "get with fuzzy search" - {
      "should return repository getFuzzyMatch" in {
        val fuzzy = "fuzzy search"
        (mockRepository getFuzzyMatch _) expects fuzzy returns Seq(source, source).pure[IdMonad]

        service.get(fuzzy) shouldEqual Seq(source, source)
      }
    }
  }
}
