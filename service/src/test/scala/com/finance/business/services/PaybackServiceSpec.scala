package com.finance.business.services

import cats.data.EitherT
import cats.{Id => IdMonad}
import cats.implicits._
import com.finance.business.model.payback.Payback
import com.finance.business.model.types.{Description, Id, ModelName, Name}
import com.finance.business.repository.PaybackRepository
import com.finance.business.validation.PaybackValidationAlgebra
import com.finance.business.validation.errors._
import com.github.nscala_time.time.Imports.DateTime
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PaybackServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[PaybackValidationAlgebra[IdMonad]]
  private val mockRepository = mock[PaybackRepository[IdMonad]]

  private val service = new PaybackService[IdMonad](mockValidationAlgebra, mockRepository)

  private val paybackId = Id(5)
  private val payback = Payback(Some(paybackId), Name("Name"), Description("Description"), DateTime.now)

  "create" - {
    "returns Left(IdMustBeNone) from validation algebra idIsNone" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Payback")))
      (mockValidationAlgebra idIsNone _) when payback returns returnVal
      (mockRepository create _) expects payback never

      service.create(payback) shouldEqual returnVal
    }
    "returns Left(NameTooLong) from validation algebra nameIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Payback"), Name("Name")))
      (mockValidationAlgebra idIsNone _) when payback returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra nameIsValid _) when payback returns returnVal
      (mockRepository create _) expects payback never

      service.create(payback) shouldEqual returnVal
    }
    "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Payback"), Description("Desc")))
      (mockValidationAlgebra idIsNone _) when payback returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when payback returns returnVal
      (mockRepository create _) expects payback never

      service.create(payback) shouldEqual returnVal
    }
    "returns Right(()) and saves model when validation passes" in {
      (mockValidationAlgebra idIsNone _) when payback returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when payback returns EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockRepository create _) expects payback returns payback.pure[IdMonad]

      service.create(payback) shouldEqual EitherT.rightT[IdMonad, ValidationError](payback)
    }
  }
  "update" - {
    "returns Left(DoesNotExist) from validation algebra exists" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Payback")))
      (mockValidationAlgebra exists _) when payback returns returnVal
      (mockRepository update _) expects payback never

      service.update(payback) shouldEqual returnVal
    }
    "returns Left(NameTooLong) from validation algebra nameIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Payback"), Name("Name")))
      (mockValidationAlgebra exists _) when payback returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra nameIsValid _) when payback returns returnVal
      (mockRepository update _) expects payback never

      service.update(payback) shouldEqual returnVal
    }
    "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Payback"), Description("Desc")))
      (mockValidationAlgebra exists _) when payback returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when payback returns returnVal
      (mockRepository update _) expects payback never

      service.update(payback) shouldEqual returnVal
    }
    "returns Right(()) and saves model when validation passes" in {
      (mockValidationAlgebra exists _) when payback returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when payback returns EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockRepository update _) expects payback returns payback.pure[IdMonad]

      service.update(payback) shouldEqual EitherT.rightT[IdMonad, ValidationError](payback)
    }
  }
  "delete" - {
    "returns Left(HasTransactions) from validation algebra hasNoTransactions" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](HasTransactions(ModelName("Payback")))
      (mockValidationAlgebra hasNoTransactions _) when paybackId returns returnVal
      (mockRepository delete _) expects paybackId never

      service.delete(paybackId) shouldEqual returnVal
    }
    "returns Right(()) and deletes when validation passes" in {
      (mockValidationAlgebra hasNoTransactions _) when paybackId returns EitherT.rightT[IdMonad, HasTransactions](())
      (mockRepository delete _) expects paybackId returns ().pure[IdMonad]

      service.delete(paybackId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
    }
  }
}
