package com.finance.business.services

import cats.data.EitherT
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.transaction.{CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.types.{Description, Id, ModelName, Usd}
import com.finance.business.repository.TransactionRepository
import com.finance.business.validation.TransactionValidationAlgebra
import com.finance.business.validation.errors._
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransactionServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[TransactionValidationAlgebra[IdMonad]]
  private val mockRepository = mock[TransactionRepository[IdMonad]]

  private val service = new TransactionService[IdMonad](mockValidationAlgebra, mockRepository)

  private val transactionId = Id(5)
  private val transaction = Transaction(
    Some(transactionId),
    Description("Description"),
    DateTime.now,
    Id(6),
    Seq(
      PaybackAmount(Id(8), Id(7), Usd(5.6), Description("AmountDescription0"), DateTime.now),
      CategoryAmount(Id(9), Id(7), Usd(32), Description("AmountDescription1"), DateTime.now),
      CategoryAmount(Id(10), Id(7), Usd(32), Description("AmountDescription2"), DateTime.now)
    )
  )
  "TransactionService" - {
    "create" - {
      "returns Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Transaction")))
        (mockValidationAlgebra idIsNone _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Transaction"), Description("Desc")))
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra accountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transaction.accountId))
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Left(DescriptionTooLong) from validation algebra amountDescAreValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Amount"), Description("Desc")))
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra categoryIdsExist" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category"), Id(4)))
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra paybackIdsExist" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Payback"), Id(4)))
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra sourceIdsExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Source"), Id(4)))
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra sourceIdsExists _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Left(DateNotInEffectiveTime) from validation algebra reportingDateWithinBudgetTime" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DateNotInEffectiveTime(DateTime.now, Seq.empty))
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra sourceIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra reportingDateWithinBudgetTime _) when transaction returns returnVal
        (mockRepository create _) expects transaction never

        service.create(transaction) shouldEqual returnVal
      }
      "returns Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when transaction returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra sourceIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra reportingDateWithinBudgetTime _) when transaction returns
          EitherT.rightT[IdMonad, DateNotInEffectiveTime](())
        (mockRepository create _) expects transaction returns transaction.pure[IdMonad]

        service.create(transaction) shouldEqual EitherT.rightT[IdMonad, ValidationError](transaction)
      }
    }
    "update" - {
      "returns Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Transaction")))
        (mockValidationAlgebra exists _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Transaction"), Description("Desc")))
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra accountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transaction.accountId))
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Left(DescriptionTooLong) from validation algebra amountDescAreValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Amount"), Description("Desc")))
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra categoryIdsExist" in {
        val returnVal =
          EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category"), Id(4)))
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra paybackIdsExist" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Payback"), Id(4)))
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra sourceIdsExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Source"), Id(4)))
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra sourceIdsExists _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Left(DateNotInEffectiveTime) from validation algebra reportingDateWithinBudgetTime" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DateNotInEffectiveTime(DateTime.now, Seq.empty))
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra sourceIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra reportingDateWithinBudgetTime _) when transaction returns returnVal
        (mockRepository update _) expects transaction never

        service.update(transaction) shouldEqual returnVal
      }
      "returns Right(()) and updates model when validation passes" in {
        (mockValidationAlgebra exists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra descriptionIsValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra accountIdExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra amountDescAreValid _) when transaction returns
          EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockValidationAlgebra categoryIdsExist _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra paybackIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra sourceIdsExists _) when transaction returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra reportingDateWithinBudgetTime _) when transaction returns
          EitherT.rightT[IdMonad, DateNotInEffectiveTime](())
        (mockRepository update _) expects transaction returns transaction.pure[IdMonad]

        service.update(transaction) shouldEqual EitherT.rightT[IdMonad, ValidationError](transaction)
      }
    }
    "delete" - {
      "returns Right(()) and deletes" in {
        (mockRepository delete _) expects transactionId returns ().pure[IdMonad]

        service.delete(transactionId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
  }
}