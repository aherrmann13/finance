package com.finance.business.services

import java.time.OffsetDateTime

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.payback.{Payback, PaybackBalance}
import com.finance.business.model.transaction.PaybackAmount
import com.finance.business.model.types._
import com.finance.business.repository.{PaybackRepository, TransactionRepository}
import com.finance.business.validation.PaybackValidationAlgebra
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PaybackServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[PaybackValidationAlgebra[IdMonad]]
  private val mockRepository = mock[PaybackRepository[IdMonad]]
  private val mockTransactionRepository = mock[TransactionRepository[IdMonad]]


  private val service = new PaybackService[IdMonad](mockValidationAlgebra, mockRepository, mockTransactionRepository)

  private val paybackId = Id(5)
  private val payback = Payback(Some(paybackId), Name("Name"), Description("Description"), OffsetDateTime.now)

  "PaybackService" - {
    "create" - {
      "should return Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Payback")))
        (mockValidationAlgebra idIsNone _) when payback returns returnVal
        (mockRepository create _) expects payback never

        service.create(payback) shouldEqual returnVal
      }
      "should return Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Payback"), Name("Name")))
        (mockValidationAlgebra idIsNone _) when payback returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra nameIsValid _) when payback returns returnVal
        (mockRepository create _) expects payback never

        service.create(payback) shouldEqual returnVal
      }
      "should return Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Payback"), Description("Desc")))
        (mockValidationAlgebra idIsNone _) when payback returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when payback returns returnVal
        (mockRepository create _) expects payback never

        service.create(payback) shouldEqual returnVal
      }
      "should return Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when payback returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when payback returns EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockRepository create _) expects payback returns payback.pure[IdMonad]

        service.create(payback) shouldEqual EitherT.rightT[IdMonad, ValidationError](payback)
      }
    }
    "update" - {
      "should return Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Payback")))
        (mockValidationAlgebra exists _) when payback returns returnVal
        (mockRepository update _) expects payback never

        service.update(payback) shouldEqual returnVal
      }
      "should return Left(NameTooLong) from validation algebra nameIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Payback"), Name("Name")))
        (mockValidationAlgebra exists _) when payback returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra nameIsValid _) when payback returns returnVal
        (mockRepository update _) expects payback never

        service.update(payback) shouldEqual returnVal
      }
      "should return Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Payback"), Description("Desc")))
        (mockValidationAlgebra exists _) when payback returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when payback returns returnVal
        (mockRepository update _) expects payback never

        service.update(payback) shouldEqual returnVal
      }
      "should return Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra exists _) when payback returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra nameIsValid _) when payback returns EitherT.rightT[IdMonad, NameTooLong](())
        (mockValidationAlgebra descriptionIsValid _) when payback returns EitherT.rightT[IdMonad, DescriptionTooLong](())
        (mockRepository update _) expects payback returns payback.pure[IdMonad]

        service.update(payback) shouldEqual EitherT.rightT[IdMonad, ValidationError](payback)
      }
    }
    "delete" - {
      "should return Left(HasTransactions) from validation algebra hasNoTransactions" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](HasTransactions(ModelName("Payback")))
        (mockValidationAlgebra hasNoTransactions _) when paybackId returns returnVal
        (mockRepository delete _) expects paybackId never

        service.delete(paybackId) shouldEqual returnVal
      }
      "should return Right(()) and deletes when validation passes" in {
        (mockValidationAlgebra hasNoTransactions _) when paybackId returns EitherT.rightT[IdMonad, HasTransactions](())
        (mockRepository delete _) expects paybackId returns ().pure[IdMonad]

        service.delete(paybackId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
    "get" - {
      "should return repository get" in {
        (mockRepository get _) expects paybackId returns OptionT.pure(payback)

        service.get(paybackId).value shouldEqual Some(payback)
      }
    }
    "getMany" - {
      "should return repository getMany" in {
        (mockRepository getMany _) expects Seq(paybackId, Id(paybackId.value + 1)) returns
          Seq(payback, payback).pure[IdMonad]

        service.getMany(Seq(paybackId, Id(paybackId.value + 1))) shouldEqual Seq(payback, payback)
      }
    }
    "getAll" - {
      "should return repository getAll" - {
        (mockRepository.getAll _).expects().returns(Seq(payback, payback).pure[IdMonad])

        service.getAll shouldEqual Seq(payback, payback)
      }
    }
    "getPaybackBalance" - {
      val dateRange = DateRange(OffsetDateTime.now, OffsetDateTime.now)

      val paybackId0 = paybackId
      val paybackId1 = Id(paybackId.value + 1)
      val payback0 = payback
      val payback1 = payback.copy(id = Some(paybackId1))

      val paybackAmount0 = PaybackAmount(paybackId, Id(5), Usd(40), Description("desc"), OffsetDateTime.now)
      val paybackAmount1 = PaybackAmount(paybackId, Id(5), Usd(6), Description("desc"), OffsetDateTime.now)
      val paybackAmount2 = PaybackAmount(paybackId1, Id(5), Usd(34), Description("desc"), OffsetDateTime.now)
      val paybackAmount3 = PaybackAmount(paybackId1, Id(5), Usd(865), Description("desc"), OffsetDateTime.now)

      "should return payback items amounts" in {
        (mockRepository getInRange _) expects dateRange returns Seq(payback0, payback1).pure[IdMonad]
        (mockTransactionRepository getByPaybackIds _) expects Seq(paybackId0, paybackId1) returns
          Seq(paybackAmount0, paybackAmount1, paybackAmount2, paybackAmount3).pure[IdMonad]

        service.getPaybackBalance(dateRange) shouldEqual Seq(
          PaybackBalance(payback0, Seq(paybackAmount0, paybackAmount1)),
          PaybackBalance(payback1, Seq(paybackAmount2, paybackAmount3))
        )
      }
      "should return payback with empty list when id is None" in {
        val paybackWithNoId = payback.copy(id = None)
        (mockRepository getInRange _) expects dateRange returns Seq(payback0, payback1, paybackWithNoId).pure[IdMonad]
        (mockTransactionRepository getByPaybackIds _) expects Seq(paybackId0, paybackId1) returns
          Seq(paybackAmount0, paybackAmount1, paybackAmount2, paybackAmount3)

        service.getPaybackBalance(dateRange) shouldEqual Seq(
          PaybackBalance(payback0, Seq(paybackAmount0, paybackAmount1)),
          PaybackBalance(payback1, Seq(paybackAmount2, paybackAmount3)),
          PaybackBalance(paybackWithNoId, Seq.empty)
        )
      }
      "should return payback with empty list when no PaybackAmounts with id" in {
        val newId = Id(1234)
        val paybackWithNewId = payback.copy(id = Some(newId))
        (mockRepository getInRange _) expects dateRange returns Seq(payback0, payback1, paybackWithNewId).pure[IdMonad]
        (mockTransactionRepository getByPaybackIds _) expects Seq(paybackId0, paybackId1, newId) returns
          Seq(paybackAmount0, paybackAmount1, paybackAmount2, paybackAmount3)

        service.getPaybackBalance(dateRange) shouldEqual Seq(
          PaybackBalance(payback0, Seq(paybackAmount0, paybackAmount1)),
          PaybackBalance(payback1, Seq(paybackAmount2, paybackAmount3)),
          PaybackBalance(paybackWithNewId, Seq.empty)
        )
      }
    }
    "getPaybackBalanceSummary" - {
      "should return sum of all payback amounts" in {
        val paybackAmount0 = PaybackAmount(paybackId, Id(5), Usd(40), Description("desc"), OffsetDateTime.now)
        val paybackAmount1 = PaybackAmount(paybackId, Id(5), Usd(6), Description("desc"), OffsetDateTime.now)
        val paybackAmount2 = PaybackAmount(paybackId, Id(5), Usd(34), Description("desc"), OffsetDateTime.now)
        val paybackAmount3 = PaybackAmount(paybackId, Id(5), Usd(865), Description("desc"), OffsetDateTime.now)

        (mockTransactionRepository.getAllPaybacks _).expects().returns(
          Seq(paybackAmount0, paybackAmount1, paybackAmount2, paybackAmount3).pure[IdMonad]
        )

        service.getPaybackBalanceSummary shouldEqual Usd(945)
      }
    }
  }
}