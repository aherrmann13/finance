package com.finance.business.validation

import cats.data.EitherT
import cats.{Id => IdMonad}
import cats.implicits._
import com.finance.business.model.account.{Account, Bank}
import com.finance.business.model.category.{Always, Category}
import com.finance.business.model.payback.Payback
import com.finance.business.model.source.Source
import com.finance.business.model.transaction.{CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.types.{Description, Id, ModelName, Name, Usd}
import com.finance.business.repository._
import com.finance.business.validation.errors.{DescriptionTooLong, DoesNotExist, IdMustBeNone}
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransactionValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {

  private val mockTransactionRepository = stub[TransactionRepository[IdMonad]]
  private val mockSourceRepository = stub[SourceRepository[IdMonad]]
  private val mockAccountRepository = stub[AccountRepository[IdMonad]]
  private val mockCategoryRepository = stub[CategoryRepository[IdMonad]]
  private val mockPaybackRepository = stub[PaybackRepository[IdMonad]]

  private val transactionValidationInterpreter = new TransactionValidationInterpreter[IdMonad](
    mockTransactionRepository,
    mockSourceRepository,
    mockAccountRepository,
    mockCategoryRepository,
    mockPaybackRepository
  )

  private val transactionName = ModelName("Transaction")
  private val amountName = ModelName("Amount")

  private val fakeTransactionWithId = Transaction(
    Some(Id(2)),
    Description("Description"),
    DateTime.now,
    Id(4),
    Id(5),
    Seq(
      PaybackAmount(Id(6), Usd(14.6), Description("amountDesc0"), DateTime.now),
      CategoryAmount(Id(7), Usd(1.6), Description("amountDesc1"), DateTime.now)
    )
  )
  private val fakeTransactionWithNoId = fakeTransactionWithId.copy(id = None)

  "idIsNone" - {
    "should return Left(IdMustBeNone) when id is Some" in {
      transactionValidationInterpreter.idIsNone(fakeTransactionWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](IdMustBeNone(transactionName)).value
    }
    "should return Right(()) when id is None" in {
      transactionValidationInterpreter.idIsNone(fakeTransactionWithNoId).value shouldEqual
        EitherT.rightT[IdMonad, IdMustBeNone](()).value
    }
  }
  "exists" - {
    "should return Left(DoesNotExist) when id is None" in {
      transactionValidationInterpreter.exists(fakeTransactionWithNoId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(transactionName)).value
    }
    "should return Left(DoesNotExist) when repository does not contain Account" in {
      (mockTransactionRepository get _).when(fakeTransactionWithId.id.get).returns(None.pure[IdMonad])
      transactionValidationInterpreter.exists(fakeTransactionWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(transactionName, fakeTransactionWithId.id)).value
    }
    "should return Right(()) when repository contains Account" in {
      (mockTransactionRepository get _)
        .when(fakeTransactionWithId.id.get)
        .returns(Some(fakeTransactionWithId).pure[IdMonad])
      transactionValidationInterpreter.exists(fakeTransactionWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "descriptionIsValid" - {
    "should return Left(DescriptionIsTooLong) when description is too long" in {
      val desc = Description((0 to 512).map(_ => "a").fold("")(_ + _))
      transactionValidationInterpreter
        .descriptionIsValid(fakeTransactionWithId.copy(description = desc)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DescriptionTooLong(transactionName, desc)).value
    }
    "should return Right(()) when description is correct length" in {
      val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))
      transactionValidationInterpreter
        .descriptionIsValid(fakeTransactionWithId.copy(description = desc)).value shouldEqual
        EitherT.rightT[IdMonad, DescriptionTooLong](()).value
    }
  }
  "sourceIdExists" - {
    "should return Left(DoesNotExist) when source does not exist" in {
      (mockSourceRepository get _)
        .when(fakeTransactionWithId.sourceId)
        .returns(None.pure[IdMonad])
      transactionValidationInterpreter.sourceIdExists(fakeTransactionWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Source"), fakeTransactionWithId.sourceId)).value
    }
    "should return Right(()) when source exists" in {
      (mockSourceRepository get _)
        .when(fakeTransactionWithId.sourceId)
        .returns(Some(Source(Some(Id(2)), Name("Name"), Description("Description"))).pure[IdMonad])
      transactionValidationInterpreter.sourceIdExists(fakeTransactionWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "accountIdExists" - {
    "should return Left(DoesNotExist) when account does not exist" in {
      (mockAccountRepository get _).when(fakeTransactionWithId.accountId).returns(None.pure[IdMonad])
      transactionValidationInterpreter.accountIdExists(fakeTransactionWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), fakeTransactionWithId.accountId)).value
    }
    "should return Right(()) when account exists" in {
      (mockAccountRepository get _)
        .when(fakeTransactionWithId.accountId)
        .returns(Some(Account(Some(Id(2)), Name("Name"), Description("Description"), Bank)).pure[IdMonad])
      transactionValidationInterpreter.accountIdExists(fakeTransactionWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "amountDescAreValid" - {
    "should return Left(DescriptionIsTooLong) for first description is too long" in {
      val desc0 = Description((0 to 512).map(_ => "a").fold("")(_ + _))
      val desc1 = Description((0 to 512).map(_ => "b").fold("")(_ + _))

      val amounts = Seq(
        PaybackAmount(Id(6), Usd(14.6), desc0, DateTime.now),
        CategoryAmount(Id(7), Usd(1.6), desc1, DateTime.now)
      )

      transactionValidationInterpreter.amountDescAreValid(
        fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DescriptionTooLong(amountName, desc0)).value
    }
    "should return Right(()) when descriptions are correct length" in {
      val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))

      val amounts = fakeTransactionWithId.amounts.map {
        case c@CategoryAmount(_, _, _, _) => c.copy(description = desc)
        case c@PaybackAmount(_, _, _, _) => c.copy(description = desc)
      }

      transactionValidationInterpreter.amountDescAreValid(
        fakeTransactionWithId.copy(amounts = amounts)
      ).value shouldEqual EitherT.rightT[IdMonad, DescriptionTooLong](()).value
    }
  }
  "categoryIdsExist" - {
    "should return Left(DoesNotExist) for first category id that does not exist" in {
      val categoryId0 = Id(15)
      val categoryId1 = Id(16)
      val amounts = Seq(
        PaybackAmount(Id(14), Usd(14.6), Description("amountDesc0"), DateTime.now),
        CategoryAmount(categoryId0, Usd(14.6), Description("amountDesc1"), DateTime.now),
        CategoryAmount(categoryId1, Usd(1.6), Description("amountDesc2"), DateTime.now)
      )

      (mockCategoryRepository get _).when(categoryId0).returns(None.pure[IdMonad])
      (mockCategoryRepository get _).when(categoryId1).returns(None.pure[IdMonad])

      transactionValidationInterpreter.categoryIdsExist(fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category"), categoryId0)).value
    }
    "should return Right(()) when category ids exist" in {
      val categoryId0 = Id(15)
      val categoryId1 = Id(16)
      val amounts = Seq(
        PaybackAmount(Id(14), Usd(14.6), Description("amountDesc0"), DateTime.now),
        CategoryAmount(categoryId0, Usd(14.6), Description("amountDesc1"), DateTime.now),
        CategoryAmount(categoryId1, Usd(1.6), Description("amountDesc2"), DateTime.now)
      )

      val category = Category(Some(Id(1)), None, Name("name"), Description("desc"), Always, Seq.empty)

      (mockCategoryRepository get _).when(categoryId0).returns(Some(category).pure[IdMonad])
      (mockCategoryRepository get _).when(categoryId1).returns(Some(category).pure[IdMonad])

      transactionValidationInterpreter.categoryIdsExist(fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "paybackIdsExist" - {
    "should return Left(DoesNotExist) for first category id that does not exist" in {
      val paybackId0 = Id(15)
      val paybackId1 = Id(16)
      val amounts = Seq(
        CategoryAmount(Id(14), Usd(14.6), Description("amountDesc0"), DateTime.now),
        PaybackAmount(paybackId0, Usd(14.6), Description("amountDesc1"), DateTime.now),
        PaybackAmount(paybackId1, Usd(1.6), Description("amountDesc2"), DateTime.now)
      )

      (mockPaybackRepository get _).when(paybackId0).returns(None.pure[IdMonad])
      (mockPaybackRepository get _).when(paybackId1).returns(None.pure[IdMonad])

      transactionValidationInterpreter.paybackIdsExists(fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Payback"), paybackId0)).value
    }
    "should return Right(()) when category ids exist" in {
      val paybackId0 = Id(15)
      val paybackId1 = Id(16)
      val amounts = Seq(
        CategoryAmount(Id(14), Usd(14.6), Description("amountDesc0"), DateTime.now),
        PaybackAmount(paybackId0, Usd(14.6), Description("amountDesc1"), DateTime.now),
        PaybackAmount(paybackId1, Usd(1.6), Description("amountDesc2"), DateTime.now)
      )

      val payback = Payback(Some(Id(1)), Name("name"), Description("desc"), DateTime.now)

      (mockPaybackRepository get _).when(paybackId0).returns(Some(payback).pure[IdMonad])
      (mockPaybackRepository get _).when(paybackId1).returns(Some(payback).pure[IdMonad])

      transactionValidationInterpreter.paybackIdsExists(fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
}
