package com.finance.business.validation

import java.time.OffsetDateTime

import cats.data.{EitherT, OptionT}
import cats.{Id => IdMonad}
import com.finance.business.model.account.{Account, Bank}
import com.finance.business.model.category.{Budget, Category}
import com.finance.business.model.payback.Payback
import com.finance.business.model.source.Source
import com.finance.business.model.transaction.{CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.types._
import com.finance.business.repository._
import com.finance.business.validation.errors.{DateNotInEffectiveTime, DescriptionTooLong, DoesNotExist, IdMustBeNone}
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
    OffsetDateTime.now,
    Id(4),
    Seq(
      PaybackAmount(Id(6), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
      CategoryAmount(Id(7), Id(5), Usd(1.6), Description("amountDesc1"), OffsetDateTime.now)
    )
  )
  private val fakeTransactionWithNoId = fakeTransactionWithId.copy(id = None)

  "TransactionValidationInterpreter" - {
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
        (mockTransactionRepository get (_: Id)).when(fakeTransactionWithId.id.get).returns(OptionT.none)
        transactionValidationInterpreter.exists(fakeTransactionWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(transactionName, fakeTransactionWithId.id)).value
      }
      "should return Right(()) when repository contains Account" in {
        (mockTransactionRepository get (_: Id))
          .when(fakeTransactionWithId.id.get)
          .returns(OptionT.pure(fakeTransactionWithId))
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
    "accountIdExists" - {
      "should return Left(DoesNotExist) when account does not exist" in {
        (mockAccountRepository get _).when(fakeTransactionWithId.accountId).returns(OptionT.none)
        transactionValidationInterpreter.accountIdExists(fakeTransactionWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), fakeTransactionWithId.accountId)).value
      }
      "should return Right(()) when account exists" in {
        (mockAccountRepository get _)
          .when(fakeTransactionWithId.accountId)
          .returns(OptionT.pure(Account(Some(Id(2)), Name("Name"), Description("Description"), Bank, Usd(50))))
        transactionValidationInterpreter.accountIdExists(fakeTransactionWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "amountDescAreValid" - {
      "should return Left(DescriptionIsTooLong) for first description is too long" in {
        val desc0 = Description((0 to 512).map(_ => "a").fold("")(_ + _))
        val desc1 = Description((0 to 512).map(_ => "b").fold("")(_ + _))

        val amounts = Seq(
          PaybackAmount(Id(6), Id(6), Usd(14.6), desc0, OffsetDateTime.now),
          CategoryAmount(Id(7), Id(6), Usd(1.6), desc1, OffsetDateTime.now)
        )

        transactionValidationInterpreter.amountDescAreValid(
          fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DescriptionTooLong(amountName, desc0)).value
      }
      "should return Right(()) when descriptions are correct length" in {
        val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))

        val amounts = fakeTransactionWithId.amounts.map {
          case c@CategoryAmount(_, _, _, _, _) => c.copy(description = desc)
          case c@PaybackAmount(_, _, _, _, _) => c.copy(description = desc)
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
          PaybackAmount(Id(14), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          CategoryAmount(categoryId0, Id(5), Usd(14.6), Description("amountDesc1"), OffsetDateTime.now),
          CategoryAmount(categoryId1, Id(5), Usd(1.6), Description("amountDesc2"), OffsetDateTime.now)
        )

        (mockCategoryRepository get _).when(categoryId0).returns(OptionT.none)
        (mockCategoryRepository get _).when(categoryId1).returns(OptionT.none)

        transactionValidationInterpreter.categoryIdsExist(fakeTransactionWithId.copy(amounts = amounts))
          .value shouldEqual EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Category"), categoryId0)).value
      }
      "should return Right(()) when category ids exist" in {
        val categoryId0 = Id(15)
        val categoryId1 = Id(16)
        val amounts = Seq(
          PaybackAmount(Id(14), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          CategoryAmount(categoryId0, Id(5), Usd(14.6), Description("amountDesc1"), OffsetDateTime.now),
          CategoryAmount(categoryId1, Id(5), Usd(1.6), Description("amountDesc2"), OffsetDateTime.now)
        )

        val category = Category(Some(Id(1)), None, Name("name"), Description("desc"), Seq.empty, Seq.empty)

        (mockCategoryRepository get _).when(categoryId0).returns(OptionT.pure(category))
        (mockCategoryRepository get _).when(categoryId1).returns(OptionT.pure(category))

        transactionValidationInterpreter.categoryIdsExist(fakeTransactionWithId.copy(amounts = amounts))
          .value shouldEqual EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "paybackIdsExist" - {
      "should return Left(DoesNotExist) for first category id that does not exist" in {
        val paybackId0 = Id(15)
        val paybackId1 = Id(16)
        val amounts = Seq(
          CategoryAmount(Id(14), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          PaybackAmount(paybackId0, Id(5), Usd(14.6), Description("amountDesc1"), OffsetDateTime.now),
          PaybackAmount(paybackId1, Id(5), Usd(1.6), Description("amountDesc2"), OffsetDateTime.now)
        )

        (mockPaybackRepository get _).when(paybackId0).returns(OptionT.none)
        (mockPaybackRepository get _).when(paybackId1).returns(OptionT.none)

        transactionValidationInterpreter.paybackIdsExists(fakeTransactionWithId.copy(amounts = amounts))
          .value shouldEqual EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Payback"), paybackId0)).value
      }
      "should return Right(()) when category ids exist" in {
        val paybackId0 = Id(15)
        val paybackId1 = Id(16)
        val amounts = Seq(
          CategoryAmount(Id(14), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          PaybackAmount(paybackId0, Id(5), Usd(14.6), Description("amountDesc1"), OffsetDateTime.now),
          PaybackAmount(paybackId1, Id(5), Usd(1.6), Description("amountDesc2"), OffsetDateTime.now)
        )

        val payback = Payback(Some(Id(1)), Name("name"), Description("desc"), OffsetDateTime.now)

        (mockPaybackRepository get _).when(paybackId0).returns(OptionT.pure(payback))
        (mockPaybackRepository get _).when(paybackId1).returns(OptionT.pure(payback))

        transactionValidationInterpreter.paybackIdsExists(fakeTransactionWithId.copy(amounts = amounts))
          .value shouldEqual EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "sourceIdsExists" - {
      val source = Source(Some(Id(2)), Name("Name"), Description("Description"))
      "should return Left(DoesNotExist) for first source id that does not exist" in {
        val amountId0 = Id(6)
        val amountId1 = Id(7)
        val amountId2 = Id(8)
        val amounts = Seq(
          PaybackAmount(Id(14), amountId0, Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          CategoryAmount(Id(15), amountId1, Usd(14.6), Description("amountDesc1"), OffsetDateTime.now),
          CategoryAmount(Id(16), amountId2, Usd(1.6), Description("amountDesc2"), OffsetDateTime.now)
        )
        (mockSourceRepository get _).when(amountId0).returns(OptionT.pure(source))
        (mockSourceRepository get _).when(amountId1).returns(OptionT.none)
        (mockSourceRepository get _).when(amountId2).returns(OptionT.none)

        transactionValidationInterpreter.sourceIdsExists(fakeTransactionWithId.copy(amounts = amounts))
          .value shouldEqual EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Source"), amountId1)).value
      }
      "should return Right(()) when source exists" in {
        val amounts = Seq(
          PaybackAmount(Id(14), Id(6), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          CategoryAmount(Id(15), Id(7), Usd(14.6), Description("amountDesc1"), OffsetDateTime.now),
          CategoryAmount(Id(16), Id(8), Usd(1.6), Description("amountDesc2"), OffsetDateTime.now)
        )
        amounts.foreach(a => (mockSourceRepository.get _).when(a.sourceId).returns(OptionT.pure(source)))

        transactionValidationInterpreter.sourceIdsExists(fakeTransactionWithId.copy(amounts = amounts))
          .value shouldEqual EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "reportingDateWithinBudgetTime" - {
      val category = Category(
        Some(Id(1)),
        None,
        Name("name"),
        Description("desc"),
        Seq.empty,
        Seq(
          Budget(Seq(DateRange(OffsetDateTime.now.minusMonths(1), OffsetDateTime.now)), Usd(13)),
          Budget(Seq(DateRange(OffsetDateTime.now, OffsetDateTime.now.plusMonths(1))), Usd(13))
        )
      )

      "should return Left(DateNotInEffectiveTime) for first reporting date not in category time" in {
        val badAmount = CategoryAmount(
          Id(7),
          Id(8),
          Usd(1.6),
          Description("amountDesc"),
          OffsetDateTime.now.minusYears(1)
        )
        (mockCategoryRepository get _).when(badAmount.categoryId).returns(OptionT.pure(category))

        val amounts = Seq(
          PaybackAmount(Id(6), Id(5), Usd(14.6), Description("amountDesc"), OffsetDateTime.now),
          CategoryAmount(badAmount.categoryId, Id(8), Usd(1.6), Description("amountDesc"), OffsetDateTime.now),
          badAmount
        )

        transactionValidationInterpreter.reportingDateWithinBudgetTime(
          fakeTransactionWithId.copy(amounts = amounts)
        ).value shouldEqual EitherT.leftT[IdMonad, Unit](
          DateNotInEffectiveTime(badAmount.reportingDate, category.budget.flatMap(_.effectiveTime))
        ).value
      }
      "should return Right(()) for PaybackAmount" in {
        val amounts = Seq(
          PaybackAmount(Id(6), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          PaybackAmount(Id(6), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          PaybackAmount(Id(6), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now)
        )

        transactionValidationInterpreter
          .reportingDateWithinBudgetTime(fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
          EitherT.rightT[IdMonad, DateNotInEffectiveTime](()).value
      }
      "should return Right(()) for CategoryAmount with category effectiveTime Always" in {
        val amounts = Seq(
          CategoryAmount(Id(6), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now.minusWeeks(1)),
          CategoryAmount(Id(7), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          CategoryAmount(Id(8), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now.plusWeeks(1))
        )

        amounts.foreach { amount =>
          (mockCategoryRepository get _)
            .when(amount.categoryId)
            .returns(OptionT.pure(category))
        }

        transactionValidationInterpreter
          .reportingDateWithinBudgetTime(fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
          EitherT.rightT[IdMonad, DateNotInEffectiveTime](()).value
      }
      "should return Right(()) for CategoryAmount with non existent category id" in {
        val amounts = Seq(
          CategoryAmount(Id(6), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now.minusYears(1)),
          CategoryAmount(Id(7), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now),
          CategoryAmount(Id(8), Id(5), Usd(14.6), Description("amountDesc0"), OffsetDateTime.now.plusYears(1))
        )

        amounts.foreach { amount => (mockCategoryRepository get _).when(amount.categoryId).returns(OptionT.none) }

        transactionValidationInterpreter
          .reportingDateWithinBudgetTime(fakeTransactionWithId.copy(amounts = amounts)).value shouldEqual
          EitherT.rightT[IdMonad, DateNotInEffectiveTime](()).value
      }
      "should return Right(()) for CategoryAmount with reportingDate within category effectiveTime" in {
        val catAmount = CategoryAmount(
          Id(7),
          Id(5),
          Usd(1.6),
          Description("amountDesc1"),
          OffsetDateTime.now.minusWeeks(1)
        )
        (mockCategoryRepository get _).when(catAmount.categoryId).returns(OptionT.some(category))

        transactionValidationInterpreter
          .reportingDateWithinBudgetTime(fakeTransactionWithId.copy(amounts = Seq(catAmount))).value shouldEqual
          EitherT.rightT[IdMonad, DateNotInEffectiveTime](()).value
      }
      "should return Right(()) for CategoryAmount with reportingDate on edge of category effectiveTime" in {
        val catAmount = CategoryAmount(
          Id(7),
          Id(5),
          Usd(1.6),
          Description("amountDesc1"),
          category.budget.head.effectiveTime.head.start
        )
        (mockCategoryRepository get _).when(catAmount.categoryId).returns(OptionT.some(category))

        transactionValidationInterpreter
          .reportingDateWithinBudgetTime(fakeTransactionWithId.copy(amounts = Seq(catAmount))).value shouldEqual
          EitherT.rightT[IdMonad, DateNotInEffectiveTime](()).value
      }
    }
  }
}