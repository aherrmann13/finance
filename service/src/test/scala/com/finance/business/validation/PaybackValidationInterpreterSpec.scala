package com.finance.business.validation

import java.time.OffsetDateTime

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.payback.Payback
import com.finance.business.model.types.{Description, Id, ModelName, Name}
import com.finance.business.repository.{PaybackRepository, TransactionRepository}
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PaybackValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockPaybackRepository = stub[PaybackRepository[IdMonad]]
  private val mockTransactionRepository = stub[TransactionRepository[IdMonad]]

  private val paybackValidationInterpreter = new PaybackValidationInterpreter[IdMonad](
    mockPaybackRepository,
    mockTransactionRepository
  )

  private val paybackName = ModelName("Payback")
  private val fakePaybackWithId = Payback(Some(Id(2)), Name("Name"), Description("Description"), OffsetDateTime.now)
  private val fakePaybackWithNoId = fakePaybackWithId.copy(id = None)

  "PaybackValidationInterpreter" - {
    "idIsNone" - {
      "should return Left(IdMustBeNone) when id is Some" in {
        paybackValidationInterpreter.idIsNone(fakePaybackWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](IdMustBeNone(paybackName)).value
      }
      "should return Right(()) when id is None" in {
        paybackValidationInterpreter.idIsNone(fakePaybackWithNoId).value shouldEqual
          EitherT.rightT[IdMonad, IdMustBeNone](()).value
      }
    }
    "exists" - {
      "should return Left(DoesNotExist) when id is None" in {
        paybackValidationInterpreter.exists(fakePaybackWithNoId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(paybackName)).value
      }
      "should return Left(DoesNotExist) when repository does not contain Account" in {
        (mockPaybackRepository get _).when(fakePaybackWithId.id.get).returns(OptionT.none)
        paybackValidationInterpreter.exists(fakePaybackWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(paybackName, fakePaybackWithId.id)).value
      }
      "should return Right(()) when repository contains Account" in {
        (mockPaybackRepository get _).when(fakePaybackWithId.id.get).returns(OptionT.pure(fakePaybackWithId))
        paybackValidationInterpreter.exists(fakePaybackWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "nameIsValid" - {
      "should return Left(NameIsTooLong) when name is too long" in {
        val name = Name((0 to 128).map(_ => "a").fold("")(_ + _))
        paybackValidationInterpreter.nameIsValid(fakePaybackWithId.copy(name = name)).value shouldEqual
          EitherT.leftT[IdMonad, Unit](NameTooLong(paybackName, name)).value
      }
      "should return Right(()) when name is correct length" in {
        val name = Name((0 to 127).map(_ => "a").fold("")(_ + _))
        paybackValidationInterpreter.nameIsValid(fakePaybackWithId.copy(name = name)).value shouldEqual
          EitherT.rightT[IdMonad, NameTooLong](()).value
      }
    }
    "descriptionIsValid" - {
      "should return Left(DescriptionIsTooLong) when description is too long" in {
        val desc = Description((0 to 512).map(_ => "a").fold("")(_ + _))
        paybackValidationInterpreter.descriptionIsValid(fakePaybackWithId.copy(description = desc)).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DescriptionTooLong(paybackName, desc)).value
      }
      "should return Right(()) when description is correct length" in {
        val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))
        paybackValidationInterpreter.descriptionIsValid(fakePaybackWithId.copy(description = desc)).value shouldEqual
          EitherT.rightT[IdMonad, DescriptionTooLong](()).value
      }
      "hasNoTransactions" - {
        "should return Left(HasTransactions) if there are transactions with account id" in {
          (mockTransactionRepository anyWithPaybackId _).when(fakePaybackWithId.id.get).returns(true.pure[IdMonad])
          paybackValidationInterpreter.hasNoTransactions(fakePaybackWithId.id.get).value shouldEqual
            EitherT.leftT[IdMonad, Unit](HasTransactions(paybackName)).value
        }
        "should return Right(()) if there are no transactions with no account id" in {
          (mockTransactionRepository anyWithPaybackId _).when(fakePaybackWithId.id.get).returns(false.pure[IdMonad])
          paybackValidationInterpreter.hasNoTransactions(fakePaybackWithId.id.get).value shouldEqual
            EitherT.rightT[IdMonad, HasTransactions](()).value
        }
      }
    }
  }
}