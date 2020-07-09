package com.finance.business.validation

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.source.Source
import com.finance.business.model.types.{Description, Id, ModelName, Name}
import com.finance.business.repository.{SourceRepository, TransactionRepository}
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SourceValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockSourceRepository = stub[SourceRepository[IdMonad]]
  private val mockTransactionRepository = stub[TransactionRepository[IdMonad]]

  private val sourceValidationInterpreter = new SourceValidationInterpreter[IdMonad](
    mockSourceRepository,
    mockTransactionRepository
  )

  private val sourceName = ModelName("Source")
  private val fakeSourceWithId = Source(Some(Id(2)), Name("Name"), Description("Description"))
  private val fakeSourceWithNoId = fakeSourceWithId.copy(id = None)

  "SourceValidationInterpreter" - {
    "idIsNone" - {
      "should return Left(IdMustBeNone) when id is Some" in {
        sourceValidationInterpreter.idIsNone(fakeSourceWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](IdMustBeNone(sourceName)).value
      }
      "should return Right(()) when id is None" in {
        sourceValidationInterpreter.idIsNone(fakeSourceWithNoId).value shouldEqual
          EitherT.rightT[IdMonad, IdMustBeNone](()).value
      }
    }
    "exists" - {
      "should return Left(DoesNotExist) when id is None" in {
        sourceValidationInterpreter.exists(fakeSourceWithNoId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(sourceName)).value
      }
      "should return Left(DoesNotExist) when repository does not contain Account" in {
        (mockSourceRepository get _).when(fakeSourceWithId.id.get).returns(OptionT.none)
        sourceValidationInterpreter.exists(fakeSourceWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(sourceName, fakeSourceWithId.id)).value
      }
      "should return Right(()) when repository contains Account" in {
        (mockSourceRepository get _).when(fakeSourceWithId.id.get).returns(OptionT.some(fakeSourceWithId))
        sourceValidationInterpreter.exists(fakeSourceWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "nameIsValid" - {
      "should return Left(NameIsTooLong) when name is too long" in {
        val name = Name((0 to 128).map(_ => "a").fold("")(_ + _))
        sourceValidationInterpreter.nameIsValid(fakeSourceWithId.copy(name = name)).value shouldEqual
          EitherT.leftT[IdMonad, Unit](NameTooLong(sourceName, name)).value
      }
      "should return Right(()) when name is correct length" in {
        val name = Name((0 to 127).map(_ => "a").fold("")(_ + _))
        sourceValidationInterpreter.nameIsValid(fakeSourceWithId.copy(name = name)).value shouldEqual
          EitherT.rightT[IdMonad, NameTooLong](()).value
      }
    }
    "descriptionIsValid" - {
      "should return Left(DescriptionIsTooLong) when description is too long" in {
        val desc = Description((0 to 512).map(_ => "a").fold("")(_ + _))
        sourceValidationInterpreter.descriptionIsValid(fakeSourceWithId.copy(description = desc)).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DescriptionTooLong(sourceName, desc)).value
      }
      "should return Right(()) when description is correct length" in {
        val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))
        sourceValidationInterpreter.descriptionIsValid(fakeSourceWithId.copy(description = desc)).value shouldEqual
          EitherT.rightT[IdMonad, DescriptionTooLong](()).value
      }
    }
    "hasNoTransactions" - {
      "should return Left(HasTransactions) if there are transactions with account id" in {
        (mockTransactionRepository anyWithSourceId _).when(fakeSourceWithId.id.get).returns(true.pure[IdMonad])
        sourceValidationInterpreter.hasNoTransactions(fakeSourceWithId.id.get).value shouldEqual
          EitherT.leftT[IdMonad, Unit](HasTransactions(sourceName)).value
      }
      "should return Right(()) if there are no transactions with no account id" in {
        (mockTransactionRepository anyWithSourceId _).when(fakeSourceWithId.id.get).returns(false.pure[IdMonad])
        sourceValidationInterpreter.hasNoTransactions(fakeSourceWithId.id.get).value shouldEqual
          EitherT.rightT[IdMonad, HasTransactions](()).value
      }
    }
  }
}