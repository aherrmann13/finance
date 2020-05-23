package com.finance.business.validation

import cats.data.EitherT
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.account.{Account, Bank}
import com.finance.business.model.transfer.Transfer
import com.finance.business.model.types._
import com.finance.business.repository.{AccountRepository, TransferRepository}
import com.finance.business.validation.errors.{DoesNotExist, IdMustBeNone}
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransferValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockTransferRepository = stub[TransferRepository[IdMonad]]
  private val mockAccountRepository = stub[AccountRepository[IdMonad]]

  private val transferValidationInterpreter = new TransferValidationInterpreter[IdMonad](
    mockTransferRepository,
    mockAccountRepository
  )

  private val transferName = ModelName("Transfer")
  private val accountName = ModelName("Account")

  private val fakeTransferWithId = Transfer(Some(Id(3)), Id(4), DateTime.now, Id(5), DateTime.now, Usd(56.7))
  private val fakeTransferWithNoId = fakeTransferWithId.copy(id = None)

  "TransferValidationInterpreter" - {
    "idIsNone" - {
      "should return Left(IdMustBeNone) when id is Some" in {
        transferValidationInterpreter.idIsNone(fakeTransferWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](IdMustBeNone(transferName)).value
      }
      "should return Right(()) when id is None" in {
        transferValidationInterpreter.idIsNone(fakeTransferWithNoId).value shouldEqual
          EitherT.rightT[IdMonad, IdMustBeNone](()).value
      }
    }
    "exists" - {
      "should return Left(DoesNotExist) when id is None" in {
        transferValidationInterpreter.exists(fakeTransferWithNoId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(transferName)).value
      }
      "should return Left(DoesNotExist) when repository does not contain Account" in {
        (mockTransferRepository get(_: Id)).when(fakeTransferWithId.id.get).returns(None.pure[IdMonad])
        transferValidationInterpreter.exists(fakeTransferWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(transferName, fakeTransferWithId.id)).value
      }
      "should return Right(()) when repository contains Account" in {
        (mockTransferRepository get(_: Id)).when(fakeTransferWithId.id.get).returns(Some(fakeTransferWithId).pure[IdMonad])
        transferValidationInterpreter.exists(fakeTransferWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "toAccountIdExists" - {
      "should return Left(DoesNotExist) when account repository does not contain to id" in {
        (mockAccountRepository get _).when(fakeTransferWithId.to).returns(None.pure[IdMonad])
        transferValidationInterpreter.toAccountIdExists(fakeTransferWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(accountName, fakeTransferWithId.to)).value
      }
      "should return Right(()) when account repository contains to id" in {
        (mockAccountRepository get _)
          .when(fakeTransferWithId.to)
          .returns(Some(Account(Some(Id(4)), Name("Name"), Description("Description"), Bank)).pure[IdMonad])
        transferValidationInterpreter.toAccountIdExists(fakeTransferWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "fromAccountIdExists" - {
      "should return Left(DoesNotExist) when account repository does not contain from id" in {
        (mockAccountRepository get _).when(fakeTransferWithId.from).returns(None.pure[IdMonad])
        transferValidationInterpreter.fromAccountIdExists(fakeTransferWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(accountName, fakeTransferWithId.from)).value
      }
      "should return Right(()) when account repository contains from id" in {
        (mockAccountRepository get _)
          .when(fakeTransferWithId.from)
          .returns(Some(Account(Some(Id(4)), Name("Name"), Description("Description"), Bank)).pure[IdMonad])
        transferValidationInterpreter.fromAccountIdExists(fakeTransferWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
  }
}
