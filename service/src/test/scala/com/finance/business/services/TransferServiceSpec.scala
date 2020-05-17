package com.finance.business.services

import cats.data.EitherT
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.transfer.Transfer
import com.finance.business.model.types.{Id, ModelName, Usd}
import com.finance.business.repository.TransferRepository
import com.finance.business.validation.TransferValidationAlgebra
import com.finance.business.validation.errors.{DoesNotExist, IdMustBeNone, ValidationError}
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransferServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[TransferValidationAlgebra[IdMonad]]
  private val mockRepository = mock[TransferRepository[IdMonad]]

  private val service = new TransferService[IdMonad](mockValidationAlgebra, mockRepository)

  private val transferId = Id(1)
  private val transfer = Transfer(Some(transferId), Id(2), DateTime.now, Id(3), DateTime.now, Usd(56))
  "TransferService" - {
    "create" - {
      "returns Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Transaction")))
        (mockValidationAlgebra idIsNone _) when transfer returns returnVal
        (mockRepository create _) expects transfer never

        service.create(transfer) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra fromAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.to))
        (mockValidationAlgebra idIsNone _) when transfer returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns returnVal
        (mockRepository create _) expects transfer never

        service.create(transfer) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra toAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.from))
        (mockValidationAlgebra idIsNone _) when transfer returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns returnVal
        (mockRepository create _) expects transfer never

        service.create(transfer) shouldEqual returnVal
      }
      "returns Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when transfer returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository create _) expects transfer returns transfer.pure[IdMonad]

        service.create(transfer) shouldEqual EitherT.rightT[IdMonad, ValidationError](transfer)
      }
    }
    "update" - {
      "returns Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Transaction")))
        (mockValidationAlgebra exists _) when transfer returns returnVal
        (mockRepository update _) expects transfer never

        service.update(transfer) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra fromAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.to))
        (mockValidationAlgebra exists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns returnVal
        (mockRepository update _) expects transfer never

        service.update(transfer) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra toAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.from))
        (mockValidationAlgebra exists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns returnVal
        (mockRepository update _) expects transfer never

        service.update(transfer) shouldEqual returnVal
      }
      "returns Right(()) and updates model when validation passes" in {
        (mockValidationAlgebra exists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository update _) expects transfer returns transfer.pure[IdMonad]

        service.update(transfer) shouldEqual EitherT.rightT[IdMonad, ValidationError](transfer)
      }
    }
    "delete" - {
      "returns Right(()) and deletes" in {
        (mockRepository delete _) expects transferId returns ().pure[IdMonad]

        service.delete(transferId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
  }
}