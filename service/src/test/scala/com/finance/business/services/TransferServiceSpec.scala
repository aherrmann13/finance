package com.finance.business.services

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.transfer.Transfer
import com.finance.business.model.types.{Id, ModelName, Usd}
import com.finance.business.repository.TransferRepository
import com.finance.business.repository.query.TransferQuery
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
      "should return Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Transaction")))
        (mockValidationAlgebra idIsNone _) when transfer returns returnVal
        (mockRepository create _) expects transfer never

        service.create(transfer) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra fromAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.to))
        (mockValidationAlgebra idIsNone _) when transfer returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns returnVal
        (mockRepository create _) expects transfer never

        service.create(transfer) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra toAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.from))
        (mockValidationAlgebra idIsNone _) when transfer returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns returnVal
        (mockRepository create _) expects transfer never

        service.create(transfer) shouldEqual returnVal
      }
      "should return Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when transfer returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository create _) expects transfer returns transfer.pure[IdMonad]

        service.create(transfer) shouldEqual EitherT.rightT[IdMonad, ValidationError](transfer)
      }
    }
    "update" - {
      "should return Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Transaction")))
        (mockValidationAlgebra exists _) when transfer returns returnVal
        (mockRepository update _) expects transfer never

        service.update(transfer) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra fromAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.to))
        (mockValidationAlgebra exists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns returnVal
        (mockRepository update _) expects transfer never

        service.update(transfer) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra toAccountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), transfer.from))
        (mockValidationAlgebra exists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns returnVal
        (mockRepository update _) expects transfer never

        service.update(transfer) shouldEqual returnVal
      }
      "should return Right(()) and updates model when validation passes" in {
        (mockValidationAlgebra exists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra fromAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra toAccountIdExists _) when transfer returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository update _) expects transfer returns transfer.pure[IdMonad]

        service.update(transfer) shouldEqual EitherT.rightT[IdMonad, ValidationError](transfer)
      }
    }
    "delete" - {
      "should return Right(()) and deletes" in {
        (mockRepository delete _) expects transferId returns ().pure[IdMonad]

        service.delete(transferId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
    "get" - {
      "should return repository get" in {
        (mockRepository get (_: Id)) expects transferId returns OptionT.pure(transfer)

        service.get(transferId).value shouldEqual Some(transfer)
      }
    }
    "getMany" - {
      "should return repository getMany" in {
        (mockRepository getMany _) expects Seq(transferId, Id(transferId.value + 1)) returns
          Seq(transfer, transfer).pure[IdMonad]

        service.getMany(Seq(transferId, Id(transferId.value + 1))) shouldEqual Seq(transfer, transfer)
      }
    }
    "getAll" - {
      "should return repository getAll" - {
        (mockRepository.getAll _).expects().returns(Seq(transfer, transfer).pure[IdMonad])

        service.getAll shouldEqual Seq(transfer, transfer)
      }
    }
    "get with query" - {
      "should return repository get with query" in {
        val query = TransferQuery(None, None, Set.empty, None, None)
        (mockRepository get (_: TransferQuery)) expects query returns Seq(transfer, transfer).pure[IdMonad]

        service.get(query) shouldEqual Seq(transfer, transfer)
      }
    }
  }
}