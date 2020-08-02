package com.finance.service.handlers

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import cats.data.EitherT
import cats.{Id => IdMonad}
import com.finance.business.model.transfer.{Transfer => TransferModel}
import com.finance.business.model.types.{Description, Id, Usd}
import com.finance.business.repository.TransferRepository
import com.finance.business.services.TransferService
import com.finance.business.validation.TransferValidationAlgebra
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.definitions.{Error, Transfer}
import com.finance.service.endpoints.transfer.{CreateTransferResponse, DeleteTransferResponse, UpdateTransferResponse}
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransferHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {
  private case object TestError extends ValidationError

  private class TransferServiceTest
      extends TransferService[IdMonad](
        stub[TransferValidationAlgebra[IdMonad]],
        stub[TransferRepository[IdMonad]]
      )

  private val mockTransferService = stub[TransferServiceTest]

  private val handler = new TransferHandlerImpl(mockTransferService)

  private val localDate = LocalDate.now
  private val offsetDateTime = OffsetDateTime.of(localDate, LocalTime.MIN, ZoneOffset.UTC)
  private val transfer = Transfer(6, 7, localDate, 8, localDate, 150)
  private val transferModel = TransferModel(Some(Id(6)), Id(7), offsetDateTime, Id(8), offsetDateTime, Usd(150))

  "TransferHandlerImpl" - {
    "createTransfer" - {
      "should return CreateTransferResponse.BadRequest on error in service" in {
        (mockTransferService.create _).when(transferModel.copy(id = None)).returns(EitherT.leftT(TestError))

        handler.createTransfer(CreateTransferResponse)(Some(transfer)) shouldEqual
          CreateTransferResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateTransferResponse.BadRequest on null body" in {
        handler.createTransfer(CreateTransferResponse)(None) shouldEqual
          CreateTransferResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateTransferResponse.Ok with account on successful create in service" in {
        (mockTransferService.create _)
          .when(transferModel.copy(id = None))
          .returns(EitherT.rightT(transferModel))

        handler.createTransfer(CreateTransferResponse)(Some(transfer)) shouldEqual
          CreateTransferResponse.Ok(transfer)
      }
    }

    "updateTransfer" - {
      val id = 6
      "should return UpdateTransferResponse.BadRequest on error in service" in {
        (mockTransferService.update _)
          .when(transferModel.copy(id = Some(Id(id))))
          .returns(EitherT.leftT(TestError))

        handler.updateTransfer(UpdateTransferResponse)(id, Some(transfer)) shouldEqual
          UpdateTransferResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return UpdateTransferResponse.BadRequest on null body" in {
        handler.updateTransfer(UpdateTransferResponse)(id, None) shouldEqual
          UpdateTransferResponse.BadRequest(Error(Some("body is null")))
      }
      "should return UpdateTransferResponse.Ok with account on successful update in service" in {
        (mockTransferService.update _)
          .when(transferModel.copy(id = Some(Id(id))))
          .returns(EitherT.rightT(transferModel))

        handler.updateTransfer(UpdateTransferResponse)(id, Some(transfer)) shouldEqual
          UpdateTransferResponse.Ok(transfer)
      }
    }

    "deleteTransfer" - {
      val id = 1
      "should return DeleteTransferResponse.BadRequest on error in service" in {
        (mockTransferService.delete _).when(Id(id)).returns(EitherT.leftT(TestError))

        handler.deleteTransfer(DeleteTransferResponse)(id) shouldEqual
          DeleteTransferResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return DeleteTransferResponse.Ok on successful delete" in {
        (mockTransferService.delete _).when(Id(id)).returns(EitherT.rightT(()))

        handler.deleteTransfer(DeleteTransferResponse)(id) shouldEqual
          DeleteTransferResponse.Ok
      }
    }
  }
}
