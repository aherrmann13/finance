package com.finance.service.handlers

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import cats.data.EitherT
import cats.{Id => IdMonad}
import com.finance.business.model.transaction.{Transaction => TransactionModel}
import com.finance.business.model.types.{Description, Id}
import com.finance.business.repository.TransactionRepository
import com.finance.business.services.TransactionService
import com.finance.business.validation.TransactionValidationAlgebra
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.definitions.{Error, Transaction}
import com.finance.service.endpoints.transaction.{
  CreateTransactionResponse,
  DeleteTransactionResponse,
  UpdateTransactionResponse
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TransactionHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {
  private case object TestError extends ValidationError

  private class TransactionServiceTest
      extends TransactionService[IdMonad](
        stub[TransactionValidationAlgebra[IdMonad]],
        stub[TransactionRepository[IdMonad]]
      )

  private val mockTransactionService = stub[TransactionServiceTest]

  private val handler = new TransactionHandlerImpl(mockTransactionService)

  private val localDate = LocalDate.now
  private val offsetDateTime = OffsetDateTime.of(localDate, LocalTime.MIN, ZoneOffset.UTC)
  private val transaction = Transaction(6, Vector.empty, "desc", localDate, 7)
  private val transactionModel = TransactionModel(Some(Id(6)), Description("desc"), offsetDateTime, Id(7), Seq.empty)

  "TransactionHandlerImpl" - {
    "createTransaction" - {
      "should return CreateTransactionResponse.BadRequest on error in service" in {
        (mockTransactionService.create _).when(transactionModel.copy(id = None)).returns(EitherT.leftT(TestError))

        handler.createTransaction(CreateTransactionResponse)(Some(transaction)) shouldEqual
          CreateTransactionResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateTransactionResponse.BadRequest on null body" in {
        handler.createTransaction(CreateTransactionResponse)(None) shouldEqual
          CreateTransactionResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateTransactionResponse.Ok with account on successful create in service" in {
        (mockTransactionService.create _)
          .when(transactionModel.copy(id = None))
          .returns(EitherT.rightT(transactionModel))

        handler.createTransaction(CreateTransactionResponse)(Some(transaction)) shouldEqual
          CreateTransactionResponse.Ok(transaction)
      }
    }

    "updateTransaction" - {
      val id = 6
      "should return UpdateTransactionResponse.BadRequest on error in service" in {
        (mockTransactionService.update _)
          .when(transactionModel.copy(id = Some(Id(id))))
          .returns(EitherT.leftT(TestError))

        handler.updateTransaction(UpdateTransactionResponse)(id, Some(transaction)) shouldEqual
          UpdateTransactionResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return UpdateTransactionResponse.BadRequest on null body" in {
        handler.updateTransaction(UpdateTransactionResponse)(id, None) shouldEqual
          UpdateTransactionResponse.BadRequest(Error(Some("body is null")))
      }
      "should return UpdateTransactionResponse.Ok with account on successful update in service" in {
        (mockTransactionService.update _)
          .when(transactionModel.copy(id = Some(Id(id))))
          .returns(EitherT.rightT(transactionModel))

        handler.updateTransaction(UpdateTransactionResponse)(id, Some(transaction)) shouldEqual
          UpdateTransactionResponse.Ok(transaction)
      }
    }

    "deleteTransaction" - {
      val id = 1
      "should return DeleteTransactionResponse.BadRequest on error in service" in {
        (mockTransactionService.delete _).when(Id(id)).returns(EitherT.leftT(TestError))

        handler.deleteTransaction(DeleteTransactionResponse)(id) shouldEqual
          DeleteTransactionResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return DeleteTransactionResponse.Ok on successful delete" in {
        (mockTransactionService.delete _).when(Id(id)).returns(EitherT.rightT(()))

        handler.deleteTransaction(DeleteTransactionResponse)(id) shouldEqual
          DeleteTransactionResponse.Ok
      }
    }
  }
}
