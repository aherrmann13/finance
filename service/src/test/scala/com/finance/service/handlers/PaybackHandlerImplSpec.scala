package com.finance.service.handlers

import java.time.{LocalDate, LocalTime, OffsetDateTime, ZoneOffset}

import cats.data.EitherT
import cats.{Id => IdMonad}
import com.finance.business.model.payback.{Payback => PaybackModel, PaybackBalance => PaybackBalanceModel}
import com.finance.business.model.transaction.{PaybackAmount => PaybackAmountModel}
import com.finance.business.model.types._
import com.finance.business.repository.{PaybackRepository, TransactionRepository}
import com.finance.business.services.PaybackService
import com.finance.business.validation.PaybackValidationAlgebra
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.definitions.{Error, Payback, PaybackAmount, PaybackBalance, PaybackBalanceQuery}
import com.finance.service.endpoints.payback._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PaybackHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {

  private case object TestError extends ValidationError

  private class PaybackServiceTest
      extends PaybackService[IdMonad](
        stub[PaybackValidationAlgebra[IdMonad]],
        stub[PaybackRepository[IdMonad]],
        stub[TransactionRepository[IdMonad]]
      )

  private val mockPaybackService = stub[PaybackServiceTest]
  private val handler = new PaybackHandlerImpl(mockPaybackService)

  private val localDate = LocalDate.now
  private val offsetDateTime = OffsetDateTime.of(localDate, LocalTime.MIN, ZoneOffset.UTC)

  private val payback = Payback(id = 5, name = "name", description = "description", date = localDate)
  private val paybackModel = PaybackModel(
    id = Some(Id(5)),
    name = Name("name"),
    description = Description("description"),
    date = offsetDateTime
  )

  private val paybackBalance = PaybackBalance(
    payback = PaybackBalance.Payback(
      id = 5,
      name = "name",
      description = "description",
      date = localDate
    ),
    paybackAmounts = Vector(
      PaybackAmount(6, 50.0, "desc0", localDate),
      PaybackAmount(7, 40.0, "desc1", localDate.plusDays(1)),
      PaybackAmount(8, 60.0, "desc2", localDate.plusDays(2))
    )
  )

  private val paybackBalanceModel = PaybackBalanceModel(
    payback = PaybackModel(
      id = Some(Id(5)),
      name = Name("name"),
      description = Description("description"),
      date = offsetDateTime
    ),
    amounts = Seq(
      PaybackAmountModel(Id(5), Id(6), Usd(50.0), Description("desc0"), offsetDateTime),
      PaybackAmountModel(Id(6), Id(7), Usd(40.0), Description("desc1"), offsetDateTime.plusDays(1)),
      PaybackAmountModel(Id(7), Id(8), Usd(60.0), Description("desc2"), offsetDateTime.plusDays(2))
    )
  )

  "PaybackHandlerImpl" - {
    "createPayback" - {
      "should return CreatePaybackResponse.BadRequest on error in service" in {
        (mockPaybackService.create _).when(paybackModel.copy(id = None)).returns(EitherT.leftT(TestError))

        handler.createPayback(CreatePaybackResponse)(Some(payback)) shouldEqual
          CreatePaybackResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreatePaybackResponse.BadRequest on null body" in {
        handler.createPayback(CreatePaybackResponse)(None) shouldEqual
          CreatePaybackResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreatePaybackResponse.Ok with payback on successful create in service" in {
        (mockPaybackService.create _).when(paybackModel.copy(id = None)).returns(EitherT.rightT(paybackModel))

        handler.createPayback(CreatePaybackResponse)(Some(payback)) shouldEqual
          CreatePaybackResponse.Ok(payback)
      }
    }
    "updatePayback" - {
      val id = 1
      "should return UpdatePaybackResponse.BadRequest on error in service" in {
        (mockPaybackService.update _).when(paybackModel.copy(id = Some(Id(id)))).returns(EitherT.leftT(TestError))

        handler.updatePayback(UpdatePaybackResponse)(id, Some(payback)) shouldEqual
          UpdatePaybackResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return UpdatePaybackResponse.BadRequest on null body" in {
        handler.updatePayback(UpdatePaybackResponse)(id, None) shouldEqual
          UpdatePaybackResponse.BadRequest(Error(Some("body is null")))
      }
      "should return UpdatePaybackResponse.Ok with payback on successful update in service" in {
        (mockPaybackService.update _).when(paybackModel.copy(id = Some(Id(id)))).returns(EitherT.rightT(paybackModel))

        handler.updatePayback(UpdatePaybackResponse)(id, Some(payback)) shouldEqual
          UpdatePaybackResponse.Ok(payback)
      }
    }
    "deletePayback" - {
      val id = 1
      "should return DeletePaybackResponse.BadRequest on error in service" in {
        (mockPaybackService.delete _).when(Id(id)).returns(EitherT.leftT(TestError))

        handler.deletePayback(DeletePaybackResponse)(id) shouldEqual
          DeletePaybackResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return DeletePaybackResponse.Ok with on successful delete in service" in {
        (mockPaybackService.delete _).when(Id(id)).returns(EitherT.rightT(()))

        handler.deletePayback(DeletePaybackResponse)(id) shouldEqual DeletePaybackResponse.Ok
      }
    }
    "getPaybackBalance" - {
      val queryRange = PaybackBalanceQuery.Range(localDate, localDate)
      val modelRange = DateRange(offsetDateTime, offsetDateTime)
      "should return GetPaybackBalanceResponse.Ok with balances from service" in {
        (mockPaybackService.getPaybackBalance _)
          .when(modelRange)
          .returns(Seq(paybackBalanceModel, paybackBalanceModel))

        handler.getPaybackBalance(GetPaybackBalanceResponse)(PaybackBalanceQuery(queryRange)) shouldEqual
          GetPaybackBalanceResponse.Ok(Vector(paybackBalance, paybackBalance))
      }
    }
    "getPaybackBalanceSummary" - {
      val amount = 100
      "should return GetPaybackBalanceResponse.Ok with balance from service" in {
        (mockPaybackService.getPaybackBalanceSummary _).when().returns(Usd(amount))

        handler.getPaybackBalanceSummary(GetPaybackBalanceSummaryResponse)() shouldEqual
          GetPaybackBalanceSummaryResponse.Ok(amount)
      }
    }
  }
}
