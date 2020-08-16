package com.finance.service.handlers

import java.time.OffsetDateTime

import cats.data.EitherT
import cats.{Id => IdMonad}
import com.finance.business.model.asset.{
  Buy => BuyModel,
  CashDividend => CashDividendModel,
  FifoSell => FifoSellModel,
  LifoSell => LifoSellModel,
  Stock => StockModel,
  StockDividend => StockDividendModel,
  StockValue => StockValueModel
}
import com.finance.business.model.types.{Id, Usd}
import com.finance.business.remotecalls.StockPriceRetriever
import com.finance.business.repository.AssetRepository
import com.finance.business.services.AssetService
import com.finance.business.validation.AssetValidationAlgebra
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.asset.{
  CreateAssetResponse,
  DeleteAssetResponse,
  GetStockValueResponse,
  UpdateAssetResponse
}
import com.finance.service.endpoints.definitions.StockAction.Type.members._
import com.finance.service.endpoints.definitions.{Asset, Error, Stock, StockAction, StockValue}
import com.finance.service.time.TimeProvider
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {

  private case object TestError extends ValidationError

  // https://github.com/paulbutcher/ScalaMock/issues/56
  // theoretically this is solved but still throws errors
  private class AssetServiceTest
      extends AssetService[IdMonad](
        stub[AssetValidationAlgebra[IdMonad]],
        stub[AssetRepository[IdMonad]],
        stub[StockPriceRetriever[IdMonad]]
      )
  private val mockAssetService = stub[AssetServiceTest]

  private val date: IdMonad[OffsetDateTime] = OffsetDateTime.now
  private implicit def idTimeProviderInstance: TimeProvider[IdMonad] =
    new TimeProvider[IdMonad] {
      override def now: IdMonad[OffsetDateTime] = date
    }

  private val handler = new AssetHandlerImpl[IdMonad](mockAssetService)

  private val stock = Stock(5, 6, "ticker", Vector.empty)
  private val stockModel = StockModel(Some(Id(5)), Id(6), "ticker", Seq.empty)
  private val stockValue = StockValue(
    stock = Stock(
      5,
      6,
      "ticker",
      Vector(
        StockAction(date, Buy, 10, 50, 500),
        StockAction(date, LifoSell, 4, 50, 200),
        StockAction(date, FifoSell, 4, 50, 200),
        StockAction(date, CashDividend, 1, 50, 50),
        StockAction(date, StockDividend, 1, 50, 50)
      )
    ),
    price = 50,
    asOf = date,
    quantity = 100,
    daysChange = 25,
    daysChangePercentage = 50,
    daysGain = 2500,
    pricePaid = 1000,
    totalGain = 4000,
    value = 5000
  )
  private val stockValueModel = StockValueModel(
    stock = StockModel(
      Some(Id(5)),
      Id(6),
      "ticker",
      Seq(
        BuyModel(date, 10, Usd(50), Usd(500)),
        LifoSellModel(date, 4, Usd(50), Usd(200)),
        FifoSellModel(date, 4, Usd(50), Usd(200)),
        CashDividendModel(date, 1, Usd(50), Usd(50)),
        StockDividendModel(date, 1, Usd(50), Usd(50))
      )
    ),
    price = Usd(50),
    asOf = date,
    quantity = 100,
    daysChange = Usd(25),
    daysChangePercentage = 50,
    daysGain = Usd(2500),
    pricePaid = Usd(1000),
    totalGain = Usd(4000),
    value = Usd(5000)
  )

  "AssetHandlerImpl" - {
    "createAsset" - {
      "should return CreateAssetResponse.BadRequest on error in service" in {
        (mockAssetService.create _).when(stockModel.copy(id = None)).returns(EitherT.leftT(TestError))

        handler.createAsset(CreateAssetResponse)(Some(Asset(Some(stock)))) shouldEqual
          CreateAssetResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateAssetResponse.BadRequest when body is None" in {
        handler.createAsset(CreateAssetResponse)(None) shouldEqual
          CreateAssetResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateAssetResponse.BadRequest when stock in body is None" in {
        handler.createAsset(CreateAssetResponse)(Some(Asset(None))) shouldEqual
          CreateAssetResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateAssetResponse.Ok with asset on successful create in service" in {
        (mockAssetService.create _).when(stockModel.copy(id = None)).returns(EitherT.rightT(stockModel))

        handler.createAsset(CreateAssetResponse)(Some(Asset(Some(stock)))) shouldEqual
          CreateAssetResponse.Ok(Asset(Some(stock)))
      }
    }
    "updateAsset" - {
      val id = 10
      "should return UpdateAssetResponse.BadRequest on error in service" in {
        (mockAssetService.update _).when(stockModel.copy(id = Some(Id(id)))).returns(EitherT.leftT(TestError))

        handler.updateAsset(UpdateAssetResponse)(id, Some(Asset(Some(stock)))) shouldEqual
          UpdateAssetResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return UpdateAssetResponse.BadRequest when body is None" in {
        handler.updateAsset(UpdateAssetResponse)(id, None) shouldEqual
          UpdateAssetResponse.BadRequest(Error(Some("body is null")))
      }
      "should return UpdateAssetResponse.BadRequest when stock in body is None" in {
        handler.updateAsset(UpdateAssetResponse)(id, Some(Asset(None))) shouldEqual
          UpdateAssetResponse.BadRequest(Error(Some("body is null")))
      }
      "should return UpdateAssetResponse.Ok with asset on successful update in service" in {
        (mockAssetService.update _).when(stockModel.copy(id = Some(Id(id)))).returns(EitherT.rightT(stockModel))

        handler.updateAsset(UpdateAssetResponse)(id, Some(Asset(Some(stock)))) shouldEqual
          UpdateAssetResponse.Ok(Asset(Some(stock)))
      }
    }
    "deleteAsset" - {
      val id = 5
      "should return DeleteAssetResponse.BadRequest on error in service" in {
        (mockAssetService.delete _).when(Id(id)).returns(EitherT.leftT(TestError))

        handler.deleteAsset(DeleteAssetResponse)(id) shouldEqual
          DeleteAssetResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return DeleteAssetResponse.Ok with asset on successful delete in service" in {
        (mockAssetService.delete _).when(Id(id)).returns(EitherT.rightT(()))

        handler.deleteAsset(DeleteAssetResponse)(id) shouldEqual DeleteAssetResponse.Ok
      }
    }
    "getStockValue" - {
      "should return GetStockValueResponse.Ok with stock value from service" in {
        (mockAssetService.getStockValue _).when(date).returns(Seq(stockValueModel, stockValueModel, stockValueModel))

        handler.getStockValue(GetStockValueResponse)() shouldEqual
          GetStockValueResponse.Ok(Vector(stockValue, stockValue, stockValue))
      }
    }
  }
}
