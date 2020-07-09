package com.finance.business.services

import java.time.OffsetDateTime

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Id => IdMonad}
import com.finance.business.model.asset._
import com.finance.business.model.types.{Id, ModelName, Usd}
import com.finance.business.operations.StockOps._
import com.finance.business.remotecalls.StockPriceRetriever
import com.finance.business.repository.AssetRepository
import com.finance.business.repository.query.StockQuery
import com.finance.business.validation.AssetValidationAlgebra
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[AssetValidationAlgebra[IdMonad]]
  private val mockRepository = mock[AssetRepository[IdMonad]]
  private val mockStockPriceRetriever = mock[StockPriceRetriever[IdMonad]]

  private val service = new AssetService[IdMonad](mockValidationAlgebra, mockRepository, mockStockPriceRetriever)

  private val assetId = Id(5)
  private val asset = new Asset {
    override val id: Option[Id] = Some(assetId)
    override val accountId: Id = Id(8)
  }

  "AssetService" - {
    "create" - {
      "should return Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Asset")))
        (mockValidationAlgebra idIsNone _) when asset returns returnVal
        (mockRepository create _) expects asset never

        service.create(asset) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra accountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account")))
        (mockValidationAlgebra idIsNone _) when asset returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra accountIdExists _) when asset returns returnVal
        (mockRepository create _) expects asset never

        service.create(asset) shouldEqual returnVal
      }
      "if asset is stock" - {
        val stock = Stock(Some(Id(2)), Id(19), "ticker", Seq.empty)

        "should return Left(StockActionsInvalid) from validation algebra stockActionsAreValid" in {
          val returnVal = EitherT.leftT[IdMonad, Unit][StockActionsInvalid](
            SellingMoreThanCurrentlyHave(FifoSell(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0)))
          )
          (mockValidationAlgebra idIsNone _) when stock returns EitherT.rightT[IdMonad, IdMustBeNone](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns returnVal
          (mockRepository create _) expects stock never

          service.create(stock) shouldEqual returnVal
        }
        "should return Right(()) and saves model when validation passes" in {
          (mockValidationAlgebra idIsNone _) when stock returns EitherT.rightT[IdMonad, IdMustBeNone](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns
            EitherT.rightT[IdMonad, StockActionsInvalid](())
          (mockRepository create _) expects stock returns stock.pure[IdMonad]

          service.create(stock) shouldEqual EitherT.rightT[IdMonad, ValidationError](stock)
        }
      }
      "should return Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when asset returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra accountIdExists _) when asset returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository create _) expects asset returns asset.pure[IdMonad]

        service.create(asset) shouldEqual EitherT.rightT[IdMonad, ValidationError](asset)
      }
    }
    "update" - {
      "should return Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Asset")))
        (mockValidationAlgebra exists _) when asset returns returnVal
        (mockRepository update _) expects asset never

        service.update(asset) shouldEqual returnVal
      }
      "should return Left(DoesNotExist) from validation algebra accountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account")))
        (mockValidationAlgebra idIsNone _) when asset returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra accountIdExists _) when asset returns returnVal
        (mockRepository create _) expects asset never

        service.create(asset) shouldEqual returnVal
      }
      "if asset is stock" - {
        val stock = Stock(Some(Id(2)), Id(11), "ticker", Seq.empty)

        "should return Left(StockActionsInvalid) from validation algebra stockActionsAreValid" in {
          val returnVal = EitherT.leftT[IdMonad, Unit][StockActionsInvalid](
            SellingMoreThanCurrentlyHave(FifoSell(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0)))
          )
          (mockValidationAlgebra exists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns returnVal
          (mockRepository update _) expects stock never

          service.update(stock) shouldEqual returnVal
        }
        "should return Right(()) and saves model when validation passes" in {
          (mockValidationAlgebra exists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns
            EitherT.rightT[IdMonad, StockActionsInvalid](())
          (mockRepository update _) expects stock returns stock.pure[IdMonad]

          service.update(stock) shouldEqual EitherT.rightT[IdMonad, ValidationError](stock)
        }
      }
      "should return Right(()) and updates model when validation passes" in {
        (mockValidationAlgebra exists _) when asset returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra accountIdExists _) when asset returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository update _) expects asset returns asset.pure[IdMonad]

        service.update(asset) shouldEqual EitherT.rightT[IdMonad, ValidationError](asset)
      }
    }
    "delete" - {
      "should return Right(()) and deletes" in {
        (mockRepository delete _) expects assetId returns ().pure[IdMonad]

        service.delete(assetId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
    "get" - {
      "should return repository get" in {
        (mockRepository get _) expects assetId returns OptionT.pure(asset)

        service.get(assetId).value shouldEqual Some(asset)
      }
    }
    "getMany" - {
      "should return repository getMany" in {
        (mockRepository getMany _) expects Seq(assetId, Id(assetId.value + 1)) returns Seq(asset, asset).pure[IdMonad]

        service.getMany(Seq(assetId, Id(assetId.value + 1))) shouldEqual Seq(asset, asset)
      }
    }
    "getAll" - {
      "should return repository getAll" - {
        (mockRepository.getAll _).expects().returns(Seq(asset, asset).pure[IdMonad])

        service.getAll shouldEqual Seq(asset, asset)
      }
    }
    "getStockValue" - {
      val stocks = Seq(
        Stock(Some(Id(4)), Id(19), "ticker0", Seq.empty),
        Stock(Some(Id(4)), Id(19), "ticker1", Seq.empty),
        Stock(Some(Id(4)), Id(19), "ticker2", Seq.empty)
      )
      val values = Seq(
        StockPriceAsOf(Usd(54.3), Usd(52.1), OffsetDateTime.now),
        StockPriceAsOf(Usd(2.35), Usd(54.59), OffsetDateTime.now),
        StockPriceAsOf(Usd(17.65), Usd(52.1), OffsetDateTime.now)
      )
      "should return each stock with price" in {
        (mockRepository.getAllStocks _).expects().returns(stocks.pure[IdMonad])
        stocks.zip(values).foreach { x =>
          (mockStockPriceRetriever.call(_: String)) expects x._1.ticker returns x._2.pure[IdMonad]
        }

        service.getStockValue shouldEqual stocks.zip(values).map { x =>
          x._1 withPrice x._2
        }
      }
    }
    "getStocks" - {
      val stocks = Seq(
        Stock(Some(Id(4)), Id(19), "ticker0", Seq.empty),
        Stock(Some(Id(4)), Id(19), "ticker1", Seq.empty),
        Stock(Some(Id(4)), Id(19), "ticker2", Seq.empty)
      )
      "should return repository getStocks" in {
        val query = StockQuery(None, None, None, Set.empty)
        (mockRepository.getStocks _).expects(query).returns(stocks.pure[IdMonad])

        service.getStocks(query) shouldEqual stocks
      }
    }
  }
}
