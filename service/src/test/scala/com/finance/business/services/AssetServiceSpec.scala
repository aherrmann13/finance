package com.finance.business.services

import cats.data.EitherT
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
import com.github.nscala_time.time.Imports.DateTime
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
      "returns Left(IdMustBeNone) from validation algebra idIsNone" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Asset")))
        (mockValidationAlgebra idIsNone _) when asset returns returnVal
        (mockRepository create _) expects asset never

        service.create(asset) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra accountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account")))
        (mockValidationAlgebra idIsNone _) when asset returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra accountIdExists _) when asset returns returnVal
        (mockRepository create _) expects asset never

        service.create(asset) shouldEqual returnVal
      }
      "if asset is stock" - {
        val stock = Stock(Some(Id(2)), Id(19), "ticker", Seq.empty)

        "returns Left(StockActionsInvalid) from validation algebra stockActionsAreValid" in {
          val returnVal = EitherT.leftT[IdMonad, Unit][StockActionsInvalid](
            SellingMoreThanCurrentlyHave(StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0)))
          )
          (mockValidationAlgebra idIsNone _) when stock returns EitherT.rightT[IdMonad, IdMustBeNone](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns returnVal
          (mockRepository create _) expects stock never

          service.create(stock) shouldEqual returnVal
        }
        "returns Right(()) and saves model when validation passes" in {
          (mockValidationAlgebra idIsNone _) when stock returns EitherT.rightT[IdMonad, IdMustBeNone](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns
            EitherT.rightT[IdMonad, StockActionsInvalid](())
          (mockRepository create _) expects stock returns stock.pure[IdMonad]

          service.create(stock) shouldEqual EitherT.rightT[IdMonad, ValidationError](stock)
        }
      }
      "returns Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when asset returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra accountIdExists _) when asset returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository create _) expects asset returns asset.pure[IdMonad]

        service.create(asset) shouldEqual EitherT.rightT[IdMonad, ValidationError](asset)
      }
    }
    "update" - {
      "returns Left(DoesNotExist) from validation algebra exists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Asset")))
        (mockValidationAlgebra exists _) when asset returns returnVal
        (mockRepository update _) expects asset never

        service.update(asset) shouldEqual returnVal
      }
      "returns Left(DoesNotExist) from validation algebra accountIdExists" in {
        val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account")))
        (mockValidationAlgebra idIsNone _) when asset returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra accountIdExists _) when asset returns returnVal
        (mockRepository create _) expects asset never

        service.create(asset) shouldEqual returnVal
      }
      "if asset is stock" - {
        val stock = Stock(Some(Id(2)), Id(11), "ticker", Seq.empty)

        "returns Left(StockActionsInvalid) from validation algebra stockActionsAreValid" in {
          val returnVal = EitherT.leftT[IdMonad, Unit][StockActionsInvalid](
            SellingMoreThanCurrentlyHave(StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0)))
          )
          (mockValidationAlgebra exists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns returnVal
          (mockRepository update _) expects stock never

          service.update(stock) shouldEqual returnVal
        }
        "returns Right(()) and saves model when validation passes" in {
          (mockValidationAlgebra exists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra accountIdExists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
          (mockValidationAlgebra stockActionsAreValid _) when stock returns
            EitherT.rightT[IdMonad, StockActionsInvalid](())
          (mockRepository update _) expects stock returns stock.pure[IdMonad]

          service.update(stock) shouldEqual EitherT.rightT[IdMonad, ValidationError](stock)
        }
      }
      "returns Right(()) and updates model when validation passes" in {
        (mockValidationAlgebra exists _) when asset returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra accountIdExists _) when asset returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockRepository update _) expects asset returns asset.pure[IdMonad]

        service.update(asset) shouldEqual EitherT.rightT[IdMonad, ValidationError](asset)
      }
    }
    "delete" - {
      "returns Right(()) and deletes" in {
        (mockRepository delete _) expects assetId returns ().pure[IdMonad]

        service.delete(assetId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
      }
    }
    "get" - {
      "returns repository get" in {
        (mockRepository get _) expects assetId returns Some(asset).pure[IdMonad]

        service.get(assetId) shouldEqual Some(asset)
      }
    }
    "getMany" - {
      "returns repository getMany" in {
        (mockRepository getMany _) expects Seq(assetId, Id(assetId.value + 1)) returns Seq(asset, asset).pure[IdMonad]

        service.getMany(Seq(assetId, Id(assetId.value + 1))) shouldEqual Seq(asset, asset)
      }
    }
    "getAll" - {
      "returns repository getAll" - {
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
        StockPriceAsOf(Usd(54.3), Usd(52.1), DateTime.now),
        StockPriceAsOf(Usd(2.35), Usd(54.59), DateTime.now),
        StockPriceAsOf(Usd(17.65), Usd(52.1), DateTime.now)
      )
      "returns each stock with price" in {
        (mockRepository.getAllStocks _).expects().returns(stocks.pure[IdMonad])
        stocks.zip(values).foreach { x =>
          mockStockPriceRetriever.call _ expects x._1.ticker returns x._2.pure[IdMonad]
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
      "returns repository getStocks" in {
        val query = StockQuery(None, Set.empty, None, None, Set.empty)
        (mockRepository.getStocks _).expects(query).returns(stocks.pure[IdMonad])

        service.getStocks(query) shouldEqual stocks
      }
    }
  }
}