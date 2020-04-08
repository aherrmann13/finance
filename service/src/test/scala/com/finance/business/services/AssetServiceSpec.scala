package com.finance.business.services

import cats.data.EitherT
import cats.{Id => IdMonad}
import cats.implicits._
import com.finance.business.model.asset.{Asset, Buy, Stock, StockAction}
import com.finance.business.model.types.{Id, ModelName, Usd}
import com.finance.business.repository.AssetRepository
import com.finance.business.validation.AssetValidationAlgebra
import com.finance.business.validation.errors._
import com.github.nscala_time.time.Imports.DateTime
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[AssetValidationAlgebra[IdMonad]]
  private val mockRepository = mock[AssetRepository[IdMonad]]

  private val service = new AssetService[IdMonad](mockValidationAlgebra, mockRepository)

  private val assetId = Id(5)
  private val asset = new Asset {
    override val id: Option[Id] = Some(assetId)
  }

  "create" - {
    "returns Left(IdMustBeNone) from validation algebra idIsNone" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Asset")))
      (mockValidationAlgebra idIsNone _) when asset returns returnVal
      (mockRepository create _) expects asset never

      service.create(asset) shouldEqual returnVal
    }
    "if asset is stock" - {
      val stock = Stock(Some(Id(2)), "ticker", Seq.empty)

      "returns Left(StockActionsInvalid) from validation algebra stockActionsAreValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit][StockActionsInvalid](
          SellingMoreThanCurrentlyHave(StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0)))
        )
        (mockValidationAlgebra idIsNone  _) when stock returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra stockActionsAreValid _) when stock returns returnVal
        (mockRepository create _) expects stock never

        service.create(stock) shouldEqual returnVal
      }
      "returns Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra idIsNone _) when stock returns EitherT.rightT[IdMonad, IdMustBeNone](())
        (mockValidationAlgebra stockActionsAreValid _) when stock returns
          EitherT.rightT[IdMonad, StockActionsInvalid](())
        (mockRepository create _) expects stock returns stock.pure[IdMonad]

        service.create(stock) shouldEqual EitherT.rightT[IdMonad, ValidationError](stock)
      }
    }
    "returns Right(()) and saves model when validation passes" in {
      (mockValidationAlgebra idIsNone _) when asset returns EitherT.rightT[IdMonad, IdMustBeNone](())
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
    "if asset is stock" - {
      val stock = Stock(Some(Id(2)), "ticker", Seq.empty)

      "returns Left(StockActionsInvalid) from validation algebra stockActionsAreValid" in {
        val returnVal = EitherT.leftT[IdMonad, Unit][StockActionsInvalid](
          SellingMoreThanCurrentlyHave(StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0)))
        )
        (mockValidationAlgebra exists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra stockActionsAreValid _) when stock returns returnVal
        (mockRepository update _) expects stock never

        service.update(stock) shouldEqual returnVal
      }
      "returns Right(()) and saves model when validation passes" in {
        (mockValidationAlgebra exists _) when stock returns EitherT.rightT[IdMonad, DoesNotExist](())
        (mockValidationAlgebra stockActionsAreValid _) when stock returns
          EitherT.rightT[IdMonad, StockActionsInvalid](())
        (mockRepository update _) expects stock returns stock.pure[IdMonad]

        service.update(stock) shouldEqual EitherT.rightT[IdMonad, ValidationError](stock)
      }
    }
    "returns Right(()) and updates model when validation passes" in {
      (mockValidationAlgebra exists _) when asset returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockRepository update _) expects asset returns asset.pure[IdMonad]

      service.update(asset) shouldEqual EitherT.rightT[IdMonad, ValidationError](asset)
    }
  }
  "delete" - {
    "returns Right(()) and deletes" in {
      (mockRepository delete  _) expects assetId returns ().pure[IdMonad]

      service.delete(assetId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
    }
  }
}
