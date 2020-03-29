package com.finance.business.validation

import cats.data.EitherT
import cats.{Id => IdMonad}
import cats.implicits._
import com.finance.business.model.asset._
import com.finance.business.model.types.{Id, ModelName, Usd}
import com.finance.business.repository.AssetRepository
import com.finance.business.validation.errors._
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockAssetRepository = stub[AssetRepository[IdMonad]]

  private val assetValidationInterpreter = new AssetValidationInterpreter[IdMonad](mockAssetRepository)

  private val fakeAssetWithId = new Asset {
    override val id: Option[Id] = Some(Id(1))
  }
  private val fakeAccountWithNoId = new Asset {
    override val id: Option[Id] = None
  }
  private val assetName = ModelName("Asset")

  "idIsNone" - {
    "should return Left(IdMustBeNone) when id is Some" in {
      assetValidationInterpreter.idIsNone(fakeAssetWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](IdMustBeNone(assetName)).value
    }
    "should return Right(()) when id is None" in {
      assetValidationInterpreter.idIsNone(fakeAccountWithNoId).value shouldEqual
        EitherT.rightT[IdMonad, IdMustBeNone](()).value
    }
  }
  "exists" - {
    "should return Left(DoesNotExist) when id is None" in {
      assetValidationInterpreter.exists(fakeAccountWithNoId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(assetName)).value
    }
    "should return Left(DoesNotExist) when repository does not contain Account" in {
      (mockAssetRepository get _).when(fakeAssetWithId.id.get).returns(None.pure[IdMonad])
      assetValidationInterpreter.exists(fakeAssetWithId).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(assetName, fakeAssetWithId.id)).value
    }
    "should return Right(()) when repository contains Account" in {
      (mockAssetRepository get _).when(fakeAssetWithId.id.get).returns(Some(fakeAssetWithId).pure[IdMonad])
      assetValidationInterpreter.exists(fakeAssetWithId).value shouldEqual
        EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }
  "stockActionsAreValid" - {
    val fakeStock = Stock(Some(Id(2)), "ticker", Seq.empty)

    "should return Left(NoStockToPayDividend) when StockDividend occurs with no units" in {
      val action0 = StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0))
      val action1 = action0.copy(units = 5)
      val action2 = action0.copy(units = action0.units + action1.units, actionType = Sell)
      val badAction = action0.copy(actionType = StockDividend)

      assetValidationInterpreter
        .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, badAction)))
        .value shouldEqual
        EitherT.leftT[IdMonad, Unit](NoStockToPayDividend(badAction)).value
    }
    "should return Left(NoStockToPayDividend) when CashDividend occurs with no units" in {
      val action0 = StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0))
      val action1 = action0.copy(units = 5)
      val action2 = action0.copy(units = action0.units + action1.units, actionType = Sell)
      val badAction = action0.copy(actionType = CashDividend)

      assetValidationInterpreter
        .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, badAction)))
        .value shouldEqual
        EitherT.leftT[IdMonad, Unit](NoStockToPayDividend(badAction)).value
    }
    "should return Left(NoStockToPayDividend) when sale occurs greater than current units" in {
      val action0 = StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0))
      val action1 = action0.copy(units = 5)
      val badAction = action0.copy(units = action0.units + action1.units + 1, actionType = Sell)
      val action2 = action0.copy(actionType = CashDividend)

      assetValidationInterpreter
        .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, badAction, action2)))
        .value shouldEqual
        EitherT.leftT[IdMonad, Unit](SellingMoreThanCurrentlyHave(badAction)).value
    }
    "should return Right(()) for empty action list" in {
      val action0 = StockAction(DateTime.now(), Buy, 6, Usd(12.0), Usd(15.0))
      val action1 = action0.copy(units = 5)
      val action2 = action0.copy(actionType = CashDividend)
      val action3 = action0.copy(actionType = Sell)

      assetValidationInterpreter
        .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, action3)))
        .value shouldEqual
        EitherT.rightT[IdMonad, StockActionsInvalid](()).value
    }
    "should return Right(()) for valid action list" in {
      assetValidationInterpreter
        .stockActionsAreValid(fakeStock)
        .value shouldEqual
        EitherT.rightT[IdMonad, StockActionsInvalid](()).value
    }
  }
}
