package com.finance.business.validation

import java.time.OffsetDateTime

import cats.data.{EitherT, OptionT}
import cats.{Id => IdMonad}
import com.finance.business.model.account.{Account, Bank}
import com.finance.business.model.asset._
import com.finance.business.model.types._
import com.finance.business.repository.{AccountRepository, AssetRepository}
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockAssetRepository = stub[AssetRepository[IdMonad]]
  private val mockAccountRepository = stub[AccountRepository[IdMonad]]

  private val assetValidationInterpreter =
    new AssetValidationInterpreter[IdMonad](mockAssetRepository, mockAccountRepository)

  private val fakeAssetWithId = Stock(Some(Id(1)), Id(15), "ticker", Seq.empty)
  private val fakeAccountWithNoId = Stock(None, Id(15), "ticker", Seq.empty)

  private val assetName = ModelName("Asset")

  "AssetValidationInterpreter" - {
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
      "should return Left(DoesNotExist) when repository does not contain Asset" in {
        (mockAssetRepository get _).when(fakeAssetWithId.id.get).returns(OptionT.none)
        assetValidationInterpreter.exists(fakeAssetWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(assetName, fakeAssetWithId.id)).value
      }
      "should return Right(()) when repository contains Account" in {
        (mockAssetRepository get _).when(fakeAssetWithId.id.get).returns(OptionT.pure(fakeAssetWithId))
        assetValidationInterpreter.exists(fakeAssetWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "accountIdExists" - {
      "should return Left(DoesNotExist) when repository does not contain Account" in {
        (mockAccountRepository get _).when(fakeAssetWithId.accountId).returns(OptionT.none)
        assetValidationInterpreter.accountIdExists(fakeAssetWithId).value shouldEqual
          EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account"), fakeAssetWithId.accountId)).value
      }
      "should return Right(()) when repository contains Account" in {
        (mockAccountRepository get _)
          .when(fakeAssetWithId.accountId)
          .returns(OptionT.pure(Account(Some(Id(4)), Name("Name"), Description("Description"), Bank, Usd(50))))
        assetValidationInterpreter.accountIdExists(fakeAssetWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "stockActionsAreValid" - {
      val fakeStock = Stock(Some(Id(2)), Id(17), "ticker", Seq.empty)

      "should return Left(NoStockToPayDividend) when StockDividend occurs with no units" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val action2 = LifoSell(OffsetDateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))
        val badAction = StockDividend(OffsetDateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, badAction)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](NoStockToPayDividend(badAction)).value
      }
      "should return Left(NoStockToPayDividend) when CashDividend occurs with no units" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val action2 = LifoSell(OffsetDateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))
        val badAction = CashDividend(OffsetDateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, badAction)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](NoStockToPayDividend(badAction)).value
      }
      "should return Left(NoStockToPayDividend) when Lifo sale occurs greater than current units" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val badAction = LifoSell(OffsetDateTime.now, action0.units + action1.units + 1, Usd(12.0), Usd(15.0))
        val action2 = CashDividend(OffsetDateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, badAction, action2)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](SellingMoreThanCurrentlyHave(badAction)).value
      }
      "should return Left(NoStockToPayDividend) when Fifo sale occurs greater than current units" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val badAction = FifoSell(OffsetDateTime.now, action0.units + action1.units + 1, Usd(12.0), Usd(15.0))
        val action2 = CashDividend(OffsetDateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, badAction, action2)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](SellingMoreThanCurrentlyHave(badAction)).value
      }
      "should return Right(()) for valid action list" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val action2 = CashDividend(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action3 = StockDividend(OffsetDateTime.now, 1, Usd(12.0), Usd(15.0))
        val action4 = FifoSell(OffsetDateTime.now, action0.units + action1.units + action3.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, action3, action4)))
          .value shouldEqual
          EitherT.rightT[IdMonad, StockActionsInvalid](()).value
      }
      "should return Right(()) for empty action list" in {
        assetValidationInterpreter
          .stockActionsAreValid(fakeStock)
          .value shouldEqual
          EitherT.rightT[IdMonad, StockActionsInvalid](()).value
      }
    }

    "stockActionsAreInOrder" - {
      val fakeStock = Stock(Some(Id(2)), Id(17), "ticker", Seq.empty)

      "should return Left(StockActionsOutOfOrder) when actions are not from earliest to latest" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val badAction = action0.copy(date = action0.date.minusDays(1))
        val action1 = action0.copy(date = action0.date.plusDays(1))

        assetValidationInterpreter
          .stockActionsAreInOrder(fakeStock.copy(actions = Seq(action0, badAction, action1)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](StockActionsOutOfOrder(badAction)).value
      }
      "should return Right(()) when actions are from earliest to latest" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(date = action0.date.plusDays(1))
        val action2 = action0.copy(date = action0.date.plusDays(2))

        assetValidationInterpreter
          .stockActionsAreInOrder(fakeStock.copy(actions = Seq(action0, action1, action2)))
          .value shouldEqual
          EitherT.rightT[IdMonad, StockActionsOutOfOrder](()).value
      }
      "should return Right(()) when actions are from earliest to latest with equal dates" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(date = action0.date)
        val action2 = action0.copy(date = action0.date)

        assetValidationInterpreter
          .stockActionsAreInOrder(fakeStock.copy(actions = Seq(action0, action1, action2)))
          .value shouldEqual
          EitherT.rightT[IdMonad, StockActionsOutOfOrder](()).value
      }
      "should return Right(()) when actions are list with one element" in {
        val action0 = Buy(OffsetDateTime.now, 6, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreInOrder(fakeStock.copy(actions = Seq(action0)))
          .value shouldEqual
          EitherT.rightT[IdMonad, StockActionsOutOfOrder](()).value
      }
      "should return Right(()) when actions are empty list" in {
        assetValidationInterpreter
          .stockActionsAreInOrder(fakeStock)
          .value shouldEqual
          EitherT.rightT[IdMonad, StockActionsOutOfOrder](()).value
      }
    }
  }
}
