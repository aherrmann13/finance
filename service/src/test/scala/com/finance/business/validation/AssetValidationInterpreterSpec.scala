package com.finance.business.validation

import cats.data.{EitherT, OptionT}
import cats.{Id => IdMonad}
import com.finance.business.model.account.{Account, Bank}
import com.finance.business.model.asset._
import com.finance.business.model.types._
import com.finance.business.repository.{AccountRepository, AssetRepository}
import com.finance.business.validation.errors._
import com.github.nscala_time.time.Imports._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AssetValidationInterpreterSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockAssetRepository = stub[AssetRepository[IdMonad]]
  private val mockAccountRepository = stub[AccountRepository[IdMonad]]

  private val assetValidationInterpreter =
    new AssetValidationInterpreter[IdMonad](mockAssetRepository, mockAccountRepository)

  private val fakeAssetWithId = new Asset {
    override val id: Option[Id] = Some(Id(1))
    override val accountId: Id = Id(15)
  }
  private val fakeAccountWithNoId = new Asset {
    override val id: Option[Id] = None
    override val accountId: Id = Id(15)
  }
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
        (mockAccountRepository get _).when(fakeAssetWithId.accountId)
          .returns(OptionT.pure(Account(Some(Id(4)), Name("Name"), Description("Description"), Bank)))
        assetValidationInterpreter.accountIdExists(fakeAssetWithId).value shouldEqual
          EitherT.rightT[IdMonad, DoesNotExist](()).value
      }
    }
    "stockActionsAreValid" - {
      val fakeStock = Stock(Some(Id(2)), Id(17), "ticker", Seq.empty)

      "should return Left(NoStockToPayDividend) when StockDividend occurs with no units" in {
        val action0 = Buy(DateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val action2 = LifoSell(DateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))
        val badAction = StockDividend(DateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, badAction)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](NoStockToPayDividend(badAction)).value
      }
      "should return Left(NoStockToPayDividend) when CashDividend occurs with no units" in {
        val action0 = Buy(DateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val action2 = LifoSell(DateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))
        val badAction = CashDividend(DateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, action2, badAction)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](NoStockToPayDividend(badAction)).value
      }
      "should return Left(NoStockToPayDividend) when Lifo sale occurs greater than current units" in {
        val action0 = Buy(DateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val badAction = LifoSell(DateTime.now, action0.units + action1.units + 1, Usd(12.0), Usd(15.0))
        val action2 = CashDividend(DateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, badAction, action2)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](SellingMoreThanCurrentlyHave(badAction)).value
      }
      "should return Left(NoStockToPayDividend) when Fifo sale occurs greater than current units" in {
        val action0 = Buy(DateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val badAction = FifoSell(DateTime.now, action0.units + action1.units + 1, Usd(12.0), Usd(15.0))
        val action2 = CashDividend(DateTime.now, action0.units + action1.units, Usd(12.0), Usd(15.0))

        assetValidationInterpreter
          .stockActionsAreValid(fakeStock.copy(actions = Seq(action0, action1, badAction, action2)))
          .value shouldEqual
          EitherT.leftT[IdMonad, Unit](SellingMoreThanCurrentlyHave(badAction)).value
      }
      "should return Right(()) for valid action list" in {
        val action0 = Buy(DateTime.now, 6, Usd(12.0), Usd(15.0))
        val action1 = action0.copy(units = 5)
        val action2 = CashDividend(DateTime.now, 6, Usd(12.0), Usd(15.0))
        val action3 = StockDividend(DateTime.now, 1, Usd(12.0), Usd(15.0))
        val action4 = FifoSell(DateTime.now, action0.units + action1.units + action3.units, Usd(12.0), Usd(15.0))

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
  }
}