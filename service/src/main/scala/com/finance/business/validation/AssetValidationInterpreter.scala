package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.asset._
import com.finance.business.model.asset.implicits._
import com.finance.business.repository.AssetRepository
import com.finance.business.validation.errors._

object AssetValidationInterpreter {

  case object StockActionValidator {
    def apply(): StockActionValidator = StockActionValidator(0)
  }

  case class StockActionValidator(currentUnits: BigDecimal) extends AnyVal {
    def +(action: StockAction): StockActionValidator = action.actionType match {
      case Buy           => copy(currentUnits = currentUnits + action.units)
      case Sell          => copy(currentUnits = currentUnits - action.units)
      case StockDividend => copy(currentUnits = currentUnits + action.units)
      case CashDividend  => this
    }

    def ?(action: StockAction): Option[StockActionsInvalid] = action.actionType match {
      case Buy           => None
      case Sell          => Option.unless(currentUnits >= action.units)(SellingMoreThanCurrentlyHave(action))
      case StockDividend => Option.unless(currentUnits >= action.units)(NoStockToPayDividend(action))
      case CashDividend  => Option.unless(currentUnits >= action.units)(NoStockToPayDividend(action))
    }
  }
}

class AssetValidationInterpreter[F[_]: Monad](assetRepository: AssetRepository[F]) extends AssetValidationAlgebra[F] {
  import AssetValidationInterpreter._

  override def idIsNone(asset: Asset): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(asset)

  override def exists(asset: Asset): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(asset.id, assetRepository.get)

  // TODO: clean up
  override def stockActionsAreValid(stock: Stock): EitherT[F, StockActionsInvalid, Unit] =
    stock.actions.toList.foldLeft(EitherT.pure[F, StockActionsInvalid](StockActionValidator())) {
      (acc: EitherT[F, StockActionsInvalid, StockActionValidator], a: StockAction) =>
        acc flatMap { v =>
          EitherT.fromOption[F](v ? a, v + a).swap
        }
    } map (_ => ())
}
