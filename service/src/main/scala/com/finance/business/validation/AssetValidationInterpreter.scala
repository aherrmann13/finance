package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import com.finance.business.model.account.implicits._
import com.finance.business.model.asset._
import com.finance.business.model.asset.implicits._
import com.finance.business.repository.{AccountRepository, AssetRepository}
import com.finance.business.validation.errors._

object AssetValidationInterpreter {

  case object StockActionValidator {
    def apply(): StockActionValidator = StockActionValidator(0)
  }

  case class StockActionValidator(currentUnits: BigDecimal) extends AnyVal {
    def +(action: StockAction): StockActionValidator = action match {
      case Buy(_, units, _, _) => copy(currentUnits = currentUnits + units)
      case LifoSell(_, units, _, _) => copy(currentUnits = currentUnits - units)
      case FifoSell(_, units, _, _) => copy(currentUnits = currentUnits - units)
      case StockDividend(_, units, _, _) => copy(currentUnits = currentUnits + units)
      case CashDividend(_, _, _, _) => this
    }

    def ?(action: StockAction): Option[StockActionsInvalid] = action match {
      case Buy(_, _, _, _) => None
      case LifoSell(_, units, _, _) => Option.unless(currentUnits >= units)(SellingMoreThanCurrentlyHave(action))
      case FifoSell(_, units, _, _) => Option.unless(currentUnits >= units)(SellingMoreThanCurrentlyHave(action))
      case StockDividend(_, units, _, _) => Option.unless(currentUnits >= units)(NoStockToPayDividend(action))
      case CashDividend(_, units, _, _) => Option.unless(currentUnits >= units)(NoStockToPayDividend(action))
    }
  }

}

class AssetValidationInterpreter[F[_] : Monad](
  assetRepository: AssetRepository[F],
  accountRepository: AccountRepository[F]
) extends AssetValidationAlgebra[F] {

  import AssetValidationInterpreter._

  override def idIsNone(asset: Asset): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(asset)

  override def exists(asset: Asset): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(asset.id, assetRepository.get)

  override def accountIdExists(asset: Asset): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(asset.accountId, accountRepository.get)

  // TODO: clean up
  override def stockActionsAreValid(stock: Stock): EitherT[F, StockActionsInvalid, Unit] =
    stock.actions.toList.foldLeft(EitherT.pure[F, StockActionsInvalid](StockActionValidator())) {
      (acc: EitherT[F, StockActionsInvalid, StockActionValidator], a: StockAction) =>
        acc flatMap { v =>
          EitherT.fromOption[F](v ? a, v + a).swap
        }
    } map(_ => ())
}
