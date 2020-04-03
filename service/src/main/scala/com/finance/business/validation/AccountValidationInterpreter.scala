package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.account._
import com.finance.business.model.account.implicits._
import com.finance.business.model.types.{Id, ModelName}
import com.finance.business.repository._
import com.finance.business.validation.errors._

object AccountValidationInterpreter {
  private val Name = ModelName("Account")
}

class AccountValidationInterpreter[F[_]: Monad](
    accountRepository: AccountRepository[F],
    transactionRepository: TransactionRepository[F],
    assetRepository: AssetRepository[F],
    paybackRepository: PaybackRepository[F]
) extends AccountValidationAlgebra[F] {
  import AccountValidationInterpreter._

  override def idIsNone(account: Account): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(account)

  override def exists(account: Account): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(account.id, accountRepository.get)

  override def nameIsValid(account: Account): EitherT[F, NameTooLong, Unit] =
    PropertyValidator.nameIsValid(account)

  override def descriptionIsValid(account: Account): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(account)

  override def accountTypeIsValid(account: Account): EitherT[F, AccountTypeInvalid, Unit] =
    account.id match {
      case Some(id) if account.accountType == Brokerage => for {
        _ <- hasNoTransactions[AccountTypeInvalid](id, BrokerageCantHaveTransactions)
        _ <- hasNoPaybacks[AccountTypeInvalid](id, BrokerageCantHavePaybacks)
      } yield EitherT.rightT[F, AccountTypeInvalid](())
      case Some(id) if account.accountType == Bank => hasNoAssets[AccountTypeInvalid](id, BankCantHaveAssets)
      case None => EitherT.rightT[F, AccountTypeInvalid](())
    }

  override def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit] =
    hasNoTransactions(id, HasTransactions(Name))

  override def hasNoAssets(id: Id): EitherT[F, HasAssets, Unit] =
    hasNoAssets(id, HasAssets(Name))

  override def hasNoPaybacks(id: Id): EitherT[F, HasPaybacks, Unit] =
    hasNoPaybacks(id, HasPaybacks(Name))

  private def hasNoTransactions[E](id: Id, onErr: => E): EitherT[F, E, Unit] =
    EitherT {
      transactionRepository.anyWithAccountId(id) map { x =>
        Either.cond(!x, (), onErr)
      }
    }

  private def hasNoAssets[E](id: Id, onErr: => E): EitherT[F, E, Unit] =
    EitherT {
      assetRepository.anyWithAccountId(id) map { x =>
        Either.cond(!x, (), onErr)
      }
    }

  private def hasNoPaybacks[E](id: Id, onErr: => E): EitherT[F, E, Unit] =
    EitherT {
      paybackRepository.anyWithAccountId(id) map { x =>
        Either.cond(!x, (), onErr)
      }
    }
}
