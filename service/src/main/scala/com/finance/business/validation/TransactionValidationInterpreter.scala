package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.account.implicits._
import com.finance.business.model.category.Always
import com.finance.business.model.category.implicits._
import com.finance.business.model.payback.implicits._
import com.finance.business.model.source.implicits._
import com.finance.business.model.timeperiod.TimePeriodRange
import com.finance.business.model.transaction.{Amount, CategoryAmount, PaybackAmount, Transaction}
import com.finance.business.model.transaction.implicits._
import com.finance.business.operations.CategoryOps._
import com.finance.business.repository._
import com.finance.business.validation.errors._
import com.github.nscala_time.time.Imports._

object TransactionValidationInterpreter {
  private def dateWithinRange(ranges: Seq[TimePeriodRange], date: DateTime): Boolean =
    ranges.exists { range =>
      (range.start.isBefore(date) || range.start.isEqual(date)) &&
        (range.end.isAfter(date) || range.end.isEqual(date))
    }
}

class TransactionValidationInterpreter[F[_] : Monad](
  transactionRepository: TransactionRepository[F],
  sourceRepository: SourceRepository[F],
  accountRepository: AccountRepository[F],
  categoryRepository: CategoryRepository[F],
  paybackRepository: PaybackRepository[F],
  timePeriodRepository: TimePeriodRepository[F]
) extends TransactionValidationAlgebra[F] {

  import TransactionValidationInterpreter._

  override def idIsNone(transaction: Transaction): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(transaction)

  override def exists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transaction.id, transactionRepository.get)

  override def descriptionIsValid(transaction: Transaction): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(transaction)

  override def sourceIdExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transaction.sourceId, sourceRepository.get)

  override def accountIdExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(transaction.accountId, accountRepository.get)

  override def amountDescAreValid(transaction: Transaction): EitherT[F, DescriptionTooLong, Unit] =
    transaction.amounts.toList.traverse(PropertyValidator.descriptionIsValid[F, Amount](_)).map(_ => ())

  override def categoryIdsExist(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    transaction.amounts.toList.flatMap {
      case CategoryAmount(categoryId, _, _, _) => Some(categoryId)
      case _ => None
    }.traverse { x =>
      PropertyValidator.exists(x, categoryRepository.get)
    }.map(_ => ())

  // TODO: clean this up
  override def reportingDateWithinCategoryTime(transaction: Transaction): EitherT[F, DateNotInEffectiveTime, Unit] =
    transaction.amounts.toList.traverse {
      case CategoryAmount(categoryId, _, _, date) =>
        EitherT {
          categoryRepository.get(categoryId) flatMap {
            case Some(x) if x.effectiveTime.isInstanceOf[Always.type] => Either.right[DateNotInEffectiveTime, Unit](()).pure[F]
            case None => Either.right[DateNotInEffectiveTime, Unit](()).pure[F]
            case Some(x) =>
              timePeriodRepository.getMany(x.effectiveTime.ids) map { ranges =>
                Either.cond(dateWithinRange(ranges, date), (), DateNotInEffectiveTime(date, x.effectiveTime))
              }
          }
        }
      case _ => EitherT.rightT[F, DateNotInEffectiveTime](())
    }.map(_ => ())

  override def paybackIdsExists(transaction: Transaction): EitherT[F, DoesNotExist, Unit] =
    transaction.amounts.toList.flatMap {
      case PaybackAmount(paybackId, _, _, _) => Some(paybackId)
      case _ => None
    }.traverse { x =>
        PropertyValidator.exists(x, paybackRepository.get)
    }.map(_ => ())
}
