package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.payback.Payback
import com.finance.business.model.payback.implicits._
import com.finance.business.model.types.{Id, ModelName}
import com.finance.business.repository.{PaybackRepository, TransactionRepository}
import com.finance.business.validation.errors._

object PaybackValidationInterpreter {
  private val Name = ModelName("Payback")
}

class PaybackValidationInterpreter[F[_]: Monad](
    paybackRepository: PaybackRepository[F],
    transactionRepository: TransactionRepository[F]
) extends PaybackValidationAlgebra[F] {
  import PaybackValidationInterpreter._

  override def idIsNone(payback: Payback): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(payback)

  override def exists(payback: Payback): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(payback, paybackRepository.get)

  override def nameIsValid(payback: Payback): EitherT[F, NameTooLong, Unit] =
    PropertyValidator.nameIsValid(payback)

  override def descriptionIsValid(payback: Payback): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(payback)

  override def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit] =
    EitherT {
      transactionRepository.anyWithPaybackId(id) map { x =>
        Either.cond(!x, (), HasTransactions(Name))
      }
    }
}
