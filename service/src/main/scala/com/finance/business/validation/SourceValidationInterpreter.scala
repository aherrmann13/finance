package com.finance.business.validation

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.source.Source
import com.finance.business.model.source.implicits._
import com.finance.business.model.types.{Id, ModelName}
import com.finance.business.repository.{SourceRepository, TransactionRepository}
import com.finance.business.validation.errors._

object SourceValidationInterpreter {
  private val Name = ModelName("Source")
}

class SourceValidationInterpreter[F[_]: Monad](
    sourceRepository: SourceRepository[F],
    transactionRepository: TransactionRepository[F]
) extends SourceValidationAlgebra[F] {
  import SourceValidationInterpreter._

  override def idIsNone(source: Source): EitherT[F, IdMustBeNone, Unit] =
    PropertyValidator.idIsNone(source)

  override def exists(source: Source): EitherT[F, DoesNotExist, Unit] =
    PropertyValidator.exists(source, sourceRepository.get)

  override def nameIsValid(source: Source): EitherT[F, NameTooLong, Unit] =
    PropertyValidator.nameIsValid(source)

  override def descriptionIsValid(source: Source): EitherT[F, DescriptionTooLong, Unit] =
    PropertyValidator.descriptionIsValid(source)

  override def hasNoTransactions(id: Id): EitherT[F, HasTransactions, Unit] =
    EitherT {
      transactionRepository.anyWithSourceId(id) map { x =>
        Either.cond(!x, (), HasTransactions(Name))
      }
    }
}
