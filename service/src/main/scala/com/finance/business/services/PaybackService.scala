package com.finance.business.services

import cats.Monad
import cats.data.{EitherT, OptionT}
import cats.implicits._
import com.finance.business.model.payback.{Payback, PaybackBalance}
import com.finance.business.model.types.{DateRange, Id, Usd}
import com.finance.business.repository.{PaybackRepository, TransactionRepository}
import com.finance.business.validation.PaybackValidationAlgebra
import com.finance.business.validation.errors.ValidationError

class PaybackService[F[_] : Monad](
  validator: PaybackValidationAlgebra[F],
  repository: PaybackRepository[F],
  transactionRepository: TransactionRepository[F]
) extends CommandService[F, Payback]
  with QueryService[F, Payback] {
  override def create(model: Payback): EitherT[F, ValidationError, Payback] =
    for {
      _ <- validator idIsNone model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      saved <- EitherT.liftF(repository create model)
    } yield saved

  override def update(model: Payback): EitherT[F, ValidationError, Payback] =
    for {
      _ <- validator exists model
      _ <- validator nameIsValid model
      _ <- validator descriptionIsValid model
      saved <- EitherT.liftF(repository update model)
    } yield saved

  override def delete(id: Id): EitherT[F, ValidationError, Unit] =
    for {
      _ <- validator hasNoTransactions id
      deleted <- EitherT.liftF(repository delete id)
    } yield deleted

  override def get(id: Id): OptionT[F, Payback] = repository.get(id)

  override def getMany(ids: Seq[Id]): F[Seq[Payback]] = repository.getMany(ids)

  override def getAll: F[Seq[Payback]] = repository.getAll

  def getPaybackBalance(range: DateRange): F[Seq[PaybackBalance]] =
    for {
      paybacks <- repository.getInRange(range)
      paybackAmounts <- transactionRepository.getByPaybackIds(paybacks.flatMap(_.id))
      groupedPaybackAmounts = paybackAmounts.groupBy(_.paybackId)
    } yield paybacks map { payback =>
      PaybackBalance(payback, payback.id.flatMap(groupedPaybackAmounts.get).getOrElse(Seq.empty))
    }

  def getPaybackBalanceSummary: F[Usd] =
    transactionRepository.getAllPaybacks map { paybacks =>
      Usd(paybacks.map(_.amount.value).sum)
    }
}
