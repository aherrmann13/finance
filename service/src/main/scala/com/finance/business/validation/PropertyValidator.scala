package com.finance.business.validation

import cats.Applicative
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.types.HasId.ops._
import com.finance.business.model.types.HasName.ops._
import com.finance.business.model.types.HasDescription.ops._
import com.finance.business.model.types.NamedModel.ops._
import com.finance.business.model.types.{HasDescription, HasId, HasName, NamedModel}
import com.finance.business.model.types.Id
import com.finance.business.validation.errors._

private[validation] object PropertyValidator {
  private val MaxNameLength = 128
  private val MaxDescLength = 512

  def idIsNone[F[_]: Applicative, M: HasId: NamedModel](m: M): EitherT[F, IdMustBeNone, Unit] =
    EitherT.cond(m.id.isEmpty, (), IdMustBeNone(m modelName))

  def exists[F[_]: Applicative, M: HasId: NamedModel](
      m: M,
      doesExist: Id => F[Option[M]]
  ): EitherT[F, DoesNotExist, Unit] =
    EitherT {
      m.id map { id =>
        doesExist(id) map { exists =>
          Either.cond(exists.nonEmpty, (), DoesNotExist(m modelName, id))
        }
      } getOrElse Either.left[DoesNotExist, Unit](DoesNotExist(m modelName)).pure[F]
    }

  def nameIsValid[F[_]: Applicative, M: HasName: NamedModel](m: M): EitherT[F, NameTooLong, Unit] =
    EitherT.cond(m.name.value.length <= MaxNameLength, (), NameTooLong(m modelName, m.name))

  def descriptionIsValid[F[_]: Applicative, M: HasDescription: NamedModel](m: M): EitherT[F, DescriptionTooLong, Unit] =
    EitherT.cond(m.description.value.length <= MaxDescLength, (), DescriptionTooLong(m modelName, m.description))
}
