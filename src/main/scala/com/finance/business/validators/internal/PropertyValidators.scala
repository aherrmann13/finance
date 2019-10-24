package com.finance.business.validators.internal

import cats.Applicative
import cats.implicits._
import cats.data.EitherT
import com.finance.business.common.Constants._
import com.finance.business.errors._

object PropertyValidators {
  def validateName[F[_]: Applicative](name: String): EitherT[F, BusinessError, Unit] =
    EitherT {
      name match {
        case x if x == null => Either.left[BusinessError, Unit](NameMustBeDefinedError).pure[F]
        case x if x.length > MaxNameLength =>
          Either.left[BusinessError, Unit](NameExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }

  def validateDesc[F[_]: Applicative](description: String): EitherT[F, BusinessError, Unit] =
    EitherT {
      description match {
        case x if x == null => Either.left[BusinessError, Unit](DescriptionMustBeDefinedError).pure[F]
        case x if x.length > MaxDescriptionLength =>
          Either.left[BusinessError, Unit](DescriptionExceedsMaxLengthError).pure[F]
        case _ => Either.right[BusinessError, Unit](()).pure[F]
      }
    }
}
