package com.finance.business.common

import cats.Applicative
import cats.data.EitherT
import cats.implicits._
import com.finance.business.errors.{BusinessError, UserDoesNotExistError}

object RelationValidator {
  def apply[F[_] : Applicative](repository: IdRepository[F]) =
    new RelationValidator[F](repository)
}

class RelationValidator[F[_] : Applicative](idRepository: IdRepository[F]) {

  def userExists(item: HasUserId): EitherT[F, BusinessError, Unit] =
    EitherT {
      idRepository.userExists(item.userId) map {
        case true => Right(())
        case false => Left(UserDoesNotExistError)
      }
    }
}
