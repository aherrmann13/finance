package com.finance.service.time

import java.time.OffsetDateTime

import cats.effect.Sync

// https://github.com/typelevel/cats-effect/issues/718
trait TimeProvider[F[_]] {
  def now: F[OffsetDateTime]
}

object TimeProvider {
  def apply[A[_]](implicit tc: TimeProvider[A]): TimeProvider[A] = tc

  implicit def syncTimeProviderInstance[F[_]: Sync]: TimeProvider[F] =
    new TimeProvider[F] {
      override def now: F[OffsetDateTime] = Sync[F].delay(OffsetDateTime.now)
    }
}
