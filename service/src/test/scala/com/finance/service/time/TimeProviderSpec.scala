package com.finance.service.time

import java.time.OffsetDateTime

import cats.effect.SyncIO
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TimeProviderSpec extends AnyFreeSpec with Matchers {
  "TimeProvider instance for Sync" - {
    "should return current time when executed" in {
      val time = TimeProvider[SyncIO].now

      val currentTime = OffsetDateTime.now

      time.unsafeRunSync should be > currentTime
    }
  }

}
