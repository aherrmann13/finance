package com.finance.business.operations

import com.finance.business.model.types.DateRange
import com.github.nscala_time.time.Imports._

object CategoryOps {
  implicit class DateRangeOperations(range: DateRange) {
    def within(otherRanges: Seq[DateRange]): Boolean =
      otherRanges.exists { otherRange =>
        (otherRange.start.isBefore(range.start) || otherRange.start.isEqual(range.start)) &&
          (otherRange.end.isAfter(range.end) || otherRange.end.isEqual(range.end))
      }

    def contains(time: DateTime): Boolean =
      (range.start.isBefore(time) || range.start.isEqual(time)) &&
        (range.end.isAfter(time) || range.end.isEqual(time))
  }
}