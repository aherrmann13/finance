package com.finance.business.operations

import java.time.OffsetDateTime

import com.finance.business.model.types.DateRange

object CategoryOps {
  implicit class DateRangeOperations(range: DateRange) {
    def within(otherRanges: Seq[DateRange]): Boolean =
      otherRanges.exists { otherRange =>
        (otherRange.start.isBefore(range.start) || otherRange.start.isEqual(range.start)) &&
        (otherRange.end.isAfter(range.end) || otherRange.end.isEqual(range.end))
      }

    def contains(time: OffsetDateTime): Boolean =
      (range.start.isBefore(time) || range.start.isEqual(time)) &&
        (range.end.isAfter(time) || range.end.isEqual(time))

    def overlaps(otherRange: DateRange): Boolean =
      (range.start.isBefore(otherRange.end) || range.start.isEqual(otherRange.end)) &&
        (range.end.isAfter(otherRange.start) || range.end.isEqual(otherRange.start))
  }
}
