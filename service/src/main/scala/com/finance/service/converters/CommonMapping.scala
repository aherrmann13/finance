package com.finance.service.converters

import java.time.{LocalTime, OffsetDateTime, ZoneOffset}

import com.finance.business.model.types.{DateRange => DateRangeModel}
import com.finance.service.endpoints.definitions.{AmountSpentInRange, AmountSpentInRangeQuery, DateRange}

object CommonMapping {
  implicit val dateRangeRequestMapping: Mapping[DateRange, DateRangeModel] = (a: DateRange) =>
    DateRangeModel(
      OffsetDateTime.of(a.start, LocalTime.MIN, ZoneOffset.UTC),
      OffsetDateTime.of(a.end, LocalTime.MIN, ZoneOffset.UTC)
    )

  implicit val amountSpentInRangeQueryRangeRequestMapping: Mapping[AmountSpentInRangeQuery.Range, DateRangeModel] =
    (a: AmountSpentInRangeQuery.Range) =>
      DateRangeModel(
        OffsetDateTime.of(a.start, LocalTime.MIN, ZoneOffset.UTC),
        OffsetDateTime.of(a.end, LocalTime.MIN, ZoneOffset.UTC)
      )

  implicit val dateRangeResponseMapping: Mapping[DateRangeModel, DateRange] = (a: DateRangeModel) =>
    DateRange(a.start.toLocalDate, a.end.toLocalDate)

  implicit val amountSpentInRangeRangeResponseMapping: Mapping[DateRangeModel, AmountSpentInRange.Range] =
    (a: DateRangeModel) => AmountSpentInRange.Range(a.start.toLocalDate, a.end.toLocalDate)
}
