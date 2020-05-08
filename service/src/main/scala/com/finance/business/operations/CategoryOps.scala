package com.finance.business.operations

import com.finance.business.model.category.{Always, Collection, EffectiveTime, Range, Single}
import com.finance.business.model.types.Id

object CategoryOps {
  implicit class EffectiveTimeOperations(time: EffectiveTime) {
    def within(anotherTime: EffectiveTime): Boolean =
      anotherTime match {
        case Always => true
        case Collection(range) => {
          time match {
            case Always => false
            case Collection(innerRange) => innerRange.forall(range.contains(_))
            case Range(innerFrom, innerTo) => (innerFrom to innerTo).forall(range.contains(_))
            case Single(innerPeriod) => range.contains(innerPeriod)
          }
        }
        case Range(from, to) => time match {
          case Always => false
          case Collection(innerRange) => innerRange.min >= from && innerRange.max <= to
          case Range(innerFrom, innerTo) => innerFrom >= from && to >= innerTo
          case Single(innerPeriod) => innerPeriod >= from &&  innerPeriod <= to
        }
        case Single(period) => time match {
          case Always => false
          case Collection(range) => range == Seq(period)
          case Range(innerFrom, innerTo) => innerFrom == period && innerTo == period
          case Single(innerPeriod) => innerPeriod == period
        }
      }

    def ids: Seq[Id] = time match {
      case Always => Seq.empty
      case Collection(range) => range map { Id(_) }
      case Range(from, to) => from to to map { Id(_) }
      case Single(period) => Seq(Id(period))
    }
  }
}