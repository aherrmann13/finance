package com.finance.business.model.operations

import com.finance.business.model.category.{Always, EffectiveTime, Range, Single}

object Category {
  implicit class EffectiveTimeOperations(time: EffectiveTime) {
    def within(anotherTime: EffectiveTime): Boolean =
      anotherTime match {
        case Always => true
        case Range(from, to) => time match {
          case Range(innerFrom, innerTo) => innerFrom >= from && to >= innerTo
          case Single(innerPeriod) => innerPeriod >= from &&  innerPeriod <= to
          case _ => false
        }
        case Single(period) => time match {
          case Single(innerPeriod) => innerPeriod == period
          case _ => false
        }
      }
  }
}