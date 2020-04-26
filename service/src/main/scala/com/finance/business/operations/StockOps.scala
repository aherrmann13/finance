package com.finance.business.operations

import com.finance.business.model.asset._
import com.finance.business.model.types.Usd

object StockOps {

  implicit class StockOperations(stock: Stock) {
    def withPrice(price: StockPriceAsOf): StockValue = {
      val quantity = getQuantity
      val pricePaid = getPricePaid
      StockValue(
        stock = stock,
        price = price.current,
        asOf = price.asOf,
        quantity = quantity,
        daysChange = Usd(price.current.value - price.open.value),
        daysChangePercentage = 100 - (price.current.value / price.open.value),
        daysGain = Usd((price.current.value * quantity) - (price.open.value * quantity)),
        pricePaid = pricePaid,
        totalGain = Usd((price.current.value * quantity) - pricePaid.value),
        value = Usd(price.current.value * quantity)
      )
    }

    private def getQuantity: BigDecimal =
      stock.actions map { action =>
        action.actionType match {
          case Buy => action.units
          case LifoSell => action.units * -1
          case FifoSell => action.units * -1
          case StockDividend => action.units
          case CashDividend => BigDecimal(0)
        }
      } sum

    private def getPricePaid: Usd =
      stock.actions map { action =>
        action.actionType match {
          case Buy => action.amountPaid
          case _ => Usd(0)
        }
      } reduceOption { (x, y) =>
        Usd(x.value + y.value)
      } getOrElse Usd(0)
  }

}
