package com.finance.business.operations

import com.finance.business.model.asset._
import com.finance.business.model.types.Usd

object StockOps {

  private object LifecycleStatus {
    def apply(value: StockPurchaseLifecycle): LifecycleStatus =
      if (value.unitsRemaining == 0) Closed(value) else Opened(value)
  }
  private sealed trait LifecycleStatus {
    val value: StockPurchaseLifecycle
  }
  private case class Opened(value: StockPurchaseLifecycle) extends LifecycleStatus
  private case class Closed(value: StockPurchaseLifecycle) extends LifecycleStatus

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
      stock.actions collect {
        case Buy(_, units, _, _) => units
        case LifoSell(_, units, _, _) => units * -1
        case FifoSell(_, units, _, _) => units * -1
        case StockDividend(_, units, _, _) => units
      } sum

    private def getPricePaid: Usd =
      stock.actions collect {
        case Buy(_, _, _, amount) => amount
      } reduceOption { (x, y) =>
        Usd(x.value + y.value)
      } getOrElse Usd(0)

    def asLifecycle: Seq[StockPurchaseLifecycle] =
      stock.actions
        .sortBy(_.date)
        .foldLeft(Seq.empty[LifecycleStatus])((current, action) => append(current, action))
        .sortBy(_.value.buy.date).map(_.value)

    // TODO: clean up
    private def append(lifecycles: Seq[LifecycleStatus], action: StockAction): Seq[LifecycleStatus] =
      action match {
        case action: Buy => lifecycles :+ Opened(StockPurchaseLifecycle(stock, action, Seq.empty))
        case action: FifoSell =>
          lifecycles.collect { case o: Opened => o }.sortBy(_.value.buy.date).headOption.map { currentLifecycle =>
            if (currentLifecycle.value.unitsRemaining < action.units) {
              val split = splitSell(action, currentLifecycle.value.unitsRemaining)
              val modifiedLifecycle = currentLifecycle.value
                .copy(lifecycle = currentLifecycle.value.lifecycle :+ split._1)
              append(lifecycles.filterNot(_ eq currentLifecycle) :+ Closed(modifiedLifecycle), split._2)
            } else {
              val modifiedLifecycle = currentLifecycle.value.copy(lifecycle = currentLifecycle.value.lifecycle :+ action)
              lifecycles.filterNot(_ eq currentLifecycle) :+ LifecycleStatus(modifiedLifecycle)
            }
          } getOrElse lifecycles
        case action: LifoSell =>
          lifecycles.collect { case o: Opened => o }.sortBy(_.value.buy.date).lastOption.map { currentLifecycle =>
            if (currentLifecycle.value.unitsRemaining < action.units) {
              val split = splitSell(action, currentLifecycle.value.unitsRemaining)
              val modifiedLifecycle = currentLifecycle.value
                .copy(lifecycle = currentLifecycle.value.lifecycle :+ split._1)
              append(lifecycles.filterNot(_ eq currentLifecycle) :+ Closed(modifiedLifecycle), split._2)
            } else {
              val modifiedLifecycle = currentLifecycle.value
                .copy(lifecycle = currentLifecycle.value.lifecycle :+ action)
              lifecycles.filterNot(_ eq currentLifecycle) :+ LifecycleStatus(modifiedLifecycle)
            }
          } getOrElse lifecycles
        case action: CashDividend =>
          val opened = lifecycles.collect { case o: Opened => o }
          val existingShareCount = opened.map(_.value.unitsRemaining).sum
          lifecycles.collect { case c: Closed => c } ++ opened.map { x =>
            val units = (action.units * x.value.unitsRemaining) / existingShareCount
            val amount = Usd((action.amount.value * x.value.unitsRemaining) / existingShareCount)
            Opened(x.value.copy(lifecycle = x.value.lifecycle :+ action.copy(units = units, amount = amount)))
          }
        case action: StockDividend =>
          val opened = lifecycles.collect { case o: Opened => o }
          val existingShareCount = opened.map(_.value.unitsRemaining).sum
          lifecycles.collect { case c: Closed => c } ++ opened.map { x =>
            val units = (action.units * x.value.unitsRemaining) / existingShareCount
            val amount = Usd((action.amount.value * x.value.unitsRemaining) / existingShareCount)
            Opened(x.value.copy(lifecycle = x.value.lifecycle :+ action.copy(units = units, amount = amount)))
          }
      }

    private def splitSell(sell: LifoSell, sold: BigDecimal): (LifoSell, LifoSell) =
      (sell.copy(units = sold), sell.copy(units = sell.units - sold))

    private def splitSell(sell: FifoSell, sold: BigDecimal): (FifoSell, FifoSell) =
      (sell.copy(units = sold), sell.copy(units = sell.units - sold))
  }

  implicit class StockPurchaseLifecycleOperations(stockPurchaseLifecycle: StockPurchaseLifecycle) {
    def unitsRemaining: BigDecimal =
      stockPurchaseLifecycle.buy.units + stockPurchaseLifecycle.lifecycle.collect {
        case LifoSell(_, units, _, _) => units * -1
        case FifoSell(_, units, _, _) => units * -1
        case StockDividend(_, units, _, _) => units
        case _ => BigDecimal(0)
      }.sum

    def valueWithPrice(price: Usd): Usd =
      (stockPurchaseLifecycle.buy +: stockPurchaseLifecycle.lifecycle) valueWithPrice price
  }

  implicit class StockActionOperations(actions: Seq[StockAction]) {
    def valueWithPrice(price: Usd): Usd =
      calculateCurrentValue(
        soldAndRemaining = actions.foldLeft((BigDecimal(0), Usd(0)))(calculateSoldAndRemaining),
        price = price
      )

    private def calculateSoldAndRemaining(current: (BigDecimal, Usd), action: StockAction): (BigDecimal, Usd) =
      action match {
        case Buy(_, units, _, _) => (current._1 + units, current._2)
        case LifoSell(_, units, _, amount) => (current._1 - units, Usd(current._2.value + amount.value))
        case FifoSell(_, units, _, amount) => (current._1 - units, Usd(current._2.value + amount.value))
        case StockDividend(_, units, _, _) => (current._1 + units, current._2)
        case CashDividend(_, _, _, amount) => (current._1, Usd(current._2.value + amount.value))
      }

    private def calculateCurrentValue(soldAndRemaining: (BigDecimal, Usd), price: Usd): Usd =
      Usd((soldAndRemaining._1 * price.value) + soldAndRemaining._2.value)
  }
}
