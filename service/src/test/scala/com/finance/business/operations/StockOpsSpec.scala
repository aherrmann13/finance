package com.finance.business.operations

import com.finance.business.model.asset._
import com.finance.business.model.types.{Id, Usd}
import com.finance.business.operations.StockOps._
import com.github.nscala_time.time.Imports._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StockOpsSpec extends AnyFreeSpec with Matchers {
  private val fakeStock = Stock(Some(Id(4)), Id(9), "ticker", Seq.empty)
  private val fakeStockPriceAsOf = StockPriceAsOf(Usd(50.5), Usd(51), DateTime.now)

  "StockOperations" - {
    "withPrice" - {
      "copies stock object" in {
        (fakeStock withPrice fakeStockPriceAsOf).stock shouldEqual fakeStock
      }
      "copies price from StockPriceAsOf" in {
        (fakeStock withPrice fakeStockPriceAsOf).price shouldEqual fakeStockPriceAsOf.current
      }
      "copies asOf from StockPriceAsOf" in {
        (fakeStock withPrice fakeStockPriceAsOf).asOf shouldEqual fakeStockPriceAsOf.asOf
      }
      "calculates quantity" - {
        val buys = Seq(
          StockAction(DateTime.now, Buy, 30.123, Usd(1), Usd(1)),
          StockAction(DateTime.now, Buy, 30.543, Usd(1), Usd(1)),
          StockAction(DateTime.now, Buy, 3.123, Usd(1), Usd(1))
        )

        val sells = Seq(
          StockAction(DateTime.now, Sell, 27.9, Usd(1), Usd(1)),
          StockAction(DateTime.now, Sell, 22.88, Usd(1), Usd(1)),
          StockAction(DateTime.now, Sell, 19.88, Usd(1), Usd(1))
        )

        val stockDividends = Seq(
          StockAction(DateTime.now, StockDividend, 6.9, Usd(1), Usd(1)),
          StockAction(DateTime.now, StockDividend, 14.537, Usd(1), Usd(1))
        )

        val cashDividends = Seq(
          StockAction(DateTime.now, CashDividend, 5.986, Usd(1), Usd(1)),
          StockAction(DateTime.now, CashDividend, 2.6532, Usd(1), Usd(1))
        )

        val buySum = buys.map(_.units).sum
        val sellSum = sells.map(_.units).sum
        val stockDividendSum = stockDividends.map(_.units).sum
        "adds stock buys" in {
          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).quantity shouldEqual buySum
        }
        "subtracts stock sells" in {
          (fakeStock.copy(actions = sells) withPrice fakeStockPriceAsOf).quantity shouldEqual sellSum * -1
        }
        "adds stock dividends" in {
          (fakeStock.copy(actions = stockDividends) withPrice fakeStockPriceAsOf).quantity shouldEqual stockDividendSum
        }
        "skips cash dividends" in {
          (fakeStock.copy(actions = cashDividends) withPrice fakeStockPriceAsOf).quantity shouldEqual 0
        }
        "combines all buys and sells" in {
          (fakeStock.copy(actions = buys ++ sells ++ stockDividends ++ cashDividends) withPrice fakeStockPriceAsOf).quantity shouldEqual buySum + (sellSum * -1) + stockDividendSum
        }
      }
      "calculates days change" in {
        (fakeStock withPrice fakeStockPriceAsOf).daysChange shouldEqual
          Usd(fakeStockPriceAsOf.current.value - fakeStockPriceAsOf.open.value)
      }
      "calculates days change percentage" in {
        (fakeStock withPrice fakeStockPriceAsOf).daysChangePercentage shouldEqual
          100 - (fakeStockPriceAsOf.current.value / fakeStockPriceAsOf.open.value)
      }
      "calculates days gain" in {
        val buys = Seq(
          StockAction(DateTime.now, Buy, 30.123, Usd(1), Usd(1)),
          StockAction(DateTime.now, Buy, 30.543, Usd(1), Usd(1)),
          StockAction(DateTime.now, Buy, 3.123, Usd(1), Usd(1))
        )

        (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).daysGain shouldEqual
          Usd(
            (fakeStockPriceAsOf.current.value * buys.map(_.units).sum) -
              (fakeStockPriceAsOf.open.value * buys.map(_.units).sum))
      }
      "calculates price paid" - {
        val buys = Seq(
          StockAction(DateTime.now, Buy, 3, Usd(67.8), Usd(165.3)),
          StockAction(DateTime.now, Buy, 3, Usd(32.90), Usd(35.87)),
          StockAction(DateTime.now, Buy, 3, Usd(52), Usd(53))
        )

        "adds stock buys" in {
          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).pricePaid shouldEqual
            Usd(buys.map(_.amountPaid.value).sum)
        }
        "ignores all other actions" in {
          (fakeStock.copy(
            actions = buys :+
              buys.head.copy(actionType = Sell) :+
              buys.head.copy(actionType = CashDividend) :+
              buys.head.copy(actionType = StockDividend)
          ) withPrice fakeStockPriceAsOf).pricePaid shouldEqual
            buys.map(_.amountPaid).reduce((x, y) => Usd(x.value + y.value))
        }
      }
      "calculates total gain" in {
        val buys = Seq(
          StockAction(DateTime.now, Buy, 3, Usd(67.8), Usd(165.3)),
          StockAction(DateTime.now, Buy, 3, Usd(32.90), Usd(35.87)),
          StockAction(DateTime.now, Buy, 3, Usd(52), Usd(53))
        )
        val totalValue = fakeStockPriceAsOf.current.value * buys.map(_.units).sum
        val pricePaid = buys.map(_.amountPaid.value).sum
        (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).totalGain shouldEqual Usd(totalValue - pricePaid)
      }
      "calculates value" in {
        val buys = Seq(
          StockAction(DateTime.now, Buy, 3, Usd(67.8), Usd(165.3)),
          StockAction(DateTime.now, Buy, 3, Usd(32.90), Usd(35.87)),
          StockAction(DateTime.now, Buy, 3, Usd(52), Usd(53))
        )

        (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).value shouldEqual
          Usd(fakeStockPriceAsOf.current.value * buys.map(_.units).sum)
      }
    }
  }
}
