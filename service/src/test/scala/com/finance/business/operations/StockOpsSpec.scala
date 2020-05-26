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

  "StockOps" - {
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
            Buy(DateTime.now, 30.123, Usd(1), Usd(1)),
            Buy(DateTime.now, 30.543, Usd(1), Usd(1)),
            Buy(DateTime.now, 3.123, Usd(1), Usd(1))
          )

          val sells = Seq(
            FifoSell(DateTime.now, 27.9, Usd(1), Usd(1)),
            LifoSell(DateTime.now, 22.88, Usd(1), Usd(1)),
            FifoSell(DateTime.now, 19.88, Usd(1), Usd(1))
          )

          val stockDividends = Seq(
            StockDividend(DateTime.now, 6.9, Usd(1), Usd(1)),
            StockDividend(DateTime.now, 14.537, Usd(1), Usd(1))
          )

          val cashDividends = Seq(
            CashDividend(DateTime.now, 5.986, Usd(1), Usd(1)),
            CashDividend(DateTime.now, 2.6532, Usd(1), Usd(1))
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
            Buy(DateTime.now, 30.123, Usd(1), Usd(1)),
            Buy(DateTime.now, 30.543, Usd(1), Usd(1)),
            Buy(DateTime.now, 3.123, Usd(1), Usd(1))
          )

          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).daysGain shouldEqual
            Usd(
              (fakeStockPriceAsOf.current.value * buys.map(_.units).sum) -
                (fakeStockPriceAsOf.open.value * buys.map(_.units).sum))
        }
        "calculates price paid" - {
          val buys = Seq(
            Buy(DateTime.now, 3, Usd(67.8), Usd(165.3)),
            Buy(DateTime.now, 3, Usd(32.90), Usd(35.87)),
            Buy(DateTime.now, 3, Usd(52), Usd(53))
          )

          "adds stock buys" in {
            (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).pricePaid shouldEqual
              Usd(buys.map(_.amount.value).sum)
          }
          "ignores all other actions" in {
            (fakeStock.copy(
              actions = buys :+
                FifoSell(DateTime.now, 27.9, Usd(1), Usd(1)) :+
                LifoSell(DateTime.now, 22.88, Usd(1), Usd(1)) :+
                StockDividend(DateTime.now, 6.9, Usd(1), Usd(1)) :+
                CashDividend(DateTime.now, 14.537, Usd(1), Usd(1))
            ) withPrice fakeStockPriceAsOf).pricePaid shouldEqual
              buys.map(_.amount).reduce((x, y) => Usd(x.value + y.value))
          }
        }
        "calculates total gain" in {
          val buys = Seq(
            Buy(DateTime.now, 3, Usd(67.8), Usd(165.3)),
            Buy(DateTime.now, 3, Usd(32.90), Usd(35.87)),
            Buy(DateTime.now, 3, Usd(52), Usd(53))
          )
          val totalValue = fakeStockPriceAsOf.current.value * buys.map(_.units).sum
          val pricePaid = buys.map(_.amount.value).sum
          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).totalGain shouldEqual Usd(totalValue - pricePaid)
        }
        "calculates value" in {
          val buys = Seq(
            Buy(DateTime.now, 3, Usd(67.8), Usd(165.3)),
            Buy(DateTime.now, 3, Usd(32.90), Usd(35.87)),
            Buy(DateTime.now, 3, Usd(52), Usd(53))
          )

          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).value shouldEqual
            Usd(fakeStockPriceAsOf.current.value * buys.map(_.units).sum)
        }
      }
    }
  }
}