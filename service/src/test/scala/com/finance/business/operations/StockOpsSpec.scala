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
      "asLifecycle" - {
        val buy = Buy(DateTime.now, 50, Usd(50.0), Usd(70.0))
        val fifoSell = FifoSell(DateTime.nextWeek, 50, Usd(50.0), Usd(70.0))
        val lifoSell = LifoSell(DateTime.nextWeek, 50, Usd(50.0), Usd(70.0))
        val cashDividend = CashDividend(DateTime.nextWeek, 50, Usd(50.0), Usd(70.0))
        val stockDividend = StockDividend(DateTime.nextWeek, 50, Usd(50.0), Usd(70.0))
        "should return all buys as new lifecycle item" in {
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy, buy, buy, buy, buy))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty)
          )
        }
        "should return items ordered by date" in {
          val buy0 = Buy(DateTime.now, 50, Usd(50.0), Usd(70.0))
          val buy1 = Buy(DateTime.now.plusDays(1), 50, Usd(50.0), Usd(70.0))
          val buy2 = Buy(DateTime.now.plusDays(2), 50, Usd(50.0), Usd(70.0))
          val buy3 = Buy(DateTime.now.plusDays(3), 50, Usd(50.0), Usd(70.0))
          val buy4 = Buy(DateTime.now.plusDays(4), 50, Usd(50.0), Usd(70.0))
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy2, buy4, buy0, buy3, buy1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy2, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy3, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy4, Seq.empty)
          )
        }
        "should return fifo sells appended to oldest item" in {
          val oldestBuy = buy.copy(date = DateTime.lastMonth)
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy, oldestBuy, buy, buy, buy, fifoSell))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, oldestBuy, Seq(fifoSell)),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty)
          )
        }
        "should return fifo sells split between items when sell is more than one buys worth" in {
          val oldestBuy = buy.copy(date = DateTime.lastMonth)
          val secondOldestBuy = buy.copy(date = DateTime.lastWeek)
          val splitFifoSell = fifoSell.copy(units = oldestBuy.units + 30)
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy, oldestBuy, secondOldestBuy, buy, buy, splitFifoSell))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, oldestBuy, Seq(splitFifoSell.copy(units = oldestBuy.units))),
            StockPurchaseLifecycle(fakeStockWithAmt, secondOldestBuy, Seq(splitFifoSell.copy(units = 30))),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty)
          )
        }
        "should not return fifo sell if all stock already sold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth)
          val buy1 = buy.copy(date = DateTime.lastWeek)
          val sell0 = fifoSell.copy(units = buy0.units)
          val sell1 = fifoSell.copy(units = buy1.units)
          val sell2 = fifoSell.copy(units = 50)
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, sell0, sell1, sell2))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(sell0)),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(sell1))
          )
        }
        "should return fifo sells to oldest items in order" in {
          val oldestBuy = buy.copy(date = DateTime.lastMonth)
          val secondOldestBuy = buy.copy(date = DateTime.lastWeek)
          val thirdOldestBuy = buy.copy(date = DateTime.lastDay)
          val fifoSell0 = fifoSell.copy(units = oldestBuy.units - 10, amount = Usd(12))
          val fifoSell1 = fifoSell.copy(units = secondOldestBuy.units + 10, amount = Usd(13))
          val fifoSell2 = fifoSell.copy(units = thirdOldestBuy.units, amount = Usd(14))
          val fakeStockWithAmt = fakeStock
            .copy(actions = Seq(buy, oldestBuy, secondOldestBuy, buy, thirdOldestBuy, fifoSell0, fifoSell1, fifoSell2))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, oldestBuy, Seq(fifoSell0, fifoSell1.copy(units = 10))),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              secondOldestBuy,
              Seq(fifoSell1.copy(units = secondOldestBuy.units))
            ),
            StockPurchaseLifecycle(fakeStockWithAmt, thirdOldestBuy, Seq(fifoSell2)),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty)
          )
        }
        "should return lifo sells appended to newest item" in {
          val newestBuy = buy.copy(date = DateTime.nextDay)
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy, newestBuy, buy, buy, buy, lifoSell))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, newestBuy, Seq(lifoSell))
          )
        }
        "should return lifo sells split between items when sell is more than one buys worth" in {
          val newestBuy = buy.copy(date = DateTime.nextMonth)
          val secondNewestBuy = buy.copy(date = DateTime.nextWeek)
          val splitLifoSell = lifoSell.copy(units = newestBuy.units + 30, date = DateTime.nextYear)
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy, newestBuy, secondNewestBuy, buy, buy, splitLifoSell))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, secondNewestBuy, Seq(splitLifoSell.copy(units = 30))),
            StockPurchaseLifecycle(fakeStockWithAmt, newestBuy, Seq(splitLifoSell.copy(units = newestBuy.units)))
          )
        }
        "should return lifo sells to newest items in order" in {
          val newestBuy = buy.copy(date = DateTime.nextMonth)
          val secondNewestBuy = buy.copy(date = DateTime.nextWeek)
          val thirdNewestBuy = buy.copy(date = DateTime.nextDay)
          val lifoSell0 = lifoSell.copy(units = newestBuy.units - 10, amount = Usd(12), date = DateTime.nextYear)
          val lifoSell1 = lifoSell.copy(units = secondNewestBuy.units + 10, amount = Usd(13), date = DateTime.nextYear)
          val lifoSell2 = lifoSell.copy(units = thirdNewestBuy.units, amount = Usd(14), date = DateTime.nextYear)
          val fakeStockWithAmt = fakeStock
            .copy(actions = Seq(buy, newestBuy, thirdNewestBuy, buy, secondNewestBuy, lifoSell0, lifoSell1, lifoSell2))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, thirdNewestBuy, Seq(lifoSell2)),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              secondNewestBuy,
              Seq(lifoSell1.copy(units = secondNewestBuy.units))
            ),
            StockPurchaseLifecycle(fakeStockWithAmt, newestBuy, Seq(lifoSell0, lifoSell1.copy(units = 10)))
          )
        }
        "should not return lifo sell if all stock already sold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth)
          val buy1 = buy.copy(date = DateTime.lastWeek)
          val sell0 = lifoSell.copy(units = buy0.units)
          val sell1 = lifoSell.copy(units = buy1.units)
          val sell2 = lifoSell.copy(units = 50)
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, sell0, sell1, sell2))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(sell0)),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(sell1))
          )
        }
        "should return cash dividend split between lifecycles with all shares unsold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 20)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend = cashDividend.copy(units = 30, amount = Usd(60))

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(dividend.copy(units = 6, amount = Usd(12)))),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(dividend.copy(units = 9, amount = Usd(18)))),
            StockPurchaseLifecycle(fakeStockWithAmt, buy2, Seq(dividend.copy(units = 15, amount = Usd(30))))
          )
        }
        "should return cash dividend split between lifecycles with some shares unsold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 20)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = cashDividend.copy(units = 30, amount = Usd(60), date = DateTime.nextDay)
          val lifoSell0 = lifoSell.copy(units = 30, date = DateTime.nextWeek)
          val dividend1 = cashDividend.copy(units = 70, amount = Usd(35), date = DateTime.nextMonth)

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, lifoSell0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(
              dividend0.copy(units = 6, amount = Usd(12)),
              dividend1.copy(units = 20, amount = Usd(10))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(
              dividend0.copy(units = 9, amount = Usd(18)),
              dividend1.copy(units = 30, amount = Usd(15))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy2, Seq(
              dividend0.copy(units = 15, amount = Usd(30)),
              lifoSell0,
              dividend1.copy(units = 20, amount = Usd(10))
            ))
          )
        }
        "should return cash dividend split between lifecycles with more shares from stock dividend" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 20)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = stockDividend.copy(units = 30, amount = Usd(60), date = DateTime.lastMonth.plusDays(1))
          val dividend1 = cashDividend.copy(units = 130, amount = Usd(260), date = DateTime.nextMonth)

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(
              dividend0,
              dividend1.copy(units = 50, amount = Usd(100))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(
              dividend1.copy(units = 30, amount = Usd(60))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy2, Seq(
              dividend1.copy(units = 50, amount = Usd(100))
            ))
          )
        }
        "should not return cash dividend if all stocks have been sold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 30)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val sell0 = fifoSell.copy(date = DateTime.lastWeek, units = 30)
          val sell1 = fifoSell.copy(date = DateTime.lastWeek, units = 30)
          val dividend0 = cashDividend.copy(units = 30, amount = Usd(60), date = DateTime.nextMonth)
          val dividend1 = cashDividend.copy(units = 130, amount = Usd(260), date = DateTime.nextMonth)

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, sell0, sell1, dividend0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(sell0)),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(sell1))
          )
        }
        "should return stock dividend split between lifecycles with all shares unsold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 20)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend = stockDividend.copy(units = 30, amount = Usd(60))

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(dividend.copy(units = 6, amount = Usd(12)))),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(dividend.copy(units = 9, amount = Usd(18)))),
            StockPurchaseLifecycle(fakeStockWithAmt, buy2, Seq(dividend.copy(units = 15, amount = Usd(30))))
          )
        }
        "should return stock dividend split between lifecycles with some shares unsold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 20)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = stockDividend.copy(units = 30, amount = Usd(60), date = DateTime.nextDay)
          val lifoSell0 = lifoSell.copy(units = 30, date = DateTime.nextWeek)
          val dividend1 = stockDividend.copy(units = 50, amount = Usd(25), date = DateTime.nextMonth)

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, lifoSell0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(
              dividend0.copy(units = 6, amount = Usd(12)),
              dividend1.copy(units = 13, amount = Usd(6.5))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(
              dividend0.copy(units = 9, amount = Usd(18)),
              dividend1.copy(units = 19.5, amount = Usd(9.75))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy2, Seq(
              dividend0.copy(units = 15, amount = Usd(30)),
              lifoSell0,
              dividend1.copy(units = 17.5, amount = Usd(8.75))
            ))
          )
        }
        "should return stock dividend split between lifecycles with more shares from stock dividend" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 20)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = stockDividend.copy(units = 30, amount = Usd(60), date = DateTime.lastMonth.plusDays(1))
          val dividend1 = stockDividend.copy(units = 130, amount = Usd(260), date = DateTime.nextMonth)

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(
              dividend0,
              dividend1.copy(units = 50, amount = Usd(100))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(
              dividend1.copy(units = 30, amount = Usd(60))
            )),
            StockPurchaseLifecycle(fakeStockWithAmt, buy2, Seq(
              dividend1.copy(units = 50, amount = Usd(100))
            ))
          )
        }
        "should not return stock dividend if all stocks have been sold" in {
          val buy0 = buy.copy(date = DateTime.lastMonth, units = 30)
          val buy1 = buy.copy(date = DateTime.lastWeek, units = 30)
          val sell0 = fifoSell.copy(date = DateTime.lastWeek, units = 30)
          val sell1 = fifoSell.copy(date = DateTime.lastWeek, units = 30)
          val dividend0 = stockDividend.copy(units = 30, amount = Usd(60), date = DateTime.nextMonth)
          val dividend1 = stockDividend.copy(units = 130, amount = Usd(260), date = DateTime.nextMonth)

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, sell0, sell1, dividend0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(sell0)),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(sell1))
          )
        }
      }
    }
    "StockPurchaseLifecycleOperations" - {
      "unitsRemaining" - {
        val buy = Buy(DateTime.now, 50, Usd(50.0), Usd(70.0))
        val fifoSell = FifoSell(DateTime.nextWeek, 10, Usd(50.0), Usd(70.0))
        val lifoSell = LifoSell(DateTime.nextWeek, 15, Usd(50.0), Usd(70.0))
        val cashDividend = CashDividend(DateTime.nextWeek, 50, Usd(50.0), Usd(70.0))
        val stockDividend = StockDividend(DateTime.nextWeek, 50, Usd(50.0), Usd(70.0))
        val lifecycle = StockPurchaseLifecycle(fakeStock, buy, Seq.empty)

        "should return units when no lifecycle items" in {
          lifecycle.unitsRemaining shouldEqual 50
        }
        "should return units minus fifo sells" in {
          lifecycle.copy(lifecycle = Seq(fifoSell, fifoSell)).unitsRemaining shouldEqual 30
        }
        "should return units minus lifo sells" in {
          lifecycle.copy(lifecycle = Seq(lifoSell, lifoSell)).unitsRemaining shouldEqual 20
        }
        "should return units plus stock dividends" in {
          lifecycle.copy(lifecycle = Seq(stockDividend)).unitsRemaining shouldEqual 100
        }
        "should return units ignoring cash dividends" in {
          lifecycle.copy(lifecycle = Seq(cashDividend)).unitsRemaining shouldEqual 50
        }
        "should return units ignoring buys" in {
          lifecycle.copy(lifecycle = Seq(buy)).unitsRemaining shouldEqual 50
        }
        "should return units minus sells plus stock dividend ignoring cash dividends and buys" in {
          lifecycle.copy(lifecycle = Seq(fifoSell, lifoSell, cashDividend, stockDividend, buy))
            .unitsRemaining shouldEqual 75
        }
      }
    }
  }
}