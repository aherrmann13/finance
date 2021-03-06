package com.finance.business.operations

import java.time.OffsetDateTime

import com.finance.business.model.asset._
import com.finance.business.model.types.Usd.implicits._
import com.finance.business.model.types.{Id, Usd}
import com.finance.business.operations.StockOps._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StockOpsSpec extends AnyFreeSpec with Matchers {
  private val fakeStock = Stock(Some(Id(4)), Id(9), "ticker", Seq.empty)
  private val fakeStockPriceAsOf = StockPriceAsOf(Usd(50.5), Usd(51), OffsetDateTime.now)

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
            Buy(OffsetDateTime.now, 30.123, Usd(1), Usd(1)),
            Buy(OffsetDateTime.now, 30.543, Usd(1), Usd(1)),
            Buy(OffsetDateTime.now, 3.123, Usd(1), Usd(1))
          )

          val sells = Seq(
            FifoSell(OffsetDateTime.now, 27.9, Usd(1), Usd(1)),
            LifoSell(OffsetDateTime.now, 22.88, Usd(1), Usd(1)),
            FifoSell(OffsetDateTime.now, 19.88, Usd(1), Usd(1))
          )

          val stockDividends = Seq(
            StockDividend(OffsetDateTime.now, 6.9, Usd(1), Usd(1)),
            StockDividend(OffsetDateTime.now, 14.537, Usd(1), Usd(1))
          )

          val cashDividends = Seq(
            CashDividend(OffsetDateTime.now, 5.986, Usd(1), Usd(1)),
            CashDividend(OffsetDateTime.now, 2.6532, Usd(1), Usd(1))
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
            (fakeStock.copy(actions = stockDividends) withPrice fakeStockPriceAsOf).quantity shouldEqual
              stockDividendSum
          }
          "skips cash dividends" in {
            (fakeStock.copy(actions = cashDividends) withPrice fakeStockPriceAsOf).quantity shouldEqual 0
          }
          "combines all buys and sells" in {
            (fakeStock.copy(actions =
              buys ++ sells ++ stockDividends ++ cashDividends
            ) withPrice fakeStockPriceAsOf).quantity shouldEqual buySum + (sellSum * -1) + stockDividendSum
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
            Buy(OffsetDateTime.now, 30.123, Usd(1), Usd(1)),
            Buy(OffsetDateTime.now, 30.543, Usd(1), Usd(1)),
            Buy(OffsetDateTime.now, 3.123, Usd(1), Usd(1))
          )

          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).daysGain shouldEqual
            Usd(
              (fakeStockPriceAsOf.current.value * buys.map(_.units).sum) -
                (fakeStockPriceAsOf.open.value * buys.map(_.units).sum)
            )
        }
        "calculates price paid" - {
          val buys = Seq(
            Buy(OffsetDateTime.now, 3, Usd(67.8), Usd(165.3)),
            Buy(OffsetDateTime.now, 3, Usd(32.90), Usd(35.87)),
            Buy(OffsetDateTime.now, 3, Usd(52), Usd(53))
          )

          "adds stock buys" in {
            (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).pricePaid shouldEqual
              Usd(buys.map(_.amount.value).sum)
          }
          "ignores all other actions" in {
            (fakeStock.copy(
              actions = buys :+
                FifoSell(OffsetDateTime.now, 27.9, Usd(1), Usd(1)) :+
                LifoSell(OffsetDateTime.now, 22.88, Usd(1), Usd(1)) :+
                StockDividend(OffsetDateTime.now, 6.9, Usd(1), Usd(1)) :+
                CashDividend(OffsetDateTime.now, 14.537, Usd(1), Usd(1))
            ) withPrice fakeStockPriceAsOf).pricePaid shouldEqual
              buys.map(_.amount).sum
          }
        }
        "calculates total gain" in {
          val buys = Seq(
            Buy(OffsetDateTime.now, 3, Usd(67.8), Usd(165.3)),
            Buy(OffsetDateTime.now, 3, Usd(32.90), Usd(35.87)),
            Buy(OffsetDateTime.now, 3, Usd(52), Usd(53))
          )
          val totalValue = fakeStockPriceAsOf.current.value * buys.map(_.units).sum
          val pricePaid = buys.map(_.amount.value).sum
          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).totalGain shouldEqual
            Usd(totalValue - pricePaid)
        }
        "calculates value" in {
          val buys = Seq(
            Buy(OffsetDateTime.now, 3, Usd(67.8), Usd(165.3)),
            Buy(OffsetDateTime.now, 3, Usd(32.90), Usd(35.87)),
            Buy(OffsetDateTime.now, 3, Usd(52), Usd(53))
          )

          (fakeStock.copy(actions = buys) withPrice fakeStockPriceAsOf).value shouldEqual
            Usd(fakeStockPriceAsOf.current.value * buys.map(_.units).sum)
        }
      }
      "asLifecycle" - {
        val buy = Buy(OffsetDateTime.now, 50, Usd(50.0), Usd(70.0))
        val fifoSell = FifoSell(OffsetDateTime.now.plusWeeks(1), 50, Usd(50.0), Usd(70.0))
        val lifoSell = LifoSell(OffsetDateTime.now.plusWeeks(1), 50, Usd(50.0), Usd(70.0))
        val cashDividend = CashDividend(OffsetDateTime.now.plusWeeks(1), 50, Usd(50.0), Usd(70.0))
        val stockDividend = StockDividend(OffsetDateTime.now.plusWeeks(1), 50, Usd(50.0), Usd(70.0))
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
          val buy0 = Buy(OffsetDateTime.now, 50, Usd(50.0), Usd(70.0))
          val buy1 = Buy(OffsetDateTime.now.plusDays(1), 50, Usd(50.0), Usd(70.0))
          val buy2 = Buy(OffsetDateTime.now.plusDays(2), 50, Usd(50.0), Usd(70.0))
          val buy3 = Buy(OffsetDateTime.now.plusDays(3), 50, Usd(50.0), Usd(70.0))
          val buy4 = Buy(OffsetDateTime.now.plusDays(4), 50, Usd(50.0), Usd(70.0))
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
          val oldestBuy = buy.copy(date = OffsetDateTime.now.minusMonths(1))
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
          val oldestBuy = buy.copy(date = OffsetDateTime.now.minusMonths(1))
          val secondOldestBuy = buy.copy(date = OffsetDateTime.now.minusWeeks(1))
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
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1))
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1))
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
          val oldestBuy = buy.copy(date = OffsetDateTime.now.minusMonths(1))
          val secondOldestBuy = buy.copy(date = OffsetDateTime.now.minusWeeks(1))
          val thirdOldestBuy = buy.copy(date = OffsetDateTime.now.minusDays(1))
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
          val newestBuy = buy.copy(date = OffsetDateTime.now.plusDays(1))
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
          val newestBuy = buy.copy(date = OffsetDateTime.now.plusMonths(1))
          val secondNewestBuy = buy.copy(date = OffsetDateTime.now.plusWeeks(1))
          val splitLifoSell = lifoSell.copy(units = newestBuy.units + 30, date = OffsetDateTime.now.plusYears(1))
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy, newestBuy, secondNewestBuy, buy, buy, splitLifoSell))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, buy, Seq.empty),
            StockPurchaseLifecycle(fakeStockWithAmt, secondNewestBuy, Seq(splitLifoSell.copy(units = 30))),
            StockPurchaseLifecycle(fakeStockWithAmt, newestBuy, Seq(splitLifoSell.copy(units = newestBuy.units)))
          )
        }
        "should return lifo sell split between two stocks with same date" in {
          val buy0 = buy.copy(date = OffsetDateTime.now)
          val buy1 = buy.copy(date = buy0.date)
          val splitLifoSell = lifoSell.copy(units = buy0.units + (buy1.units / 2), date = buy0.date.plusDays(1))
          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, splitLifoSell))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(splitLifoSell.copy(units = buy0.units))),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(splitLifoSell.copy(units = buy1.units / 2)))
          )
        }

        "should return lifo sells to newest items in order" in {
          val newestBuy = buy.copy(date = OffsetDateTime.now.plusMonths(1))
          val secondNewestBuy = buy.copy(date = OffsetDateTime.now.plusWeeks(1))
          val thirdNewestBuy = buy.copy(date = OffsetDateTime.now.plusDays(1))
          val lifoSell0 = lifoSell
            .copy(units = newestBuy.units - 10, amount = Usd(12), date = OffsetDateTime.now.plusYears(1))
          val lifoSell1 = lifoSell
            .copy(units = secondNewestBuy.units + 10, amount = Usd(13), date = OffsetDateTime.now.plusYears(1))
          val lifoSell2 = lifoSell
            .copy(units = thirdNewestBuy.units, amount = Usd(14), date = OffsetDateTime.now.plusYears(1))
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
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1))
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1))
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
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 20)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
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
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 20)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = cashDividend.copy(units = 30, amount = Usd(60), date = OffsetDateTime.now.plusDays(1))
          val lifoSell0 = lifoSell.copy(units = 30, date = OffsetDateTime.now.plusWeeks(1))
          val dividend1 = cashDividend.copy(units = 70, amount = Usd(35), date = OffsetDateTime.now.plusWeeks(1))

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, lifoSell0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy0,
              Seq(
                dividend0.copy(units = 6, amount = Usd(12)),
                dividend1.copy(units = 20, amount = Usd(10))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy1,
              Seq(
                dividend0.copy(units = 9, amount = Usd(18)),
                dividend1.copy(units = 30, amount = Usd(15))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy2,
              Seq(
                dividend0.copy(units = 15, amount = Usd(30)),
                lifoSell0,
                dividend1.copy(units = 20, amount = Usd(10))
              )
            )
          )
        }
        "should return cash dividend split between lifecycles with more shares from stock dividend" in {
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 20)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = stockDividend
            .copy(units = 30, amount = Usd(60), date = OffsetDateTime.now.minusMonths(1).plusDays(1))
          val dividend1 = cashDividend.copy(units = 130, amount = Usd(260), date = OffsetDateTime.now.plusWeeks(1))

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy0,
              Seq(
                dividend0,
                dividend1.copy(units = 50, amount = Usd(100))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy1,
              Seq(
                dividend1.copy(units = 30, amount = Usd(60))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy2,
              Seq(
                dividend1.copy(units = 50, amount = Usd(100))
              )
            )
          )
        }
        "should not return cash dividend if all stocks have been sold" in {
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 30)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val sell0 = fifoSell.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val sell1 = fifoSell.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val dividend0 = cashDividend.copy(units = 30, amount = Usd(60), date = OffsetDateTime.now.plusWeeks(1))
          val dividend1 = cashDividend.copy(units = 130, amount = Usd(260), date = OffsetDateTime.now.plusWeeks(1))

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, sell0, sell1, dividend0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(fakeStockWithAmt, buy0, Seq(sell0)),
            StockPurchaseLifecycle(fakeStockWithAmt, buy1, Seq(sell1))
          )
        }
        "should return stock dividend split between lifecycles with all shares unsold" in {
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 20)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
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
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 20)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = stockDividend.copy(units = 30, amount = Usd(60), date = OffsetDateTime.now.plusDays(1))
          val lifoSell0 = lifoSell.copy(units = 30, date = OffsetDateTime.now.plusWeeks(1))
          val dividend1 = stockDividend.copy(units = 50, amount = Usd(25), date = OffsetDateTime.now.plusWeeks(1))

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, lifoSell0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy0,
              Seq(
                dividend0.copy(units = 6, amount = Usd(12)),
                dividend1.copy(units = 13, amount = Usd(6.5))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy1,
              Seq(
                dividend0.copy(units = 9, amount = Usd(18)),
                dividend1.copy(units = 19.5, amount = Usd(9.75))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy2,
              Seq(
                dividend0.copy(units = 15, amount = Usd(30)),
                lifoSell0,
                dividend1.copy(units = 17.5, amount = Usd(8.75))
              )
            )
          )
        }
        "should return stock dividend split between lifecycles with more shares from stock dividend" in {
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 20)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val buy2 = buy.copy(units = 50)
          val dividend0 = stockDividend
            .copy(units = 30, amount = Usd(60), date = OffsetDateTime.now.minusMonths(1).plusDays(1))
          val dividend1 = stockDividend.copy(units = 130, amount = Usd(260), date = OffsetDateTime.now.plusWeeks(1))

          val fakeStockWithAmt = fakeStock.copy(actions = Seq(buy0, buy1, buy2, dividend0, dividend1))
          fakeStockWithAmt.asLifecycle shouldEqual Seq(
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy0,
              Seq(
                dividend0,
                dividend1.copy(units = 50, amount = Usd(100))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy1,
              Seq(
                dividend1.copy(units = 30, amount = Usd(60))
              )
            ),
            StockPurchaseLifecycle(
              fakeStockWithAmt,
              buy2,
              Seq(
                dividend1.copy(units = 50, amount = Usd(100))
              )
            )
          )
        }
        "should not return stock dividend if all stocks have been sold" in {
          val buy0 = buy.copy(date = OffsetDateTime.now.minusMonths(1), units = 30)
          val buy1 = buy.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val sell0 = fifoSell.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val sell1 = fifoSell.copy(date = OffsetDateTime.now.minusWeeks(1), units = 30)
          val dividend0 = stockDividend.copy(units = 30, amount = Usd(60), date = OffsetDateTime.now.plusWeeks(1))
          val dividend1 = stockDividend.copy(units = 130, amount = Usd(260), date = OffsetDateTime.now.plusWeeks(1))

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
        val buy = Buy(OffsetDateTime.now, 50, Usd(50.0), Usd(70.0))
        val fifoSell = FifoSell(OffsetDateTime.now.plusWeeks(1), 10, Usd(50.0), Usd(70.0))
        val lifoSell = LifoSell(OffsetDateTime.now.plusWeeks(1), 15, Usd(50.0), Usd(70.0))
        val cashDividend = CashDividend(OffsetDateTime.now.plusWeeks(1), 50, Usd(50.0), Usd(70.0))
        val stockDividend = StockDividend(OffsetDateTime.now.plusWeeks(1), 50, Usd(50.0), Usd(70.0))
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
          lifecycle
            .copy(lifecycle = Seq(fifoSell, lifoSell, cashDividend, stockDividend, buy))
            .unitsRemaining shouldEqual 75
        }
      }
      "valueWithPrice" - {
        "should return price of buy concatenated with all other actions" in {
          StockPurchaseLifecycle(
            stock = fakeStock,
            buy = Buy(OffsetDateTime.now, 10, Usd(60), Usd(605)),
            lifecycle = Seq(
              StockDividend(OffsetDateTime.now, 2, Usd(70), Usd(140)),
              CashDividend(OffsetDateTime.now, 2, Usd(70), Usd(140)),
              LifoSell(OffsetDateTime.now, 5, Usd(70), Usd(350)),
              FifoSell(OffsetDateTime.now, 5, Usd(80), Usd(400))
            )
          ) valueWithPrice Usd(80) shouldEqual Usd(1050)
        }
      }
    }
    "StockActionOperations" - {
      "valueWithPrice" - {
        "should return value of current stock at the given price" in {
          Seq(
            Buy(OffsetDateTime.now, 10, Usd(60), Usd(605)),
            Buy(OffsetDateTime.now, 10, Usd(60), Usd(605))
          ) valueWithPrice Usd(40) shouldEqual Usd(800)
        }
        "should return value of what was sold regardless of the given price" in {
          Seq(
            Buy(OffsetDateTime.now, 10, Usd(60), Usd(605)),
            LifoSell(OffsetDateTime.now, 5, Usd(70), Usd(350)),
            FifoSell(OffsetDateTime.now, 5, Usd(80), Usd(400))
          ) valueWithPrice Usd(40) shouldEqual Usd(750)
        }
        "should return value of cash dividend as the amount of the dividend" in {
          Seq(
            Buy(OffsetDateTime.now, 10, Usd(60), Usd(605)),
            CashDividend(OffsetDateTime.now, 2, Usd(70), Usd(140))
          ) valueWithPrice Usd(40) shouldEqual Usd(540)
        }
        "should return value of stock dividend as the amount times the current price" in {
          Seq(
            Buy(OffsetDateTime.now, 10, Usd(60), Usd(605)),
            StockDividend(OffsetDateTime.now, 2, Usd(70), Usd(140))
          ) valueWithPrice Usd(40) shouldEqual Usd(480)
        }
      }
    }
  }
}
