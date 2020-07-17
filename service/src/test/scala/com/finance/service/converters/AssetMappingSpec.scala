package com.finance.service.converters

import java.time.OffsetDateTime

import com.finance.business.model.asset.{
  Asset => AssetModel,
  Buy => BuyModel,
  CashDividend => CashDividendModel,
  FifoSell => FifoSellModel,
  LifoSell => LifoSellModel,
  Stock => StockModel,
  StockDividend => StockDividendModel,
  StockValue => StockValueModel
}
import com.finance.business.model.types.{Id, Usd}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import com.finance.service.converters.AssetMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.StockAction.Type.members.{
  Buy,
  CashDividend,
  FifoSell,
  LifoSell,
  StockDividend
}
import com.finance.service.endpoints.definitions.{Asset, Stock, StockAction, StockValue}

class AssetMappingSpec extends AnyFreeSpec with Matchers {

  private val date = OffsetDateTime.now

  "AssetMapping" - {
    "should contain implicits that" - {
      "map stock request to stock model" in {
        Stock(
          5,
          6,
          "ticker",
          Vector(
            StockAction(date, Buy, 10, 50, 500),
            StockAction(date, LifoSell, 4, 50, 200),
            StockAction(date, FifoSell, 4, 50, 200),
            StockAction(date, CashDividend, 1, 50, 50),
            StockAction(date, StockDividend, 1, 50, 50)
          )
        ).mapTo[StockModel] shouldEqual StockModel(
          Some(Id(5)),
          Id(6),
          "ticker",
          Seq(
            BuyModel(date, 10, Usd(50), Usd(500)),
            LifoSellModel(date, 4, Usd(50), Usd(200)),
            FifoSellModel(date, 4, Usd(50), Usd(200)),
            CashDividendModel(date, 1, Usd(50), Usd(50)),
            StockDividendModel(date, 1, Usd(50), Usd(50))
          )
        )
      }
      "map asset model to asset response" in {
        val model: AssetModel = StockModel(
          Some(Id(5)),
          Id(6),
          "ticker",
          Seq(
            BuyModel(date, 10, Usd(50), Usd(500)),
            LifoSellModel(date, 4, Usd(50), Usd(200)),
            FifoSellModel(date, 4, Usd(50), Usd(200)),
            CashDividendModel(date, 1, Usd(50), Usd(50)),
            StockDividendModel(date, 1, Usd(50), Usd(50))
          )
        )

        model.mapTo[Asset] shouldEqual Asset(
          Some(
            Stock(
              5,
              6,
              "ticker",
              Vector(
                StockAction(date, Buy, 10, 50, 500),
                StockAction(date, LifoSell, 4, 50, 200),
                StockAction(date, FifoSell, 4, 50, 200),
                StockAction(date, CashDividend, 1, 50, 50),
                StockAction(date, StockDividend, 1, 50, 50)
              )
            )
          )
        )
      }
      "map stock value model to stock value response" in {
        StockValueModel(
          stock = StockModel(
            Some(Id(5)),
            Id(6),
            "ticker",
            Seq(
              BuyModel(date, 10, Usd(50), Usd(500)),
              LifoSellModel(date, 4, Usd(50), Usd(200)),
              FifoSellModel(date, 4, Usd(50), Usd(200)),
              CashDividendModel(date, 1, Usd(50), Usd(50)),
              StockDividendModel(date, 1, Usd(50), Usd(50))
            )
          ),
          price = Usd(50),
          asOf = date,
          quantity = 100,
          daysChange = Usd(25),
          daysChangePercentage = 50,
          daysGain = Usd(2500),
          pricePaid = Usd(1000),
          totalGain = Usd(4000),
          value = Usd(5000)
        ).mapTo[StockValue] shouldEqual StockValue(
          stock = Stock(
            5,
            6,
            "ticker",
            Vector(
              StockAction(date, Buy, 10, 50, 500),
              StockAction(date, LifoSell, 4, 50, 200),
              StockAction(date, FifoSell, 4, 50, 200),
              StockAction(date, CashDividend, 1, 50, 50),
              StockAction(date, StockDividend, 1, 50, 50)
            )
          ),
          price = 50,
          asOf = date,
          quantity = 100,
          daysChange = 25,
          daysChangePercentage = 50,
          daysGain = 2500,
          pricePaid = 1000,
          totalGain = 4000,
          value = 5000
        )
      }
    }
  }
}
