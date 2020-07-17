package com.finance.service.converters

import com.finance.business.model.asset.{
  Asset => AssetModel,
  Buy => BuyModel,
  CashDividend => CashDividendModel,
  FifoSell => FifoSellModel,
  LifoSell => LifoSellModel,
  Stock => StockModel,
  StockAction => StockActionModel,
  StockDividend => StockDividendModel,
  StockValue => StockValueModel
}
import com.finance.business.model.types.{Id, Usd}
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.StockAction.Type.members._
import com.finance.service.endpoints.definitions.{Asset, Stock, StockAction, StockValue}

object AssetMapping {
  private implicit val stockActionRequestMapping: Mapping[StockAction, StockActionModel] =
    (a: StockAction) =>
      a.`type` match {
        case Buy => BuyModel(a.date, a.units, Usd(a.unitPrice), Usd(a.amountPaid))
        case LifoSell => LifoSellModel(a.date, a.units, Usd(a.unitPrice), Usd(a.amountPaid))
        case FifoSell => FifoSellModel(a.date, a.units, Usd(a.unitPrice), Usd(a.amountPaid))
        case CashDividend => CashDividendModel(a.date, a.units, Usd(a.unitPrice), Usd(a.amountPaid))
        case StockDividend => StockDividendModel(a.date, a.units, Usd(a.unitPrice), Usd(a.amountPaid))
      }

  private implicit val stockActionResponseMapping: Mapping[StockActionModel, StockAction] = {
    case BuyModel(date, units, unitPrice, amountPaid) =>
      StockAction(date, Buy, units, unitPrice.value, amountPaid.value)
    case LifoSellModel(date, units, unitPrice, amountPaid) =>
      StockAction(date, LifoSell, units, unitPrice.value, amountPaid.value)
    case FifoSellModel(date, units, unitPrice, amountPaid) =>
      StockAction(date, FifoSell, units, unitPrice.value, amountPaid.value)
    case CashDividendModel(date, units, unitPrice, amountPaid) =>
      StockAction(date, CashDividend, units, unitPrice.value, amountPaid.value)
    case StockDividendModel(date, units, unitPrice, amountPaid) =>
      StockAction(date, StockDividend, units, unitPrice.value, amountPaid.value)
  }

  private implicit val stockModelResponseMapping: Mapping[StockModel, Stock] = (a: StockModel) => {
    Stock(
      id = a.id.map(_.value).getOrElse(-1),
      accountId = a.accountId.value,
      ticker = a.ticker,
      actions = a.actions.map(_.mapTo[StockAction]).toVector
    )
  }

  implicit val stockRequestMapping: Mapping[Stock, StockModel] = (a: Stock) =>
    StockModel(
      id = Some(Id(a.id)),
      accountId = Id(a.accountId),
      ticker = a.ticker,
      actions = a.actions.map(_.mapTo[StockActionModel])
    )

  implicit val stockResponseMapping: Mapping[AssetModel, Asset] = {
    case s: StockModel => Asset(stock = Some(s.mapTo[Stock]))
  }

  implicit val stockValueResponseMapping: Mapping[StockValueModel, StockValue] = (a: StockValueModel) => {
    StockValue(
      stock = a.stock.mapTo[Stock],
      price = a.price.value,
      asOf = a.asOf,
      quantity = a.quantity,
      daysChange = a.daysChange.value,
      daysChangePercentage = a.daysChangePercentage,
      daysGain = a.daysGain.value,
      pricePaid = a.pricePaid.value,
      totalGain = a.totalGain.value,
      value = a.value.value
    )
  }
}
