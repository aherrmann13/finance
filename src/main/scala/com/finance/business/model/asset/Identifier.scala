package com.finance.business.model.asset

sealed trait Identifier

case class Stock(ticker: String) extends Identifier