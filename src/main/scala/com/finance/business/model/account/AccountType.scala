package com.finance.business.model.account

sealed trait AccountType

case object Brokerage extends AccountType
case object Bank extends AccountType
