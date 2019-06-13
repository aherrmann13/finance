package com.finance.business.account

sealed trait AccountType

case object Brokerage extends AccountType
case object Bank extends AccountType
