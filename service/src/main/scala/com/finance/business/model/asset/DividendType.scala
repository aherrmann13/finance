package com.finance.business.model.asset

sealed trait DividendType

case object Cash extends DividendType
case object Stock extends DividendType