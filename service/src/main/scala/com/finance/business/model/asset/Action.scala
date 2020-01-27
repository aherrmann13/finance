package com.finance.business.model.asset

sealed trait Action;

case object Buy extends Action
case object Sell extends Action
case object Other extends Action