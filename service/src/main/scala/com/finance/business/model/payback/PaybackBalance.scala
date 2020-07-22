package com.finance.business.model.payback

// TODO: use this model?
import com.finance.business.model.transaction.PaybackAmount

case class PaybackBalance(payback: Payback, amounts: Seq[PaybackAmount])
