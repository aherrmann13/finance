package com.finance.business.model.payback

import com.finance.business.model.transaction.PaybackAmount

case class PaybackBalance(payback: Payback, amounts: Seq[PaybackAmount])
