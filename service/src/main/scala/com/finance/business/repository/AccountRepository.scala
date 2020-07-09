package com.finance.business.repository

import com.finance.business.model.account.Account

trait AccountRepository[F[_]] extends Repository[F, Account]
