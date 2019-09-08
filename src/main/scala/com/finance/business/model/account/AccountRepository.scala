package com.finance.business.model.account

import com.finance.business.common.Repository

trait AccountRepository[F[_]] extends Repository[F, Account]
