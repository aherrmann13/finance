package com.finance.persistence.fakes

import cats.Monad
import com.finance.business.model.account.Account
import com.finance.business.repository.AccountRepository

class FakeAccountRepository[F[_]: Monad] extends FakeRepository[F, Account] with AccountRepository[F]
