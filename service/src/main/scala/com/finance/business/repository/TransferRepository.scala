package com.finance.business.repository

import com.finance.business.model.transfer.Transfer

trait TransferRepository[F[_]] extends Repository[F, Transfer]