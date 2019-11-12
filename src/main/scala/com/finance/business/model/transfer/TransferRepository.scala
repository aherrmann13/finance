package com.finance.business.model.transfer

import com.finance.business.common.Repository

trait TransferRepository[F[_]] extends Repository[F, Transfer]