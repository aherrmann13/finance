package com.finance.business.model.asset

import com.finance.business.common.Repository

trait AssetRepository[F[_]] extends Repository[F, Asset]
