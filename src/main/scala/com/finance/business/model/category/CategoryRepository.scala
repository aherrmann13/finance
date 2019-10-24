package com.finance.business.model.category

import com.finance.business.common.Repository

trait CategoryRepository[F[_]] extends Repository[F, Category]