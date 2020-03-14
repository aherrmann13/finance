package com.finance.business.repository

import com.finance.business.model.category.Category

trait CategoryRepository[F[_]] extends Repository[F, Category]