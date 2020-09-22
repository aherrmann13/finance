package com.finance.persistence.fakes

import cats.Monad
import com.finance.business.model.category.Category
import com.finance.business.repository.CategoryRepository

class FakeCategoryRepository[F[_]: Monad] extends FakeRepository[F, Category] with CategoryRepository[F]
