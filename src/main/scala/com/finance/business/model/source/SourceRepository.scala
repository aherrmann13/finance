package com.finance.business.model.source

import com.finance.business.common.Repository

trait SourceRepository[F[_]] extends Repository[F, Source]