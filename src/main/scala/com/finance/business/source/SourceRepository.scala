package com.finance.business.source

import com.finance.business.common.Repository

trait SourceRepository[F[_]] extends Repository[F, Source]