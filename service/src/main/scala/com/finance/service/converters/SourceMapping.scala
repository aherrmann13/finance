package com.finance.service.converters

import com.finance.business.model.source.{Source => SourceModel}
import com.finance.business.model.types.{Description, Id, Name}
import com.finance.service.endpoints.definitions.Source

object SourceMapping {
  implicit val sourceRequestMapping: Mapping[Source, SourceModel] = (a: Source) =>
    SourceModel(
      id = Some(Id(a.id)),
      name = Name(a.name),
      description = Description(a.description)
    )

  implicit val sourceResponseMapping: Mapping[SourceModel, Source] = (a: SourceModel) =>
    Source(
      id = a.id.map(_.value).getOrElse(-1),
      name = a.name.value,
      description = a.description.value
    )
}
