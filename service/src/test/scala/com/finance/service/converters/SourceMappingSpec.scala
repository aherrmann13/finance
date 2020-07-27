package com.finance.service.converters

import com.finance.business.model.source.{Source => SourceModel}
import com.finance.business.model.types.{Description, Id, Name}
import com.finance.service.converters.Mapping._
import com.finance.service.converters.SourceMapping._
import com.finance.service.endpoints.definitions.Source
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SourceMappingSpec extends AnyFreeSpec with Matchers {

  "SourceMapping" - {
    "should contain implicits that" - {
      "map source request to source model" in {
        Source(
          id = 2,
          name = "name",
          description = "desc"
        ).mapTo[SourceModel] shouldEqual SourceModel(
          id = Some(Id(2)),
          name = Name("name"),
          description = Description("desc")
        )
      }

      "map source model to source response" in {
        Source(
          id = 2,
          name = "name",
          description = "desc"
        ).mapTo[SourceModel] shouldEqual SourceModel(
          id = Some(Id(2)),
          name = Name("name"),
          description = Description("desc")
        )
      }
    }
  }
}
