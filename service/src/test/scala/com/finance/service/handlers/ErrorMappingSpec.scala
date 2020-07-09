package com.finance.service.handlers

import com.finance.business.validation.errors.ValidationError
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.Error
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ErrorMappingSpec extends AnyFreeSpec with Matchers {
  private case object TestErr extends ValidationError
  "ErrorMapping" - {
    "should contain implicits that" - {
      "map NullBodyError to response Error" in {
        NullBodyError.mapTo[Error] shouldEqual Error(Some("body is null"))
      }
      "map unknown ValidationError to response Error" in {
        TestErr.mapTo[Error] shouldEqual Error(Some("unknown error occurred"))
      }
    }
  }
}
