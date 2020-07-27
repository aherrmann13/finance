package com.finance.service.handlers

import cats.data.EitherT
import cats.{Id => IdMonad}
import com.finance.business.model.source.{Source => SourceModel}
import com.finance.business.model.types.{Description, Id, Name}
import com.finance.business.repository.SourceRepository
import com.finance.business.services.SourceService
import com.finance.business.validation.SourceValidationAlgebra
import com.finance.business.validation.errors.ValidationError
import com.finance.service.endpoints.definitions.{Error, Source, SourceQuery}
import com.finance.service.endpoints.source.{
  CreateSourceResponse,
  DeleteSourceResponse,
  GetSourcesResponse,
  UpdateSourceResponse
}
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SourceHandlerImplSpec extends AnyFreeSpec with Matchers with MockFactory {
  private case object TestError extends ValidationError

  private class SourceServiceTest
      extends SourceService[IdMonad](stub[SourceValidationAlgebra[IdMonad]], stub[SourceRepository[IdMonad]])

  private val mockSourceService = stub[SourceServiceTest]

  private val handler = new SourceHandlerImpl(mockSourceService)

  private val source = Source(5, "name", "desc")
  private val sourceModel = SourceModel(Some(Id(5)), Name("name"), Description("desc"))

  "SourceHandlerImpl" - {
    "createSource" - {
      "should return CreateSourceResponse.BadRequest on error in service" in {
        (mockSourceService.create _).when(sourceModel.copy(id = None)).returns(EitherT.leftT(TestError))

        handler.createSource(CreateSourceResponse)(Some(source)) shouldEqual
          CreateSourceResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return CreateSourceResponse.BadRequest on null body" in {
        handler.createSource(CreateSourceResponse)(None) shouldEqual
          CreateSourceResponse.BadRequest(Error(Some("body is null")))
      }
      "should return CreateSourceResponse.Ok with account on successful create in service" in {
        (mockSourceService.create _).when(sourceModel.copy(id = None)).returns(EitherT.rightT(sourceModel))

        handler.createSource(CreateSourceResponse)(Some(source)) shouldEqual
          CreateSourceResponse.Ok(source)
      }
    }

    "updateSource" - {
      val id = 6
      "should return UpdateSourceResponse.BadRequest on error in service" in {
        (mockSourceService.update _).when(sourceModel.copy(id = Some(Id(id)))).returns(EitherT.leftT(TestError))

        handler.updateSource(UpdateSourceResponse)(id, Some(source)) shouldEqual
          UpdateSourceResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return UpdateSourceResponse.BadRequest on null body" in {
        handler.updateSource(UpdateSourceResponse)(id, None) shouldEqual
          UpdateSourceResponse.BadRequest(Error(Some("body is null")))
      }
      "should return UpdateSourceResponse.Ok with account on successful update in service" in {
        (mockSourceService.update _).when(sourceModel.copy(id = Some(Id(id)))).returns(EitherT.rightT(sourceModel))

        handler.updateSource(UpdateSourceResponse)(id, Some(source)) shouldEqual
          UpdateSourceResponse.Ok(source)
      }
    }

    "deleteSource" - {
      val id = 1
      "should return DeleteSourceResponse.BadRequest on error in service" in {
        (mockSourceService.delete _).when(Id(id)).returns(EitherT.leftT(TestError))

        handler.deleteSource(DeleteSourceResponse)(id) shouldEqual
          DeleteSourceResponse.BadRequest(Error(Some("unknown error occurred")))
      }
      "should return DeleteSourceResponse.Ok on successful delete" in {
        (mockSourceService.delete _).when(Id(id)).returns(EitherT.rightT(()))

        handler.deleteSource(DeleteSourceResponse)(id) shouldEqual
          DeleteSourceResponse.Ok
      }
    }

    "getSources" - {
      val query = SourceQuery(Some("search"))
      "should return GetSourcesResponse.Ok with sources from service" in {
        (mockSourceService.get(_: String)).when(query.search.get).returns(Seq(sourceModel, sourceModel))

        handler.getSources(GetSourcesResponse)(Some(query)) shouldEqual
          GetSourcesResponse.Ok(Vector(source, source))
      }

      "should return GetSourcesResponse.Ok with all sources when no filter provided" in {
        (mockSourceService.get(_: String)).when("").returns(Seq(sourceModel, sourceModel, sourceModel))

        handler.getSources(GetSourcesResponse)(None) shouldEqual
          GetSourcesResponse.Ok(Vector(source, source, source))
      }

      "should return GetSourcesResponse.Ok with all sources when no search string provided" in {
        (mockSourceService.get(_: String)).when("").returns(Seq(sourceModel, sourceModel, sourceModel))

        handler.getSources(GetSourcesResponse)(Some(query.copy(search = None))) shouldEqual
          GetSourcesResponse.Ok(Vector(source, source, source))
      }
    }
  }
}
