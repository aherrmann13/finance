package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.model.category.{Category => CategoryModel}
import com.finance.business.model.types.{Id, DateRange => DateRangeModel}
import com.finance.business.services.CategoryService
import com.finance.service.converters.CategoryMapping._
import com.finance.service.converters.CommonMapping._
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.category._
import com.finance.service.endpoints.definitions.{
  AmountSpentInRange,
  AmountSpentInRangeQuery,
  Category,
  CategoryAmountSpent,
  Error
}

class CategoryHandlerImpl[F[_]: Monad](
  categoryService: CategoryService[F]
) extends CategoryHandler[F] {
  override def createCategory(respond: CreateCategoryResponse.type)(body: Option[Category]): F[CreateCategoryResponse] =
    body.map { b =>
      categoryService
        .create(b.mapTo[CategoryModel].copy(id = None))
        .fold[CreateCategoryResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Category])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[CreateCategoryResponse])

  override def updateCategory(
    respond: UpdateCategoryResponse.type
  )(id: Int, body: Option[Category]): F[UpdateCategoryResponse] =
    body.map { b =>
      categoryService
        .update(b.mapTo[CategoryModel].copy(id = Some(Id(id))))
        .fold[UpdateCategoryResponse](
          e => respond.BadRequest(e.mapTo[Error]),
          r => respond.Ok(r.mapTo[Category])
        )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[UpdateCategoryResponse])

  override def deleteCategory(respond: DeleteCategoryResponse.type)(id: Int): F[DeleteCategoryResponse] =
    categoryService
      .delete(Id(id))
      .fold[DeleteCategoryResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        _ => respond.Ok
      )

  override def getAllCategories(respond: GetAllCategoriesResponse.type)(): F[GetAllCategoriesResponse] =
    categoryService.getAll.map { categories =>
      respond.Ok(categories.map(_.mapTo[Category]).toVector)
    }

  override def getAmountSpentInRange(respond: GetAmountSpentInRangeResponse.type)(
    query: AmountSpentInRangeQuery
  ): F[GetAmountSpentInRangeResponse] =
    categoryService.getAmountSpentInRange(query.range.mapTo[DateRangeModel]).map { amounts =>
      respond.Ok(
        AmountSpentInRange(
          amounts.map(_.mapTo[CategoryAmountSpent]).toVector,
          query.range.mapTo[DateRangeModel].mapTo[AmountSpentInRange.Range]
        )
      )
    }
}
