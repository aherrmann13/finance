package com.finance.service.converters

import com.finance.business.model.category.{
  Budget => BudgetModel,
  BudgetAmountSpent => BudgetAmountSpentModel,
  Category => CategoryModel,
  CategoryAmountSpent => CategoryAmountSpentModel
}
import com.finance.business.model.types.{Description, Id, Name, Usd, DateRange => DateRangeModel}
import com.finance.service.converters.CommonMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.definitions.{Budget, BudgetAmountSpent, Category, CategoryAmountSpent, DateRange}

object CategoryMapping {
  private implicit val budgetRequestMapping: Mapping[Budget, BudgetModel] = (a: Budget) =>
    BudgetModel(
      effectiveTime = a.effectiveTime.map(_.mapTo[DateRangeModel]),
      amount = Usd(a.amount)
    )

  private implicit val budgetResponseMapping: Mapping[BudgetModel, Budget] = (a: BudgetModel) =>
    Budget(
      effectiveTime = a.effectiveTime.map(_.mapTo[DateRange]).toVector,
      amount = a.amount.value
    )

  implicit val categoryRequestMapping: Mapping[Category, CategoryModel] = (a: Category) =>
    CategoryModel(
      id = Some(Id(a.id)),
      parentId = a.parentId.map(Id(_)),
      name = Name(a.name),
      description = Description(a.description),
      effectiveTime = a.effectiveTime.map(_.mapTo[DateRangeModel]),
      budget = a.budget.map(_.mapTo[BudgetModel])
    )

  implicit val categoryResponseMapping: Mapping[CategoryModel, Category] = (a: CategoryModel) =>
    Category(
      id = a.id.map(_.value).getOrElse(-1),
      parentId = a.parentId.map(_.value),
      name = a.name.value,
      description = a.description.value,
      effectiveTime = a.effectiveTime.map(_.mapTo[DateRange]).toVector,
      budget = a.budget.map(_.mapTo[Budget]).toVector
    )

  private implicit val nestedCategoryResponseMapping: Mapping[CategoryModel, CategoryAmountSpent.Category] =
    (a: CategoryModel) =>
      CategoryAmountSpent.Category(
        id = a.id.map(_.value).getOrElse(-1),
        parentId = a.parentId.map(_.value),
        name = a.name.value,
        description = a.description.value,
        effectiveTime = a.effectiveTime.map(_.mapTo[DateRange]).toVector,
        budget = a.budget.map(_.mapTo[Budget]).toVector
      )

  private implicit val budgetAmountSpentResponseMapping: Mapping[BudgetAmountSpentModel, BudgetAmountSpent] =
    (a: BudgetAmountSpentModel) =>
      BudgetAmountSpent(
        effectiveTime = a.effectiveTime.map(_.mapTo[DateRange]).toVector,
        in = a.in.value,
        out = a.out.value,
        amount = a.amount.value
      )

  implicit val categoryAmountSpentResponseMapping: Mapping[CategoryAmountSpentModel, CategoryAmountSpent] =
    (a: CategoryAmountSpentModel) =>
      CategoryAmountSpent(
        category = a.category.mapTo[CategoryAmountSpent.Category],
        budgetAmountSpent = a.spent.map(_.mapTo[BudgetAmountSpent]).toVector
      )
}
