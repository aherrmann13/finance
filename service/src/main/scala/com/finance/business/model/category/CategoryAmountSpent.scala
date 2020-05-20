package com.finance.business.model.category

case class CategoryAmountSpent(category: Category, spent: Seq[BudgetAmountSpent])
