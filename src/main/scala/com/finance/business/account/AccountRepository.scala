package com.finance.business.account

trait AccountRepository[F[_]] {
  def create(account: Account): F[Account]

  def update(account: Account) : F[Account]

  def delete(userId: Int, id: Int) : F[Unit]

  def get(userId: Int, id: Int): F[Option[Account]]

  def getMany(userId: Int, id: Seq[Int]): F[Seq[Account]]

  def getAll(userId: Int): F[Seq[Account]]
}
