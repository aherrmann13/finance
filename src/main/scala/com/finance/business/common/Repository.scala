package com.finance.business.common

trait Repository[F[_], Item] {
  def create(item: Item): F[Item]

  def update(item: Item) : F[Item]

  def delete(userId: Int, id: Int) : F[Unit]

  def get(userId: Int, id: Int): F[Option[Item]]

  def getMany(userId: Int, id: Seq[Int]): F[Seq[Item]]

  def getAll(userId: Int): F[Seq[Item]]
}