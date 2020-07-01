package com.finance.service.handlers

import cats.Monad
import cats.implicits._
import com.finance.business.model.account.{Account => AccountModel}
import com.finance.business.model.types.Id
import com.finance.business.services.AccountService
import com.finance.service.converters.AccountMapping._
import com.finance.service.converters.ErrorMapping._
import com.finance.service.converters.Mapping._
import com.finance.service.endpoints.account._
import com.finance.service.endpoints.definitions.{Account, Error}

class AccountHandlerImpl[F[_] : Monad](
  accountService: AccountService[F]
) extends AccountHandler[F] {
  override def createAccount(respond: CreateAccountResponse.type)(body: Option[Account]): F[CreateAccountResponse] =
    body.map { b =>
      accountService.create(b.mapTo[AccountModel].copy(id = None)).fold[CreateAccountResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        r => respond.Ok(r.mapTo[Account])
      )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[CreateAccountResponse])

  override def updateAccount(respond: UpdateAccountResponse.type)
    (id: Int, body: Option[Account]): F[UpdateAccountResponse] =
    body.map { b =>
      accountService.update(b.mapTo[AccountModel].copy(id = Some(Id(id)))).fold[UpdateAccountResponse](
        e => respond.BadRequest(e.mapTo[Error]),
        r => respond.Ok(r.mapTo[Account])
      )
    }.getOrElse(respond.BadRequest(NullBodyError.mapTo[Error]).pure[F].widen[UpdateAccountResponse])

  override def deleteAccount(respond: DeleteAccountResponse.type)(id: Int): F[DeleteAccountResponse] =
    accountService.delete(Id(id)).fold[DeleteAccountResponse](
      e => respond.BadRequest(e.mapTo[Error]),
      _ => respond.Ok
    )

  override def getAllAccounts(respond: GetAllAccountsResponse.type)(): F[GetAllAccountsResponse] =
    accountService.getAll.map { accounts =>
      GetAllAccountsResponse.Ok(accounts.map(_.mapTo[Account]).toVector)
    }
}
