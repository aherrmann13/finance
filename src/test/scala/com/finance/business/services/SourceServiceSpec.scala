package com.finance.business.services

import cats.Monad
import cats.data.EitherT
import cats.effect.IO
import com.finance.business.common.{IdRepository, RelationValidator}
import com.finance.business.errors._
import com.finance.business.model.source._
import com.finance.business.model.transaction.TransactionRepository
import com.finance.business.validators.SourceValidator
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}

class SourceServiceSpec extends FreeSpec with Matchers with MockFactory {
  private val repository = stub[SourceRepository[IO]]

  //https://github.com/paulbutcher/ScalaMock/issues/170
  class SourceValidatorWithIO(sourceRepository: SourceRepository[IO], transactionRepository: TransactionRepository[IO])
      extends SourceValidator[IO](sourceRepository, transactionRepository)

  class RelationValidatorWithIO(repository: IdRepository[IO]) extends RelationValidator[IO](repository)

  private val validator = mock[SourceValidatorWithIO]
  private val relationValidator = mock[RelationValidatorWithIO]

  private val service = SourceService(repository, validator, relationValidator)

  val fakeSource = Source(Option(1), 2, "name", "description")

  "Source service" - {
    "create" - {
      "should return Left(BusinessError) error on invalid properties" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.leftT[IO, Unit](NameMustBeDefinedError))

        val result = service.create(fakeSource)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(BusinessError) error when source exists" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Source))
          .expects(fakeSource)
          .returning(EitherT.leftT[IO, Unit](SourceAlreadyExistsError))

        val result = service.create(fakeSource)

        result.value.unsafeRunSync shouldBe Left(SourceAlreadyExistsError)
      }
      "should return Left(UserDoesNotExistError) when user does not exist" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Source))
          .expects(fakeSource)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeSource)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.create(fakeSource)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }
      "should return Right(Source) on success" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .doesNotExist(_: Source))
          .expects(fakeSource)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeSource)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.create _ when fakeSource).returns(IO(fakeSource))

        val result = service.create(fakeSource)

        result.value.unsafeRunSync shouldBe Right(fakeSource)
      }
    }
    "update" - {
      "should return Left(BusinessError) error on invalid properties" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.leftT[IO, Unit](NameMustBeDefinedError))

        val result = service.update(fakeSource)

        result.value.unsafeRunSync shouldBe Left(NameMustBeDefinedError)
      }
      "should return Left(BusinessError) error when source does not exist" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Source))
          .expects(fakeSource)
          .returning(EitherT.leftT[IO, Unit](SourceDoesNotExistError))

        val result = service.update(fakeSource)

        result.value.unsafeRunSync shouldBe Left(SourceDoesNotExistError)
      }
      "should return Left(UserDoesNotExistError) when user does not exist" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Source))
          .expects(fakeSource)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeSource)
          .returning(EitherT.leftT[IO, Unit](UserDoesNotExistError))

        val result = service.update(fakeSource)

        result.value.unsafeRunSync shouldBe Left(UserDoesNotExistError)
      }
      "should return Right(Source) on success" in {
        (validator
          .propertiesAreValid(_: Source)(_: Monad[IO]))
          .expects(fakeSource, *)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (validator
          .exists(_: Source))
          .expects(fakeSource)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (relationValidator.userExists _)
          .expects(fakeSource)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.update _ when fakeSource).returns(IO(fakeSource))

        val result = service.update(fakeSource)

        result.value.unsafeRunSync shouldBe Right(fakeSource)
      }
    }
    "delete" - {
      "should return Left(ReferencedByTransactionError) if transaction exists" in {
        (validator
          .hasTransactions(_: Source))
          .expects(fakeSource)
          .returning(EitherT.leftT[IO, Unit](ReferencedByTransactionError))

        val result = service.delete(fakeSource)

        (repository.delete _).verify(*, *) never

        result.value.unsafeRunSync shouldBe Left(ReferencedByTransactionError)
      }
      "should return Right(()) and delete source on success" in {
        (validator
          .hasTransactions(_: Source))
          .expects(fakeSource)
          .returning(EitherT.rightT[IO, BusinessError](()))

        (repository.delete _).when(fakeSource.userId, fakeSource.id.get).returns(IO(()))

        val result = service.delete(fakeSource)

        result.value.unsafeRunSync shouldBe Right(())
      }

      "should return Right(()) and skip delete when source has no id" in {
        val result = service.delete(fakeSource.copy(id = None))

        result.value.unsafeRunSync shouldBe Right(())

        (repository.delete _).verify(*, *) never
      }
    }
    "get" - {
      "should call get on repository" in {
        val userId = 5
        val id = 3
        service.get(userId, id)

        (repository.get _).verify(userId, id)
      }

      "should return results from repository" in {
        (repository.get _).when(fakeSource.userId, fakeSource.id.get).returns(IO(Some(fakeSource)))

        val result = service.get(fakeSource.userId, fakeSource.id.get)
        result.unsafeRunSync shouldBe Option.apply(fakeSource)
      }
    }
    "getMany" - {
      "should call getMany on repository" in {
        val userId = 4
        val ids = Seq(1, 2, 3, 4)
        service.getMany(userId, ids)

        (repository.getMany _).verify(userId, ids)
      }

      "should return results from repository" in {
        val sources = Seq(fakeSource, fakeSource.copy(id = Option.apply(5)))
        val ids = sources.flatMap(_.id)
        (repository.getMany _).when(fakeSource.userId, ids).returns(IO(sources))

        val result = service.getMany(fakeSource.userId, ids)
        result.unsafeRunSync shouldBe sources
      }
    }
    "getAll" - {
      "should call getAll on repository" in {
        val userId = 4
        service.getAll(userId)

        (repository.getAll _).verify(userId)
      }

      "should return results from repository" in {
        val sources = Seq(fakeSource, fakeSource.copy(id = Option.apply(5)))
        (repository.getAll _).when(fakeSource.userId).returns(IO(sources))

        val result = service.getAll(fakeSource.userId)
        result.unsafeRunSync shouldBe sources
      }
    }
  }
}
