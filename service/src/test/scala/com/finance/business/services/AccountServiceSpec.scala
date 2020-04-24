package com.finance.business.services

import cats.data.EitherT
import cats.{Id => IdMonad}
import cats.implicits._
import com.finance.business.model.account.{Account, Bank}
import com.finance.business.model.types.{Description, Id, ModelName, Name}
import com.finance.business.repository.AccountRepository
import com.finance.business.validation.AccountValidationAlgebra
import com.finance.business.validation.errors._
import org.scalamock.scalatest.MockFactory
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AccountServiceSpec extends AnyFreeSpec with Matchers with MockFactory {
  private val mockValidationAlgebra = stub[AccountValidationAlgebra[IdMonad]]
  private val mockRepository = mock[AccountRepository[IdMonad]]

  private val service = new AccountService[IdMonad](mockValidationAlgebra, mockRepository)

  private val accountId = Id(4)
  private val account = Account(Some(accountId), Name("Name"), Description("Description"), Bank)

  "create" - {
    "returns Left(IdMustBeNone) from validation algebra idIsNone" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](IdMustBeNone(ModelName("Account")))
      (mockValidationAlgebra idIsNone _) when account returns returnVal
      (mockRepository create _) expects account never

      service.create(account) shouldEqual returnVal
    }
    "returns Left(NameTooLong) from validation algebra nameIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Account"), Name("Name")))
      (mockValidationAlgebra idIsNone _) when account returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra nameIsValid _) when account returns returnVal
      (mockRepository create _) expects account never

      service.create(account) shouldEqual returnVal
    }
    "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Account"), Description("Desc")))
      (mockValidationAlgebra idIsNone _) when account returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra nameIsValid _) when account returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when account returns returnVal
      (mockRepository create _) expects account never

      service.create(account) shouldEqual returnVal
    }
    "returns Right(()) and saves model when validation passes" in {
      (mockValidationAlgebra idIsNone _) when account returns EitherT.rightT[IdMonad, IdMustBeNone](())
      (mockValidationAlgebra nameIsValid _) when account returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when account returns EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockRepository create _) expects account returns account.pure[IdMonad]

      service.create(account) shouldEqual EitherT.rightT[IdMonad, ValidationError](account)
    }
  }
  "update" - {
    "returns Left(DoesNotExist) from validation algebra exists" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DoesNotExist(ModelName("Account")))
      (mockValidationAlgebra exists _) when account returns returnVal
      (mockRepository update _) expects account never

      service.update(account) shouldEqual returnVal
    }
    "returns Left(NameTooLong) from validation algebra nameIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](NameTooLong(ModelName("Account"), Name("Name")))
      (mockValidationAlgebra exists _) when account returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra nameIsValid _) when account returns returnVal
      (mockRepository update _) expects account never

      service.update(account) shouldEqual returnVal
    }
    "returns Left(DescriptionTooLong) from validation algebra descriptionIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](DescriptionTooLong(ModelName("Account"), Description("Desc")))
      (mockValidationAlgebra exists _) when account returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra nameIsValid _) when account returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when account returns returnVal
      (mockRepository update _) expects account never

      service.update(account) shouldEqual returnVal
    }
    "returns Left(AccountTypeInvalid) from validation algebra accountTypeIsValid" in {
      val returnVal = EitherT.leftT[IdMonad, Unit][AccountTypeInvalid](BankCantHaveAssets)
      (mockValidationAlgebra exists _) when account returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra nameIsValid _) when account returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when account returns EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockValidationAlgebra accountTypeIsValid _) when account returns returnVal
      (mockRepository update _) expects account never

      service.update(account) shouldEqual returnVal
    }
    "returns Right(()) and updates model when validation passes" in {
      (mockValidationAlgebra exists _) when account returns EitherT.rightT[IdMonad, DoesNotExist](())
      (mockValidationAlgebra nameIsValid _) when account returns EitherT.rightT[IdMonad, NameTooLong](())
      (mockValidationAlgebra descriptionIsValid _) when account returns EitherT.rightT[IdMonad, DescriptionTooLong](())
      (mockValidationAlgebra accountTypeIsValid _) when account returns EitherT.rightT[IdMonad, AccountTypeInvalid](())
      (mockRepository update _) expects account returns account.pure[IdMonad]

      service.update(account) shouldEqual EitherT.rightT[IdMonad, ValidationError](account)
    }
  }
  "delete" - {
    "returns Left(HasTransactions) from validation algebra hasNoTransactions" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](HasTransactions(ModelName("Account")))
      (mockValidationAlgebra hasNoTransactions _) when accountId returns returnVal
      (mockRepository delete _) expects accountId never

      service.delete(accountId) shouldEqual returnVal
    }
    "returns Left(HasAssets) from validation algebra hasNoAssets" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](HasAssets(ModelName("Account")))
      (mockValidationAlgebra hasNoTransactions _) when accountId returns EitherT.rightT[IdMonad, HasTransactions](())
      (mockValidationAlgebra hasNoAssets _) when accountId returns returnVal
      (mockRepository delete _) expects accountId never

      service.delete(accountId) shouldEqual returnVal
    }
    "returns Left(HasPaybacks) from validation algebra hasNoPaybacks" in {
      val returnVal = EitherT.leftT[IdMonad, Unit](HasPaybacks(ModelName("Account")))
      (mockValidationAlgebra hasNoTransactions _) when accountId returns EitherT.rightT[IdMonad, HasTransactions](())
      (mockValidationAlgebra hasNoAssets _) when accountId returns EitherT.rightT[IdMonad, HasAssets](())
      (mockValidationAlgebra hasNoPaybacks _) when accountId returns returnVal
      (mockRepository delete _) expects accountId never

      service.delete(accountId) shouldEqual returnVal
    }
    "returns Right(()) and deletes when validation passes" in {
      (mockValidationAlgebra hasNoTransactions _) when accountId returns EitherT.rightT[IdMonad, HasTransactions](())
      (mockValidationAlgebra hasNoAssets _) when accountId returns EitherT.rightT[IdMonad, HasAssets](())
      (mockValidationAlgebra hasNoPaybacks _) when accountId returns EitherT.rightT[IdMonad, HasPaybacks](())
      (mockRepository delete _) expects accountId returns ().pure[IdMonad]

      service.delete(accountId) shouldEqual EitherT.rightT[IdMonad, ValidationError](())
    }
  }
  "get" - {
    "returns repository get" in {
      (mockRepository get _) expects accountId returns Some(account).pure[IdMonad]

      service.get(accountId) shouldEqual Some(account)
    }
  }
  "getMany" - {
    "returns repository getMany" in {
      (mockRepository getMany _) expects Seq(accountId, Id(accountId.value + 1)) returns
        Seq(account, account).pure[IdMonad]

      service.getMany(Seq(accountId, Id(accountId.value + 1))) shouldEqual Seq(account, account)
    }
  }
  "getAll" - {
    "returns repository getAll" - {
      (mockRepository.getAll _).expects().returns(Seq(account, account).pure[IdMonad])

      service.getAll shouldEqual Seq(account, account)
    }
  }
}
