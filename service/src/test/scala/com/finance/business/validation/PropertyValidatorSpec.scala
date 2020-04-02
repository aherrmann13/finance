package com.finance.business.validation

import cats.{Id => IdMonad}
import cats.data.EitherT
import cats.implicits._
import com.finance.business.model.types.{Description, HasDescription, HasId, HasName, Id, ModelName, Name, NamedModel}
import com.finance.business.validation.PropertyValidator._
import com.finance.business.validation.errors._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class PropertyValidatorSpec extends AnyFreeSpec with Matchers {

  private case class TestModel(id: Option[Id], name: Name, description: Description)

  private val modelName = ModelName("testModel")
  private val fakeModel = TestModel(Some(Id(2)), Name("name"), Description("desc"))

  private implicit val hasId: HasId[TestModel] = (target: TestModel) => target.id
  private implicit val hasName: HasName[TestModel] = (target: TestModel) => target.name
  private implicit val hasDescription: HasDescription[TestModel] = (target: TestModel) => target.description
  private implicit val hasModelName: NamedModel[TestModel] = _ => ModelName("testModel")

  "idIsNone" - {
    "should return Left(IdMustBeNone) when id is Some" in {
      idIsNone[IdMonad, TestModel](fakeModel).value shouldEqual
        EitherT.leftT[IdMonad, Unit](IdMustBeNone(modelName)).value
    }
    "should return Right(()) when id is None" in {
      idIsNone[IdMonad, TestModel](fakeModel.copy(id = None)).value shouldEqual
        EitherT.rightT[IdMonad, IdMustBeNone](()).value
    }
  }

  "exists" - {
    "should return Left(DoesNotExist) when id is None" in {
      exists[IdMonad, TestModel, TestModel](
        fakeModel.copy(id = None), _ => Some(fakeModel).pure[IdMonad]
      ).value shouldEqual EitherT.leftT[IdMonad, Unit](DoesNotExist(modelName)).value
    }
    "should return Left(DoesNotExist) when exists func returns F[None]" in {
      val id = Some(Id(1))
      exists[IdMonad, TestModel, TestModel](fakeModel.copy(id = id), _ => None.pure[IdMonad]).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DoesNotExist(modelName, id)).value
    }
    "should return Right(()) when exists func returns F[Some]" in {
      exists[IdMonad, TestModel, TestModel](
        fakeModel.copy(id = Some(Id(1))), _ => Some(fakeModel).pure[IdMonad]
      ).value shouldEqual EitherT.rightT[IdMonad, DoesNotExist](()).value
    }
  }

  "nameIsValid" - {
    "should return Left(NameTooLong) when name is too long" in {
      val name = Name((0 to 128).map(_ => "a").fold("")(_ + _))
      nameIsValid[IdMonad, TestModel](fakeModel.copy(name = name)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](NameTooLong(modelName, name)).value
    }
    "should return Right(()) when name is correct length" in {
      val name = Name((0 to 127).map(_ => "a").fold("")(_ + _))
      nameIsValid[IdMonad, TestModel](fakeModel.copy(name = name)).value shouldEqual
        EitherT.rightT[IdMonad, NameTooLong](()).value
    }
  }

  "descriptionIsValid" - {
    "should return Left(DescriptionTooLong) when description is too long" in {
      val desc = Description((0 to 512).map(_ => "a").fold("")(_ + _))
      descriptionIsValid[IdMonad, TestModel](fakeModel.copy(description = desc)).value shouldEqual
        EitherT.leftT[IdMonad, Unit](DescriptionTooLong(modelName, desc)).value
    }
    "should return Right(()) when description is correct length" in {
      val desc = Description((0 to 511).map(_ => "a").fold("")(_ + _))
      descriptionIsValid[IdMonad, TestModel](fakeModel.copy(description = desc)).value shouldEqual
        EitherT.rightT[IdMonad, DescriptionTooLong](()).value
    }
  }
}
