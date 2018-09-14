package com.kinigitbyday.relagen
import com.kinigitbyday.relagen.model.{Relagen, Relation}
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}

object RelagenSpec {
  def businessOwnerGen: Gen[BusinessOwner] = for {
    businessId <- Gen.alphaNumStr
    ownerId <- Gen.alphaNumStr
    id <- Gen.alphaNumStr
  } yield BusinessOwner(businessId, ownerId, id)

  def businessGen: Gen[Business] = for {
    id <- Gen.alphaNumStr
    name <- Gen.alphaNumStr
    size <- Gen.choose(1, 100)
  } yield Business(id, name, size)

  def personGen: Gen[Person] = for {
    id <- Gen.alphaNumStr
    name <- Gen.alphaNumStr
    taxInfoId <- Gen.alphaNumStr
  } yield Person(id, name, taxInfoId)

  def taxInfoGen: Gen[TaxInfo] = for {
    id <- Gen.alphaNumStr
    whatever <- Gen.numStr
  } yield TaxInfo(id, whatever)
}

class RelagenSpec extends FlatSpec with Matchers {
  import RelagenSpec._

  "Relagen" should "generate the correct number of non-optional relations" in {
    val generatedData = sampleRelagen.generate(5)

    generatedData.count(_.isInstanceOf[BusinessOwner]) shouldEqual 5
    generatedData.count(_.isInstanceOf[Business]) shouldEqual 5
    generatedData.count(_.isInstanceOf[Person]) shouldEqual 5
    generatedData.count(_.isInstanceOf[TaxInfo]) shouldEqual 5
  }

  it should "generate data with actual relations" in {
    val generatedData = sampleRelagen.generate(5)
    val businessOwners = generatedData.filter(_.isInstanceOf[BusinessOwner]).map(_.asInstanceOf[BusinessOwner])
    businessOwners.forall(businessOwner => {
      val business = generatedData.filter(_.isInstanceOf[Business]).map(_.asInstanceOf[Business]).
        find(_.id == businessOwner.business)

      val person = generatedData.filter(_.isInstanceOf[Person]).map(_.asInstanceOf[Person]).
        find(_.id == businessOwner.owner)

      val taxInfo = person.flatMap(person =>
        generatedData.filter(_.isInstanceOf[TaxInfo]).map(_.asInstanceOf[TaxInfo]).
          find(_.id == person.taxInfo)
      )

      business.isDefined && person.isDefined && taxInfo.isDefined
    }) shouldBe true
  }

  def sampleRelagen = {
    val b = Relagen(businessGen)
    val t = Relagen(taxInfoGen)
    val p = Relagen(personGen).withRelation[TaxInfo](
      t,
      s => t.dataGen.sample.get.copy(id = s.taxInfo)
    )
    Relagen(businessOwnerGen).
      withRelation[Business](
      b,
      s => b.dataGen.sample.get.copy(id = s.business)
    ).
      withRelation[Person](
      p,
      s => p.dataGen.sample.get.copy(id = s.owner)
    )
  }
}

case class Person(
  id: String,
  name: String,
  taxInfo: String
)
case class TaxInfo(
  id: String,
  whatever: String
)
case class Business(
  id: String,
  name: String,
  size: Int
)
case class BusinessOwner(
  business: String,
  owner: String,
  id: String
)
