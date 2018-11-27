package com.kinigitbyday.relagen

import com.kinigitbyday.relagen.gen.{Relagen, RelationType}
import com.kinigitbyday.relagen.gen.implicits.RelagenImplicits._
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
    val businessOwners = generatedData.allOfType[BusinessOwner]
    businessOwners.forall(businessOwner => {
      val business = generatedData.allOfType[Business].find(_.id == businessOwner.business)

      val person = generatedData.allOfType[Person].find(_.id == businessOwner.owner)

      val taxInfo = person.flatMap(person =>
        generatedData.allOfType[TaxInfo].find(_.id == person.taxInfo)
      )

      business.isDefined && person.isDefined && taxInfo.isDefined
    }) shouldBe true
  }

  it should "handle one to many relations" in {
    val generatedData = Relagen(businessOwnerGen).withRelation[Business](
      Relagen(businessGen),
      (s, b) => b.copy(id = s.business),
      relationType = RelationType.OneToMany
    ).generate(5)
    val businessOwners = generatedData.allOfType[BusinessOwner]
    businessOwners.forall(businessOwner => {
      val businesses = generatedData.allOfType[Business].filter(_.id == businessOwner.business)
      businesses.nonEmpty
    })
    businessOwners.exists(businessOwner => {
      val businesses = generatedData.allOfType[Business].filter(_.id == businessOwner.business)
      businesses.size >= 2
    })
  }

  def sampleRelagen = {
    val business = Relagen(businessGen)
    val taxInfo = Relagen(taxInfoGen)
    val person = Relagen(personGen).withRelation[TaxInfo](
      taxInfo,
      (s, t) => t.copy(id = s.taxInfo)
    )
    Relagen(businessOwnerGen).withRelation[Business](
      business,
      (s, b) => b.copy(id = s.business)
    ).withRelation[Person](
      person,
      (s, p) => p.copy(id = s.owner)
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
