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

  "Relagen" should "work" in {
    val b = Relagen(businessGen)
    val t = Relagen(taxInfoGen)
    val p = Relagen(personGen).withRelation[TaxInfo](
      t,
      optional = false,
      s => t.dataGen.sample.get.copy(id = s.taxInfo)
    )
    val something = Relagen(businessOwnerGen).
      withRelation[Business](
        b,
        optional = false,
        s => b.dataGen.sample.get.copy(id = s.business)
      ).
      withRelation[Person](
        p,
        optional = false,
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
