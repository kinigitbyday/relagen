package com.kinigitbyday.relagen.gen

import org.scalacheck.Gen

case class Relagen[T <: Product](
  dataGen: Gen[T],
  relations: List[Relation[T, _ <: Product]] = List.empty
) {

  def withRelation[D <: Product](
    entity: Relagen[D],
    keyFunction: (T, D) => D,
    optional: Boolean = false,
    relationType: RelationType = RelationType.OneToOne
  ): Relagen[T] = {
    this.copy(relations = relations ++ List(Relation(this, entity, keyFunction, optional, relationType)))
  }

  def generate(n: Int = 10): List[Product] = {
    (1 to n).toList.map(_ => dataGen.sample.get).
      flatMap(data => List(data) ++ relations.flatMap(_.generate(data)))
  }
}

case class Relation[S <: Product, D <: Product](
  sourceEntity: Relagen[S],
  destinationEntity: Relagen[D],
  keyFunction: (S, D) => D,
  optional: Boolean,
  relationType: RelationType
) {

  def generate(data: S): List[Product] = {
    if (!optional || math.random > 0.5) {
      val destinationData = relationType match {
        case RelationType.OneToOne => List(keyFunction(data, destinationEntity.dataGen.sample.get))
        case RelationType.OneToMany => Gen.nonEmptyListOf(destinationEntity.dataGen).sample.get.map(keyFunction(data, _))
      }
      destinationData ++ destinationData.flatMap(d => destinationEntity.relations.flatMap(_.generate(d)))
    } else {
      List.empty
    }
  }
}

