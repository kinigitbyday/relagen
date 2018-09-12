package com.kinigitbyday.relagen.model
import org.scalacheck.Gen

case class Relagen[T <: Product](
  dataGen: Gen[T],
  relations: List[Relation[T, _ <: Product]] = List.empty
) {

  def withRelation[D <: Product](entity: Relagen[D], optional: Boolean, keyFunction: T => D): Relagen[T] = {
    this.copy(relations = relations ++ List(Relation(this, entity, optional, keyFunction)))
  }

  def generate: List[Product] = {
    dataGen.sample.map(data => {
      List(data) ++ relations.flatMap(_.generate(data))
    }).get
  }
}

case class Relation[S <: Product, D <: Product](
  sourceEntity: Relagen[S],
  destinationEntity: Relagen[D],
  optional: Boolean,
  keyFunction: S => D
) {

  def generate(data: S): List[Product] = {
    val destinationData = keyFunction(data)
    List(destinationData) ++ destinationEntity.relations.flatMap(_.generate(destinationData))
  }
}

