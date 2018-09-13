package com.kinigitbyday.relagen.model
import org.scalacheck.Gen

case class Relagen[T <: Product](
  dataGen: Gen[T],
  relations: List[Relation[T, _ <: Product]] = List.empty
) {

  def withRelation[D <: Product](entity: Relagen[D], keyFunction: T => D, optional: Boolean = false): Relagen[T] = {
    this.copy(relations = relations ++ List(Relation(this, entity, keyFunction, optional)))
  }

  def generate(n: Int = 10): List[Product] = {
    (0 to n).toList.map(_ => dataGen.sample.get).
      flatMap(data => List(data) ++ relations.flatMap(_.generate(data)))
  }
}

case class Relation[S <: Product, D <: Product](
  sourceEntity: Relagen[S],
  destinationEntity: Relagen[D],
  keyFunction: S => D,
  optional: Boolean = false
) {

  def generate(data: S): List[Product] = {
    if (!optional || math.random > 0.5) {
      val destinationData = keyFunction(data)
      List(destinationData) ++ destinationEntity.relations.flatMap(_.generate(destinationData))
    } else {
      List.empty
    }
  }
}

