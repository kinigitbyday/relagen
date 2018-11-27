package com.kinigitbyday.relagen.gen.implicits

object RelagenImplicits {

  implicit class ProductListWithTypedCollect(productList: List[Product]) {

    def allOfType[T <: Product : Manifest]: List[T] = productList.collect {case x: T => x}
  }

}
