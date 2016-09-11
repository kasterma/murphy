package net.kasterma

/**
  * Created by kasterma on 11/09/16.
  */

trait Rule {
  val TOTAL_NOS = 100

  def size: Int
  def prob(x: Int): Double
  def prob(xs: Int*): Double = prob(xs.toList)
  def prob(xs: List[Int]): Double = xs map prob product
}
