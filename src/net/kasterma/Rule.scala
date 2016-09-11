package net.kasterma

/**
  * Created by kasterma on 11/09/16.
  */

trait Rule {
  val TOTAL_NOS = 100

  def size: Int
  def prob(x: Int): Double
}
