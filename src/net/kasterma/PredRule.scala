package net.kasterma

import scala.language.postfixOps

/**
  * Created by kasterma on 11/09/16.
  */
class PredRule(val pred: Int => Boolean, val name: String) extends Rule {
  val _members: List[Int] = 1 to TOTAL_NOS filter pred toList
  def size = _members.length
  def prob(x: Int) = if (pred(x)) 1.0 / size else 0.0
  override def toString: String = name
}
