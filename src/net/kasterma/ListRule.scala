package net.kasterma

/**
  * Created by kasterma on 11/09/16.
  */


class ListRule(member: Int, members: Int*) extends Rule {
val _members: List[Int] = List(member) ++ members

  def size: Int = _members.length
  def prob(x: Int): Double = if (_members contains x) 1.0 / size.toDouble else 0.0
  override def toString: String = _members.mkString(" ")
}
