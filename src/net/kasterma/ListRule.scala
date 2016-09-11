package net.kasterma

/**
  * Created by kasterma on 11/09/16.
  */
object Rule {
  val TOTAL_NOS = 100
}

class ListRule (member: Int, members: Int*) {
  val _members: List[Int] = List(member) ++ members

  def size = _members.length
  def prob(x: Int) = if(_members contains x) Rule.TOTAL_NOS / size else 0
}
