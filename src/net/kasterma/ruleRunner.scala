package net.kasterma

/**
  * Created by kasterma on 11/09/16.
  */
object ruleRunner {
  def main(args: Array[String]): Unit = {
    val r1 = new ListRule(1,2,3)
    println(r1.prob(2))
    println(r1.prob(5))

    val r2 = new PredRule((x: Int) => x % 30 == 0, "multiples of 30")
    println(r2.prob(1))
    println(r2.prob(80))
    println(1 to 100 map ((x:Int) => r2.prob(x)) groupBy ((x:Double) => x) mapValues (v => v.length))

    val rs = RuleSet(r1, r2)
    println(rs.probs)
    println(rs)
  }
}
