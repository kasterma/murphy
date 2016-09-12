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

    var rs = RuleSet(r1, r2)
    println(rs.probs)
    println(rs)
    rs = rs.update(30)
    println(rs)

    val rs2 = RuleSet(new PredRule(x => x % 10 == 0, "multiples of 10"),
      new PredRule(x => x % 5 == 0, "multiples of 5"))
    var rs3 = rs.union(rs2, 0.5)
    println(rs3)
    rs3 = rs3.update(10)
    rs3 = rs3.update(10,20,30)
    println(rs3)
    println(rs3.prob(40))
    println(rs3.prob(50))
    println(rs3.prob(5))
    println(rs3.prob(6))
    println(1 to 100 map (x => rs3.prob(x)) sum)
    println(1 to 100 map (x => rs3.prob(x)))
  }
}
