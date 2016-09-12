package net.kasterma

/**
  * Created by kasterma on 11/09/16.
  */

object RuleSet {
  def apply(rules: List[Rule]): RuleSet = {
    val unif_prob = 1.0 / rules.length
    new RuleSet(rules, rules map (x => unif_prob), List.empty)
  }
  def apply(rules: List[Rule], probs: List[Double]): RuleSet = new RuleSet(rules, probs, List.empty)

  def apply(rule: Rule, rules: Rule*) : RuleSet = apply(List(rule) ++ rules)
}

class RuleSet (val rules: List[Rule], val probs: List[Double], val seen: List[Int]) {
  def update(x: Int): RuleSet = {
    val consistent_rules_wp = rules zip probs filter { case (rule, prob) => rule.prob(x) > 0 }
    val consistent_rules_wl = consistent_rules_wp map {case (rule, prob) => (rule, prob * rule.prob(x))}
    val total_prob = consistent_rules_wl map { case (rule, prob) => prob } sum

    new RuleSet(consistent_rules_wp map {case (rule, prob) => rule },
      consistent_rules_wl map {case (rule, prob) => prob/total_prob},
      List(x) ++ seen)
  }

  def update(xs: Int*): RuleSet =  xs.foldLeft(this)((rs: RuleSet, x:Int) => rs.update(x))

  def prob(x: Int): Double = {
    rules zip probs map { case (rule, prob) => rule.prob(x) * prob } sum
  }

  def union(rs2: RuleSet, alpha: Double): RuleSet = {
    val new_probs = (probs map (x => (1-alpha) * x)) ++ (rs2.probs map (x => alpha * x))
    new RuleSet(rules ++ rs2.rules, new_probs, seen ++ rs2.seen)
  }

  override def toString: String = (rules zip probs toString) + ", seen: " + seen.toString
}
