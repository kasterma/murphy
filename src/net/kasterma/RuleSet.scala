package net.kasterma

import scala.language.postfixOps

/**
  * Created by kasterma on 11/09/16.
  */

object RuleSet {
  def apply(rules: List[Rule]): RuleSet = {
    val unif_prob = 1.0 / rules.length
    new RuleSet(rules, rules map (x => unif_prob))
  }
  def apply(rules: List[Rule], probs: List[Double]): RuleSet = new RuleSet(rules, probs)

  def apply(rule: Rule, rules: Rule*) : RuleSet = apply(List(rule) ++ rules)
}

class RuleSet (val rules: List[Rule], val probs: List[Double]) {
  def update(x: Int): RuleSet = {
    val consistent_rules_wp = rules zip probs filter { case (rule, prob) => rule.prob(x) > 0 }
    val total_prob = consistent_rules_wp map { case (rule, prob) => prob } sum

    new RuleSet(consistent_rules_wp map {case (rule, prob) => rule },
      consistent_rules_wp map {case (rule, prob) => prob/total_prob})
  }

  override def toString: String = rules zip probs toString
}
