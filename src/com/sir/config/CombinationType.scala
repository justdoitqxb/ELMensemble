package com.sir.config

object CombinationType extends Enumeration{
  type CombinationType = Value
  val Vote, WeightVote, Sum,  Average = Value 
  private val symbol = Array.apply("sum", "Sum", "vote", "Vote", "average", "Average", "weight", "weightVote", "WeightVote")
  
  private[sir] def fromString(name: String): CombinationType = name match { 
    case "sum" | "Sum" => Sum
    case "vote" | "Vote" => Vote
    case "average" | "Average" => Average
    case "weight" | "weightVote" | "WeightVote" => WeightVote
    case _ => throw new IllegalArgumentException(s"Did not recognize CombinationType name: $name. Symbol limited in " + symbol.mkString("[", ",", "]")) 
  } 
}