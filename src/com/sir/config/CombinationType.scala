package com.sir.config

object CombinationType extends Enumeration{
  type CombinationType = Value
  val Sum, Vote, Average = Value 
  private val symbol = Array.apply("sum", "Sum", "vote", "Vote", "average", "Average")
  
  private[sir] def fromString(name: String): CombinationType = name match { 
    case "sum" | "Sum" => Sum
    case "vote" | "Vote" => Vote
    case "average" | "Average" => Average
    case _ => throw new IllegalArgumentException(s"Did not recognize CombinationType name: $name. Symbol limited in " + symbol.mkString("[", ",", "]")) 
  } 
}