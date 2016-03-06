package com.sir.config

object ELMType extends Enumeration{
  type ELMType = Value
  val Classification, Regression = Value 
  private val symbol = Array.apply("classification", "Classification", "CLASSIFICATION", "regression", "Regression", "REGRESSION")
  
  private[sir] def fromString(name: String): ELMType = name match { 
    case "classification" | "Classification" | "CLASSIFICATION" => Classification 
    case "regression" | "Regression" | "REGRESSION" => Regression 
    case _ => throw new IllegalArgumentException(s"Did not recognize ELMType name: $name. Symbol limited in " + symbol.mkString("[", ",", "]")) 
  } 
}