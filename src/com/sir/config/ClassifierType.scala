package com.sir.config

object ClassifierType extends Enumeration{
  type ClassifierType = Value
  val ELM, KernelELM, Mix = Value 
  private val symbol = Array.apply("elm", "ELM", "mix", "Mix")
  
  private[sir] def formString(name: String): ClassifierType = name match { 
    case "elm" | "ELM" => ELM
    case "mix" | "Mix" => Mix
    case "kernelelm" | "KernelELM" => KernelELM
    case _ => throw new IllegalArgumentException(s"Did not recognize StrategyType name: $name, symbol limited in " + symbol.mkString("[", ",", "]")) 
  } 
}